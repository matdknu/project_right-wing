#!/usr/bin/env Rscript
# Discursos presidenciales — repertorios A–D y co-ocurrencias H1
# Salidas: outputs/imagenes/canon_discursos_*.png

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(arrow)
})

for (p in c("analysis/_helpers.R", "../_helpers.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}
for (p in c("analysis/_diccionario.R", "../_diccionario.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}

root <- project_root()
out_fig <- out_imagenes(root)

disc <- read_parquet(canon_path("discursos.parquet", root)) |>
  mutate(fecha = as.Date(fecha)) |>
  filter(!is.na(fecha), fecha >= as.Date("2025-01-01"))

# Filtrar Kast si el título lo indica; si no, usar todos post-2025
kast_like <- disc |>
  filter(str_detect(titulo, regex("Kast|José Antonio|Jose Antonio", ignore_case = TRUE)) |
           fecha >= GOBIERNO_KAST_INICIO)
if (nrow(kast_like) < 10) kast_like <- disc

kast_like <- annotate_repertorios(kast_like)

theme_df <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        panel.grid.minor = element_blank())

rep_cols <- names(kast_like)[grepl("^r_[ACD]", names(kast_like))]
rep_sum <- kast_like |>
  summarise(across(all_of(rep_cols), ~ mean(.x) * 100)) |>
  pivot_longer(everything(), names_to = "col", values_to = "pct") |>
  mutate(codigo = sub("^r_", "", col)) |>
  left_join(DICCIONARIO |> select(codigo, familia, etiqueta), by = "codigo") |>
  arrange(desc(pct))

p_rep <- rep_sum |>
  ggplot(aes(fct_reorder(paste0(codigo, " ", etiqueta), pct), pct, fill = familia)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(title = "Repertorios A–D en discursos (Kast / 2025–26)",
       subtitle = paste0("n = ", nrow(kast_like), " discursos"),
       x = NULL, y = "% discursos", fill = NULL) +
  theme_df

ggsave(file.path(out_fig, "canon_discursos_repertorios.png"), p_rep, width = 10, height = 6, dpi = 150)

# Co-ocurrencias H1
co <- kast_like |>
  summarise(
    H1_mercado_familia = mean(co_H1_mercado_familia) * 100,
    H1_mercado_orden = mean(co_H1_mercado_orden) * 100,
    H2_mercado_radical = mean(co_H2_mercado_radical) * 100,
    H4_mercado_sin_A1 = mean(co_H4_mercado_sin_A1) * 100,
    C_sin_A = mean(co_C_sin_A) * 100
  ) |>
  pivot_longer(everything(), names_to = "indicador", values_to = "pct")

p_co <- co |>
  ggplot(aes(fct_reorder(indicador, pct), pct)) +
  geom_col(fill = "#1B4F72", width = 0.7) +
  coord_flip() +
  labs(title = "Co-ocurrencias guía (discursos)",
       subtitle = "H1 = mercado∩familia/orden; H2 = mercado∩radical; H4 = mercado sin A1",
       x = NULL, y = "% discursos") +
  theme_df

ggsave(file.path(out_fig, "canon_discursos_coocurrencias.png"), p_co, width = 9, height = 5, dpi = 150)

# Serie mensual
serie <- kast_like |>
  mutate(mes = floor_date(fecha, "month")) |>
  group_by(mes) |>
  summarise(
    A = mean(r_A1 | r_A2 | r_A3 | r_A4 | r_A5) * 100,
    C = mean(r_C1 | r_C2 | r_C3 | r_C4 | r_C5) * 100,
    .groups = "drop"
  ) |>
  pivot_longer(-mes, names_to = "familia", values_to = "pct")

p_serie <- serie |>
  ggplot(aes(mes, pct, color = familia)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(title = "Presencia mensual familia A vs C (discursos)",
       x = NULL, y = "% discursos con ≥1 código", color = NULL) +
  theme_df

ggsave(file.path(out_fig, "canon_discursos_serie.png"), p_serie, width = 10, height = 5, dpi = 150)

append_hipotesis(data.frame(
  hipotesis = c("H1", "H1", "H2", "H4"),
  indicador = c("pct_discursos_A5_A3", "pct_discursos_A5_A2",
                "pct_discursos_C_sin_A", "pct_discursos_A5_sin_A1"),
  valor = round(c(co$pct[co$indicador == "H1_mercado_familia"],
                  co$pct[co$indicador == "H1_mercado_orden"],
                  co$pct[co$indicador == "C_sin_A"],
                  co$pct[co$indicador == "H4_mercado_sin_A1"]), 2),
  n = nrow(kast_like),
  fecha_corte = Sys.Date()
), root)

cat("Discursos analizados:", nrow(kast_like), "\n")
print(co)
cat("Figuras →", out_fig, "\n")
