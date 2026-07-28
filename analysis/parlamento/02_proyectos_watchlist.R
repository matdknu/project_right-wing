#!/usr/bin/env Rscript
# Proyectos de ley — watchlist -05 y repertorios en agenda legislativa
# Salidas: outputs/imagenes/canon_pdl_*.png

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

proyectos <- read_parquet(canon_path("proyectos.parquet", root)) |>
  mutate(fecha = as.Date(fecha))
votaciones <- read_parquet(canon_path("votaciones.parquet", root)) |>
  mutate(fecha = as.Date(fecha))
menciones <- read_parquet(canon_path("menciones_repertorio.parquet", root)) |>
  filter(tipo == "proyecto")

theme_df <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        panel.grid.minor = element_blank())

# Watchlist -05
wl <- proyectos |>
  filter(es_mensaje_ejecutivo == TRUE | es_prioritario == TRUE)

n_vot_wl <- votaciones |>
  filter(boletin %in% wl$boletin) |>
  count(boletin, name = "n_votaciones")

wl_plot <- wl |>
  left_join(n_vot_wl, by = "boletin") |>
  mutate(
    n_votaciones = replace_na(n_votaciones, 0L),
    label = paste0(boletin, "\n", str_trunc(coalesce(nombre, ""), 40))
  )

p_wl <- wl_plot |>
  ggplot(aes(fct_reorder(boletin, n_votaciones), n_votaciones,
             fill = es_prioritario)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "grey60", "TRUE" = "#922B21"),
                    labels = c("FALSE" = "Otros -05", "TRUE" = "Prioritario")) +
  labs(title = "Mensajes del Ejecutivo (-05) — votaciones en Cámara",
       x = NULL, y = "N° votaciones", fill = NULL) +
  theme_df

ggsave(file.path(out_fig, "canon_pdl_watchlist_votos.png"), p_wl, width = 9, height = 5, dpi = 150)

# Repertorios en proyectos (todos + -05)
proy_ann <- annotate_repertorios(proyectos)
rep_cols <- names(proy_ann)[grepl("^r_[ACD]", names(proy_ann))]

rep_all <- proy_ann |>
  summarise(across(all_of(rep_cols), ~ mean(.x) * 100)) |>
  pivot_longer(everything(), names_to = "col", values_to = "pct") |>
  mutate(codigo = sub("^r_", "", col), subset = "todos")

rep_05 <- proy_ann |>
  filter(es_mensaje_ejecutivo == TRUE) |>
  summarise(across(all_of(rep_cols), ~ mean(.x) * 100)) |>
  pivot_longer(everything(), names_to = "col", values_to = "pct") |>
  mutate(codigo = sub("^r_", "", col), subset = "mensajes_-05")

rep_cmp <- bind_rows(rep_all, rep_05) |>
  left_join(DICCIONARIO |> select(codigo, etiqueta, familia), by = "codigo")

p_rep <- rep_cmp |>
  filter(pct > 0) |>
  ggplot(aes(fct_reorder(codigo, pct), pct, fill = subset)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(title = "Repertorios en proyectos de ley",
       subtitle = "Códigos A–D en nombre/materia/ministerios",
       x = NULL, y = "% proyectos", fill = NULL) +
  theme_df

ggsave(file.path(out_fig, "canon_pdl_repertorios.png"), p_rep, width = 9, height = 5.5, dpi = 150)

# H4: A1 en mensajes -05
a1_05 <- proy_ann |>
  filter(es_mensaje_ejecutivo == TRUE) |>
  summarise(pct = mean(r_A1) * 100, n = n())

append_hipotesis(data.frame(
  hipotesis = "H4",
  indicador = "pct_proyectos05_A1",
  valor = round(a1_05$pct, 2),
  n = a1_05$n,
  fecha_corte = Sys.Date()
), root)

# Volumen votaciones por boletín prioritario en el tiempo
prio <- c("18216-05", "18296-05")
p_time <- votaciones |>
  filter(boletin %in% prio) |>
  mutate(mes = floor_date(fecha, "month")) |>
  count(mes, boletin) |>
  ggplot(aes(mes, n, fill = boletin)) +
  geom_col(position = "dodge", width = 20) +
  labs(title = "Votaciones mensuales — PDL y Endeudamiento",
       x = NULL, y = "Votaciones", fill = "Boletín") +
  theme_df

ggsave(file.path(out_fig, "canon_pdl_timeline.png"), p_time, width = 9, height = 5, dpi = 150)

cat("Proyectos:", nrow(proyectos), "| -05:", sum(proyectos$es_mensaje_ejecutivo, na.rm = TRUE), "\n")
cat("A1 en -05:", round(a1_05$pct, 1), "%\n")
cat("Figuras →", out_fig, "\n")
