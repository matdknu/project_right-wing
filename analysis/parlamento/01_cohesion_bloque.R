#!/usr/bin/env Rscript
# Cámara — cohesión bloque derecha (REP, UDI, RN, PNL) y convergencia REP–PNL (H3)
# Salidas: outputs/imagenes/canon_camara_*.png

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

votaciones <- read_parquet(canon_path("votaciones.parquet", root)) |>
  mutate(fecha = as.Date(fecha))
votos <- read_parquet(canon_path("votos.parquet", root))
dips <- read_parquet(canon_path("diputados.parquet", root))

# Normalizar partido
dips <- dips |>
  mutate(partido = toupper(str_trim(as.character(partido))))

votos_j <- votos |>
  left_join(dips |> select(diputado_id, partido), by = "diputado_id") |>
  filter(partido %in% BLOQUE_DERECHA) |>
  mutate(
    voto_bin = case_when(
      voto_norm == "a_favor" ~ 1L,
      voto_norm == "en_contra" ~ 0L,
      TRUE ~ NA_integer_
    )
  ) |>
  filter(!is.na(voto_bin))

# Rice por partido
rice <- votos_j |>
  group_by(votacion_id, partido) |>
  summarise(
    n = n(),
    p_si = mean(voto_bin),
    .groups = "drop"
  ) |>
  mutate(rice = abs(2 * p_si - 1)) |>
  group_by(partido) |>
  summarise(
    rice_medio = mean(rice),
    n_votaciones = n(),
    .groups = "drop"
  ) |>
  arrange(desc(rice_medio))

theme_df <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        panel.grid.minor = element_blank())

p_rice <- rice |>
  ggplot(aes(fct_reorder(partido, rice_medio), rice_medio, fill = partido)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.3f", rice_medio)), hjust = -0.1, size = 3.5) +
  coord_flip(ylim = c(0, 1.1)) +
  labs(title = "Cohesión Rice — bloque derecha 2026",
       subtitle = "1 = unanimidad; 0 = división 50/50",
       x = NULL, y = "Rice medio") +
  theme_df

ggsave(file.path(out_fig, "canon_camara_rice.png"), p_rice, width = 8, height = 5, dpi = 150)

# Convergencia REP–PNL: % votaciones donde moda REP == moda PNL
modas <- votos_j |>
  filter(partido %in% c("REP", "PNL")) |>
  group_by(votacion_id, partido) |>
  summarise(moda = as.integer(round(mean(voto_bin))), .groups = "drop") |>
  pivot_wider(names_from = partido, values_from = moda) |>
  filter(!is.na(REP), !is.na(PNL)) |>
  mutate(acuerdo = as.integer(REP == PNL))

conv_pct <- 100 * mean(modas$acuerdo)
n_conv <- nrow(modas)

p_conv <- modas |>
  count(acuerdo) |>
  mutate(label = if_else(acuerdo == 1, "Acuerdo REP–PNL", "Divergencia")) |>
  ggplot(aes(label, n, fill = label)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  labs(title = "Convergencia REP–PNL en votaciones 2026",
       subtitle = sprintf("Acuerdo = %.1f%% (n = %d)", conv_pct, n_conv),
       x = NULL, y = "Votaciones") +
  theme_df

ggsave(file.path(out_fig, "canon_camara_rep_pnl.png"), p_conv, width = 8, height = 5, dpi = 150)

# Rice mensual REP
rice_mes <- votos_j |>
  left_join(votaciones |> select(votacion_id, fecha), by = "votacion_id") |>
  filter(!is.na(fecha), partido == "REP") |>
  mutate(mes = floor_date(fecha, "month")) |>
  group_by(mes, votacion_id) |>
  summarise(rice = abs(2 * mean(voto_bin) - 1), .groups = "drop") |>
  group_by(mes) |>
  summarise(rice_medio = mean(rice), .groups = "drop")

p_mes <- rice_mes |>
  ggplot(aes(mes, rice_medio)) +
  geom_line(color = "#922B21", linewidth = 0.9) +
  geom_point(color = "#922B21") +
  ylim(0, 1) +
  labs(title = "Rice mensual — REP", x = NULL, y = "Rice") +
  theme_df

ggsave(file.path(out_fig, "canon_camara_rice_rep_mes.png"), p_mes, width = 9, height = 4.5, dpi = 150)

append_hipotesis(data.frame(
  hipotesis = c("H3", "H3"),
  indicador = c("convergencia_REP_PNL_pct", "rice_REP"),
  valor = round(c(conv_pct, rice$rice_medio[rice$partido == "REP"][1]), 2),
  n = c(n_conv, rice$n_votaciones[rice$partido == "REP"][1]),
  fecha_corte = Sys.Date()
), root)

cat("Rice por partido:\n")
print(rice)
cat(sprintf("Convergencia REP–PNL: %.1f%% (n=%d)\n", conv_pct, n_conv))
cat("Figuras →", out_fig, "\n")

