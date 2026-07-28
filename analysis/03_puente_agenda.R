#!/usr/bin/env Rscript
# Puente agenda: eventos críticos ±7 días (prensa + discursos + votos)
# Salidas: outputs/imagenes/canon_puente_*.png + resultados_hipotesis.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(arrow)
})

for (p in c("analysis/_helpers.R", "../_helpers.R", "analysis/_helpers.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}
for (p in c("analysis/_diccionario.R", "../_diccionario.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}

root <- project_root()
out_fig <- out_imagenes(root)

prensa <- read_parquet(canon_path("prensa.parquet", root)) |>
  mutate(fecha = as.Date(fecha))
discursos <- read_parquet(canon_path("discursos.parquet", root)) |>
  mutate(fecha = as.Date(fecha))
votaciones <- read_parquet(canon_path("votaciones.parquet", root)) |>
  mutate(fecha = as.Date(fecha))
menciones <- read_parquet(canon_path("menciones_repertorio.parquet", root)) |>
  mutate(fecha = as.Date(fecha))

theme_df <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        panel.grid.minor = element_blank())

ventana <- 7L

eco <- EVENTOS_CRITICOS |>
  mutate(
    ini = fecha - ventana,
    fin = fecha + ventana
  ) |>
  rowwise() |>
  mutate(
    n_prensa = sum(prensa$fecha >= ini & prensa$fecha <= fin, na.rm = TRUE),
    n_discursos = sum(discursos$fecha >= ini & discursos$fecha <= fin, na.rm = TRUE),
    n_votos_boletin = sum(
      votaciones$boletin == boletin &
        votaciones$fecha >= ini & votaciones$fecha <= fin,
      na.rm = TRUE
    ),
    n_votos_any = sum(votaciones$fecha >= ini & votaciones$fecha <= fin, na.rm = TRUE),
    pct_C_prensa = {
      ids <- prensa$unidad_id[prensa$fecha >= ini & prensa$fecha <= fin]
      m <- menciones |> filter(tipo == "prensa", unidad_id %in% ids, substr(codigo, 1, 1) == "C")
      if (!length(ids)) 0 else 100 * n_distinct(m$unidad_id) / length(ids)
    }
  ) |>
  ungroup()

eco_long <- eco |>
  select(label, n_prensa, n_discursos, n_votos_boletin, n_votos_any, pct_C_prensa) |>
  pivot_longer(-label, names_to = "serie", values_to = "valor")

p_eco <- eco_long |>
  filter(serie != "pct_C_prensa") |>
  ggplot(aes(label, valor, fill = serie)) +
  geom_col(position = "dodge", width = 0.7) +
  labs(
    title = "Eco mediático ↔ legislativo (±7 días)",
    subtitle = "Eventos críticos: PDL (2026-04-22) y Endeudamiento (2026-06-17)",
    x = NULL, y = "Conteo", fill = NULL
  ) +
  scale_fill_brewer(palette = "Set2",
                    labels = c(
                      n_discursos = "Discursos",
                      n_prensa = "Prensa",
                      n_votos_any = "Votaciones (todas)",
                      n_votos_boletin = "Votaciones boletín"
                    )) +
  theme_df

ggsave(file.path(out_fig, "canon_puente_eco.png"), p_eco, width = 10, height = 5.5, dpi = 150)

# Serie diaria prensa en ventana de cada evento (faceta)
daily <- map_dfr(seq_len(nrow(EVENTOS_CRITICOS)), function(i) {
  ev <- EVENTOS_CRITICOS[i, ]
  prensa |>
    filter(fecha >= ev$fecha - ventana, fecha <= ev$fecha + ventana) |>
    count(fecha, name = "n_prensa") |>
    mutate(evento = ev$label, evento_fecha = ev$fecha)
})

p_daily <- daily |>
  ggplot(aes(fecha, n_prensa)) +
  geom_col(fill = "#2E86AB", width = 0.9) +
  geom_vline(aes(xintercept = evento_fecha), linetype = "dashed", color = "#922B21") +
  facet_wrap(~evento, scales = "free_x") +
  labs(title = "Volumen diario de prensa en ventana del evento",
       x = NULL, y = "Artículos") +
  theme_df

ggsave(file.path(out_fig, "canon_puente_prensa_diaria.png"), p_daily, width = 11, height = 5, dpi = 150)

# Guardar eco tabular
write_csv(eco, canon_path("puente_eventos.csv", root))

append_hipotesis(data.frame(
  hipotesis = c("H2", "H2"),
  indicador = c(
    paste0("pct_C_prensa_ventana_", eco$evento_id[1]),
    paste0("pct_C_prensa_ventana_", eco$evento_id[2])
  ),
  valor = round(eco$pct_C_prensa, 2),
  n = eco$n_prensa,
  fecha_corte = Sys.Date()
), root)

cat("Puente eventos:\n")
print(eco |> select(label, n_prensa, n_discursos, n_votos_boletin, pct_C_prensa))
cat("Figuras →", out_fig, "\n")
cat("CSV →", canon_path("puente_eventos.csv", root), "\n")
cat("Hipótesis →", canon_path("resultados_hipotesis.csv", root), "\n")
