#!/usr/bin/env Rscript
# Mensajes ejecutivo 18216-05 (PDL) y 18296-05 (endeudamiento)
# Lee congreso.db → outputs/imagenes/
# Uso: Rscript analysis/parlamento/07_mensajes_pdl.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(RSQLite)
  library(scales)
  library(patchwork)
})

# Raíz del proyecto (funciona desde analysis/prensa o analysis/parlamento)
.source_helpers <- function() {
  candidates <- c(
    file.path("analysis", "_helpers.R"),
    file.path("..", "_helpers.R"),
    file.path("..", "..", "analysis", "_helpers.R")
  )
  for (p in candidates) {
    if (file.exists(p)) { source(p, local = FALSE); return(invisible(TRUE)) }
  }
  stop("No se encontró analysis/_helpers.R")
}
.source_helpers()
root <- project_root()

db_path <- file.path(root, "data/raw/congreso.db")
out_img <- file.path(root, "outputs/imagenes")
dir.create(out_img, recursive = TRUE, showWarnings = FALSE)

boletines <- c("18216-05", "18296-05")
partidos <- c("REP", "UDI", "RN", "PNL", "PDG", "IND", "FA", "PS", "PC")
colores <- c(
  REP = "#C0392B", UDI = "#2C5282", RN = "#4299E1", PNL = "#D69E2E",
  PDG = "#38A169", IND = "#718096", FA = "#805AD5", PS = "#E53E3E", PC = "#9B2C2C"
)

conn <- dbConnect(SQLite(), db_path)

votaciones <- dbGetQuery(conn, sprintf("
  SELECT votacion_id, boletin, nombre_proyecto, articulo, tipo_votacion,
         objeto_votacion, total_si, total_no, resultado,
         substr(fecha,1,10) AS fecha
  FROM votaciones
  WHERE boletin IN ('%s')
  ORDER BY boletin, fecha, votacion_id
", paste(boletines, collapse = "','"))) |> as_tibble()

votos <- dbGetQuery(conn, sprintf("
  SELECT v.votacion_id, d.partido, v.voto_norm AS voto
  FROM votos v
  JOIN diputados d ON v.diputado_id = d.diputado_id
  JOIN votaciones vot ON v.votacion_id = vot.votacion_id
  WHERE vot.boletin IN ('%s')
    AND v.voto_norm IN ('a_favor','en_contra')
    AND d.partido IN ('%s')
", paste(boletines, collapse = "','"), paste(partidos, collapse = "','"))) |> as_tibble()

dbDisconnect(conn)

votaciones <- votaciones |>
  mutate(
    clase = case_when(
      str_detect(str_to_lower(paste(articulo, objeto_votacion)), "indicaci") ~ "Indicación",
      tipo_votacion == "1" ~ "General",
      TRUE ~ "Artículo"
    ),
    proyecto_corto = if_else(
      boletin == "18216-05", "PDL reconstrucción", "Endeudamiento 2026"
    ),
    aprobada = total_si > total_no
  )

pct <- votos |>
  inner_join(votaciones |> select(votacion_id, boletin, clase, proyecto_corto), by = "votacion_id") |>
  group_by(boletin, proyecto_corto, clase, partido, votacion_id) |>
  summarise(pct_favor = mean(voto == "a_favor"), n = n(), .groups = "drop")

# Promedio por clase (indicaciones / general / artículo)
pct_clase <- pct |>
  group_by(boletin, proyecto_corto, clase, partido) |>
  summarise(
    pct_favor = mean(pct_favor),
    n_votaciones = n(),
    .groups = "drop"
  ) |>
  mutate(partido = factor(partido, levels = partidos))

# ── Panel: general vs indicación ──────────────────────────────────
p_comp <- pct_clase |>
  filter(clase %in% c("General", "Indicación")) |>
  ggplot(aes(partido, pct_favor, fill = clase)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  facet_wrap(~proyecto_corto, ncol = 1) +
  scale_fill_manual(values = c(General = "#2B6CB0", Indicación = "#C05621")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.05)) +
  labs(
    title = "Mensajes del Ejecutivo: general vs indicaciones",
    subtitle = "18216-05 (PDL) · 18296-05 (endeudamiento) — % a favor por bancada",
    x = NULL, y = "% a favor", fill = NULL,
    caption = "Fuente: congreso.db"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

# ── Detalle 18296: las 3 votaciones ───────────────────────────────
det_296 <- pct |>
  filter(boletin == "18296-05") |>
  left_join(
    votaciones |> select(votacion_id, total_si, total_no, clase_v = clase),
    by = "votacion_id"
  ) |>
  mutate(
    label_vot = case_when(
      clase_v == "General" ~ "General\n(94–52)",
      votacion_id == 89179 ~ "Indicación art. 2\n(73–73)",
      votacion_id == 89180 ~ "Indicación art. 3\n(72–74)",
      TRUE ~ as.character(votacion_id)
    ),
    partido = factor(partido, levels = partidos)
  )

p_296 <- ggplot(det_296, aes(partido, pct_favor, fill = partido)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  facet_wrap(~label_vot, nrow = 1) +
  scale_fill_manual(values = colores) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.05)) +
  labs(
    title = "18296-05 Endeudamiento — las 3 votaciones",
    subtitle = "Oficialismo aprueba el mensaje y tumba las indicaciones de la oposición",
    x = NULL, y = "% a favor"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# ── Heatmap PDL: partidos × clase ─────────────────────────────────
p_pdl <- pct_clase |>
  filter(boletin == "18216-05", clase %in% c("General", "Indicación", "Artículo")) |>
  mutate(
    clase = factor(clase, levels = c("General", "Artículo", "Indicación")),
    partido = factor(partido, levels = rev(partidos))
  ) |>
  ggplot(aes(clase, partido, fill = pct_favor)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f", pct_favor * 100)), size = 3.2, color = "white") +
  scale_fill_gradient2(low = "#C53030", mid = "#ECC94B", high = "#276749",
                       midpoint = 0.5, labels = percent_format(), name = "% a favor") +
  labs(
    title = "18216-05 PDL reconstrucción",
    subtitle = sprintf("%d votaciones · %d indicaciones",
                       sum(votaciones$boletin == "18216-05"),
                       sum(votaciones$boletin == "18216-05" & votaciones$clase == "Indicación")),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

# ── Acuerdo REP–PNL en ambos ──────────────────────────────────────
posicion <- function(v) names(sort(table(v), decreasing = TRUE))[1]

pos <- votos |>
  inner_join(votaciones |> select(votacion_id, boletin, clase), by = "votacion_id") |>
  group_by(votacion_id, boletin, clase, partido) |>
  summarise(pos = posicion(voto), .groups = "drop") |>
  pivot_wider(names_from = partido, values_from = pos)

acuerdo_par <- function(df, a, b) {
  ok <- df |> filter(!is.na(.data[[a]]), !is.na(.data[[b]]))
  if (!nrow(ok)) return(NA_real_)
  mean(ok[[a]] == ok[[b]])
}

resumen_pares <- pos |>
  group_by(boletin, clase) |>
  group_modify(~ tibble(
    REP_PNL = acuerdo_par(.x, "REP", "PNL"),
    REP_UDI = acuerdo_par(.x, "REP", "UDI"),
    REP_FA  = acuerdo_par(.x, "REP", "FA"),
    n = nrow(.x)
  )) |>
  ungroup()

# Guardar figuras
ggsave(file.path(out_img, "mensajes_18216_18296_general_vs_indicacion.png"),
       p_comp, width = 10, height = 8, dpi = 150, bg = "white")

ggsave(file.path(out_img, "endeudamiento_18296_detalle.png"),
       p_296, width = 11, height = 4.5, dpi = 150, bg = "white")

ggsave(file.path(out_img, "pdl_18216_heatmap.png"),
       p_pdl, width = 7, height = 5.5, dpi = 150, bg = "white")

combo <- p_comp / (p_296 | p_pdl) +
  plot_annotation(
    title = "Agenda ejecutivo Kast: PDL y endeudamiento",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
  )
ggsave(file.path(out_img, "mensajes_18216_18296.png"),
       combo, width = 13, height = 11, dpi = 150, bg = "white")

cat("PNG → outputs/imagenes/mensajes_18216_18296.png\n")
cat("PNG → outputs/imagenes/mensajes_18216_18296_general_vs_indicacion.png\n")
cat("PNG → outputs/imagenes/endeudamiento_18296_detalle.png\n")
cat("PNG → outputs/imagenes/pdl_18216_heatmap.png\n\n")

cat("── Acuerdo posicional ──\n")
print(resumen_pares |> mutate(across(where(is.numeric), ~ round(.x, 2))))

cat("\n── Generales ──\n")
votaciones |>
  filter(clase == "General") |>
  select(boletin, fecha, total_si, total_no, aprobada) |>
  print()

cat("\n── Indicaciones: aprobadas vs rechazadas ──\n")
votaciones |>
  filter(clase == "Indicación") |>
  count(boletin, aprobada) |>
  print()

cat("\nDone.\n")
