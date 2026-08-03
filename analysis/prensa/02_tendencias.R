#!/usr/bin/env Rscript
# Análisis de tendencias 2026 — prensa (total) + repertorios A–D + cámara
# Preferencia: data/processed/canon/prensa.parquet
#
# Uso:
#   Rscript analysis/prensa/02_tendencias.R
# Salidas: outputs/imagenes/tendencia_*.png

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(arrow)
  library(stringi)
})

for (p in c("analysis/_helpers.R", "../_helpers.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}
for (p in c("analysis/_diccionario.R", "../_diccionario.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}

root <- project_root()
out_fig <- out_imagenes(root)

theme_ng <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# ── Datos ─────────────────────────────────────────────────────────
path_canon <- canon_path("prensa.parquet", root)
if (file.exists(path_canon)) {
  message(">>> Prensa canónica: ", path_canon)
  prensa <- read_parquet(path_canon) |>
    mutate(
      fecha = as.Date(fecha),
      fuente = tolower(as.character(fuente)),
      texto = coalesce(texto, paste(titulo, bajada, cuerpo))
    )
} else {
  message(">>> Fallback unificada")
  prensa <- read_parquet(prensa_unificada_path(root)) |>
    mutate(
      fecha = as.Date(fecha),
      fuente = tolower(as.character(fuente)),
      texto = paste(coalesce(titulo, ""), coalesce(bajada, ""), coalesce(cuerpo, ""))
    )
}

prensa <- prensa |>
  filter(!is.na(fecha), fecha >= as.Date("2026-01-01")) |>
  mutate(
    semana = floor_date(fecha, "week", week_start = 1),
    mes = floor_date(fecha, "month"),
    txt = fold_text(texto)
  )

menciones <- tryCatch(
  read_parquet(canon_path("menciones_repertorio.parquet", root)) |>
    mutate(fecha = as.Date(fecha)) |>
    filter(tipo == "prensa", !is.na(fecha), fecha >= as.Date("2026-01-01")),
  error = function(e) tibble()
)

votaciones <- tryCatch(
  read_parquet(canon_path("votaciones.parquet", root)) |>
    mutate(fecha = as.Date(fecha)),
  error = function(e) tibble()
)

message("Artículos: ", nrow(prensa), " | ", min(prensa$fecha), " → ", max(prensa$fecha))
print(count(prensa, fuente, sort = TRUE))

# Actores / temas
ACT <- tribble(
  ~slug, ~label, ~pat,
  "kast", "Kast", "\\bkast\\b",
  "republicanos", "Republicanos", "republican",
  "udi", "UDI", "\\budi\\b",
  "seguridad", "Seguridad", "seguridad|delincuencia|narcotrafico",
  "migracion", "Migración", "migracion|migrantes|frontera",
  "pdl", "PDL/reconstrucción", "\\bpdl\\b|megarreforma|reconstruccion"
)

for (i in seq_len(nrow(ACT))) {
  prensa[[paste0("a_", ACT$slug[i])]] <- str_detect(prensa$txt, ACT$pat[i])
}

# ── 1) Volumen semanal por fuente ─────────────────────────────────
vol <- prensa |> count(semana, fuente, name = "n")

p1 <- vol |>
  filter(fuente != "emol") |>
  ggplot(aes(semana, n, color = fuente)) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.2) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Tendencia — volumen semanal de prensa (sin EMOL)",
    subtitle = "Corpus total 2026, sin filtro derecha",
    x = NULL, y = "Artículos / semana", color = NULL
  ) +
  theme_ng + theme(legend.text = element_text(size = 8))

ggsave(file.path(out_fig, "tendencia_volumen_fuentes.png"), p1, width = 11, height = 5.5, dpi = 150)

p1b <- vol |>
  filter(fuente == "emol") |>
  ggplot(aes(semana, n)) +
  geom_line(linewidth = 0.9, color = "#1B4F72") +
  geom_point(size = 1.4, color = "#1B4F72") +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Tendencia — volumen semanal EMOL", x = NULL, y = "Artículos / semana") +
  theme_ng

ggsave(file.path(out_fig, "tendencia_volumen_emol.png"), p1b, width = 10, height = 4.5, dpi = 150)

# ── 2) Actores / temas (% semanal, medios principales) ────────────
fuentes_focus <- intersect(c("emol", "biobio", "meganoticias", "theclinic"), unique(prensa$fuente))
actor_cols <- paste0("a_", ACT$slug)

sem_act <- prensa |>
  filter(fuente %in% fuentes_focus, !is.na(semana)) |>
  group_by(semana) |>
  summarise(across(all_of(actor_cols), ~ 100 * mean(.x)), n = n(), .groups = "drop") |>
  pivot_longer(all_of(actor_cols), names_to = "slug", values_to = "pct") |>
  mutate(slug = sub("^a_", "", slug)) |>
  left_join(ACT |> select(slug, label), by = "slug")

p2 <- sem_act |>
  filter(n >= 30) |>  # semanas con masa mínima
  ggplot(aes(semana, pct, color = label)) +
  geom_line(linewidth = 0.85) +
  labs(
    title = "Tendencia — menciones en prensa (% artículos / semana)",
    subtitle = paste("Fuentes:", paste(fuentes_focus, collapse = ", ")),
    x = NULL, y = "% artículos", color = NULL
  ) +
  theme_ng

ggsave(file.path(out_fig, "tendencia_actores.png"), p2, width = 11, height = 5.5, dpi = 150)

# Faceta por tema clave
p2b <- sem_act |>
  filter(n >= 30, slug %in% c("kast", "seguridad", "migracion", "pdl", "republicanos")) |>
  ggplot(aes(semana, pct)) +
  geom_area(alpha = 0.35, fill = "#2E86AB") +
  geom_line(color = "#1B4F72", linewidth = 0.7) +
  facet_wrap(~label, scales = "free_y", ncol = 2) +
  labs(
    title = "Tendencia por tema clave",
    x = NULL, y = "% artículos / semana"
  ) +
  theme_ng + theme(legend.position = "none")

ggsave(file.path(out_fig, "tendencia_temas_facet.png"), p2b, width = 11, height = 7, dpi = 150)

# ── 3) Repertorios A vs C (mensual) ───────────────────────────────
if (nrow(menciones)) {
  ids_prensa <- unique(prensa$unidad_id)
  fam <- menciones |>
    filter(unidad_id %in% ids_prensa) |>
    mutate(
      mes = floor_date(fecha, "month"),
      familia = substr(codigo, 1, 1)
    ) |>
    count(mes, familia, name = "hits") |>
    group_by(mes) |>
    mutate(pct = 100 * hits / sum(hits)) |>
    ungroup()

  p3 <- fam |>
    filter(familia %in% c("A", "C", "D")) |>
    ggplot(aes(mes, pct, fill = familia)) +
    geom_area(alpha = 0.9) +
    scale_fill_manual(values = c(A = "#1B4F72", C = "#922B21", D = "#B9770E")) +
    labs(
      title = "Tendencia — composición de repertorios (menciones)",
      subtitle = "A = gremialista · C = derecha radical · D = libertario",
      x = NULL, y = "% menciones del mes", fill = NULL
    ) +
    theme_ng

  ggsave(file.path(out_fig, "tendencia_repertorios_AC.png"), p3, width = 10, height = 5, dpi = 150)

  # Códigos individuales top
  top_cod <- menciones |>
    filter(unidad_id %in% ids_prensa) |>
    mutate(mes = floor_date(fecha, "month")) |>
    count(mes, codigo) |>
    group_by(mes) |>
    mutate(pct = 100 * n / sum(n)) |>
    ungroup() |>
    filter(codigo %in% c("A1", "A2", "A5", "C1", "C2", "C3"))

  p3b <- top_cod |>
    ggplot(aes(mes, pct, color = codigo)) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 1.5) +
    labs(
      title = "Tendencia — códigos A–C seleccionados (% menciones / mes)",
      x = NULL, y = "%", color = NULL
    ) +
    theme_ng

  ggsave(file.path(out_fig, "tendencia_codigos_detalle.png"), p3b, width = 10, height = 5.5, dpi = 150)
}

# ── 4) Cámara: volumen de votaciones semanal ──────────────────────
if (nrow(votaciones)) {
  v_sem <- votaciones |>
    filter(!is.na(fecha)) |>
    mutate(semana = floor_date(fecha, "week", week_start = 1)) |>
    count(semana, name = "n")

  p4 <- v_sem |>
    ggplot(aes(semana, n)) +
    geom_col(fill = "#2E86AB", width = 5) +
    labs(
      title = "Tendencia — votaciones en Cámara (semanal)",
      x = NULL, y = "Votaciones"
    ) +
    theme_ng

  ggsave(file.path(out_fig, "tendencia_camara_volumen.png"), p4, width = 10, height = 4.5, dpi = 150)

  # -05 vs resto
  v05 <- votaciones |>
    filter(!is.na(fecha)) |>
    mutate(
      semana = floor_date(fecha, "week", week_start = 1),
      tipo = if_else(str_detect(as.character(boletin), "-05$"), "Mensaje -05", "Otros")
    ) |>
    count(semana, tipo)

  p4b <- v05 |>
    ggplot(aes(semana, n, fill = tipo)) +
    geom_col(position = "stack", width = 5) +
    scale_fill_manual(values = c("Mensaje -05" = "#922B21", "Otros" = "grey70")) +
    labs(
      title = "Tendencia — votaciones: mensajes ejecutivo vs resto",
      x = NULL, y = "Votaciones", fill = NULL
    ) +
    theme_ng

  ggsave(file.path(out_fig, "tendencia_camara_05.png"), p4b, width = 10, height = 4.5, dpi = 150)
}

# ── Resumen consola ───────────────────────────────────────────────
cat("\n=== RESUMEN TENDENCIAS 2026 ===\n")
cat("Prensa:", nrow(prensa), "artículos\n")
ultimas <- prensa |>
  filter(semana >= max(semana, na.rm = TRUE) - 21) |>
  summarise(across(all_of(actor_cols), ~ round(100 * mean(.x), 1)))
cat("Últimas 4 semanas (% artículos):\n")
print(ultimas)
cat("\nFiguras →", out_fig, "/tendencia_*.png\n")
