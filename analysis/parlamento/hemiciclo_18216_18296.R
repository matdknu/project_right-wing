#!/usr/bin/env Rscript
# Hemiciclos + polarización — 18216-05 (PDL) y 18296-05 (endeudamiento)
# Fuente: congreso.db → outputs/imagenes/ (+ HTML interactivo)
# Uso: Rscript analysis/parlamento/hemiciclo_18216_18296.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(RSQLite)
  library(scales)
  library(patchwork)
  library(plotly)
  library(htmlwidgets)
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
out_html <- file.path(root, "outputs/imagenes")
dir.create(out_img, recursive = TRUE, showWarnings = FALSE)

boletines <- c("18216-05", "18296-05")

orden_ideo <- c(
  "FA", "PC", "PS", "PPD", "DC", "PSC", "EVOP", "PL", "PDG",
  "IND", "PNL", "RN", "UDI", "REP", "FRVS", "PR", "DEM", "PAH"
)

colores_voto <- c(
  a_favor = "#22C55E", en_contra = "#EF4444",
  abstencion = "#EAB308", no_vota = "#64748B", dispensado = "#94A3B8"
)

colores_bloque <- c(Izquierda = "#7C3AED", Centro = "#64748B", Derecha = "#DC2626")

conn <- dbConnect(SQLite(), db_path)

votaciones <- dbGetQuery(conn, sprintf("
  SELECT votacion_id, boletin, nombre_proyecto, articulo, tipo_votacion,
         objeto_votacion, total_si, total_no, total_abstencion, resultado,
         substr(fecha,1,10) AS fecha
  FROM votaciones WHERE boletin IN ('%s')
  ORDER BY boletin, fecha, votacion_id
", paste(boletines, collapse = "','"))) |> as_tibble()

votos <- dbGetQuery(conn, sprintf("
  SELECT v.votacion_id, v.diputado_id, v.nombre_diputado,
         d.partido, d.apellido_paterno, v.voto_norm AS voto
  FROM votos v
  LEFT JOIN diputados d ON v.diputado_id = d.diputado_id
  JOIN votaciones vot ON v.votacion_id = vot.votacion_id
  WHERE vot.boletin IN ('%s')
", paste(boletines, collapse = "','"))) |> as_tibble()

dbDisconnect(conn)

votaciones <- votaciones |>
  mutate(
    clase = case_when(
      str_detect(str_to_lower(paste(coalesce(articulo, ""), coalesce(objeto_votacion, ""))),
                 "indicaci") ~ "Indicación",
      tipo_votacion == "1" ~ "General",
      TRUE ~ "Artículo"
    ),
    proyecto = if_else(boletin == "18216-05", "PDL reconstrucción", "Endeudamiento"),
    aprobada = total_si > total_no,
    margen = total_si - total_no,
    # Índice de polarización en sala: 1 = 50/50, 0 = unanimidad
    polarizacion_sala = 1 - abs(total_si - total_no) / pmax(total_si + total_no, 1)
  )

votos <- votos |>
  mutate(
    partido = if_else(is.na(partido) | partido == "", "IND", partido),
    apellido = coalesce(apellido_paterno, str_extract(nombre_diputado, "^\\S+")),
    bloque = case_when(
      partido %in% c("FA", "PC", "PS", "PPD", "DC", "PSC") ~ "Izquierda",
      partido %in% c("REP", "UDI", "RN", "PNL") ~ "Derecha",
      TRUE ~ "Centro"
    )
  )

# ── Layout hemiciclo fijo (izq → der = ideología) ─────────────────
diputados <- votos |>
  distinct(diputado_id, partido, apellido, bloque) |>
  mutate(partido_ord = factor(partido, levels = orden_ideo)) |>
  arrange(partido_ord, apellido) |>
  mutate(
    idx = row_number(),
    n = n(),
    theta = pi - (idx - 1) / pmax(n - 1, 1) * pi,
    # 3 filas tipo cámara
    fila = ((idx - 1) %% 3) + 1,
    radio = 0.72 + (fila - 1) * 0.14,
    x = radio * cos(theta),
    y = radio * sin(theta)
  )

layout_xy <- diputados |> select(diputado_id, x, y, partido_layout = partido, bloque_layout = bloque)

frame_votacion <- function(vid) {
  meta <- votaciones |> filter(votacion_id == vid)
  votos |>
    filter(votacion_id == vid) |>
    left_join(layout_xy, by = "diputado_id") |>
    mutate(
      voto_lab = recode(voto,
        a_favor = "A favor", en_contra = "En contra",
        abstencion = "Abstención", no_vota = "Ausente", dispensado = "Dispensado",
        .default = "Otro"
      ),
      titulo = meta$clase[1],
      boletin = meta$boletin[1],
      proyecto = meta$proyecto[1],
      resultado = sprintf("%d–%d", meta$total_si[1], meta$total_no[1]),
      polar = meta$polarizacion_sala[1],
      hover = paste0(
        apellido, " (", partido, ")<br>",
        voto_lab, "<br>", meta$boletin[1], " · ", meta$clase[1]
      )
    )
}

plot_hemi <- function(fr, titulo = NULL, sub = NULL, show_leg = FALSE) {
  ggplot(fr, aes(x, y, color = voto)) +
    # Arco guía
    annotate("path",
             x = cos(seq(0, pi, length.out = 80)),
             y = sin(seq(0, pi, length.out = 80)),
             color = "#CBD5E1", linewidth = 0.4) +
    geom_point(size = 2.6, alpha = 0.92) +
    scale_color_manual(values = colores_voto, name = NULL,
                       labels = c(a_favor = "A favor", en_contra = "En contra",
                                  abstencion = "Abstención", no_vota = "Ausente",
                                  dispensado = "Dispensado")) +
    coord_fixed(xlim = c(-1.15, 1.15), ylim = c(-0.15, 1.2)) +
    labs(title = titulo, subtitle = sub) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12, color = "#F1F5F9"),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "#94A3B8"),
      legend.position = if (show_leg) "bottom" else "none",
      plot.background = element_rect(fill = "#0B1220", color = NA),
      legend.text = element_text(color = "#E2E8F0"),
      legend.background = element_rect(fill = "#0B1220", color = NA)
    )
}

# ══════════════════════════════════════════════════════════════════
# 1. ENDEUDAMIENTO 18296 — 3 hemiciclos (general ↔ flip)
# ══════════════════════════════════════════════════════════════════
ids_296 <- c(89178L, 89179L, 89180L)
labs_296 <- c("GENERAL\n94–52 aprobado", "INDICACIÓN art. 2\n73–73 empate", "INDICACIÓN art. 3\n72–74 rechazada")

plots_296 <- map2(ids_296, labs_296, function(vid, lab) {
  fr <- frame_votacion(vid)
  plot_hemi(fr, titulo = lab, sub = paste("polarización sala:", round(unique(fr$polar), 2)))
})

p_296 <- wrap_plots(plots_296, nrow = 1) +
  plot_annotation(
    title = "18296-05 Endeudamiento — el flip del hemiciclo",
    subtitle = "Misma cámara · izquierda←centro→derecha · verde = a favor · rojo = en contra",
    caption = "En general: derecha a favor. En indicaciones: se invierten los bloques.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5, color = "#F1F5F9"),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#94A3B8"),
      plot.caption = element_text(hjust = 0.5, size = 9, color = "#64748B"),
      plot.background = element_rect(fill = "#0B1220", color = NA)
    )
  )

ggsave(file.path(out_img, "hemiciclo_18296_polarizacion.png"),
       p_296, width = 15, height = 5.8, dpi = 160, bg = "#0B1220")
cat("→ hemiciclo_18296_polarizacion.png\n")

# ══════════════════════════════════════════════════════════════════
# 2. PDL 18216 — general + indicación típica + artículo
# ══════════════════════════════════════════════════════════════════
gen_216 <- votaciones |> filter(boletin == "18216-05", clase == "General") |> pull(votacion_id)
ind_216 <- votaciones |>
  filter(boletin == "18216-05", clase == "Indicación") |>
  arrange(desc(polarizacion_sala)) |>
  slice_head(n = 2) |>
  pull(votacion_id)

# Si hay pocos, tomar los primeros
if (length(ind_216) < 2) {
  ind_216 <- votaciones |> filter(boletin == "18216-05", clase == "Indicación") |>
    slice_head(n = 2) |> pull(votacion_id)
}

ids_216 <- c(gen_216[1], ind_216)
metas_216 <- votaciones |> filter(votacion_id %in% ids_216) |>
  mutate(lab = paste0(clase, "\n", total_si, "–", total_no))

plots_216 <- map(ids_216, function(vid) {
  m <- metas_216 |> filter(votacion_id == vid)
  fr <- frame_votacion(vid)
  plot_hemi(fr,
            titulo = m$lab[1],
            sub = paste0("id ", vid, " · polar=", round(m$polarizacion_sala[1], 2)))
})

p_216 <- wrap_plots(plots_216, nrow = 1) +
  plot_annotation(
    title = "18216-05 PDL reconstrucción — general vs indicaciones más polarizadas",
    subtitle = "Hemiciclo ideológico · el color se invierte entre general e indicación",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#F1F5F9"),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#94A3B8"),
      plot.background = element_rect(fill = "#0B1220", color = NA)
    )
  )

ggsave(file.path(out_img, "hemiciclo_18216_polarizacion.png"),
       p_216, width = 14, height = 5.5, dpi = 160, bg = "#0B1220")
cat("→ hemiciclo_18216_polarizacion.png\n")

# ══════════════════════════════════════════════════════════════════
# 3. Índice de polarización a lo largo del PDL (90 votaciones)
# ══════════════════════════════════════════════════════════════════
# Polarización de bloques: |%favor_derecha − %favor_izquierda|
bloc_pol <- votos |>
  filter(voto %in% c("a_favor", "en_contra"), bloque %in% c("Izquierda", "Derecha")) |>
  inner_join(votaciones |> select(votacion_id, boletin, clase, fecha, polarizacion_sala, margen),
             by = "votacion_id") |>
  group_by(votacion_id, boletin, clase, fecha, bloque) |>
  summarise(pct_favor = mean(voto == "a_favor"), .groups = "drop") |>
  pivot_wider(names_from = bloque, values_from = pct_favor) |>
  mutate(
    polar_bloques = abs(Derecha - Izquierda),
    alineacion = if_else(Derecha > 0.5 & Izquierda < 0.5, "Der↑ Izq↓",
                  if_else(Izquierda > 0.5 & Derecha < 0.5, "Izq↑ Der↓", "Mixto"))
  )

p_pol_series <- bloc_pol |>
  filter(boletin == "18216-05") |>
  arrange(fecha, votacion_id) |>
  mutate(orden = row_number()) |>
  ggplot(aes(orden, polar_bloques, color = clase, fill = clase)) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "#64748B", linewidth = 0.4) +
  geom_segment(aes(x = orden, xend = orden, y = 0, yend = polar_bloques), alpha = 0.35) +
  geom_point(size = 2.2, alpha = 0.9) +
  scale_color_manual(values = c(General = "#38BDF8", Indicación = "#F97316", Artículo = "#A78BFA")) +
  scale_fill_manual(values = c(General = "#38BDF8", Indicación = "#F97316", Artículo = "#A78BFA")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.05)) +
  labs(
    title = "Polarización de bloques a lo largo del PDL (18216-05)",
    subtitle = "|% a favor derecha − % a favor izquierda| · 1 = bloques espejo",
    x = "Orden de votación en el proyecto",
    y = "Polarización entre bloques",
    color = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#F1F5F9"),
    plot.subtitle = element_text(color = "#94A3B8"),
    plot.background = element_rect(fill = "#0B1220", color = NA),
    panel.background = element_rect(fill = "#0B1220", color = NA),
    panel.grid.major = element_line(color = "#1E293B"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#94A3B8"),
    axis.title = element_text(color = "#CBD5E1"),
    legend.position = "top",
    legend.text = element_text(color = "#E2E8F0"),
    legend.background = element_blank()
  )

ggsave(file.path(out_img, "polarizacion_serie_18216.png"),
       p_pol_series, width = 12, height = 5, dpi = 160, bg = "#0B1220")
cat("→ polarizacion_serie_18216.png\n")

# ══════════════════════════════════════════════════════════════════
# 4. Espectro: % a favor por partido (general vs indicación) — polar
# ══════════════════════════════════════════════════════════════════
orden_plot <- c("FA", "PC", "PS", "PDG", "IND", "PNL", "RN", "UDI", "REP")

espectro <- votos |>
  filter(voto %in% c("a_favor", "en_contra"), partido %in% orden_plot) |>
  inner_join(votaciones |> select(votacion_id, boletin, clase, proyecto), by = "votacion_id") |>
  filter(clase %in% c("General", "Indicación")) |>
  group_by(boletin, proyecto, clase, partido) |>
  summarise(pct_favor = mean(voto == "a_favor"), .groups = "drop") |>
  mutate(
    partido = factor(partido, levels = orden_plot),
    # Ángulo tipo hemiciclo para geom_segment radial
    idx = as.integer(partido),
    theta = pi - (idx - 1) / (length(orden_plot) - 1) * pi,
    x_end = pct_favor * cos(theta),
    y_end = pct_favor * sin(theta)
  )

p_espectro <- ggplot(espectro, aes(color = clase)) +
  annotate("path", x = cos(seq(0, pi, length.out = 60)), y = sin(seq(0, pi, length.out = 60)),
           color = "#334155", linewidth = 0.5) +
  annotate("path", x = 0.5 * cos(seq(0, pi, length.out = 60)), y = 0.5 * sin(seq(0, pi, length.out = 60)),
           color = "#1E293B", linewidth = 0.4, linetype = "dashed") +
  geom_segment(aes(x = 0, y = 0, xend = x_end, yend = y_end), linewidth = 1.4, alpha = 0.85) +
  geom_point(aes(x = x_end, y = y_end), size = 3) +
  geom_text(
    data = espectro |> filter(clase == "General", boletin == "18216-05") |> distinct(partido, theta),
    aes(x = 1.12 * cos(theta), y = 1.12 * sin(theta), label = partido),
    inherit.aes = FALSE, color = "#E2E8F0", size = 3, fontface = "bold"
  ) +
  facet_wrap(~proyecto) +
  scale_color_manual(values = c(General = "#38BDF8", Indicación = "#F97316")) +
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.1, 1.35)) +
  labs(
    title = "Espectro de voto (hemiciclo partidario)",
    subtitle = "Longitud del rayo = % a favor · azul = general · naranja = indicaciones",
    color = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#F1F5F9"),
    plot.subtitle = element_text(hjust = 0.5, color = "#94A3B8", size = 9),
    strip.text = element_text(face = "bold", color = "#E2E8F0", size = 11),
    legend.position = "top",
    legend.text = element_text(color = "#E2E8F0"),
    plot.background = element_rect(fill = "#0B1220", color = NA)
  )

ggsave(file.path(out_img, "espectro_polar_18216_18296.png"),
       p_espectro, width = 12, height = 6.5, dpi = 160, bg = "#0B1220")
cat("→ espectro_polar_18216_18296.png\n")

# ══════════════════════════════════════════════════════════════════
# 5. Comparación polarización media por tipo
# ══════════════════════════════════════════════════════════════════
resumen_pol <- bloc_pol |>
  group_by(boletin, clase) |>
  summarise(
    polar_media = mean(polar_bloques, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(
    proyecto = if_else(boletin == "18216-05", "PDL", "Endeudamiento"),
    label = sprintf("%.0f%%", polar_media * 100)
  )

p_resumen <- ggplot(resumen_pol, aes(clase, polar_media, fill = clase)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = label), vjust = -0.4, color = "#F1F5F9", fontface = "bold", size = 4) +
  facet_wrap(~proyecto) +
  scale_fill_manual(values = c(General = "#38BDF8", Indicación = "#F97316", Artículo = "#A78BFA")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.15)) +
  labs(
    title = "Polarización media entre bloques (Der vs Izq)",
    subtitle = "En indicaciones la cámara se parte; en general también, pero con signos invertidos",
    x = NULL, y = "Polarización media"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#F1F5F9"),
    plot.subtitle = element_text(color = "#94A3B8", size = 9),
    plot.background = element_rect(fill = "#0B1220", color = NA),
    panel.background = element_rect(fill = "#0B1220", color = NA),
    panel.grid = element_line(color = "#1E293B"),
    strip.text = element_text(face = "bold", color = "#E2E8F0"),
    axis.text = element_text(color = "#94A3B8"),
    axis.title = element_text(color = "#CBD5E1")
  )

ggsave(file.path(out_img, "polarizacion_bloques_18216_18296.png"),
       p_resumen, width = 9, height = 5, dpi = 160, bg = "#0B1220")
cat("→ polarizacion_bloques_18216_18296.png\n")

# ══════════════════════════════════════════════════════════════════
# 6. Panel resumen + HTML interactivo
# ══════════════════════════════════════════════════════════════════
combo <- (p_296) / (p_216) / (p_espectro | p_resumen) +
  plot_layout(heights = c(1.1, 1, 1.1))

ggsave(file.path(out_img, "panel_polarizacion_18216_18296.png"),
       combo, width = 15, height = 16, dpi = 140, bg = "#0B1220")
cat("→ panel_polarizacion_18216_18296.png\n")

# HTML: dropdown solo de estos dos boletines
all_ids <- votaciones$votacion_id
# Priorizar: generales + indicaciones + top polarización
priority <- votaciones |>
  arrange(boletin, desc(clase == "General"), desc(polarizacion_sala)) |>
  slice_head(n = 35)

frames <- map(priority$votacion_id, frame_votacion) |> list_rbind()

first <- priority$votacion_id[1]
fr0 <- frames |> filter(votacion_id == first)

p0 <- plot_ly(
  fr0, x = ~x, y = ~y,
  type = "scatter", mode = "markers",
  marker = list(
    size = 12,
    color = recode(fr0$voto,
      a_favor = "#22C55E", en_contra = "#EF4444",
      abstencion = "#EAB308", no_vota = "#64748B", dispensado = "#94A3B8",
      .default = "#64748B"
    ),
    line = list(width = 0.4, color = "#0F172A")
  ),
  text = ~hover, hoverinfo = "text"
) |>
  layout(
    title = list(
      text = paste0("<b>Hemiciclo · ", priority$proyecto[1], " · ", priority$clase[1], "</b><br>",
                    "<sup>", first, " · ", priority$total_si[1], "–", priority$total_no[1],
                    " · polar=", round(priority$polarizacion_sala[1], 2), "</sup>"),
      font = list(size = 14, color = "#F1F5F9")
    ),
    xaxis = list(visible = FALSE, scaleanchor = "y"),
    yaxis = list(visible = FALSE),
    paper_bgcolor = "#0B1220",
    plot_bgcolor = "#0B1220",
    margin = list(t = 80),
    height = 580, width = 900
  )

buttons <- map(seq_len(nrow(priority)), function(i) {
  vid <- priority$votacion_id[i]
  fr <- frames |> filter(votacion_id == vid)
  cols <- recode(fr$voto,
    a_favor = "#22C55E", en_contra = "#EF4444",
    abstencion = "#EAB308", no_vota = "#64748B", dispensado = "#94A3B8",
    .default = "#64748B"
  )
  list(
    method = "update",
    args = list(
      list(
        x = list(fr$x), y = list(fr$y), text = list(fr$hover),
        marker = list(color = list(cols), size = 12,
                      line = list(width = 0.4, color = "#0F172A"))
      ),
      list(title = list(
        text = paste0(
          "<b>Hemiciclo · ", priority$proyecto[i], " · ", priority$clase[i], "</b><br>",
          "<sup>", vid, " · ", priority$total_si[i], "–", priority$total_no[i],
          " · polar=", round(priority$polarizacion_sala[i], 2), "</sup>"
        )
      ))
    ),
    label = sprintf("%s · %s · %d–%d",
                    priority$boletin[i], priority$clase[i],
                    priority$total_si[i], priority$total_no[i])
  )
})

p_html <- p0 |>
  layout(updatemenus = list(list(
    type = "dropdown", x = 0.01, y = 1.15,
    bgcolor = "#1E293B", bordercolor = "#334155",
    font = list(color = "#E2E8F0", size = 11),
    buttons = buttons
  )))

html_path <- file.path(out_html, "hemiciclo_18216_18296.html")
saveWidget(p_html, html_path, selfcontained = TRUE)
cat("→ hemiciclo_18216_18296.html (interactivo)\n")

# Consola
cat("\n── Polarización media Der–Izq ──\n")
print(as.data.frame(resumen_pol))
cat("\nDone.\n")
