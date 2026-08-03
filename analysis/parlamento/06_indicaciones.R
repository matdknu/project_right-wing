#!/usr/bin/env Rscript
# Indicaciones parlamentarias: leyes, comparación general vs indicación, hemiciclo interactivo
# Uso: Rscript analysis/parlamento/06_indicaciones.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(RSQLite)
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

db_path   <- file.path(root, "data/raw/congreso.db")
out_ind   <- file.path(root, "outputs/volatilidad/indicaciones")
out_img   <- file.path(root, "outputs/imagenes")
dir.create(out_ind, recursive = TRUE, showWarnings = FALSE)
dir.create(out_img, recursive = TRUE, showWarnings = FALSE)

partidos_foco <- c("REP", "UDI", "RN", "PNL", "PDG", "FA", "PS", "PC", "IND")
partidos_todos <- c(partidos_foco, "PPD", "DC", "PSC", "PL", "EVOP", "FRVS", "PR", "DEM", "PAH")

etiquetas <- c(
  REP = "Republicano", UDI = "UDI", RN = "RN", PNL = "PNL",
  PDG = "PDG", IND = "Independientes", FA = "Frente Amplio",
  PS = "Socialista", PC = "Comunista", PPD = "PPD", DC = "DC"
)

orden_ideologico <- c(
  "FA", "PC", "PS", "PPD", "DC", "PSC", "EVOP", "PL", "PDG", "DEM", "PAH",
  "IND", "PNL", "RN", "UDI", "REP", "FRVS", "PR"
)

colores_partido <- c(
  REP = "#E53E3E", UDI = "#2B6CB0", RN = "#4299E1", PNL = "#D69E2E",
  PDG = "#38A169", IND = "#718096", FA = "#805AD5", PS = "#E53E3E",
  PC = "#C53030", PPD = "#F6AD55", DC = "#48BB78", PSC = "#FC8181",
  PL = "#9F7AEA", EVOP = "#4FD1C5", FRVS = "#A0AEC0", PR = "#CBD5E0",
  DEM = "#68D391", PAH = "#B794F4"
)

colores_voto <- c(
  a_favor = "#38A169", en_contra = "#E53E3E",
  abstencion = "#ECC94B", no_vota = "#A0AEC0", dispensado = "#CBD5E0"
)

# ── Cargar datos ──────────────────────────────────────────────────
conn <- dbConnect(SQLite(), db_path)

votaciones <- dbGetQuery(conn, "
  SELECT votacion_id, boletin, nombre_proyecto, descripcion, fecha,
         articulo, tipo_votacion, objeto_votacion,
         total_si, total_no, total_abstencion, resultado
  FROM votaciones
") |> as_tibble()

votos <- dbGetQuery(conn, "
  SELECT v.votacion_id, v.diputado_id, v.nombre_diputado,
         d.partido, d.apellido_paterno, v.voto_norm AS voto
  FROM votos v
  LEFT JOIN diputados d ON v.diputado_id = d.diputado_id
") |> as_tibble()

dbDisconnect(conn)

votaciones <- votaciones |>
  mutate(
    fecha = as.Date(substr(fecha, 1, 10)),
    es_indicacion = str_detect(str_to_lower(paste(articulo, objeto_votacion)), "indicaci"),
    es_general = tipo_votacion == "1" | str_detect(str_to_lower(tipo_votacion), "general"),
    clase_voto = case_when(
      es_indicacion ~ "Indicación",
      es_general ~ "General",
      !is.na(articulo) & articulo != "" ~ "Artículo",
      TRUE ~ "Otro"
    ),
    proyecto_corto = str_trunc(coalesce(nombre_proyecto, descripcion, boletin), 70),
    aprobada = total_si > total_no
  )

votos <- votos |>
  mutate(
    partido = if_else(partido %in% partidos_todos, partido, "IND"),
    apellido = coalesce(apellido_paterno, str_extract(nombre_diputado, "^\\S+"))
  )

cat(sprintf("Votaciones: %d | Indicaciones: %d\n",
            nrow(votaciones), sum(votaciones$es_indicacion)))

# ── Catálogo de leyes con indicaciones ────────────────────────────
leyes_indicaciones <- votaciones |>
  filter(es_indicacion) |>
  group_by(boletin, nombre_proyecto) |>
  summarise(
    n_indicaciones = n(),
    n_aprobadas = sum(aprobada),
    primera = min(fecha),
    ultima = max(fecha),
    tipos = paste(sort(unique(clase_voto)), collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(desc(n_indicaciones), desc(primera)) |>
  mutate(
    pct_aprob = round(100 * n_aprobadas / n_indicaciones, 1),
    url = paste0(
      "https://www.camara.cl/legislacion/boletines/boletin.aspx?numero=",
      str_extract(boletin, "^\\d+"), "&tipo=", str_extract(boletin, "\\d+$")
    )
  )

write_csv(leyes_indicaciones, file.path(out_ind, "leyes_con_indicaciones.csv"))
cat("Leyes con indicaciones:", nrow(leyes_indicaciones), "→", file.path(out_ind, "leyes_con_indicaciones.csv"), "\n")

# ── % a favor por partido: indicación vs general ──────────────────
votos_clase <- votos |>
  inner_join(votaciones |> select(votacion_id, clase_voto, boletin, proyecto_corto), by = "votacion_id") |>
  filter(voto %in% c("a_favor", "en_contra"), partido %in% partidos_foco)

por_partido_clase <- votos_clase |>
  group_by(partido, clase_voto) |>
  summarise(
    n = n(),
    pct_favor = mean(voto == "a_favor"),
    .groups = "drop"
  ) |>
  mutate(
    etiqueta = recode(partido, !!!etiquetas),
    partido = factor(partido, levels = orden_ideologico[orden_ideologico %in% partidos_foco])
  )

write_csv(por_partido_clase, file.path(out_ind, "comparacion_indicacion_vs_general.csv"))

p_comp <- ggplot(por_partido_clase, aes(x = partido, y = pct_favor, fill = clase_voto)) +
  geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.92) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c("Indicación" = "#805AD5", "General" = "#3182CE",
                               "Artículo" = "#38A169", "Otro" = "#A0AEC0")) +
  scale_x_discrete(labels = function(x) recode(x, !!!etiquetas)) +
  labs(
    title = "¿Cómo votan las bancadas en indicaciones vs votos generales?",
    subtitle = "% posiciones a favor (2026 · Cámara)",
    x = NULL, y = "% a favor", fill = "Tipo de votación"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "bottom"
  )

ggsave(file.path(out_img, "indicaciones_vs_general_partidos.png"),
       p_comp, width = 10, height = 6, dpi = 150, bg = "white")

# ── Top leyes: barras indicaciones ────────────────────────────────
top_leyes <- leyes_indicaciones |>
  slice_head(n = 12) |>
  mutate(proyecto_label = paste0(boletin, "\n", str_trunc(nombre_proyecto, 45)))

p_leyes <- ggplot(top_leyes, aes(x = reorder(proyecto_label, n_indicaciones), y = n_indicaciones)) +
  geom_col(fill = "#553C9A", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = n_indicaciones), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Proyectos de ley con más votaciones sobre indicaciones (2026)",
    subtitle = "Boletín + título abreviado",
    x = NULL, y = "N° votaciones sobre indicaciones"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(out_img, "leyes_top_indicaciones.png"),
       p_leyes, width = 11, height = 7, dpi = 150, bg = "white")

# ── Layout hemiciclo (posiciones fijas por diputado) ──────────────
diputados <- votos |>
  distinct(diputado_id, partido, apellido) |>
  mutate(
    partido = if_else(is.na(partido) | partido == "", "IND", partido),
    partido_ord = factor(partido, levels = orden_ideologico)
  ) |>
  arrange(partido_ord, apellido) |>
  mutate(
    idx_global = row_number(),
    n_total = n(),
    # Hemiciclo: FA izquierda → REP derecha
    theta = pi - (idx_global - 1) / (n_total - 1) * pi,
    x = cos(theta),
    y = sin(theta)
  )

# ── Hemiciclo interactivo (plotly + dropdown) ─────────────────────
indic_vots <- votaciones |>
  filter(es_indicacion) |>
  arrange(desc(fecha), votacion_id)

# Incluir también votación general del endeudamiento y PDL para contraste
destacadas <- c(89178, 89179, 89180, 88893, 88904)
ids_plot <- unique(c(destacadas, indic_vots$votacion_id))

build_frame <- function(vid) {
  meta <- votaciones |> filter(votacion_id == vid)
  if (nrow(meta) == 0) return(NULL)
  votos |>
    filter(votacion_id == vid) |>
    left_join(
      diputados |> select(diputado_id, x, y, partido_layout = partido, apellido_dip = apellido),
      by = "diputado_id"
    ) |>
    mutate(
      partido = coalesce(partido, partido_layout, "IND"),
      apellido = coalesce(apellido_dip, str_extract(nombre_diputado, "^\\S+")),
      votacion_id = vid,
      titulo = str_trunc(meta$objeto_votacion[1], 120),
      boletin = meta$boletin[1],
      clase = meta$clase_voto[1],
      voto_label = recode(voto,
        a_favor = "A favor", en_contra = "En contra",
        abstencion = "Abstención", no_vota = "No vota", dispensado = "Dispensado"
      ),
      hover = paste0(
        apellido, " (", partido, ")<br>",
        "Voto: ", voto_label, "<br>",
        meta$boletin[1], "<br>",
        str_trunc(meta$objeto_votacion[1], 100)
      )
    )
}

frames <- map(ids_plot, build_frame) |> compact() |> list_rbind()

if (nrow(frames) == 0) stop("Sin datos para hemiciclo interactivo")

# Dropdown buttons (máx 40 para no saturar el HTML; priorizar indicaciones recientes)
meta_dropdown <- votaciones |>
  filter(votacion_id %in% unique(frames$votacion_id)) |>
  arrange(desc(fecha), votacion_id) |>
  mutate(
    label = paste0(
      votacion_id, " · ", boletin, " · ",
      str_trunc(str_remove(objeto_votacion, "^\\[[0-9]+\\]\\s*"), 55)
    )
  )

if (nrow(meta_dropdown) > 40) {
  meta_dropdown <- bind_rows(
    meta_dropdown |> filter(votacion_id %in% destacadas),
    meta_dropdown |> filter(!votacion_id %in% destacadas) |> slice_head(n = 40 - length(destacadas))
  ) |> distinct(votacion_id, .keep_all = TRUE)
}

first_id <- meta_dropdown$votacion_id[1]

p0 <- frames |>
  filter(votacion_id == first_id) |>
  plot_ly(
    x = ~x, y = ~y,
    color = ~voto_label,
    colors = c("A favor" = "#38A169", "En contra" = "#E53E3E",
               "Abstención" = "#ECC94B", "No vota" = "#A0AEC0", "Dispensado" = "#CBD5E0"),
    type = "scatter", mode = "markers",
    marker = list(size = 11, line = list(width = 0.5, color = "#1A202C")),
    text = ~hover, hoverinfo = "text",
    showlegend = TRUE
  ) |>
  layout(
    title = list(
      text = paste0("<b>Hemiciclo · Votación ", first_id, "</b><br>",
                    "<sup>", meta_dropdown$label[1], "</sup>"),
      font = list(size = 14)
    ),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "",
                 scaleanchor = "x", scaleratio = 1),
    paper_bgcolor = "#0F172A",
    plot_bgcolor = "#0F172A",
    font = list(color = "#E2E8F0"),
    legend = list(orientation = "h", y = -0.08),
    height = 620, width = 900
  )

buttons <- map(seq_len(nrow(meta_dropdown)), function(i) {
  vid <- meta_dropdown$votacion_id[i]
  fr <- frames |> filter(votacion_id == vid)
  list(
    method = "restyle",
    args = list(
      list(
        x = list(fr$x),
        y = list(fr$y),
        text = list(fr$hover),
        marker = list(color = list(
          recode(fr$voto,
            a_favor = "#38A169", en_contra = "#E53E3E",
            abstencion = "#ECC94B", no_vota = "#A0AEC0",
            dispensado = "#CBD5E0", .default = "#A0AEC0"
          )
        ))
      )
    ),
    label = meta_dropdown$label[i],
    args2 = list(
      list(
        title = paste0(
          "<b>Hemiciclo · Votación ", vid, "</b><br>",
          "<sup>", meta_dropdown$label[i], "</sup>"
        )
      )
    )
  )
})

# plotly buttons: title update needs separate updatemenus entry — simplify with label only
p_hemi <- p0 |>
  layout(
    updatemenus = list(list(
      type = "dropdown",
      direction = "down",
      x = 0.02, y = 1.12,
      showactive = TRUE,
      buttons = buttons
    ))
  )

html_path <- file.path(out_ind, "hemiciclo_indicaciones.html")
saveWidget(p_hemi, html_path, selfcontained = TRUE)
cat("Hemiciclo interactivo →", html_path, "\n")

# ── Endeudamiento: 3 votaciones lado a lado (estático) ──────────
ids_endeud <- c(89178, 89179, 89180)
labels_endeud <- c("General", "Indic. art. 2", "Indic. art. 3")

endeud_plots <- map2(ids_endeud, labels_endeud, function(vid, lab) {
  fr <- frames |> filter(votacion_id == vid)
  ggplot(fr, aes(x = x, y = y, color = voto)) +
    geom_point(size = 2.8, alpha = 0.9) +
    scale_color_manual(values = colores_voto, name = "Voto") +
    coord_fixed() +
    labs(title = lab, subtitle = paste("Votación", vid)) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
      plot.subtitle = element_text(hjust = 0.5, size = 8, color = "grey50"),
      legend.position = "none",
      plot.background = element_rect(fill = "#F7FAFC", color = NA)
    )
})

p_endeud <- wrap_plots(endeud_plots, nrow = 1) +
  plot_annotation(
    title = "Boletín 18296-05 — Endeudamiento 2026",
    subtitle = "Hemiciclo por votación: verde = a favor · rojo = en contra",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave(file.path(out_img, "hemiciclo_endeudamiento_18296.png"),
       p_endeud, width = 14, height = 5, dpi = 150, bg = "white")

cat("\n── Resumen leyes con más indicaciones ──\n")
print(as.data.frame(leyes_indicaciones |> slice_head(n = 8) |>
                      select(boletin, n_indicaciones, pct_aprob, nombre_proyecto)))

cat("\nDone.\n")
