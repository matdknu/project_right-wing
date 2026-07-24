#!/usr/bin/env Rscript
# Redes / eco-cámaras partidarias — patrones de voto entre bancadas
# Uso: Rscript analysis/parlamento/red_ecos_partidos.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(igraph)
  library(ggraph)
  library(tidygraph)
  library(scales)
  library(RSQLite)
  library(patchwork)
  library(plotly)
  library(htmlwidgets)
  library(visNetwork)
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
out_dir <- file.path(root, "outputs/volatilidad/ecos")
out_img <- file.path(root, "outputs/imagenes")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_img, recursive = TRUE, showWarnings = FALSE)

partidos_red <- c("REP", "UDI", "RN", "PNL", "PDG", "FA", "PS", "PC", "IND", "PPD", "DC")

etiquetas <- c(
  REP = "Republicano", UDI = "UDI", RN = "RN", PNL = "PNL",
  PDG = "PDG", IND = "Independientes", FA = "Frente Amplio",
  PS = "Socialista", PC = "Comunista", PPD = "PPD", DC = "DC"
)

bloques <- c(
  REP = "Oficialismo", UDI = "Oficialismo", RN = "Oficialismo", PNL = "Oficialismo",
  IND = "Mixto", PDG = "Centro", FA = "Oposición", PS = "Oposición",
  PC = "Oposición", PPD = "Oposición", DC = "Oposición"
)

colores_bloque <- c(
  Oficialismo = "#E53E3E", Oposición = "#805AD5",
  Centro = "#38A169", Mixto = "#718096"
)

colores_nodo <- c(
  REP = "#E53E3E", UDI = "#2B6CB0", RN = "#4299E1", PNL = "#D69E2E",
  PDG = "#38A169", IND = "#718096", FA = "#805AD5", PS = "#E53E3E",
  PC = "#C53030", PPD = "#F6AD55", DC = "#48BB78"
)

# ── Cargar ────────────────────────────────────────────────────────
conn <- dbConnect(SQLite(), db_path)

votaciones <- dbGetQuery(conn, "
  SELECT votacion_id, boletin, nombre_proyecto, fecha, articulo,
         tipo_votacion, objeto_votacion, total_si, total_no
  FROM votaciones
") |> as_tibble()

votos <- dbGetQuery(conn, "
  SELECT v.votacion_id, d.partido, v.voto_norm AS voto
  FROM votos v
  JOIN diputados d ON v.diputado_id = d.diputado_id
  WHERE v.voto_norm IN ('a_favor', 'en_contra')
") |> as_tibble()

dbDisconnect(conn)

votaciones <- votaciones |>
  mutate(
    fecha = as.Date(substr(fecha, 1, 10)),
    es_indicacion = str_detect(str_to_lower(paste(articulo, objeto_votacion)), "indicaci"),
    es_general = tipo_votacion == "1",
    clase = case_when(
      es_indicacion ~ "Indicación",
      es_general ~ "General",
      !is.na(articulo) & articulo != "" ~ "Artículo",
      TRUE ~ "Todas"
    ),
    proyecto = str_trunc(coalesce(nombre_proyecto, boletin), 60)
  )

votos <- votos |>
  filter(partido %in% partidos_red) |>
  inner_join(votaciones |> select(votacion_id, clase, boletin, proyecto), by = "votacion_id")

posicion_partido <- function(v) {
  rel <- v[v %in% c("a_favor", "en_contra")]
  if (!length(rel)) return(NA_character_)
  names(sort(table(rel), decreasing = TRUE))[1]
}

posiciones <- votos |>
  group_by(votacion_id, partido, clase, boletin, proyecto) |>
  summarise(posicion = posicion_partido(voto), .groups = "drop") |>
  filter(!is.na(posicion))

# ── Matriz de acuerdo entre partidos ─────────────────────────────
calc_acuerdo <- function(df, min_vot = 5) {
  wide <- df |>
    select(votacion_id, partido, posicion) |>
    pivot_wider(names_from = partido, values_from = posicion)

  parts <- intersect(partidos_red, names(wide)[-1])
  n <- length(parts)
  mat <- matrix(NA, n, n, dimnames = list(parts, parts))

  for (i in seq_along(parts)) {
    for (j in seq_along(parts)) {
      if (i == j) next
      a <- parts[i]; b <- parts[j]
      comp <- wide |> filter(!is.na(.data[[a]]), !is.na(.data[[b]]))
      if (nrow(comp) >= min_vot) {
        mat[i, j] <- mean(comp[[a]] == comp[[b]])
      }
    }
  }
  mat
}

export_edges <- function(mat, label) {
  as.data.frame(as.table(mat)) |>
    as_tibble() |>
    rename(partido_a = Var1, partido_b = Var2, acuerdo = Freq) |>
    filter(partido_a != partido_b, !is.na(acuerdo)) |>
    mutate(
      red = label,
      par = paste(pmin(as.character(partido_a), as.character(partido_b)),
                  pmax(as.character(partido_a), as.character(partido_b)), sep = "–")
    ) |>
    group_by(red, par) |>
    summarise(
      partido_a = first(partido_a), partido_b = first(partido_b),
      acuerdo = mean(acuerdo), .groups = "drop"
    ) |>
    mutate(acuerdo_pct = round(acuerdo * 100, 1))
}

# Redes por clase de votación
redes_cfg <- list(
  list(label = "Todas", filter = function(d) d),
  list(label = "Indicación", filter = function(d) d |> filter(clase == "Indicación")),
  list(label = "General", filter = function(d) d |> filter(clase == "General")),
  list(label = "Artículo", filter = function(d) d |> filter(clase == "Artículo"))
)

# Boletines watchlist
for (bol in c("18216-05", "18296-05", "18036-05")) {
  redes_cfg <- c(redes_cfg, list(
    list(label = paste0("Boletín ", bol), filter = function(d, b = bol) d |> filter(boletin == b))
  ))
}

edges_all <- map_dfr(redes_cfg, function(cfg) {
  sub <- cfg$filter(posiciones)
  if (nrow(sub) < 10) return(tibble())
  export_edges(calc_acuerdo(sub), cfg$label)
})
cat("Pares en memoria:", nrow(edges_all), "(sin CSV — fuente = congreso.db)\n")

# ── Layout eco-cámara (MDS sobre distancia = 1 - acuerdo) ─────────
build_graph <- function(mat, min_edge = 0.55) {
  mat[is.na(mat)] <- 0
  diag(mat) <- 0
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]

  g <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE, diag = FALSE)
  E(g)$weight <- pmax(E(g)$weight, 0.01)

  # Filtrar aristas débiles para ver estructura de eco-cámaras
  g_sparse <- delete_edges(g, E(g)[weight < min_edge])
  if (ecount(g_sparse) == 0) g_sparse <- g

  dist <- 1 - mat
  diag(dist) <- 0
  # MDS: partidos similares quedan juntos
  mds <- cmdscale(as.dist(dist), k = 2)
  if (ncol(mds) < 2) mds <- cbind(mds[, 1], 0)

  layout_df <- tibble(
    name = rownames(mds),
    x = mds[, 1],
    y = mds[, 2]
  )

  list(graph = g, graph_sparse = g_sparse, layout = layout_df, mat = mat)
}

plot_eco_red <- function(g_obj, title, subtitle, min_edge_label = 0.60) {
  g <- g_obj$graph_sparse
  lay <- create_layout(g, layout = "manual",
                       x = g_obj$layout$x[match(V(g)$name, g_obj$layout$name)],
                       y = g_obj$layout$y[match(V(g)$name, g_obj$layout$name)])

  node_df <- tibble(name = V(g)$name) |>
    mutate(
      label = recode(name, !!!etiquetas),
      bloque = bloques[name],
      color = colores_nodo[name]
    )

  lay <- lay |>
    left_join(node_df, by = "name") |>
    mutate(bloque = coalesce(bloque, "Mixto"))

  edge_df <- as_data_frame(g, what = "edges") |>
    left_join(lay |> select(name, x, y), by = c("from" = "name")) |>
    left_join(lay |> select(name, x, y), by = c("to" = "name"), suffix = c("", "2")) |>
    mutate(
      xm = (x + x2) / 2, ym = (y + y2) / 2,
      pct = sprintf("%.0f%%", weight * 100),
      show_label = weight >= min_edge_label
    )

  ggraph(lay) +
    geom_edge_link(aes(width = weight, alpha = weight, color = weight),
                   lineend = "round", show.legend = FALSE) +
    geom_text(
      data = edge_df |> filter(show_label),
      aes(x = xm, y = ym, label = pct),
      inherit.aes = FALSE, size = 3, color = "grey25"
    ) +
    geom_node_point(aes(size = 14, color = I(color)), alpha = 0.95) +
    geom_node_label(aes(label = label), size = 3.2, fontface = "bold", color = "white") +
    scale_edge_width_continuous(range = c(0.4, 4)) +
    scale_edge_alpha_continuous(range = c(0.25, 0.9)) +
    scale_edge_color_gradient2(low = "#FC8181", mid = "#A0AEC0", high = "#48BB78",
                               midpoint = 0.75, guide = "none") +
    labs(title = title, subtitle = subtitle,
         caption = "Distancia = disenso · arista gruesa = alta convergencia · layout MDS") +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40", lineheight = 1.1),
      plot.caption = element_text(size = 8, color = "grey55", hjust = 0.5),
      plot.background = element_rect(fill = "#FAFAFA", color = NA),
      plot.margin = margin(12, 12, 12, 12)
    )
}

# Comparación Indicación vs General
pos_ind <- posiciones |> filter(clase == "Indicación")
pos_gen <- posiciones |> filter(clase == "General")
pos_tod <- posiciones

g_ind <- build_graph(calc_acuerdo(pos_ind), min_edge = 0.55)
g_gen <- build_graph(calc_acuerdo(pos_gen), min_edge = 0.55)
g_tod <- build_graph(calc_acuerdo(pos_tod), min_edge = 0.70)

p_ind <- plot_eco_red(
  g_ind, "Eco-cámara: INDICACIONES",
  "Oposición junta (FA–PS–PC) vs bloque oficialista — pocas aristas cruzadas"
)
p_gen <- plot_eco_red(
  g_gen, "Eco-cámara: VOTOS GENERALES",
  "Oficialismo más cohesionado; PDG/IND más centrales"
)
p_tod <- plot_eco_red(
  g_tod, "Eco-cámara: TODAS las votaciones 2026",
  "747 votaciones · aristas ≥70% acuerdo posicional"
)

p_combo <- (p_ind | p_gen) / p_tod +
  plot_annotation(
    title = "Redes de convergencia partidaria — Cámara 2026",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

ggsave(file.path(out_img, "eco_camaras_partidos.png"),
       p_combo, width = 14, height = 13, dpi = 150, bg = "white")
cat("PNG →", file.path(out_img, "eco_camaras_partidos.png"), "\n")

# PDL y endeudamiento
for (bol in c("18216-05", "18296-05")) {
  sub <- posiciones |> filter(boletin == bol)
  if (nrow(sub) < 5) next
  g_b <- build_graph(calc_acuerdo(sub), min_edge = 0.50)
  p_b <- plot_eco_red(
    g_b, paste0("Eco-cámara · ", bol),
    unique(sub$proyecto)[1]
  )
  fname <- file.path(out_img, paste0("eco_camara_", gsub("-", "_", bol), ".png"))
  ggsave(fname, p_b, width = 8, height = 7, dpi = 150, bg = "white")
  cat("PNG →", fname, "\n")
}

# ── Heatmap acuerdo (complemento) ─────────────────────────────────
heatmap_data <- edges_all |>
  filter(red %in% c("Indicación", "General", "Todas")) |>
  mutate(
    partido_a = factor(partido_a, levels = partidos_red),
    partido_b = factor(partido_b, levels = partidos_red)
  )

p_heat <- ggplot(heatmap_data, aes(partido_a, partido_b, fill = acuerdo)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.0f", acuerdo_pct)), size = 2.8, color = "white") +
  facet_wrap(~red, nrow = 1) +
  scale_fill_gradient2(low = "#E53E3E", mid = "#ECC94B", high = "#38A169",
                       midpoint = 0.65, labels = percent_format(), name = "Acuerdo") +
  scale_x_discrete(labels = function(x) recode(x, !!!etiquetas)) +
  scale_y_discrete(labels = function(x) recode(x, !!!etiquetas)) +
  labs(title = "Matriz de acuerdo posicional entre bancadas",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

ggsave(file.path(out_img, "heatmap_acuerdo_partidos.png"),
       p_heat, width = 14, height = 5.5, dpi = 150, bg = "white")

# ── Red interactiva (visNetwork) ──────────────────────────────────
to_vis <- function(mat, title) {
  parts <- rownames(mat)
  nodes <- tibble(
    id = parts,
    label = recode(parts, !!!etiquetas),
    group = bloques[parts],
    color = colores_nodo[parts],
    value = pmax(rowSums(mat, na.rm = TRUE) * 3, 8)
  )

  edges <- as.data.frame(as.table(mat)) |>
    as_tibble() |>
    rename(from = Var1, to = Var2, acuerdo = Freq) |>
    filter(from != to, !is.na(acuerdo), acuerdo >= 0.50) |>
    mutate(
      width = acuerdo * 12,
      title = sprintf("%s – %s: %.0f%% acuerdo", from, to, acuerdo * 100),
      label = sprintf("%.0f%%", acuerdo * 100)
    )

  visNetwork(nodes, edges, height = "580px", width = "100%", main = title) |>
    visNodes(color = list(background = nodes$color, border = "#2D3748",
                          highlight = list(border = "#F6E05E"))) |>
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1), nodesIdSelection = TRUE) |>
    visInteraction(navigationButtons = TRUE) |>
    visPhysics(solver = "forceAtlas2Based",
               forceAtlas2Based = list(gravitationalConstant = -80,
                                       centralGravity = 0.01,
                                       springLength = 120)) |>
    visLayout(randomSeed = 42) |>
    visEdges(smooth = list(type = "dynamic"), color = list(opacity = 0.7)) |>
    visNodes(font = list(size = 15, face = "bold", color = "#1A202C"))
}

mat_ind <- calc_acuerdo(pos_ind)
mat_gen <- calc_acuerdo(pos_gen)
mat_tod <- calc_acuerdo(pos_tod)

vis_configs <- list(
  list(file = "red_ecos_indicaciones.html", widget = to_vis(mat_ind, "Indicaciones")),
  list(file = "red_ecos_general.html", widget = to_vis(mat_gen, "Votos generales")),
  list(file = "red_ecos_todas.html", widget = to_vis(mat_tod, "Todas las votaciones 2026"))
)

for (cfg in vis_configs) {
  path <- file.path(out_dir, cfg$file)
  htmlwidgets::saveWidget(cfg$widget, path, selfcontained = TRUE)
  cat("HTML interactivo →", path, "\n")
}

index_html <- file.path(out_dir, "red_ecos_interactiva.html")
writeLines(c(
  "<!DOCTYPE html><html><head><meta charset='utf-8'>",
  "<title>Redes de convergencia — Cámara 2026</title>",
  "<style>body{font-family:sans-serif;max-width:720px;margin:2rem auto;line-height:1.6}",
  "a{display:block;margin:.5rem 0;font-size:1.1rem}</style></head><body>",
  "<h1>Redes de convergencia partidaria</h1>",
  "<p>Arrastra nodos · zoom · clic en partido · grosor = % acuerdo posicional</p>",
  sprintf("<a href='%s'>Indicaciones</a>", vis_configs[[1]]$file),
  sprintf("<a href='%s'>Votos generales</a>", vis_configs[[2]]$file),
  sprintf("<a href='%s'>Todas las votaciones 2026</a>", vis_configs[[3]]$file),
  sprintf("<p>Heatmap y PNG estáticos: <code>%s</code></p>", out_img),
  "</body></html>"
), index_html)
cat("Índice →", index_html, "\n")

# ── Resumen consola ───────────────────────────────────────────────
ofic <- c("REP", "UDI", "RN", "PNL")
opos <- c("FA", "PS", "PC")
pair_pct <- function(red, p1, p2) {
  par <- paste(sort(c(p1, p2)), collapse = "–")
  row <- edges_all |> filter(.data$red == red, .data$par == par)
  if (nrow(row)) row$acuerdo_pct[1] else NA_real_
}

cat("\n── Acuerdo oficialismo (Indicaciones) ──\n")
for (p in combn(ofic, 2, simplify = FALSE)) {
  pct <- pair_pct("Indicación", p[1], p[2])
  if (!is.na(pct)) cat(sprintf("  %s–%s: %.0f%%\n", p[1], p[2], pct))
}

cat("\n── Acuerdo oposición (Indicaciones) ──\n")
for (p in combn(opos, 2, simplify = FALSE)) {
  pct <- pair_pct("Indicación", p[1], p[2])
  if (!is.na(pct)) cat(sprintf("  %s–%s: %.0f%%\n", p[1], p[2], pct))
}

cat("\n── Acuerdo cruzado REP–FA (Indicaciones vs General) ──\n")
for (red in c("Indicación", "General")) {
  pct <- pair_pct(red, "REP", "FA")
  if (!is.na(pct)) cat(sprintf("  %s: %.0f%%\n", red, pct))
}

cat("\nDone.\n")
