#!/usr/bin/env Rscript
# Piloto: votante identificado + red de desajuste / disciplina partidaria
# Uso: Rscript analysis/parlamento/09_piloto_votantes.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(igraph)
  library(ggraph)
  library(scales)
  library(RSQLite)
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

db_path       <- file.path(root, "data/raw/congreso.db")
out_vol       <- file.path(root, "outputs/volatilidad")
out_img       <- file.path(root, "outputs/imagenes")
dir.create(out_vol, recursive = TRUE, showWarnings = FALSE)
dir.create(out_img, recursive = TRUE, showWarnings = FALSE)

N_VOTACIONES  <- 10L
partidos_foco <- c("REP", "UDI", "RN", "PNL")

etiquetas <- c(
  REP = "Republicano", UDI = "UDI", RN = "RN", PNL = "PNL",
  PDG = "PDG", IND = "Independientes", FA = "Frente Amplio",
  PS = "Socialista", PC = "Comunista", PPD = "PPD", DC = "Demócrata Cristiano",
  PSC = "PSC", PL = "PL", EVOP = "Evópoli", FRVS = "FRVS", PR = "PR",
  DEM = "Demócratas", PAH = "PAH"
)

# Orden ideológico aproximado (para layout circular)
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

# ── Cargar datos ──────────────────────────────────────────────────
conn <- dbConnect(SQLite(), db_path)
ids_piloto <- dbGetQuery(conn, sprintf("
  SELECT DISTINCT v.votacion_id
  FROM votos v JOIN votaciones vot ON v.votacion_id = vot.votacion_id
  ORDER BY vot.fecha DESC, v.votacion_id DESC LIMIT %d
", N_VOTACIONES))$votacion_id

votos <- dbGetQuery(conn, sprintf("
  SELECT v.votacion_id, vot.boletin, vot.descripcion AS proyecto,
         vot.materia, vot.nombre_proyecto, vot.articulo, vot.tipo_votacion,
         vot.objeto_votacion, vot.fecha, vot.resultado,
         v.diputado_id, v.nombre_diputado,
         d.partido, d.apellido_paterno, v.opcion, v.voto_norm AS voto
  FROM votos v
  LEFT JOIN diputados d ON v.diputado_id = d.diputado_id
  LEFT JOIN votaciones vot ON v.votacion_id = vot.votacion_id
  WHERE v.votacion_id IN (%s)
", paste(ids_piloto, collapse = ","))) |> as_tibble()
dbDisconnect(conn)

votos <- votos |>
  mutate(
    fecha = as.Date(substr(fecha, 1, 10)),
    partido = if_else(is.na(partido) | partido == "", "S/P", partido),
    apellido = str_trim(apellido_paterno),
    votante = coalesce(nombre_diputado, apellido),
    foco = partido %in% partidos_foco
  )

cat(sprintf("Piloto: %d votaciones · %d votos · %d partidos\n",
            n_distinct(votos$votacion_id), nrow(votos),
            n_distinct(votos$partido)))

# ── Tabla nominal (todos los partidos) ────────────────────────────
nominal <- votos |>
  transmute(
    votacion_id, fecha, boletin, proyecto, materia, nombre_proyecto,
    articulo, tipo_votacion, objeto_votacion, resultado,
    diputado_id, votante, apellido, partido, foco, opcion, voto
  )

write_csv(nominal, file.path(out_vol, "votos_nominales_piloto.csv"))

# ── Disidencias por votante (todos los partidos) ──────────────────
mayoria_partido <- nominal |>
  filter(voto %in% c("a_favor", "en_contra")) |>
  group_by(votacion_id, partido) |>
  summarise(posicion_partido = names(which.max(table(voto))), .groups = "drop")

disidentes <- nominal |>
  filter(voto %in% c("a_favor", "en_contra")) |>
  left_join(mayoria_partido, by = c("votacion_id", "partido")) |>
  mutate(alineado = voto == posicion_partido)

resumen_votante <- disidentes |>
  group_by(diputado_id, votante, apellido, partido, foco) |>
  summarise(
    n_votaciones = n(), n_disidencia = sum(!alineado),
    volatilidad = n_disidencia / n(),
    disciplina = 1 - volatilidad,
    .groups = "drop"
  ) |>
  mutate(perfil = case_when(
    volatilidad == 0 ~ "Disciplinado",
    volatilidad <= 0.2 ~ "Alineado",
    volatilidad <= 0.4 ~ "Moderado",
    TRUE ~ "Volátil"
  ))

write_csv(resumen_votante, file.path(out_vol, "volatilidad_por_votante.csv"))
write_csv(disidentes, file.path(out_vol, "votos_con_alineacion.csv"))

# ── Disciplina / desorden por PARTIDO (todos) ─────────────────────
stats_partido <- resumen_votante |>
  group_by(partido, foco) |>
  summarise(
    n_diputados = n(),
    disciplina_media = mean(disciplina),
    volatilidad_media = mean(volatilidad),
    rice_partido = 1 - volatilidad_media,
    n_disidentes = sum(n_disidencia > 0),
    n_volatiles = sum(volatilidad > 0.2),
    pct_volatiles = n_volatiles / n_diputados,
    orden = case_when(
      volatilidad_media < 0.05 ~ "Muy ordenado",
      volatilidad_media < 0.12 ~ "Ordenado",
      volatilidad_media < 0.22 ~ "Moderado",
      TRUE ~ "Desordenado"
    ),
    .groups = "drop"
  ) |>
  mutate(
    etiqueta = recode(partido, !!!etiquetas, .default = partido),
    etiqueta = paste0(etiqueta, "\n", scales::percent(disciplina_media, 0.1))
  ) |>
  arrange(volatilidad_media)

write_csv(stats_partido, file.path(out_vol, "volatilidad_por_partido.csv"))

cat("\n── Disciplina partidaria (todos) ──\n")
print(as.data.frame(stats_partido |>
  select(partido, n_diputados, disciplina_media, volatilidad_media, orden, foco)))

cat("\n── Foco REP / UDI / RN / PNL ──\n")
print(as.data.frame(stats_partido |> filter(foco) |>
  select(partido, disciplina_media, n_volatiles, orden)))

# ── Matriz acuerdo entre PARTIDOS (posición mayoritaria) ──────────
posiciones <- nominal |>
  filter(voto %in% c("a_favor", "en_contra")) |>
  group_by(votacion_id, partido) |>
  summarise(pos = names(which.max(table(voto))), .groups = "drop")

partidos_presentes <- posiciones |> distinct(partido) |> pull() |>
  intersect(orden_ideologico) |> unique()
extra <- setdiff(posiciones$partido, partidos_presentes)
partidos_presentes <- c(partidos_presentes, extra)

pos_wide <- posiciones |>
  pivot_wider(names_from = partido, values_from = pos)

acuerdo_mat <- matrix(NA, length(partidos_presentes), length(partidos_presentes),
                      dimnames = list(partidos_presentes, partidos_presentes))
for (a in partidos_presentes) {
  for (b in partidos_presentes) {
    if (a == b) next
    comp <- pos_wide |> filter(!is.na(.data[[a]]), !is.na(.data[[b]]))
    if (nrow(comp) > 0) acuerdo_mat[a, b] <- mean(comp[[a]] == comp[[b]])
  }
}
acuerdo_mat[is.na(acuerdo_mat)] <- 0
acuerdo_sym <- acuerdo_mat
acuerdo_sym[lower.tri(acuerdo_sym)] <- t(acuerdo_sym)[lower.tri(acuerdo_sym)]

g_partidos <- graph_from_adjacency_matrix(acuerdo_sym, mode = "undirected",
                                          weighted = TRUE, diag = FALSE)
V(g_partidos)$name <- partidos_presentes

node_stats <- stats_partido |>
  filter(partido %in% partidos_presentes) |>
  mutate(
    name = partido,
    label = recode(partido, !!!etiquetas, .default = partido),
    foco = partido %in% partidos_foco
  )

# Layout circular manual (orden ideológico)
ord <- intersect(orden_ideologico, partidos_presentes)
ord <- c(ord, setdiff(partidos_presentes, ord))
n_p <- length(ord)
angles <- seq(0, 2 * pi * (1 - 1/n_p), length.out = n_p) - pi/2
coords_manual <- tibble(
  name = ord,
  x = cos(angles) * 1.1,
  y = sin(angles) * 1.1
)

lay_p <- create_layout(g_partidos, layout = "manual", x = coords_manual$x[match(V(g_partidos)$name, coords_manual$name)],
                     y = coords_manual$y[match(V(g_partidos)$name, coords_manual$name)])

lay_p <- lay_p |>
  left_join(node_stats, by = "name") |>
  mutate(
    foco = coalesce(foco, FALSE),
    disciplina_media = coalesce(disciplina_media, 0.5),
    volatilidad_media = coalesce(volatilidad_media, 0.2),
    orden = coalesce(orden, "Moderado"),
    label = coalesce(label, name)
  )

# ── RED PARTIDOS (hero chart) ─────────────────────────────────────
p_red_partidos <- ggraph(lay_p) +
  geom_edge_link(
    aes(width = weight, alpha = weight, color = weight),
    lineend = "round", show.legend = FALSE
  ) +
  geom_node_point(
    aes(size = disciplina_media, fill = volatilidad_media, color = foco),
    shape = 21, stroke = 0
  ) +
  geom_node_point(
    data = lay_p |> filter(foco),
    aes(size = disciplina_media),
    shape = 21, fill = NA, color = "#F6E05E", stroke = 2.8
  ) +
  geom_node_point(
    data = lay_p |> filter(!foco),
    aes(size = disciplina_media),
    shape = 21, fill = NA, color = "white", stroke = 1.2
  ) +
  geom_node_text(aes(label = label), size = 3.3, fontface = "bold",
                 color = "grey95", lineheight = 0.85) +
  scale_size_continuous(range = c(6, 22), name = "Disciplina\nbancada") +
  scale_fill_gradient2(
    low = "#48BB78", mid = "#ECC94B", high = "#FC8181",
    midpoint = 0.15, name = "Desajuste\ninterno",
    labels = percent_format(accuracy = 1)
  ) +
  scale_color_manual(values = c("TRUE" = "#F6E05E", "FALSE" = "white"),
                     guide = "none") +
  scale_edge_width_continuous(range = c(0.3, 3.5)) +
  scale_edge_alpha_continuous(range = c(0.2, 0.85)) +
  scale_edge_color_gradient2(low = "#FC8181", mid = "#A0AEC0", high = "#63B3ED",
                           midpoint = 0.75, guide = "none") +
  labs(
    title = "Convergencia entre bancadas",
    subtitle = "Arista = % acuerdo posicional · tamaño = disciplina · color relleno = desajuste interno · anillo dorado = foco (REP/UDI/RN/PNL)",
    caption = sprintf("%d votaciones · opendata.camara.cl", N_VOTACIONES)
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.title = element_text(size = 17, face = "bold", hjust = 0.5, color = "grey95"),
    plot.subtitle = element_text(size = 9.5, hjust = 0.5, color = "grey75", lineheight = 1.15,
                                margin = margin(b = 8)),
    plot.caption = element_text(size = 8, color = "grey60", hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(color = "grey85"),
    legend.title = element_text(color = "grey90", face = "bold"),
    plot.background = element_rect(fill = "#0F172A", color = NA),
    panel.background = element_rect(fill = "#0F172A", color = NA),
    plot.margin = margin(16, 16, 16, 16)
  )

# ── RED FOCO: diputados REP/UDI/RN/PNL ───────────────────────────
vot_foco <- resumen_votante |> filter(partido %in% partidos_foco)

mat <- disidentes |>
  filter(diputado_id %in% vot_foco$diputado_id) |>
  mutate(score = if_else(voto == "a_favor", 1L, -1L)) |>
  select(diputado_id, votacion_id, score) |>
  pivot_wider(names_from = votacion_id, values_from = score, values_fill = 0) |>
  column_to_rownames("diputado_id") |> as.matrix()

ids <- rownames(mat)
n <- length(ids)
ac <- matrix(0, n, n, dimnames = list(ids, ids))
for (i in seq_len(n)) for (j in seq_len(n)) {
  if (i == j) next
  vi <- mat[i, ]; vj <- mat[j, ]
  act <- (vi != 0) | (vj != 0)
  if (sum(act) > 0) ac[i, j] <- mean(vi[act] == vj[act])
}

edges <- which(ac >= 0.85 & upper.tri(ac), arr.ind = TRUE)
el <- if (nrow(edges) > 0) {
  tibble(from = ids[edges[, 1]], to = ids[edges[, 2]], weight = ac[edges])
} else tibble(from = character(), to = character(), weight = numeric())

g_foco <- graph_from_data_frame(el, directed = FALSE, vertices = vot_foco |>
  transmute(
    name = as.character(diputado_id),
    label = apellido,
    partido,
    volatilidad,
    disciplina,
    disidente = volatilidad > 0.2
  ))

# Layout por columnas de partido
cols <- c(REP = 0, UDI = 1, RN = 2, PNL = 3)
set.seed(7)
v_coords <- vot_foco |>
  mutate(name = as.character(diputado_id)) |>
  group_by(partido) |>
  mutate(idx = row_number()) |>
  ungroup() |>
  transmute(
    name,
    x = cols[partido] + runif(n(), -0.12, 0.12),
    y = (idx - mean(idx)) * 0.18 + runif(n(), -0.04, 0.04)
  )

lay_f <- create_layout(
  g_foco, layout = "manual",
  x = v_coords$x[match(V(g_foco)$name, v_coords$name)],
  y = v_coords$y[match(V(g_foco)$name, v_coords$name)]
)

disidentes_foco <- lay_f |> filter(.data$disidente)
disciplinados_foco <- lay_f |> filter(.data$disidente | .data$disciplina > 0.95)

p_red_foco <- ggraph(lay_f) +
  geom_edge_link(aes(alpha = weight), color = "#4A5568", width = 0.4, show.legend = FALSE) +
  geom_node_point(aes(color = partido, size = disciplina, shape = disidente), alpha = 0.95) +
  geom_node_point(data = disidentes_foco,
                  aes(size = disciplina), shape = 21, fill = NA, color = "#FC8181", stroke = 2) +
  geom_node_text(data = disciplinados_foco,
                 aes(label = label), size = 2.6, repel = TRUE, color = "grey20",
                 fontface = "bold", max.overlaps = 25) +
  scale_color_manual(values = colores_partido, name = "Partido") +
  scale_size_continuous(range = c(2.5, 7), name = "Disciplina") +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17),
                     name = "Disidente", labels = c("No", "Sí")) +
  facet_wrap(~ partido, nrow = 1, labeller = labeller(partido = etiquetas)) +
  labs(
    title = "Micro-red: votantes del foco (REP · UDI · RN · PNL)",
    subtitle = "Columnas = partido · rojo = disidente · etiquetas en disciplinados y disidentes",
    caption = NULL
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#4A5568"),
    strip.text = element_text(face = "bold", size = 11, color = "#1A202C"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "#F8FAFC", color = NA)
  )

# ── Barras: orden vs desorden ─────────────────────────────────────
p_bars <- stats_partido |>
  mutate(
    etiqueta_corta = recode(partido, !!!etiquetas, .default = partido),
    etiqueta_corta = fct_reorder(etiqueta_corta, disciplina_media),
    alpha_foco = if_else(foco, 1, 0.55)
  ) |>
  ggplot(aes(disciplina_media, etiqueta_corta, fill = partido)) +
  geom_col(aes(alpha = alpha_foco), width = 0.7) +
  geom_text(aes(label = orden), hjust = -0.05, size = 3, color = "#4A5568") +
  scale_fill_manual(values = colores_partido, guide = "none") +
  scale_alpha_identity() +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1.12),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "¿Tiene orden la bancada?", x = "Disciplina media", y = NULL) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    panel.grid.major.y = element_blank()
  )

# ── Composición final ─────────────────────────────────────────────
p_final <- (p_red_partidos | p_bars) / p_red_foco +
  plot_layout(heights = c(1.1, 0.9)) +
  plot_annotation(
    title = "Desajuste y disciplina legislativa — piloto Cámara 2026",
    subtitle = "Todos los partidos · foco en Republicanos, UDI, RN y PNL",
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#4A5568")
    )
  )

ggsave(file.path(out_img, "red_votantes_piloto.png"), p_final,
       width = 18, height = 14, dpi = 300, bg = "white")
ggsave(file.path(out_img, "red_partidos_convergencia.png"), p_red_partidos,
       width = 12, height = 11, dpi = 300, bg = "#0F172A")

cat("\nOutputs públicos:\n")
cat("  outputs/volatilidad/*.csv\n")
cat("  outputs/imagenes/red_votantes_piloto.png\n")
cat("  outputs/imagenes/red_partidos_convergencia.png\n")
