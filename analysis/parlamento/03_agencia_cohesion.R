#!/usr/bin/env Rscript
# Agencia vs cohesión oficialista — H3 / H4
# Lee solo congreso.db (tabla analisis_agencia + votos).
# Output: figures en outputs/imagenes/ (no CSV intermedios).
#
# Uso:
#   python3 data/scripts/build_derived.py   # actualizar tablas derivadas
#   Rscript analysis/parlamento/03_agencia_cohesion.R

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

if (!file.exists(db_path)) stop("No existe congreso.db — correr scrapers + data/scripts/build_derived.py")

conn <- dbConnect(SQLite(), db_path)

# ¿Existe tabla derivada?
tabs <- dbGetQuery(conn, "SELECT name FROM sqlite_master WHERE type='table'")$name
if (!"analisis_agencia" %in% tabs) {
  dbDisconnect(conn)
  stop("Falta tabla analisis_agencia. Correr: python3 data/scripts/build_derived.py")
}

agencia <- dbGetQuery(conn, "SELECT * FROM analisis_agencia") |> as_tibble()

# Watchlist -05: acuerdo posicional REP–PNL y solicitantes
watch_bol <- dbGetQuery(conn, "
  SELECT DISTINCT boletin FROM votaciones
  WHERE boletin LIKE '%-05'
     OR boletin IN ('18216-05','18296-05','18036-05')
")$boletin

# Posiciones por partido en watchlist
pos_watch <- dbGetQuery(conn, "
  SELECT v.votacion_id, vot.boletin, d.partido, v.voto_norm AS voto
  FROM votos v
  JOIN diputados d ON v.diputado_id = d.diputado_id
  JOIN votaciones vot ON v.votacion_id = vot.votacion_id
  WHERE vot.boletin LIKE '%-05'
    AND v.voto_norm IN ('a_favor','en_contra')
    AND d.partido IN ('REP','UDI','RN','PNL','FA','PS','PC')
") |> as_tibble()

dbDisconnect(conn)

posicion <- function(v) names(sort(table(v), decreasing = TRUE))[1]

pos_wide <- pos_watch |>
  group_by(votacion_id, boletin, partido) |>
  summarise(pos = posicion(voto), .groups = "drop") |>
  pivot_wider(names_from = partido, values_from = pos)

pair_acuerdo <- function(df, a, b) {
  ok <- df |> filter(!is.na(.data[[a]]), !is.na(.data[[b]]))
  if (nrow(ok) < 2) return(tibble(partido_a = a, partido_b = b, acuerdo = NA_real_, n = 0L))
  tibble(
    partido_a = a, partido_b = b,
    acuerdo = mean(ok[[a]] == ok[[b]]),
    n = nrow(ok)
  )
}

pares_watch <- bind_rows(
  pair_acuerdo(pos_wide, "REP", "PNL"),
  pair_acuerdo(pos_wide, "REP", "UDI"),
  pair_acuerdo(pos_wide, "REP", "RN"),
  pair_acuerdo(pos_wide, "REP", "FA"),
  pair_acuerdo(pos_wide, "PNL", "UDI")
)

# ── Plot 1: Agencia (solicitantes) vs cohesión (Rice indicaciones) ─
oficialismo <- c("REP", "UDI", "RN", "PNL")
agencia_plot <- agencia |>
  filter(partido %in% c(oficialismo, "FA", "PS", "PC", "PDG")) |>
  mutate(
    foco = partido %in% oficialismo,
    label = partido,
    rice_pct = rice_indicaciones * 100,
    sol_per_capita = ifelse(n_diputados > 0, n_solicitantes / n_diputados, 0)
  )

p1 <- ggplot(agencia_plot, aes(sol_per_capita, rice_pct, color = bloque, size = n_diputados)) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = median(agencia_plot$sol_per_capita, na.rm = TRUE),
             linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.9) +
  ggrepel::geom_text_repel(aes(label = label), size = 3.5, fontface = "bold",
                           show.legend = FALSE, max.overlaps = 20) +
  scale_color_manual(values = c(
    Oficialismo = "#C0392B", Oposición = "#805AD5",
    Centro = "#38A169", Mixto = "#718096"
  )) +
  scale_size_continuous(range = c(4, 12), name = "Diputados") +
  labs(
    title = "Agencia vs cohesión — Cámara 2026",
    subtitle = "Eje X: solicitantes de votación separada por diputado · Y: Rice en indicaciones",
    x = "Solicitantes / n° diputados del partido",
    y = "Cohesión interna (Rice × 100)",
    color = "Bloque",
    caption = "Fuente: congreso.db · tabla analisis_agencia"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ── Plot 2: Barras agencia oficialismo + oposición ────────────────
p2 <- agencia_plot |>
  mutate(partido = fct_reorder(partido, n_solicitantes)) |>
  ggplot(aes(partido, n_solicitantes, fill = bloque)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n_solicitantes), hjust = -0.15, size = 3.2) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c(
    Oficialismo = "#C0392B", Oposición = "#805AD5",
    Centro = "#38A169", Mixto = "#718096"
  ), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Quién pide votación separada",
    subtitle = "Agencia en sala (no solo disciplina de voto)",
    x = NULL, y = "N° menciones como solicitante"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# ── Plot 3: Acuerdo con REP en mensajes -05 (watchlist) ───────────
p3 <- pares_watch |>
  filter(!is.na(acuerdo)) |>
  mutate(
    par = paste(partido_a, "–", partido_b),
    par = fct_reorder(par, acuerdo)
  ) |>
  ggplot(aes(par, acuerdo, fill = acuerdo)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.0f%%\n(n=%d)", acuerdo * 100, n)),
            hjust = -0.1, size = 3, lineheight = 0.9) +
  coord_flip(clip = "off") +
  scale_fill_gradient2(low = "#E53E3E", mid = "#ECC94B", high = "#38A169",
                       midpoint = 0.7, guide = "none") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.15)) +
  labs(
    title = "Acuerdo posicional en mensajes -05",
    subtitle = "H3: ¿REP y PNL votan igual en agenda fiscal del Ejecutivo?",
    x = NULL, y = "% acuerdo"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# ── Plot 4: Autores de mociones vs solicitantes (oficialismo) ─────
p4 <- agencia |>
  filter(partido %in% oficialismo) |>
  select(partido, n_solicitantes, n_autores_mociones) |>
  pivot_longer(-partido, names_to = "tipo", values_to = "n") |>
  mutate(tipo = recode(tipo,
    n_solicitantes = "Solicita votación separada",
    n_autores_mociones = "Autor de moción"
  )) |>
  ggplot(aes(partido, n, fill = tipo)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#C0392B", "#2C5282")) +
  labs(
    title = "Oficialismo: iniciar vs pelear en sala",
    subtitle = "Autores de mociones (iniciativa) vs solicitantes (conflicto)",
    x = NULL, y = "N°", fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

combo <- (p2 | p1) / (p3 | p4) +
  plot_annotation(
    title = "Agencia vs cohesión oficialista — evidencia H3/H4 (Cámara 2026)",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
  )

outfile <- file.path(out_img, "agencia_vs_cohesion.png")
ggsave(outfile, combo, width = 13, height = 10, dpi = 150, bg = "white")
cat("PNG →", outfile, "\n")

# Consola: lectura rápida
cat("\n── analisis_agencia ──\n")
agencia |>
  arrange(desc(n_solicitantes)) |>
  mutate(across(c(rice_indicaciones, rice_general, acuerdo_con_rep, acuerdo_con_pnl),
                ~ round(.x, 2))) |>
  print(n = 15)

cat("\n── Pares watchlist -05 ──\n")
print(pares_watch |> mutate(acuerdo = round(acuerdo, 2)))
cat("\nDone.\n")
