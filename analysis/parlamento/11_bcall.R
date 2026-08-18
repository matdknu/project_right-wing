#!/usr/bin/env Rscript
# B-Call (Bidimensional Analysis of Roll Call) — Cámara de Diputados
# Toro-Maureira et al. (2025), Frontiers in Political Science.
# d1 = posición ideológica (media de u_ij); d2 = volatilidad/cohesión (sd).
# Salidas: outputs/imagenes/bcall_*.png + data/processed/canon/bcall_*.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(RSQLite)
  library(ggrepel)
  library(bcall)
  library(gifski)
})

for (p in c("analysis/_helpers.R", "../_helpers.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}
for (p in c("analysis/_diccionario.R", "../_diccionario.R")) {
  if (file.exists(p)) { source(p, local = FALSE); break }
}

root <- project_root()
out_fig <- out_imagenes(root)
db <- congreso_db_path(root)

# Pivot derecha → d1 > 0 = cercanos a la derecha
PIVOT <- "José Carlos Meza"
THRESHOLD <- 0.15

# Partidos no en roster 2026–30 pero relevantes (periodo anterior / cambios)
PARTIDO_OVERRIDE <- c(
  `1135` = "PNL"  # Johannes Kaiser (hasta mar 2026)
)

# Familia partidaria para colorear / clustering manual
FAMILIA <- c(
  REP = "derecha", UDI = "derecha", RN = "derecha", PNL = "derecha",
  EVOP = "derecha", PSC = "derecha",
  FA = "izquierda", PC = "izquierda", PS = "izquierda", PPD = "izquierda",
  FRVS = "izquierda", PL = "izquierda", DC = "izquierda", PAH = "izquierda",
  PDG = "otros", IND = "otros", DEM = "otros", PR = "otros"
)

col_partido <- c(
  REP = "#C53030", UDI = "#E2B100", RN = "#2563EB", PNL = "#111111",
  EVOP = "#805AD5", PSC = "#DD6B20",
  FA = "#38A169", PC = "#9B2C2C", PS = "#E53E3E", PPD = "#D53F8C",
  FRVS = "#2F855A", PL = "#ED8936", DC = "#4299E1",
  PDG = "#718096", IND = "#A0AEC0", DEM = "#CBD5E0", PR = "#A0AEC0",
  OTRO = "#718096"
)

theme_df <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey35", size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ── Helpers ───────────────────────────────────────────────────────
voto_bcall <- function(voto_norm) {
  case_when(
    voto_norm == "a_favor" ~ 1L,
    voto_norm == "en_contra" ~ -1L,
    voto_norm == "abstencion" ~ 0L,
    TRUE ~ NA_integer_  # no_vota, dispensado → ausente
  )
}

build_rollcall <- function(con, anio_min, anio_max = anio_min,
                           fecha_min = NULL, fecha_max = NULL) {
  q <- "
    SELECT v.votacion_id,
           v.diputado_id,
           coalesce(
             nullif(trim(d.nombre || ' ' || coalesce(d.apellido_paterno, '')), ''),
             nullif(trim(v.nombre_diputado), ''),
             'dip_' || v.diputado_id
           ) AS legislator,
           upper(trim(coalesce(d.partido, ''))) AS partido,
           v.voto_norm
    FROM votos v
    JOIN votaciones vo ON vo.votacion_id = v.votacion_id
    LEFT JOIN diputados d ON d.diputado_id = v.diputado_id
    WHERE cast(substr(vo.fecha, 1, 4) AS INTEGER) BETWEEN ? AND ?
  "
  params <- list(anio_min, anio_max)
  if (!is.null(fecha_min)) {
    q <- paste0(q, " AND date(vo.fecha) >= date(?)")
    params <- c(params, list(fecha_min))
  }
  if (!is.null(fecha_max)) {
    q <- paste0(q, " AND date(vo.fecha) <= date(?)")
    params <- c(params, list(fecha_max))
  }

  raw <- dbGetQuery(con, q, params = params) |>
    as_tibble() |>
    mutate(
      partido = dplyr::coalesce(
        PARTIDO_OVERRIDE[as.character(diputado_id)],
        na_if(partido, ""),
        "OTRO"
      ),
      v = voto_bcall(voto_norm)
    )

  # Un nombre por diputado_id (prioriza el más frecuente)
  nombres <- raw |>
    count(diputado_id, legislator, partido, sort = TRUE) |>
    group_by(diputado_id) |>
    slice_head(n = 1) |>
    ungroup() |>
    mutate(
      # rownames únicos
      row_id = if_else(
        duplicated(legislator) | duplicated(legislator, fromLast = TRUE),
        paste0(legislator, " [", diputado_id, "]"),
        legislator
      )
    )

  mat <- raw |>
    select(diputado_id, votacion_id, v) |>
    distinct(diputado_id, votacion_id, .keep_all = TRUE) |>
    pivot_wider(names_from = votacion_id, values_from = v) |>
    left_join(nombres |> select(diputado_id, row_id, partido, legislator), by = "diputado_id")

  meta <- mat |> select(diputado_id, row_id, legislator, partido)
  roll <- mat |>
    select(-diputado_id, -legislator, -partido) |>
    column_to_rownames("row_id") |>
    as.data.frame()

  list(rollcall = roll, meta = meta, n_votaciones = ncol(roll), n_legislators = nrow(roll))
}

run_bcall_periodo <- function(con, label, anio_min, anio_max = anio_min,
                              fecha_min = NULL, fecha_max = NULL,
                              pivot = PIVOT, threshold = THRESHOLD) {
  message("=== B-Call ", label, " (", anio_min, "–", anio_max, ") ===")
  built <- build_rollcall(con, anio_min, anio_max, fecha_min, fecha_max)
  message("  matriz: ", built$n_legislators, " legisladores × ",
          built$n_votaciones, " votaciones")

  pivot_use <- pivot
  if (!pivot_use %in% rownames(built$rollcall)) {
    # buscar coincidencia parcial en meta
    hit <- built$meta$row_id[str_detect(built$meta$row_id, fixed(pivot))]
    if (length(hit)) {
      pivot_use <- hit[[1]]
      message("  pivot resuelto → ", pivot_use)
    } else {
      # fallback: REP con más participación
      rc <- built$rollcall
      part <- rowMeans(!is.na(rc))
      reps <- built$meta$row_id[built$meta$partido == "REP"]
      if (length(reps)) {
        pivot_use <- reps[which.max(part[reps])]
        message("  pivot fallback REP → ", pivot_use)
      } else {
        pivot_use <- NULL
        message("  pivot automático (bcall_auto)")
      }
    }
  }

  fit <- bcall_auto(
    built$rollcall,
    distance_method = 1L,
    pivot = pivot_use,
    threshold = threshold,
    verbose = TRUE
  )

  res <- fit$results |>
    as_tibble() |>
    rename(row_id = legislator) |>
    left_join(built$meta, by = "row_id") |>
    mutate(
      familia = coalesce(FAMILIA[partido], "otros"),
      bloque = partido %in% BLOQUE_DERECHA,
      periodo = label,
      anio_min = anio_min,
      anio_max = anio_max
    )

  list(fit = fit, results = res, built = built, pivot = pivot_use)
}

plot_mapa <- function(res, titulo, subtitulo) {
  lab <- res |>
    mutate(
      mostrar = bloque | d2 >= quantile(d2, 0.92, na.rm = TRUE) |
        d1 <= quantile(d1, 0.05, na.rm = TRUE) |
        d1 >= quantile(d1, 0.95, na.rm = TRUE)
    )

  ggplot(res, aes(d1, d2, color = partido)) +
    geom_hline(yintercept = median(res$d2, na.rm = TRUE),
               linetype = "dashed", color = "grey70") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
    geom_point(aes(size = bloque, alpha = bloque)) +
    geom_text_repel(
      data = lab |> filter(mostrar),
      aes(label = word(legislator, -1)),
      size = 2.8, max.overlaps = 30, show.legend = FALSE,
      min.segment.length = 0.2
    ) +
    scale_color_manual(values = col_partido, name = NULL) +
    scale_size_manual(values = c("FALSE" = 2.2, "TRUE" = 3.4), guide = "none") +
    scale_alpha_manual(values = c("FALSE" = 0.45, "TRUE" = 0.95), guide = "none") +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "d1 — posición ideológica (− izquierda · + derecha)",
      y = "d2 — volatilidad (↓ cohesión · ↑ volátil)",
      caption = "B-Call (Toro-Maureira et al. 2025). Pivot derecha → d1>0 cercano a la derecha."
    ) +
    theme_df
}

plot_bloque <- function(res, titulo) {
  der <- res |> filter(bloque)
  stopifnot(nrow(der) > 0)

  ggplot(der, aes(d1, d2, color = partido)) +
    geom_point(size = 3.5, alpha = 0.9) +
    geom_text_repel(
      aes(label = word(legislator, -1)),
      size = 3, max.overlaps = 40, show.legend = FALSE
    ) +
    scale_color_manual(values = col_partido, name = NULL) +
    labs(
      title = titulo,
      subtitle = "Solo REP · UDI · RN · PNL — d1 ideología, d2 volatilidad",
      x = "d1 (ideología)", y = "d2 (volatilidad)",
      caption = "Baja d2 = vota coherente con su posición; alta d2 = comportamiento volátil."
    ) +
    theme_df
}

plot_densidades <- function(res, titulo) {
  # Curvas gaussianas N(d1, d2^2) por legislador del bloque (muestra)
  der <- res |> filter(bloque) |> arrange(d1)
  xs <- seq(min(der$d1, na.rm = TRUE) - 2 * max(der$d2, na.rm = TRUE),
            max(der$d1, na.rm = TRUE) + 2 * max(der$d2, na.rm = TRUE),
            length.out = 200)
  dens <- der |>
    mutate(apellido = word(legislator, -1)) |>
    rowwise() |>
    summarise(
      apellido = apellido, partido = partido, d1 = d1, d2 = d2,
      x = list(xs),
      y = list(dnorm(xs, mean = d1, sd = pmax(d2, 1e-4))),
      .groups = "drop"
    ) |>
    unnest(c(x, y))

  ggplot(dens, aes(x, y, color = partido, group = apellido)) +
    geom_line(alpha = 0.55, linewidth = 0.6) +
    scale_color_manual(values = col_partido, name = NULL) +
    labs(
      title = titulo,
      subtitle = "Cada curva = N(d1, d2²) del legislador (bloque derecha)",
      x = "eje ideológico (centrado en d1)", y = "densidad",
      caption = "Media ≈ posición; ancho ≈ cohesión/volatilidad."
    ) +
    theme_df
}

# ── Animación GIF (evolución acumulada) ───────────────────────────
build_bcall_frames <- function(con, cortes, fecha_inicio = "2026-03-11",
                               min_votaciones = 25L) {
  frames <- map(cortes, function(fcorte) {
    built <- build_rollcall(con, 2026L, 2026L, fecha_min = fecha_inicio, fecha_max = fcorte)
    if (built$n_votaciones < min_votaciones) {
      message("  skip ", fcorte, " (solo ", built$n_votaciones, " votaciones)")
      return(NULL)
    }
    out <- run_bcall_periodo(
      con, as.character(fcorte), 2026L, 2026L,
      fecha_min = fecha_inicio, fecha_max = fcorte
    )
    out$results |>
      mutate(
        corte = as.Date(fcorte),
        corte_lab = format(as.Date(fcorte), "%d %b %Y"),
        n_vot = built$n_votaciones
      )
  })
  bind_rows(compact(frames))
}

save_bcall_gif <- function(frames, titulo, path, width = 9, height = 6.5, fps = 5) {
  tmp <- tempfile("bcall_frames_")
  dir.create(tmp)
  frames <- frames |>
    arrange(corte) |>
    mutate(corte_lab = format(corte, "%d %b %Y"))
  cortes <- unique(frames$corte)
  paths <- character(length(cortes))

  for (i in seq_along(cortes)) {
    corte <- cortes[[i]]
    corte_chr <- format(corte, "%d %b %Y")
    sub <- frames |> filter(corte == .env$corte)
    trail <- frames |> filter(corte <= .env$corte)
    p <- ggplot(sub, aes(d1, d2, colour = partido)) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "grey75") +
      geom_path(
        data = trail,
        aes(group = row_id),
        alpha = 0.25, linewidth = 0.4, inherit.aes = TRUE
      ) +
      geom_point(aes(size = d2), alpha = 0.92) +
      geom_text(
        data = sub |> filter(
          d2 >= quantile(d2, 0.75, na.rm = TRUE) |
            d1 >= quantile(d1, 0.9, na.rm = TRUE)
        ),
        aes(label = stringr::word(legislator, -1)),
        size = 2.5, vjust = -0.9, check_overlap = TRUE, show.legend = FALSE
      ) +
      scale_colour_manual(values = col_partido, name = NULL) +
      scale_size_continuous(range = c(2.8, 5.5), name = "d2 volatilidad") +
      labs(
        title = titulo,
        subtitle = paste0(corte_chr, " · votaciones acumuladas desde 11-mar-2026"),
        x = "d1 — posición ideológica (− izquierda · + derecha)",
        y = "d2 — volatilidad (↑ menos coherente)",
        caption = "B-Call (Toro-Maureira et al. 2025). Rutas = trayectoria acumulada."
      ) +
      theme_df +
      coord_cartesian(clip = "off")

    paths[[i]] <- file.path(tmp, sprintf("frame_%02d.png", i))
    ggsave(paths[[i]], p, width = width, height = height, dpi = 110)
    message("  frame ", i, "/", length(cortes), " → ", basename(paths[[i]]))
  }

  gifski::gifski(paths, path, width = width * 110, height = height * 110, delay = 1 / fps)
  unlink(tmp, recursive = TRUE)
  message("  GIF → ", path)
}

# ── Run ───────────────────────────────────────────────────────────
con <- dbConnect(SQLite(), db)
on.exit(dbDisconnect(con), add = TRUE)

periodos <- list(
  # Legislatura actual (desde instalación 2026–30)
  list(label = "2026", anio_min = 2026L, anio_max = 2026L,
       fecha_min = "2026-03-11", fecha_max = NULL),
  # Legislatura anterior completa
  list(label = "2023_2025", anio_min = 2023L, anio_max = 2025L,
       fecha_min = NULL, fecha_max = NULL)
)
period_keep <- Sys.getenv("BCALL_PERIOD")
if (nzchar(period_keep)) {
  periodos <- Filter(function(p) p$label == period_keep, periodos)
}

all_res <- list()
# BCALL_REPLOT=1 → rehacer PNG/GIF desde CSV (sin reajustar el modelo)
replot_only <- identical(Sys.getenv("BCALL_REPLOT"), "1")

for (p in periodos) {
  csv_path <- canon_path(paste0("bcall_", p$label, ".csv"), root)
  if (replot_only && file.exists(csv_path)) {
    message("  replot desde ", csv_path)
    res <- read_csv(csv_path, show_col_types = FALSE)
    out <- list(results = res, pivot = PIVOT, fit = list(metadata = list(pivot = PIVOT)))
  } else {
    out <- run_bcall_periodo(
      con, p$label, p$anio_min, p$anio_max,
      fecha_min = p$fecha_min, fecha_max = p$fecha_max
    )
    write_csv(out$results, csv_path)
    message("  CSV → ", csv_path)
  }
  all_res[[p$label]] <- out

  n_ok <- nrow(out$results)
  pivot_lab <- out$pivot %||% out$fit$metadata$pivot %||% "?"
  sub <- sprintf(
    "%s · n=%d legisladores · pivot=%s · threshold=%.0f%%",
    p$label, n_ok, pivot_lab, 100 * THRESHOLD
  )

  p_mapa <- plot_mapa(
    out$results,
    sprintf("B-Call Cámara — mapa ideología × volatilidad (%s)", p$label),
    sub
  )
  ggsave(
    file.path(out_fig, paste0("bcall_mapa_", p$label, ".png")),
    p_mapa, width = 11, height = 8, dpi = 160
  )

  if (any(out$results$bloque)) {
    p_bloq <- plot_bloque(
      out$results,
      sprintf("B-Call — bloque derecha (%s)", p$label)
    )
    ggsave(
      file.path(out_fig, paste0("bcall_bloque_", p$label, ".png")),
      p_bloq, width = 10, height = 7.5, dpi = 160
    )

    p_dens <- plot_densidades(
      out$results,
      sprintf("B-Call — curvas gaussianas del bloque (%s)", p$label)
    )
    ggsave(
      file.path(out_fig, paste0("bcall_densidades_", p$label, ".png")),
      p_dens, width = 10, height = 6.5, dpi = 160
    )
  }

  # Ranking volatilidad dentro del bloque
  rank <- out$results |>
    filter(bloque) |>
    arrange(desc(d2)) |>
    mutate(apellido = word(legislator, -1))

  if (nrow(rank)) {
    p_rank <- rank |>
      slice_head(n = 20) |>
      mutate(apellido = fct_reorder(apellido, d2)) |>
      ggplot(aes(apellido, d2, fill = partido)) +
      geom_col(width = 0.75) +
      coord_flip() +
      scale_fill_manual(values = col_partido, name = NULL) +
      labs(
        title = sprintf("Más volátiles del bloque (d2) — %s", p$label),
        subtitle = "Mayor d2 = votos menos coherentes con la posición ideológica",
        x = NULL, y = "d2 (volatilidad B-Call)"
      ) +
      theme_df
    ggsave(
      file.path(out_fig, paste0("bcall_volatiles_", p$label, ".png")),
      p_rank, width = 9, height = 7, dpi = 160
    )
  }

  message("  pivot usado: ", pivot_lab)
  message("  d1 rango: [", round(min(out$results$d1, na.rm = TRUE), 3), ", ",
          round(max(out$results$d1, na.rm = TRUE), 3), "]")
  message("  d2 mediana bloque: ",
          round(median(out$results$d2[out$results$bloque], na.rm = TRUE), 3))
}

# GIF evolución 2026 (cortes mensuales acumulados — más liviano)
frames_csv <- canon_path("bcall_frames_2026.csv", root)
if (replot_only && file.exists(frames_csv)) {
  message("\n=== B-Call GIF 2026 (replot desde frames CSV) ===")
  frames_2026 <- read_csv(frames_csv, show_col_types = FALSE) |>
    mutate(corte = as.Date(corte))
} else {
  cortes_2026 <- c(
    "2026-03-31", "2026-04-30", "2026-05-31",
    "2026-06-30", "2026-07-31", "2026-08-17"
  )
  message("\n=== B-Call GIF 2026 (bloque derecha) ===")
  frames_2026 <- build_bcall_frames(con, cortes_2026, fecha_inicio = "2026-03-11")
  if (nrow(frames_2026)) {
    frames_2026 <- frames_2026 |> filter(bloque)
    write_csv(frames_2026, frames_csv)
  }
}
if (exists("frames_2026") && nrow(frames_2026)) {
  gif_der <- file.path(out_fig, "bcall_evolucion_derecha.gif")
  save_bcall_gif(
    frames_2026,
    "B-Call — evolución ideología × volatilidad (bloque derecha)",
    gif_der,
    fps = 5
  )
}

# Resumen combinado 2026
res26 <- all_res[["2026"]]$results
cat("\n── Top 10 d1 (más derecha) 2026 ──\n")
print(res26 |> arrange(desc(d1)) |> select(legislator, partido, d1, d2) |> slice_head(n = 10))
cat("\n── Top 10 d2 (más volátiles) en bloque 2026 ──\n")
print(res26 |> filter(bloque) |> arrange(desc(d2)) |>
        select(legislator, partido, d1, d2) |> slice_head(n = 10))
cat("\nFiguras → ", out_fig, "\n", sep = "")
cat("Para replotear en consola:\n")
cat("  res26 <- readr::read_csv(canon_path('bcall_2026.csv'))\n")
cat("  print(plot_mapa(res26, 'B-Call 2026', 'desde CSV'))\n")


