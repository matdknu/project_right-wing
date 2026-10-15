# ==============================================================================
# 05_regresiones_votos.R
#
# ¿Qué mueve el Sí y el No? ¿Qué eventos cambian el voto de la derecha?
#
# Usa llm/analisis/votos_codificados.csv.gz (04_indicaciones_bcall.R): un
# registro por diputado y votación, con el bloque del diputado y lo que el LLM
# codificó de la indicación.
#
# Modelos de probabilidad lineal con efectos fijos por diputado (fixest) y
# errores estándar agrupados por votación. Los coeficientes se leen en puntos
# de probabilidad (0,10 = 10 puntos más).
#
# A. ¿Qué mueve el Sí? Un modelo por bloque:
#      Sí ~ dirección LLM (5 ejes) + autoría x gobierno + tipo de votación
#           + tema | diputado
#    A1. Coeficientes. La dirección LLM vale +1 si el Sí va hacia el polo +
#        del eje (Pro-mercado, Provisión privada, Conservadora, Orden y
#        castigo, Soberanista), -1 si va hacia el polo - y 0 si no toca el eje.
#        Su coeficiente es cuánto cambia la probabilidad de votar Sí cuando la
#        indicación va hacia el polo + en vez de no tocar el eje.
#    A2. Qué explica más: R² within (lo que explica cada modelo además de
#        quién es el diputado) que agrega cada grupo de variables, y cuánto
#        pierde el modelo completo si se quita cada grupo. "Techo" = R² within
#        de un modelo con un efecto fijo por votación: lo máximo que puede
#        explicar cualquier característica de la votación.
#    A3. La dirección LLM por período legislativo.
#
# B. Eventos disruptivos. Para cada evento (una fecha, la aparición de un
#    partido o la primera votación de una persona), en una ventana antes y
#    después dentro del mismo período legislativo:
#      y ~ post + tendencia + contenido LLM de la votación | diputado
#    y = votar con la mayoría de la izquierda / con la mayoría de la nueva
#        derecha / Sí / hacia el polo conservador / hacia el polo pro-mercado.
#    post > 0: después del evento el grupo vota más así, a igual contenido de
#    lo que se vota y descontada la tendencia.
#
# Salidas: CSV en llm/regresiones/ y figuras en llm/regresiones/figuras/.
# ==============================================================================

pacman::p_load(
  tidyverse,
  fixest,
  here
)

# ------------------------------------------------------------------------------
# 1. Settings
# ------------------------------------------------------------------------------

bloques_modelo <- c(
  "Nueva derecha",
  "Derecha tradicional",
  "IND con la derecha",
  "Izquierda"
)

confianza_aceptada <- c("Alta", "Media")
excluir_admisibilidad <- TRUE
min_obs_modelo <- 500        # votos mínimos para estimar un modelo
min_votaciones_tema <- 30    # temas con menos votaciones van a "Otros temas"
min_diputados_bloque <- 2    # para definir el voto mayoritario de un bloque

# Gobiernos (orientación del Ejecutivo)
GOBIERNOS <- tribble(
  ~inicio,      ~presidente,   ~orientacion,
  "2010-03-11", "Piñera I",    "derecha",
  "2014-03-11", "Bachelet II", "izquierda",
  "2018-03-11", "Piñera II",   "derecha",
  "2022-03-11", "Boric",       "izquierda",
  "2026-03-11", "Kast",        "derecha"
)

# Eventos. tipo = "fecha" (valor = AAAA-MM-DD), "partido" (valor = sigla: el
# evento es la primera votación con un diputado de ese partido) o "persona"
# (valor = parte del nombre: el evento es su primera votación).
EVENTOS <- tribble(
  ~evento,                         ~tipo,     ~valor,
  "Estallido social",              "fecha",   "2019-10-18",
  "Plebiscito de salida",          "fecha",   "2022-09-04",
  "Elección Consejo Constitucional", "fecha", "2023-05-07",
  "Aparece el PNL",                "partido", "PNL"
  # "Entra <nombre>",              "persona", "<apellido>"
)
# Una persona que entra al inicio de un período no tiene "antes" dentro del
# período: para ella usa una fecha (p. ej. el lanzamiento de su candidatura).

ventana_dias <- 365  # días antes y después del evento (sin salir del período)

# Grupos para los eventos: un bloque o una sigla
grupos_evento <- c(
  "Derecha tradicional",
  "UDI",
  "RN",
  "EVOP",
  "IND con la derecha"
)

resultados_evento <- c(
  con_izquierda = "Vota con la mayoría de la izquierda",
  con_nueva_derecha = "Vota con la mayoría de la nueva derecha",
  si = "Vota Sí",
  hacia_conservador = "Vota hacia el polo conservador",
  hacia_mercado = "Vota hacia el polo pro-mercado"
)

EJES <- tribble(
  ~eje,             ~polo_mas,           ~polo_menos,          ~etiqueta,
  "economica",      "Pro-mercado",       "Pro-Estado",         "Económica: Pro-mercado",
  "subsidiariedad", "Provisión privada", "Provisión estatal",  "Subsidiariedad: Provisión privada [provisional]",
  "valorica",       "Conservadora",      "Progresista",        "Valórica: Conservadora",
  "orden",          "Orden y castigo",   "Garantías",          "Orden: Orden y castigo",
  "nacion",         "Soberanista",       "Pluralista",         "Nación: Soberanista"
)

COMPUESTOS <- list(
  mercado         = c("economica", "subsidiariedad"),
  conservadurismo = c("valorica", "orden", "nacion")
)

# Grupos de variables del modelo A
VARIABLES <- list(
  `Gobierno y autoría` = "autor * gobierno_derecha",
  `Tipo de votación`   = "objeto + naturaleza + operacion + presupuesto",
  `Tema`               = "tema",
  `Dirección LLM`      = paste0("pol_", EJES$eje, collapse = " + ")
)

colores_bloque <- c(
  "Nueva derecha" = "#C53030",
  "Derecha tradicional" = "#1E3A8A",
  "IND con la derecha" = "#0F766E",
  "Izquierda" = "#6B7280",
  UDI = "#E2B100",
  RN = "#2563EB",
  EVOP = "#805AD5"
)

votos_file <- here::here("data", "analisis", "bcall", "votos_codificados.csv.gz")
output_dir <- here::here("data", "analisis", "regresiones")
figures_dir <- here::here("data", "analisis", "regresiones", "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

figure_width <- 13.333
figure_height <- 7.5
figure_dpi <- 300

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

section <- function(title) {
  cat(
    "\n============================================================\n",
    title,
    "\n",
    "============================================================\n",
    sep = ""
  )
}

write_out <- function(x, name) {
  if ("eje" %in% names(x)) {
    x$provisional <- x$eje %in% c("subsidiariedad", "mercado")
  } else if ("term" %in% names(x)) {
    x$provisional <- str_detect(x$term, "subsidiariedad|mercado")
  } else if ("resultado" %in% names(x)) {
    x$provisional <- x$resultado %in% "hacia_mercado"
  }
  write_csv(x, file.path(output_dir, name), na = "")
}

save_figure <- function(plot, name) {
  print(plot)
  ggsave(file.path(figures_dir, name), plot, width = figure_width, height = figure_height,
         units = "in", dpi = figure_dpi, bg = "white")
}

theme_figura <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

inicio_legislativo <- function(fecha) {
  anio <- as.integer(format(fecha, "%Y"))
  anio - as.integer(fecha < as.Date(paste0(anio, "-03-11")))
}

# Quita los términos con una variable que no varía en la muestra (p. ej. el
# gobierno dentro de un solo período)
sin_constantes <- function(rhs, data) {
  terminos <- attr(terms(as.formula(paste("~", rhs))), "term.labels")
  vars <- all.vars(as.formula(paste("~", rhs)))
  constantes <- vars[map_lgl(vars, \(v) n_distinct(data[[v]], na.rm = TRUE) < 2)]
  terminos <- keep(terminos, \(t) !any(all.vars(as.formula(paste("~", t))) %in% constantes))
  if (length(terminos) == 0) "1" else paste(terminos, collapse = " + ")
}

# Modelo de probabilidad lineal con efectos fijos por diputado. Devuelve NULL
# si no se puede estimar.
estimar_lpm <- function(rhs, data, fe = "diputado_id", y = "y") {
  if (nrow(data) < min_obs_modelo || n_distinct(data[[y]], na.rm = TRUE) < 2) return(NULL)
  f <- as.formula(paste0(y, " ~ ", sin_constantes(rhs, data), " | ", fe))
  m <- tryCatch(
    feols(f, data = data, cluster = ~votacion_id, warn = FALSE, notes = FALSE),
    error = identity
  )
  if (inherits(m, "error")) {
    cat("   no se pudo estimar:", conditionMessage(m), "\n")
    return(NULL)
  }
  m
}

coeficientes <- function(m) {
  ct <- coeftable(m)
  tibble(
    term = rownames(ct),
    estimate = ct[, 1],
    std_error = ct[, 2],
    p_value = ct[, 4]
  ) |>
    mutate(
      conf_low = estimate - 1.96 * std_error,
      conf_high = estimate + 1.96 * std_error
    )
}

r2_within <- function(m) if (is.null(m)) NA_real_ else unname(r2(m, "wr2"))

# Proporción de la varianza de y que explican las medias por grupo
r2_medias <- function(y, grupo) {
  1 - sum((y - ave(y, grupo))^2) / sum((y - mean(y))^2)
}

# Techo: R² within (descontado el diputado) de un modelo con un efecto fijo
# por votación. Se calcula con proyecciones alternadas.
r2_techo <- function(y, diputado, votacion, tol = 1e-10) {
  y_within <- y - ave(y, diputado)
  r <- y_within
  for (i in 1:200) {
    previo <- r
    r <- r - ave(r, votacion)
    r <- r - ave(r, diputado)
    if (max(abs(r - previo)) < tol) break
  }
  1 - sum(r^2) / sum(y_within^2)
}

# ==============================================================================
# DATOS
# ==============================================================================

section("DATOS")

if (!file.exists(votos_file)) {
  stop("No encuentro ", votos_file, ". Corre primero 04_indicaciones_bcall.R.", call. = FALSE)
}

votos <- read_csv(votos_file, col_types = cols(.default = col_character()), na = "", progress = FALSE) |>
  mutate(
    votacion_id = as.integer(votacion_id),
    diputado_id = as.integer(diputado_id),
    fecha = as.Date(fecha),
    voto_bcall = as.integer(voto_bcall),
    llm_evaluable = as.logical(llm_evaluable)
  )

# Gobierno de turno
gobiernos <- GOBIERNOS |> mutate(inicio = as.Date(inicio)) |> arrange(inicio)
i_gob <- findInterval(votos$fecha, gobiernos$inicio)
i_gob[i_gob == 0] <- NA
votos <- votos |>
  mutate(
    gobierno = gobiernos$presidente[i_gob],
    gobierno_derecha = as.integer(gobiernos$orientacion[i_gob] == "derecha")
  )

# Dirección del Sí en cada eje: +1, -1 o 0 (no toca el eje, neutral, mixta o
# confianza baja)
for (k in seq_len(nrow(EJES))) {
  x <- votos[[paste0("llm_", EJES$eje[k])]]
  pol <- case_when(x == EJES$polo_mas[k] ~ 1L, x == EJES$polo_menos[k] ~ -1L, TRUE ~ 0L)
  votos[[paste0("pol_", EJES$eje[k])]] <- if_else(votos$llm_confianza %in% confianza_aceptada, pol, 0L)
}
for (nombre in names(COMPUESTOS)) {
  suma <- rowSums(as.matrix(votos[paste0("pol_", COMPUESTOS[[nombre]])]))
  votos[[paste0("pol_", nombre)]] <- as.integer(sign(suma))
}

# Voto mayoritario de la izquierda y de la nueva derecha en cada votación
modal_de <- function(b, nombre) {
  votos |>
    filter(bloque == b, !is.na(voto_bcall)) |>
    count(votacion_id, voto_bcall) |>
    mutate(diputados = sum(n), .by = votacion_id) |>
    filter(diputados >= min_diputados_bloque) |>
    slice_max(n, n = 1, with_ties = FALSE, by = votacion_id) |>
    select(votacion_id, !!nombre := voto_bcall)
}

votos <- votos |>
  left_join(modal_de("Izquierda", "modal_izquierda"), by = "votacion_id") |>
  left_join(modal_de("Nueva derecha", "modal_nueva"), by = "votacion_id") |>
  mutate(
    si = if_else(voto_bcall %in% c(-1L, 1L), as.integer(voto_bcall == 1L), NA_integer_),
    con_izquierda = if_else(!is.na(voto_bcall) & !is.na(modal_izquierda), as.integer(voto_bcall == modal_izquierda), NA_integer_),
    con_nueva_derecha = if_else(
      !is.na(voto_bcall) & !is.na(modal_nueva) & bloque != "Nueva derecha",
      as.integer(voto_bcall == modal_nueva), NA_integer_
    ),
    hacia_conservador = if_else(si %in% 0:1 & pol_conservadurismo != 0L,
                                as.integer(voto_bcall * pol_conservadurismo == 1L), NA_integer_),
    hacia_mercado = if_else(si %in% 0:1 & pol_mercado != 0L,
                            as.integer(voto_bcall * pol_mercado == 1L), NA_integer_)
  )

# Votaciones con codificación LLM utilizable
codificadas <- votos |>
  filter(
    llm_evaluable %in% TRUE,
    !(excluir_admisibilidad & llm_objeto %in% "Admisibilidad")
  )

# Factores con su categoría de referencia
temas_frecuentes <- codificadas |>
  distinct(votacion_id, llm_tema) |>
  count(llm_tema, sort = TRUE) |>
  filter(n >= min_votaciones_tema, !is.na(llm_tema)) |>
  pull(llm_tema)

codificadas <- codificadas |>
  mutate(
    autor = fct_relevel(factor(autor_heuristico), "Parlamentaria"),
    objeto = fct_relevel(factor(llm_objeto), "Indicación"),
    naturaleza = factor(
      case_when(
        llm_naturaleza == "Sustantiva" ~ "Sustantiva",
        llm_naturaleza %in% c("Técnica", "Procedimental") ~ "No sustantiva"
      ),
      levels = c("Sustantiva", "No sustantiva")
    ),
    operacion = fct_relevel(factor(llm_operacion), "Agregar"),
    presupuesto = fct_relevel(factor(llm_presupuesto), "No"),
    # referencia: el tema más frecuente
    tema = factor(if_else(llm_tema %in% temas_frecuentes, llm_tema, "Otros temas"),
                  levels = c(temas_frecuentes, "Otros temas"))
  )

cat("Votos:", nrow(votos), "| en votaciones codificadas y evaluables:", nrow(codificadas), "\n")
cat("Votaciones codificadas:", n_distinct(codificadas$votacion_id), "\n")
cat("Tema de referencia:", temas_frecuentes[1], "\n\n")
codificadas |>
  filter(!is.na(si)) |>
  count(bloque, name = "votos_si_no") |>
  print()

# ==============================================================================
# A. ¿QUÉ MUEVE EL SÍ?
# ==============================================================================

section("A. ¿QUÉ MUEVE EL SÍ? (modelo completo por bloque)")

rhs_completo <- paste(unlist(VARIABLES), collapse = " + ")

datos_bloque <- map(set_names(bloques_modelo), \(b) {
  codificadas |>
    filter(bloque == b, !is.na(si), !is.na(gobierno_derecha)) |>
    mutate(y = si)
})

modelos_a <- imap(datos_bloque, \(d, b) {
  cat(b, ": ", nrow(d), " votos\n", sep = "")
  estimar_lpm(rhs_completo, d)
})

coef_a <- imap_dfr(compact(modelos_a), \(m, b) mutate(coeficientes(m), bloque = b, .before = 1))
write_out(coef_a, "A_coeficientes.csv")

cat("\nDirección LLM: cambio en la probabilidad de votar Sí cuando la indicación va hacia el polo +\n")
coef_a |>
  filter(str_starts(term, "pol_")) |>
  mutate(
    eje = str_remove(term, "^pol_"),
    valor = sprintf("%+.3f%s", estimate, case_when(p_value < 0.01 ~ "**", p_value < 0.05 ~ "*", TRUE ~ ""))
  ) |>
  select(bloque, eje, valor) |>
  pivot_wider(names_from = bloque, values_from = valor) |>
  print(width = Inf)

cat("\nAutoría y gobierno\n")
coef_a |>
  filter(str_detect(term, "^autor|gobierno")) |>
  mutate(valor = sprintf("%+.3f%s", estimate, case_when(p_value < 0.01 ~ "**", p_value < 0.05 ~ "*", TRUE ~ ""))) |>
  select(bloque, term, valor) |>
  pivot_wider(names_from = bloque, values_from = valor) |>
  print(width = Inf)

# ------------------------------------------------------------------------------
# A2. ¿Qué explica más?
# ------------------------------------------------------------------------------

section("A2. ¿QUÉ EXPLICA MÁS? (R² within)")

r2_a <- imap_dfr(datos_bloque, \(d, b) {
  if (is.null(modelos_a[[b]])) return(NULL)
  completo <- r2_within(modelos_a[[b]])

  # Se agrega un grupo a la vez, en el orden de VARIABLES
  acumulado <- map_dbl(seq_along(VARIABLES), \(k) {
    r2_within(estimar_lpm(paste(unlist(VARIABLES[1:k]), collapse = " + "), d))
  })

  # Lo que pierde el modelo completo sin cada grupo
  sin_grupo <- map_dbl(seq_along(VARIABLES), \(k) {
    r2_within(estimar_lpm(paste(unlist(VARIABLES[-k]), collapse = " + "), d))
  })

  techo <- r2_techo(d$y, d$diputado_id, d$votacion_id)

  tibble(
    bloque = b,
    grupo = names(VARIABLES),
    r2_acumulado = acumulado,
    aporte_al_agregar = acumulado - lag(acumulado, default = 0),
    aporte_unico = completo - sin_grupo,
    r2_completo = completo,
    r2_techo_votacion = techo,
    r2_diputado = r2_medias(d$y, d$diputado_id),
    votos = nrow(d)
  )
})

write_out(r2_a, "A_r2_por_grupo.csv")

r2_a |>
  mutate(across(where(is.double), \(x) round(x, 3))) |>
  print(n = Inf, width = Inf)

cat(
  "\nr2_diputado: cuánto del Sí/No explica solo quién es el diputado.",
  "\nr2_completo: cuánto explica el modelo además del diputado (R² within).",
  "\nr2_techo_votacion: lo máximo que podría explicar cualquier característica de la votación.\n"
)

# ------------------------------------------------------------------------------
# A3. Dirección LLM por período
# ------------------------------------------------------------------------------

section("A3. DIRECCIÓN LLM POR PERÍODO")

coef_periodo <- imap_dfr(datos_bloque, \(d, b) {
  map_dfr(sort(unique(d$periodo)), \(p) {
    m <- estimar_lpm(rhs_completo, filter(d, periodo == p))
    if (is.null(m)) return(NULL)
    coeficientes(m) |>
      filter(str_starts(term, "pol_")) |>
      mutate(bloque = b, periodo = p, votos = nobs(m), .before = 1)
  })
})

write_out(coef_periodo, "A_direccion_por_periodo.csv")

coef_periodo |>
  mutate(eje = str_remove(term, "^pol_"), estimate = round(estimate, 3)) |>
  select(bloque, periodo, eje, estimate) |>
  pivot_wider(names_from = eje, values_from = estimate) |>
  arrange(bloque, periodo) |>
  print(n = Inf, width = Inf)

# ==============================================================================
# B. EVENTOS DISRUPTIVOS
# ==============================================================================

section("B. EVENTOS DISRUPTIVOS")

# Fecha de cada evento
fecha_evento <- function(tipo, valor) {
  switch(tipo,
    fecha = as.Date(valor),
    partido = suppressWarnings(min(votos$fecha[votos$partido_votacion == valor], na.rm = TRUE)),
    persona = suppressWarnings(min(
      votos$fecha[str_detect(votos$legislator, regex(valor, ignore_case = TRUE))],
      na.rm = TRUE
    ))
  )
}

eventos <- EVENTOS |>
  mutate(fecha = as.Date(map2_dbl(tipo, valor, fecha_evento), origin = "1970-01-01")) |>
  filter(is.finite(fecha))

print(eventos)

# Inicio y fin del período legislativo de cada fecha
limites_periodo <- function(fecha) {
  inicio <- inicio_legislativo(fecha)
  inicio <- inicio - ((inicio - 2010) %% 4)
  c(as.Date(paste0(inicio, "-03-11")), as.Date(paste0(inicio + 4, "-03-11")))
}

# Controles: contenido de la votación según el LLM
controles_evento <- paste(VARIABLES[c("Tipo de votación", "Tema", "Dirección LLM")], collapse = " + ")
controles_evento <- paste("autor +", controles_evento)

efectos_evento <- pmap_dfr(eventos, \(evento, tipo, valor, fecha) {
  limites <- limites_periodo(fecha)
  desde <- max(fecha - ventana_dias, limites[1])
  hasta <- min(fecha + ventana_dias, limites[2])

  ventana <- codificadas |>
    filter(fecha >= desde, fecha < hasta) |>
    mutate(
      post = as.integer(fecha >= !!fecha),
      tendencia = as.numeric(fecha - !!fecha) / 365
    )

  map_dfr(grupos_evento, \(g) {
    d_grupo <- filter(ventana, bloque == g | partido_votacion == g)
    map_dfr(names(resultados_evento), \(r) {
      d <- d_grupo |>
        mutate(y = .data[[r]]) |>
        filter(!is.na(y))
      if (n_distinct(d$post) < 2) return(NULL)
      m <- estimar_lpm(paste("post + tendencia +", controles_evento), d)
      if (is.null(m)) return(NULL)
      coeficientes(m) |>
        filter(term == "post") |>
        mutate(
          evento = evento,
          fecha_evento = fecha,
          grupo = g,
          resultado = r,
          media_antes = mean(d$y[d$post == 0]),
          votos = nobs(m),
          votaciones_antes = n_distinct(d$votacion_id[d$post == 0]),
          votaciones_despues = n_distinct(d$votacion_id[d$post == 1]),
          # identifican el efecto: diputados que votan antes y después
          diputados_ambos_lados = sum(tapply(d$post, d$diputado_id, \(x) n_distinct(x) == 2)),
          .before = 1
        ) |>
        select(-term)
    })
  })
})

write_out(efectos_evento, "B_efectos_eventos.csv")

efectos_evento |>
  mutate(
    efecto = sprintf("%+.3f%s", estimate, case_when(p_value < 0.01 ~ "**", p_value < 0.05 ~ "*", TRUE ~ "")),
    media_antes = round(media_antes, 2)
  ) |>
  select(evento, grupo, resultado, media_antes, efecto, votos, diputados_ambos_lados) |>
  print(n = Inf, width = Inf)

# Serie descriptiva por trimestre (sin controles)
series_evento <- codificadas |>
  mutate(trimestre = lubridate::floor_date(fecha, "quarter")) |>
  filter(bloque %in% setdiff(bloques_modelo, "Izquierda")) |>
  select(trimestre, bloque, all_of(names(resultados_evento))) |>
  pivot_longer(all_of(names(resultados_evento)), names_to = "resultado", values_to = "y") |>
  filter(!is.na(y)) |>
  summarise(proporcion = mean(y), votos = n(), .by = c(trimestre, bloque, resultado)) |>
  filter(votos >= 30)

write_out(series_evento, "B_series_trimestrales.csv")

# ==============================================================================
# FIGURAS
# ==============================================================================

section("FIGURAS")

etiqueta_eje <- set_names(EJES$etiqueta, paste0("pol_", EJES$eje))

plot_direccion <- coef_a |>
  filter(term %in% names(etiqueta_eje)) |>
  mutate(eje = factor(etiqueta_eje[term], levels = rev(EJES$etiqueta)),
         bloque = factor(bloque, levels = bloques_modelo)) |>
  ggplot(aes(x = estimate, y = eje, color = bloque)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high), position = position_dodge(width = 0.6)) +
  scale_color_manual(values = colores_bloque, name = NULL) +
  scale_x_continuous(labels = \(x) sprintf("%+.0f pts", 100 * x)) +
  labs(
    title = "¿Qué mueve el Sí? La dirección de la indicación según el LLM",
    subtitle = "Cambio en la probabilidad de votar Sí cuando el Sí va hacia el polo nombrado (vs. no tocar el eje)",
    x = NULL,
    y = NULL,
    caption = "Modelo de probabilidad lineal con efectos fijos por diputado, controlando autoría x gobierno, tipo de votación y tema. Naturaleza entra como Sustantiva / No sustantiva. IC 95%, errores agrupados por votación. Subsidiariedad es provisional."
  ) +
  theme_figura()

save_figure(plot_direccion, "01_que_mueve_el_si.png")

plot_r2 <- r2_a |>
  mutate(
    grupo = factor(grupo, levels = rev(names(VARIABLES))),
    bloque = factor(bloque, levels = bloques_modelo)
  ) |>
  ggplot(aes(x = aporte_unico, y = grupo, fill = bloque)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = colores_bloque, name = NULL) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 0.1), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "¿Qué explica más el Sí y el No?",
    subtitle = "Varianza que pierde el modelo completo si se quita cada grupo de variables (R² within)",
    x = "Aporte único al R² within",
    y = NULL,
    caption = "El efecto del diputado ya está descontado. Ver A_r2_por_grupo.csv para el aporte acumulado y el techo."
  ) +
  theme_figura()

save_figure(plot_r2, "02_que_explica_mas.png")

plot_periodo <- coef_periodo |>
  filter(term %in% names(etiqueta_eje)) |>
  mutate(eje = factor(etiqueta_eje[term], levels = EJES$etiqueta),
         bloque = factor(bloque, levels = bloques_modelo)) |>
  ggplot(aes(x = periodo, y = estimate, color = bloque, group = bloque)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(aes(ymin = conf_low, ymax = conf_high), position = position_dodge(width = 0.4)) +
  geom_line(position = position_dodge(width = 0.4), alpha = 0.6) +
  facet_wrap(vars(eje), ncol = 3) +
  scale_color_manual(values = colores_bloque, name = NULL) +
  labs(
    title = "Lo que mueve el Sí, por período legislativo",
    subtitle = "Efecto de que el Sí vaya hacia el polo nombrado",
    x = NULL,
    y = "Puntos de probabilidad",
    caption = "Subsidiariedad es provisional (kappa reducido 0,42)."
  ) +
  theme_figura(13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_figure(plot_periodo, "03_direccion_por_periodo.png")

if (nrow(efectos_evento) > 0) {
  plot_eventos <- efectos_evento |>
    mutate(
      resultado = factor(resultados_evento[resultado], levels = resultados_evento),
      evento = fct_reorder(evento, fecha_evento)
    ) |>
    ggplot(aes(x = estimate, y = fct_rev(evento), color = grupo)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    geom_pointrange(aes(xmin = conf_low, xmax = conf_high), position = position_dodge(width = 0.6)) +
    facet_wrap(vars(resultado), nrow = 1, labeller = label_wrap_gen(22)) +
    scale_color_manual(values = colores_bloque, name = NULL) +
    scale_x_continuous(labels = \(x) sprintf("%+.0f", 100 * x)) +
    labs(
      title = "Eventos disruptivos: cambio en la probabilidad de cada tipo de voto",
      subtitle = paste0("Después vs. antes del evento (", ventana_dias, " días a cada lado, dentro del período), a igual contenido de la votación"),
      x = "Puntos de probabilidad",
      y = NULL,
      caption = "Efectos fijos por diputado y tendencia lineal; IC 95%, errores agrupados por votación. El resultado hacia el polo pro-mercado es provisional."
    ) +
    theme_figura(12)

  save_figure(plot_eventos, "04_eventos_disruptivos.png")
}

if (nrow(series_evento) > 0) {
  plot_series <- series_evento |>
    mutate(resultado = factor(resultados_evento[resultado], levels = resultados_evento)) |>
    ggplot(aes(x = trimestre, y = proporcion, color = bloque)) +
    geom_vline(data = eventos, aes(xintercept = fecha), linetype = "dashed", color = "grey50") +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.2) +
    facet_wrap(vars(resultado), ncol = 1, scales = "free_y") +
    scale_color_manual(values = colores_bloque, name = NULL) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    labs(
      title = "Cómo vota la derecha en el tiempo",
      subtitle = paste0("Proporción trimestral, sin controles. Líneas punteadas: ", paste(eventos$evento, collapse = ", ")),
      x = NULL,
      y = NULL,
      caption = "El panel hacia el polo pro-mercado es provisional."
    ) +
    theme_figura(12)

  save_figure(plot_series, "05_series_eventos.png")
}

cat("\nResultados en: ", output_dir, "\nFiguras en: ", figures_dir, "\n", sep = "")

section("REGRESSION ANALYSIS COMPLETED")
