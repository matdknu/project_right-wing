# ==============================================================================
# 28_revista_modelo_conjunto_C.R
#
# B2 (cierre). Especificación C del modelo conjunto y A sin la Ley de Presupuestos.
#   A: todos los controles interactuados con el bloque (= Tabla 5 por separado).
#   B: controles comunes; solo los cinco ejes × bloque.
#   C: controles comunes, más la interacción bloque × (autoría × gobierno); el resto
#      de los controles (objeto, naturaleza, operación, presupuesto, tema) comunes.
#   A sin presupuesto: A en las votaciones con llm_presupuesto == "No".
# Todas con efecto fijo diputado × bloque y errores agrupados por diputado y votación.
# Reporta la brecha nueva − tradicional (y las demás) por eje, lado a lado.
# Fuente de votos: izquierda corregida de 19 (espejo).
#
# Salidas: data/analisis/revista/b2_modelo_conjunto/especificaciones/.
# ==============================================================================

pacman::p_load(tidyverse, fixest, here)

fuente <- "corregida"
archivo <- here("data", "analisis", "revista", "a1_izquierda", "espejo", "data", "analisis", "bcall", "votos_codificados.csv.gz")
output_dir <- here("data", "analisis", "revista", "b2_modelo_conjunto", "especificaciones")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")

EJES <- tribble(
  ~eje, ~polo_mas, ~polo_menos,
  "economica", "Pro-mercado", "Pro-Estado",
  "subsidiariedad", "Provisión privada", "Provisión estatal",
  "valorica", "Conservadora", "Progresista",
  "orden", "Orden y castigo", "Garantías",
  "nacion", "Soberanista", "Pluralista"
)
BLOQUES <- c("Nueva derecha", "Derecha tradicional", "IND con la derecha", "Izquierda")
GOB <- tibble(inicio = as.Date(c("2010-03-11", "2014-03-11", "2018-03-11", "2022-03-11", "2026-03-11")),
              orientacion = c("derecha", "izquierda", "derecha", "izquierda", "derecha"))

v <- read_csv(archivo, col_types = cols(.default = "c"), na = "", progress = FALSE) |>
  mutate(fecha = as.Date(fecha), voto_bcall = as.integer(voto_bcall), votacion_id = as.integer(votacion_id),
         diputado_id = as.integer(diputado_id), llm_evaluable = as.logical(llm_evaluable)) |>
  filter(fecha >= as.Date("2016-03-11"), fecha <= as.Date("2026-03-10"), llm_evaluable %in% TRUE,
         !llm_objeto %in% "Admisibilidad", voto_bcall %in% c(-1L, 1L), bloque %in% BLOQUES)
ig <- findInterval(v$fecha, GOB$inicio); ig[ig == 0] <- NA
v$gobierno_derecha <- as.integer(GOB$orientacion[ig] == "derecha")
for (k in seq_len(nrow(EJES))) {
  x <- v[[paste0("llm_", EJES$eje[k])]]
  pol <- case_when(x == EJES$polo_mas[k] ~ 1L, x == EJES$polo_menos[k] ~ -1L, TRUE ~ 0L)
  v[[paste0("pol_", EJES$eje[k])]] <- if_else(v$llm_confianza %in% c("Alta", "Media"), pol, 0L)
}
temas <- v |> distinct(votacion_id, llm_tema) |> count(llm_tema, sort = TRUE) |>
  filter(n >= 30, !is.na(llm_tema)) |> pull(llm_tema)
v <- v |> mutate(
  si = as.integer(voto_bcall == 1L),
  autor = fct_relevel(factor(autor_heuristico), "Parlamentaria"),
  objeto = fct_relevel(factor(llm_objeto), "Indicación"),
  naturaleza = factor(case_when(llm_naturaleza == "Sustantiva" ~ "Sustantiva",
                                llm_naturaleza %in% c("Técnica", "Procedimental") ~ "No sustantiva"),
                      levels = c("Sustantiva", "No sustantiva")),
  operacion = fct_relevel(factor(llm_operacion), "Agregar"),
  presupuesto = fct_relevel(factor(llm_presupuesto), "No"),
  tema = factor(if_else(llm_tema %in% temas, llm_tema, "Otros temas"), levels = c(temas, "Otros temas"))
) |> filter(!is.na(si), !is.na(gobierno_derecha))

pol_vars <- paste0("pol_", EJES$eje)
X_ctrl <- model.matrix(~ autor * gobierno_derecha + objeto + naturaleza + operacion + presupuesto + tema, v)[, -1, drop = FALSE]
colnames(X_ctrl) <- make.names(colnames(X_ctrl))

es_autor_gob <- grepl("^autor|^gobierno_derecha", colnames(X_ctrl))

armar <- function(datos, X, modo) {
  d <- datos |> select(si, diputado_id, votacion_id, bloque, all_of(pol_vars))
  cols <- list()
  for (b in BLOQUES) {
    en <- as.numeric(datos$bloque == b)
    for (p in pol_vars) cols[[paste0(p, "__", make.names(b))]] <- datos[[p]] * en
    if (modo == "A") {
      for (j in colnames(X)) {
        z <- X[, j] * en
        if (var(z) > 0) cols[[paste0("c_", j, "__", make.names(b))]] <- z
      }
    }
    if (modo == "C") {
      for (j in colnames(X)[es_autor_gob]) {
        z <- X[, j] * en
        if (var(z) > 0) cols[[paste0("c_", j, "__", make.names(b))]] <- z
      }
    }
  }
  comunes <- switch(modo, A = character(), B = colnames(X), C = colnames(X)[!es_autor_gob])
  for (j in comunes) if (var(X[, j]) > 0) cols[[paste0("c_", j)]] <- X[, j]
  bind_cols(d, as_tibble(cols))
}

ajustar <- function(d) {
  rhs <- setdiff(names(d), c("si", "diputado_id", "votacion_id", "bloque", pol_vars))
  m <- feols(as.formula(paste("si ~", paste0("`", rhs, "`", collapse = " + "), "| diputado_id^bloque")), d,
             vcov = ~diputado_id + votacion_id, warn = FALSE, notes = FALSE)
  list(m = m, m1 = summary(m, vcov = ~votacion_id))
}

coef_eje <- function(fit, version) {
  m <- fit$m; m1 <- fit$m1
  b <- coef(m); V <- vcov(m); V1 <- vcov(m1)
  ids <- expand_grid(eje = EJES$eje, bloque = BLOQUES)
  ids$nombre <- paste0("pol_", ids$eje, "__", make.names(ids$bloque))
  filas <- pmap_dfr(ids, \(eje, bloque, nombre) tibble(
    version = version, eje = eje, tipo = "coeficiente", contraste = bloque,
    estimate = unname(b[nombre]), se_dos_vias = sqrt(V[nombre, nombre]), se_votacion = sqrt(V1[nombre, nombre])))
  contrastes <- tribble(
    ~contraste, ~a, ~b,
    "Nueva − tradicional", "Nueva derecha", "Derecha tradicional",
    "Nueva − IND con la derecha", "Nueva derecha", "IND con la derecha",
    "Tradicional − izquierda", "Derecha tradicional", "Izquierda",
    "Nueva − izquierda", "Nueva derecha", "Izquierda"
  )
  brechas <- expand_grid(eje = EJES$eje, contrastes) |>
    pmap_dfr(\(eje, contraste, a, b2) {
      na <- paste0("pol_", eje, "__", make.names(a)); nb <- paste0("pol_", eje, "__", make.names(b2))
      tibble(version = version, eje = eje, tipo = "brecha", contraste = contraste,
             estimate = unname(b[na] - b[nb]),
             se_dos_vias = sqrt(V[na, na] + V[nb, nb] - 2 * V[na, nb]),
             se_votacion = sqrt(V1[na, na] + V1[nb, nb] - 2 * V1[na, nb]))
    })
  bind_rows(filas, brechas) |>
    mutate(estimate = 100 * estimate, se_dos_vias = 100 * se_dos_vias, se_votacion = 100 * se_votacion,
           z = estimate / se_dos_vias, p_dos_vias = 2 * pnorm(-abs(z)),
           conf_low = estimate - 1.96 * se_dos_vias, conf_high = estimate + 1.96 * se_dos_vias,
           n = nobs(m), diputados = n_distinct(v$diputado_id), votaciones = n_distinct(v$votacion_id))
}

sin_pres <- v$llm_presupuesto == "No"
cat("Ajustando A, B, C y A sin presupuesto …\n")
res <- bind_rows(
  coef_eje(ajustar(armar(v, X_ctrl, "A")), "A: controles por bloque"),
  coef_eje(ajustar(armar(v, X_ctrl, "B")), "B: controles comunes"),
  coef_eje(ajustar(armar(v, X_ctrl, "C")), "C: comunes + bloque × autoría × gobierno"),
  coef_eje(ajustar(armar(v[sin_pres, ], X_ctrl[sin_pres, , drop = FALSE], "A")), "A sin Ley de Presupuestos")
)
res <- res |> mutate(votaciones = if_else(version == "A sin Ley de Presupuestos", n_distinct(v$votacion_id[sin_pres]), votaciones),
                      diputados = if_else(version == "A sin Ley de Presupuestos", n_distinct(v$diputado_id[sin_pres]), diputados))
escribir(res, "01_coeficientes_y_brechas.csv")

ref <- read_csv(here("data", "analisis", "revista", "a1_izquierda", "espejo", "data", "analisis", "regresiones", "ventana_2016_2026", "A_coeficientes_llm.csv"), show_col_types = FALSE)
chequeo <- res |> filter(version == "A: controles por bloque", tipo == "coeficiente") |>
  mutate(term = paste0("pol_", eje), bloque = contraste) |>
  inner_join(ref |> filter(str_starts(term, "pol_")), by = c("bloque", "term"), suffix = c("", "_tabla5"))
cat("\nA frente a la Tabla 5 por separado, diferencia máxima del coeficiente (pp):",
    signif(max(abs(chequeo$estimate - 100 * chequeo$estimate_tabla5)), 3), "\n")

lado <- res |> filter(tipo == "brecha") |>
  mutate(celda = sprintf("%+.1f (%.1f; p=%.3f)", estimate, se_dos_vias, p_dos_vias)) |>
  select(eje, contraste, version, celda) |>
  pivot_wider(names_from = version, values_from = celda)
escribir(lado, "02_brechas_lado_a_lado.csv")
cat("\nBrechas por eje, puntos (EE a dos vías; p)\n")
print(lado |> filter(contraste %in% c("Nueva − tradicional", "Tradicional − izquierda")), n = Inf, width = Inf)

n_obs <- res |> distinct(version, n, votaciones)
escribir(n_obs, "03_tamanos.csv"); print(n_obs)
cat("\nEscrito en", output_dir, "\n")
