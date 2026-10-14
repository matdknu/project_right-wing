# ==============================================================================
# 27_revista_modelo_conjunto.R
#
# B2 de la versión de revista. La Tabla 5 en un solo modelo, con interacciones
# bloque × eje, efectos fijos por diputado y errores agrupados en dos vías
# (diputado y votación). La brecha nueva − tradicional sale de un coeficiente
# (diferencia de los dos coeficientes del eje, con su error estándar), igual que
# las brechas de cada derecha con la izquierda.
#
# Especificación de cada bloque: la de 05/19 (autor × gobierno, objeto,
# naturaleza, operación, presupuesto, tema, los cinco ejes, | diputado). El efecto fijo
# es diputado × bloque, porque quien cambia de bloque (p. ej. de independiente a IND con la
# derecha) tiene un efecto distinto en cada modelo por separado.
# Versión A: todos los controles interactuados con el bloque (reproduce los
#   coeficientes de los cuatro modelos por separado; solo cambian los errores).
# Versión B: controles comunes, solo los cinco ejes interactuados con el bloque.
# Los errores a dos vías se comparan con los de una vía (votación) de la Tabla 5.
# Fuente de votos: la izquierda corregida de 19 (espejo); FUENTE=original usa la de 04.
#
# Entradas: votos_codificados.csv.gz (espejo de 19 u original).
# Salidas:  data/analisis/revista/b2_modelo_conjunto/.
# ==============================================================================

pacman::p_load(tidyverse, fixest, here)

fuente <- Sys.getenv("FUENTE", "corregida")
archivo <- if (fuente == "original") here("data", "analisis", "bcall", "votos_codificados.csv.gz") else
  here("data", "analisis", "revista", "a1_izquierda", "espejo", "data", "analisis", "bcall", "votos_codificados.csv.gz")
output_dir <- here("data", "analisis", "revista", "b2_modelo_conjunto", fuente)
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

armar <- function(interactuar_controles) {
  d <- v |> select(si, diputado_id, votacion_id, bloque, all_of(pol_vars))
  cols <- list()
  for (b in BLOQUES) {
    en <- as.numeric(v$bloque == b)
    for (p in pol_vars) cols[[paste0(p, "__", make.names(b))]] <- v[[p]] * en
    if (interactuar_controles) {
      for (j in colnames(X_ctrl)) {
        z <- X_ctrl[, j] * en
        if (var(z) > 0) cols[[paste0("c_", j, "__", make.names(b))]] <- z
      }
    }
  }
  if (!interactuar_controles) for (j in colnames(X_ctrl)) cols[[paste0("c_", j)]] <- X_ctrl[, j]
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

cat("Armando versión A (controles por bloque) …\n")
fitA <- ajustar(armar(TRUE))
cat("Armando versión B (controles comunes) …\n")
fitB <- ajustar(armar(FALSE))
res <- bind_rows(coef_eje(fitA, "A: controles por bloque"), coef_eje(fitB, "B: controles comunes"))
escribir(res, "01_coeficientes_y_brechas.csv")

# Reproducción de la Tabla 5 (por separado, error por votación) para la versión A
ref <- if (fuente == "original") read_csv(here("data", "analisis", "regresiones", "ventana_2016_2026", "A_coeficientes_llm.csv"), show_col_types = FALSE) else
  read_csv(here("data", "analisis", "revista", "a1_izquierda", "espejo", "data", "analisis", "regresiones", "ventana_2016_2026", "A_coeficientes_llm.csv"), show_col_types = FALSE)
chequeo <- res |> filter(version == "A: controles por bloque", tipo == "coeficiente") |>
  mutate(term = paste0("pol_", eje), bloque = contraste) |>
  inner_join(ref |> filter(str_starts(term, "pol_")), by = c("bloque", "term"), suffix = c("", "_tabla5"))
cat("\nVersión A frente a los modelos separados de la Tabla 5: máxima diferencia en el coeficiente (pp):",
    signif(max(abs(chequeo$estimate - 100 * chequeo$estimate_tabla5)), 3), "\n")
cat("Error estándar por votación de la versión A frente al de la Tabla 5, máxima diferencia (pp):",
    signif(max(abs(chequeo$se_votacion - 100 * chequeo$std_error)), 3), "\n")
escribir(select(chequeo, bloque, term, estimate_pp = estimate, tabla5_pp = estimate_tabla5, se_votacion, se_tabla5 = std_error), "02_chequeo_tabla5.csv")

cat("\nBrechas por eje (puntos porcentuales, errores a dos vías)\n")
print(res |> filter(tipo == "brecha", contraste %in% c("Nueva − tradicional", "Tradicional − izquierda")) |>
        select(version, eje, contraste, estimate, se_dos_vias, se_votacion, p_dos_vias, conf_low, conf_high) |>
        mutate(across(where(is.double), \(x) round(x, 3))), n = Inf, width = Inf)

# R con el coeficiente conjunto (delta method no; cociente de puntuales como la Tabla 6)
r <- res |> filter(version == "A: controles por bloque", tipo == "brecha", contraste %in% c("Nueva − tradicional", "Tradicional − izquierda")) |>
  select(eje, contraste, estimate) |> pivot_wider(names_from = contraste, values_from = estimate) |>
  mutate(R = `Nueva − tradicional` / `Tradicional − izquierda`)
escribir(r, "03_R_modelo_conjunto.csv")
print(mutate(r, across(where(is.double), \(x) round(x, 3))))
cat("\nEscrito en", output_dir, "\n")
