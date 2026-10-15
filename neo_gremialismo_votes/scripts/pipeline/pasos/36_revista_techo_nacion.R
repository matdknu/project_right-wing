# ==============================================================================
# 36_revista_techo_nacion.R
#
# Prueba del techo en nación (y en los demás ejes): probabilidad de votar Sí de
# cada bloque ante un texto que va hacia el polo de derecha del eje (+1), ante
# uno que no toca el eje (0) y ante uno hacia el polo contrario (−1). Es la
# especificación A de la Tabla 5 (un modelo por bloque, efecto fijo diputado,
# errores por votación), con la izquierda corregida (espejo del 19).
#
# Dos versiones:
#   - Observada: % de Sí entre los votos de cada bloque en las votaciones con
#     pol = +1, 0 y −1 (con el número de votaciones).
#   - Predicha: promedio de la probabilidad predicha por el modelo de cada bloque
#     sobre todas sus observaciones, fijando el eje en +1, 0 y −1 y dejando lo demás
#     como está (efecto fijo del diputado incluido).
# También reproduce los coeficientes de la Tabla 5 por bloque (incluida la columna
# de izquierda y el número de votos) para el apéndice.
#
# Salidas: data/analisis/revista/c_paper/36_*.csv
# ==============================================================================

pacman::p_load(tidyverse, fixest, here)

output_dir <- here("data", "analisis", "revista", "c_paper")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")

EJES <- tribble(
  ~eje, ~polo_mas, ~polo_menos,
  "economica", "Pro-mercado", "Pro-Estado", "subsidiariedad", "Provisión privada", "Provisión estatal",
  "valorica", "Conservadora", "Progresista", "orden", "Orden y castigo", "Garantías", "nacion", "Soberanista", "Pluralista")
BLOQUES <- c("Nueva derecha", "Derecha tradicional", "IND con la derecha", "Izquierda")
GOB <- tibble(inicio = as.Date(c("2010-03-11", "2014-03-11", "2018-03-11", "2022-03-11", "2026-03-11")),
              orientacion = c("derecha", "izquierda", "derecha", "izquierda", "derecha"))

v <- read_csv(here("data", "analisis", "revista", "a1_izquierda", "espejo", "data", "analisis", "bcall", "votos_codificados.csv.gz"),
              col_types = cols(.default = "c"), na = "", progress = FALSE) |>
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
temas <- v |> distinct(votacion_id, llm_tema) |> count(llm_tema, sort = TRUE) |> filter(n >= 30, !is.na(llm_tema)) |> pull(llm_tema)
v <- v |> mutate(
  si = as.integer(voto_bcall == 1L),
  autor = fct_relevel(factor(autor_heuristico), "Parlamentaria"), objeto = fct_relevel(factor(llm_objeto), "Indicación"),
  naturaleza = factor(case_when(llm_naturaleza == "Sustantiva" ~ "Sustantiva", llm_naturaleza %in% c("Técnica", "Procedimental") ~ "No sustantiva"),
                      levels = c("Sustantiva", "No sustantiva")),
  operacion = fct_relevel(factor(llm_operacion), "Agregar"), presupuesto = fct_relevel(factor(llm_presupuesto), "No"),
  tema = factor(if_else(llm_tema %in% temas, llm_tema, "Otros temas"), levels = c(temas, "Otros temas"))
) |> filter(!is.na(si), !is.na(gobierno_derecha))

pol_vars <- paste0("pol_", EJES$eje)
f <- as.formula(paste("si ~ autor * gobierno_derecha + objeto + naturaleza + operacion + presupuesto + tema +",
                      paste(pol_vars, collapse = " + "), "| diputado_id"))

coefs <- list(); obs <- list(); pred <- list(); tam <- list(); att <- list()
for (b in BLOQUES) {
  d <- filter(v, bloque == b)
  m <- feols(f, d, vcov = ~votacion_id, warn = FALSE, notes = FALSE)
  ct <- coeftable(m)
  coefs[[b]] <- tibble(bloque = b, eje = EJES$eje, coef_pp = 100 * ct[pol_vars, 1], ee_pp = 100 * ct[pol_vars, 2], p = ct[pol_vars, 4])
  tam[[b]] <- tibble(bloque = b, votos = nrow(d), diputados = n_distinct(d$diputado_id), votaciones = n_distinct(d$votacion_id))
  for (e in EJES$eje) {
    pv <- paste0("pol_", e)
    obs[[length(obs) + 1]] <- d |> summarise(si_pct = 100 * mean(si), votos = n(), votaciones = n_distinct(votacion_id), .by = all_of(pv)) |>
      rename(pol = all_of(pv)) |> mutate(bloque = b, eje = e)
    # Sobre los votos de los textos que sí van hacia el polo de derecha (pol = +1): Sí observado,
    # Sí predicho tal como son y Sí predicho si el mismo texto no tocara el eje (pol = 0)
    sub <- d[d[[pv]] == 1L, ]
    if (nrow(sub) > 0) {
      nd0 <- sub; nd0[[pv]] <- 0L
      att[[length(att) + 1]] <- tibble(bloque = b, eje = e, votos = nrow(sub), votaciones = n_distinct(sub$votacion_id),
        si_observado_pct = 100 * mean(sub$si), si_predicho_pct = 100 * mean(predict(m, newdata = sub)),
        si_sin_tocar_eje_pct = 100 * mean(predict(m, newdata = nd0)))
    }
    for (valor in c(1L, 0L, -1L)) {
      nd <- d; nd[[pv]] <- valor
      pred[[length(pred) + 1]] <- tibble(bloque = b, eje = e, pol = valor, si_predicho_pct = 100 * mean(predict(m, newdata = nd)))
    }
  }
}
att <- bind_rows(att) |> mutate(aumento_pp = si_predicho_pct - si_sin_tocar_eje_pct)
escribir(att, "36_techo_sobre_textos_del_polo.csv")
coefs <- bind_rows(coefs); obs <- bind_rows(obs); pred <- bind_rows(pred); tam <- bind_rows(tam)
escribir(coefs, "36_tabla5_coeficientes_por_bloque.csv"); escribir(tam, "36_tabla5_tamanos.csv")
escribir(obs, "36_si_observado_por_polo.csv"); escribir(pred, "36_si_predicho_por_polo.csv")

cat("Tabla 5, izquierda corregida (pp)\n")
print(coefs |> select(bloque, eje, coef_pp) |> mutate(coef_pp = round(coef_pp, 1)) |> pivot_wider(names_from = bloque, values_from = coef_pp))
print(tam)

tab <- left_join(obs |> rename(si_observado_pct = si_pct), pred, by = c("bloque", "eje", "pol")) |>
  mutate(bloque = factor(bloque, BLOQUES)) |> arrange(eje, bloque, desc(pol))
escribir(tab, "36_techo.csv")
cat("\nNación: % de Sí observado y predicho, por bloque y polo del texto\n")
print(tab |> filter(eje == "nacion") |> mutate(across(where(is.double), \(x) round(x, 1))), n = Inf)
cat("\nSobre los votos de textos del polo de derecha (pol = +1): Sí observado, predicho y predicho si el texto no tocara el eje\n")
print(att |> filter(eje %in% c("nacion", "valorica")) |> mutate(across(where(is.double), \(x) round(x, 1))), n = Inf)
cat("\nValórico\n"); print(tab |> filter(eje == "valorica") |> mutate(across(where(is.double), \(x) round(x, 1))), n = Inf)
cat("Escrito en", output_dir, "\n")
