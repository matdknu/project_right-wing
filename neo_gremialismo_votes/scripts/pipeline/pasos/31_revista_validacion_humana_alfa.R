# ==============================================================================
# 31_revista_validacion_humana_alfa.R
#
# A2. Corre cuando existan las codificaciones humanas (no codifica nada).
# Lee, en data/analisis/revista/a2_validacion_humana/codificadas/ (o en
# CODIFICADAS_DIR), los archivos llenos:
#   ejes_A.{xlsx,csv}  ejes_B.{xlsx,csv}  termometro_A.{xlsx,csv}  termometro_B.{xlsx,csv}
# (la planilla con el mismo formato que las de planillas/), y los cruza por id con
# clave_privada/ (etiquetas del modelo y pesos de diseño).
#
# Calcula:
#   1. Alfa de Krippendorff por variable: A–B (entre personas), A–modelo y B–modelo
#      (personas frente al modelo) y A–B–modelo (tres codificadores). Métrica
#      nominal; ordinal para confianza y para el termómetro; intervalo para los
#      ejes en forma pol (+1 / 0 / −1 con la regla de confianza Alta/Media de la
#      Tabla 5). IC al 95 % por bootstrap de textos (REPLICAS, semilla fija).
#      También % de acuerdo (simple y ponderado por el diseño de la muestra).
#   2. Tabla 5 en la submuestra: los LPM por bloque de la Tabla 5 (misma
#      especificación) en las votaciones de los textos de la muestra, con los ejes
#      del modelo y con los de cada persona; muestra los cinco coeficientes pol_*.
#
# Salidas: data/analisis/revista/a2_validacion_humana/resultados/ (o SALIDA).
# ==============================================================================

pacman::p_load(tidyverse, fixest, readxl, here)

REPLICAS <- as.integer(Sys.getenv("REPLICAS", "1000"))
SEMILLA <- 20261014
base_dir <- here("data", "analisis", "revista", "a2_validacion_humana")
cod_dir <- Sys.getenv("CODIFICADAS_DIR", file.path(base_dir, "codificadas"))
output_dir <- Sys.getenv("SALIDA", file.path(base_dir, "resultados"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")

# ---- 0. Lectura y normalización ----------------------------------------------
leer_planilla <- function(nombre) {
  ruta <- list.files(cod_dir, pattern = paste0("^", nombre, "\\.(xlsx|csv)$"), full.names = TRUE)
  if (length(ruta) == 0) stop("Falta ", nombre, ".xlsx o .csv en ", cod_dir, call. = FALSE)
  ruta <- ruta[1]
  d <- if (str_detect(ruta, "xlsx$")) read_excel(ruta, sheet = 1, col_types = "text")
       else read_csv(ruta, col_types = cols(.default = "c"), na = "", progress = FALSE)
  d |> mutate(across(everything(), \(x) na_if(str_squish(x), "")))
}
canon <- function(x) str_squish(str_replace_all(str_to_lower(stringi::stri_trans_general(x, "Latin-ASCII")), "[^a-z0-9+-]+", " "))
a_cat <- function(x, validos) validos[match(canon(x), canon(validos))]
num_termo <- function(x) {
  y <- suppressWarnings(as.numeric(x)); out <- ifelse(is.na(y), x, as.character(as.integer(round(y))))
  ifelse(canon(out) == "no aplica", "No aplica", out)
}
CAT <- read_csv(file.path(base_dir, "categorias_permitidas.csv"), show_col_types = FALSE) |>
  mutate(opciones = map(categorias, \(x) str_split_1(x, " \\| ")))
validos <- \(planilla, variable) CAT$opciones[[which(CAT$planilla == planilla & CAT$variable == variable)]]

EJES <- tribble(
  ~eje, ~polo_mas, ~polo_menos,
  "economica", "Pro-mercado", "Pro-Estado", "subsidiariedad", "Provisión privada", "Provisión estatal",
  "valorica", "Conservadora", "Progresista", "orden", "Orden y castigo", "Garantías",
  "nacion", "Soberanista", "Pluralista")

normalizar <- function(d, planilla) {
  for (v in setdiff(names(d), c("id", "texto", "observaciones"))) {
    d[[v]] <- if (v == "termometro") num_termo(d[[v]]) else {
      val <- validos(planilla, v); coalesce(a_cat(d[[v]], val), if_else(is.na(d[[v]]), NA_character_, paste0("??", d[[v]])))
    }
  }
  d
}
invalidas <- \(d, nombre) {
  malos <- d |> select(-any_of(c("texto", "observaciones"))) |> pivot_longer(-id) |> filter(str_starts(value, "\\?\\?"))
  if (nrow(malos) > 0) { cat("Valores fuera de las categorías permitidas en", nombre, ":\n"); print(count(malos, name, value)); stop("Corrige la planilla.", call. = FALSE) }
}

ejes_A <- leer_planilla("ejes_A") |> normalizar("ejes"); ejes_B <- leer_planilla("ejes_B") |> normalizar("ejes")
term_A <- leer_planilla("termometro_A") |> normalizar("termometro"); term_B <- leer_planilla("termometro_B") |> normalizar("termometro")
walk2(list(ejes_A, ejes_B, term_A, term_B), c("ejes_A", "ejes_B", "termometro_A", "termometro_B"), invalidas)
clave_e <- read_csv(file.path(base_dir, "clave_privada", "ejes_clave.csv"), col_types = cols(.default = "c"), na = "") |> mutate(peso = as.numeric(peso))
clave_t <- read_csv(file.path(base_dir, "clave_privada", "termometro_clave.csv"), col_types = cols(.default = "c"), na = "") |> mutate(peso = as.numeric(peso))

# ---- 1. Alfa de Krippendorff --------------------------------------------------
# M: matriz codificadores × unidades (NA = sin dato). niveles: valores posibles ordenados.
alfa_k <- function(M, metrica = c("nominal", "ordinal", "intervalo"), niveles = NULL) {
  metrica <- match.arg(metrica)
  if (is.null(niveles)) niveles <- sort(unique(na.omit(as.vector(M))))
  if (length(niveles) < 2) return(NA_real_)
  K <- length(niveles); O <- matrix(0, K, K)
  for (u in seq_len(ncol(M))) {
    x <- match(M[, u], niveles); x <- x[!is.na(x)]; m <- length(x)
    if (m < 2) next
    for (i in seq_len(m)) for (j in seq_len(m)) if (i != j) O[x[i], x[j]] <- O[x[i], x[j]] + 1 / (m - 1)
  }
  nc <- rowSums(O); n <- sum(nc)
  if (n <= 1) return(NA_real_)
  delta <- switch(metrica,
    nominal = 1 - diag(K),
    intervalo = outer(seq_len(K), seq_len(K), \(a, b) (suppressWarnings(as.numeric(niveles[a])) - suppressWarnings(as.numeric(niveles[b])))^2),
    ordinal = outer(seq_len(K), seq_len(K), Vectorize(\(a, b) if (a == b) 0 else (sum(nc[min(a, b):max(a, b)]) - (nc[a] + nc[b]) / 2)^2)))
  Do <- sum(O * delta) / n
  De <- sum(outer(nc, nc) * delta) / (n * (n - 1))
  if (De == 0) return(NA_real_)
  1 - Do / De
}
acuerdo_simple <- function(x, y, w) {
  ok <- !is.na(x) & !is.na(y); if (!any(ok)) return(c(NA_real_, NA_real_))
  c(mean(x[ok] == y[ok]), weighted.mean(x[ok] == y[ok], w[ok]))
}

# Tabla de variables a comparar en una planilla: devuelve lista de tibbles por codificador
construir_ejes <- function() {
  M <- clave_e |> transmute(id, peso, m_evaluable = if_else(llm_evaluable == "TRUE", "Sí", "No"),
                            m_naturaleza = llm_naturaleza, m_presupuesto = llm_presupuesto,
                            m_economica = llm_economica, m_subsidiariedad = llm_subsidiariedad,
                            m_valorica = llm_valorica, m_orden = llm_orden, m_nacion = llm_nacion, m_confianza = llm_confianza)
  names(M) <- str_remove(names(M), "^m_")
  list(A = ejes_A, B = ejes_B, M = M)
}
agregar_derivadas <- function(d, con_eval = TRUE) {
  for (k in seq_len(nrow(EJES))) {
    e <- EJES$eje[k]
    d[[paste0("aplica_", e)]] <- if_else(is.na(d[[e]]), NA_character_, if_else(d[[e]] == "No aplica", "No", "Sí"))
    ok <- d$confianza %in% c("Alta", "Media") & d$evaluable %in% "Sí"
    pol <- case_when(d[[e]] == EJES$polo_mas[k] ~ 1, d[[e]] == EJES$polo_menos[k] ~ -1, !is.na(d[[e]]) ~ 0)
    d[[paste0("pol_", e)]] <- as.character(if_else(ok, pol, if_else(is.na(pol), NA_real_, 0)))
  }
  d
}
metricas <- function(v, planilla) {
  if (v == "confianza") list("ordinal", c("Baja", "Media", "Alta"))
  else if (str_starts(v, "pol_")) list("intervalo", c("-1", "0", "1"))
  else if (v == "termometro") list("ordinal", as.character(-3:3))
  else if (v == "termo_signo") list("ordinal", c("-1", "0", "1"))
  else list("nominal", NULL)
}

alfas <- function(R, w, variables, planilla, grupo = "todas") {
  ids <- R[[1]]$id
  pares <- list("A–B (personas)" = c("A", "B"), "A–modelo" = c("A", "M"), "B–modelo" = c("B", "M"), "A–B–modelo" = c("A", "B", "M"))
  map_dfr(variables, \(v) {
    mt <- metricas(v, planilla)
    imap_dfr(pares, \(who, nombre) {
      M <- do.call(rbind, map(who, \(r) R[[r]][[v]][match(ids, R[[r]]$id)]))
      est <- alfa_k(M, mt[[1]], mt[[2]])
      set.seed(SEMILLA)
      bs <- replicate(REPLICAS, { s <- sample(seq_along(ids), replace = TRUE); alfa_k(M[, s, drop = FALSE], mt[[1]], mt[[2]]) })
      ac <- if (length(who) == 2) acuerdo_simple(M[1, ], M[2, ], w) else c(NA_real_, NA_real_)
      tibble(planilla = planilla, variable = v, comparacion = nombre, metrica = mt[[1]],
             unidades = sum(colSums(!is.na(M)) >= 2), alfa = est,
             ic_bajo = quantile(bs, .025, na.rm = TRUE), ic_alto = quantile(bs, .975, na.rm = TRUE),
             acuerdo = ac[1], acuerdo_ponderado = ac[2])
    })
  })
}

E <- construir_ejes() |> map(agregar_derivadas)
ids_e <- E$M$id; w_e <- clave_e$peso[match(ids_e, clave_e$id)]
variables_e <- c("evaluable", "naturaleza", "presupuesto", EJES$eje, "confianza", paste0("aplica_", EJES$eje), paste0("pol_", EJES$eje))
res_e <- alfas(E, w_e, variables_e, "ejes")

# Termómetro
Tm <- list(A = term_A, B = term_B,
           M = clave_t |> transmute(id, peso, evaluable = if_else(modelo_evaluable == "TRUE", "Sí", "No"), termometro = modelo_termometro,
                                    mecanismo = modelo_mecanismo, confianza = modelo_confianza)) |>
  map(\(d) d |> mutate(aplica_termo = if_else(is.na(termometro), NA_character_, if_else(termometro == "No aplica", "No", "Sí")),
                       termometro_num = suppressWarnings(as.character(as.integer(termometro))),
                       termo_signo = as.character(sign(suppressWarnings(as.integer(termometro))))))
Tn <- Tm; for (r in names(Tn)) Tn[[r]]$termometro <- Tn[[r]]$termometro_num   # solo valores numéricos; «No aplica» queda NA
w_t <- clave_t$peso[match(Tm$M$id, clave_t$id)]
res_t <- bind_rows(
  alfas(Tm, w_t, c("evaluable", "aplica_termo", "mecanismo", "confianza"), "termometro"),
  alfas(Tn, w_t, c("termometro", "termo_signo"), "termometro (solo textos con valor numérico)"))

res <- bind_rows(res_e, res_t) |>
  mutate(lectura = case_when(is.na(alfa) ~ "sin datos o sin variación",
                             alfa >= 0.8 ~ "≥ 0,80: fiable", alfa >= 0.667 ~ "0,667–0,80: provisional", TRUE ~ "< 0,667: insuficiente"))
escribir(res, "01_alfa_krippendorff.csv")
cat("Alfa de Krippendorff (comparación; unidades; alfa [IC 95 %]; acuerdo)\n")
print(res |> mutate(across(c(alfa, ic_bajo, ic_alto, acuerdo, acuerdo_ponderado), \(x) round(x, 3))) |>
        select(planilla, variable, comparacion, unidades, alfa, ic_bajo, ic_alto, acuerdo, acuerdo_ponderado, lectura), n = Inf, width = Inf)

# Resumen: personas vs. personas y modelo
resumen <- res |> filter(comparacion %in% c("A–B (personas)", "A–modelo", "B–modelo")) |>
  select(planilla, variable, comparacion, alfa) |> pivot_wider(names_from = comparacion, values_from = alfa) |>
  mutate(`modelo−personas (media de A y B)` = (`A–modelo` + `B–modelo`) / 2 - `A–B (personas)`)
escribir(resumen, "02_alfa_resumen.csv")

# ---- 2. Tabla 5 en la submuestra ----------------------------------------------
BLOQUES <- c("Nueva derecha", "Derecha tradicional", "IND con la derecha", "Izquierda")
GOB <- tibble(inicio = as.Date(c("2010-03-11", "2014-03-11", "2018-03-11", "2022-03-11", "2026-03-11")),
              orientacion = c("derecha", "izquierda", "derecha", "izquierda", "derecha"))
archivo <- here("data", "analisis", "revista", "a1_izquierda", "espejo", "data", "analisis", "bcall", "votos_codificados.csv.gz")
v <- read_csv(archivo, col_types = cols(.default = "c"), na = "", progress = FALSE) |>
  mutate(fecha = as.Date(fecha), voto_bcall = as.integer(voto_bcall), votacion_id = as.integer(votacion_id),
         diputado_id = as.integer(diputado_id), llm_evaluable = as.logical(llm_evaluable)) |>
  filter(fecha >= as.Date("2016-03-11"), fecha <= as.Date("2026-03-10"), llm_evaluable %in% TRUE,
         !llm_objeto %in% "Admisibilidad", voto_bcall %in% c(-1L, 1L), bloque %in% BLOQUES)
ig <- findInterval(v$fecha, GOB$inicio); ig[ig == 0] <- NA
v$gobierno_derecha <- as.integer(GOB$orientacion[ig] == "derecha")
temas <- v |> distinct(votacion_id, llm_tema) |> count(llm_tema, sort = TRUE) |> filter(n >= 30, !is.na(llm_tema)) |> pull(llm_tema)
ind <- read_csv(here("data", "llm", "resultados", "indicaciones_codificadas.csv"), col_types = cols(.default = "c"), na = "") |>
  transmute(votacion_id = as.integer(votacion_id), texto_id)
mapa <- ind |> filter(texto_id %in% clave_e$texto_id)           # texto_id → todas sus votaciones
v <- v |> inner_join(mapa, by = "votacion_id") |> left_join(select(clave_e, texto_id, id_texto = id), by = "texto_id")

pol_de <- function(d_codif, fuente) {
  # d_codif: tibble con id y pol_*; devuelve v con pol_* de la fuente
  out <- v
  for (e in EJES$eje) out[[paste0("pol_", e)]] <- if (fuente == "Modelo") {
    x <- out[[paste0("llm_", e)]]
    pol <- case_when(x == EJES$polo_mas[match(e, EJES$eje)] ~ 1L, x == EJES$polo_menos[match(e, EJES$eje)] ~ -1L, TRUE ~ 0L)
    if_else(out$llm_confianza %in% c("Alta", "Media"), pol, 0L)
  } else as.integer(d_codif[[paste0("pol_", e)]][match(out$id_texto, d_codif$id)])
  out |> filter(if_all(all_of(paste0("pol_", EJES$eje)), \(x) !is.na(x)))
}
tabla5 <- function(d) {
  d <- d |> mutate(si = as.integer(voto_bcall == 1L),
    autor = fct_relevel(factor(autor_heuristico), "Parlamentaria"), objeto = fct_relevel(factor(llm_objeto), "Indicación"),
    naturaleza = factor(case_when(llm_naturaleza == "Sustantiva" ~ "Sustantiva", llm_naturaleza %in% c("Técnica", "Procedimental") ~ "No sustantiva"),
                        levels = c("Sustantiva", "No sustantiva")),
    operacion = fct_relevel(factor(llm_operacion), "Agregar"), presupuesto = fct_relevel(factor(llm_presupuesto), "No"),
    tema = factor(if_else(llm_tema %in% temas, llm_tema, "Otros temas"), levels = c(temas, "Otros temas"))) |>
    filter(!is.na(si), !is.na(gobierno_derecha))
  map_dfr(BLOQUES, \(b) {
    db <- filter(d, bloque == b)
    if (nrow(db) < 200) return(tibble())
    # Se descartan los controles sin variación en la submuestra
    ctrl <- c("autor * gobierno_derecha", "objeto", "naturaleza", "operacion", "presupuesto", "tema")
    ctrl <- ctrl[map_lgl(ctrl, \(z) all(map_int(all.vars(parse(text = z)[[1]]), \(a) n_distinct(db[[a]])) > 1))]
    f <- as.formula(paste("si ~", paste(c(ctrl, paste0("pol_", EJES$eje)), collapse = " + "), "| diputado_id"))
    m <- tryCatch(feols(f, db, vcov = ~votacion_id, warn = FALSE, notes = FALSE), error = \(e) NULL)
    if (is.null(m)) return(tibble())
    ct <- as_tibble(coeftable(m), rownames = "termino") |> filter(str_starts(termino, "pol_"))
    if (nrow(ct) == 0) return(tibble())
    ct |> transmute(bloque = b, eje = str_remove(termino, "pol_"), coef_pp = 100 * Estimate, ee_pp = 100 * `Std. Error`,
                    p = `Pr(>|t|)`, n_obs = nobs(m), votaciones = n_distinct(db$votacion_id))
  })
}
fuentes <- list("Modelo" = NULL, "Codificador A" = E$A, "Codificador B" = E$B)
res5 <- imap_dfr(fuentes, \(cd, nombre) tabla5(pol_de(cd, nombre)) |> mutate(fuente = nombre, .before = 1))
escribir(res5, "03_tabla5_submuestra.csv")
cat("\nTabla 5 en la submuestra (puntos porcentuales por unidad de pol)\n")
if (nrow(res5) > 0) print(res5 |> mutate(across(where(is.double), \(x) round(x, 3))) |>
  pivot_wider(id_cols = c(bloque, eje), names_from = fuente, values_from = c(coef_pp, ee_pp)), n = Inf, width = Inf)
cat("\nEscrito en", output_dir, "\n")
