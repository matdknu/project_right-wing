# 19_izquierda_v2.R
# Corrige la definición del bloque de izquierda y reestima lo que depende de 04.
#
# PASO 1:
#   - inventario de etiquetas de partido 2010-2026, con diputados por año legislativo;
#   - propuesta editable partido -> bloque y familia para el cero de la escala P.
# PASO 2 (tras aprobar tabla_bloques.csv):
#   - copia aislada del proyecto en izquierda_v2/espejo, con copias de 04-18 que solo
#     cambian las listas de partidos y la familia FA; 15 no se corre (usa API) y sus
#     salidas, que no dependen de los partidos, se copian;
#   - validación: la copia con la definición original reproduce 04 y la Tabla 5;
#   - tabla antes/después.
#
# Todo se escribe en data/analisis/izquierda_v2/. No se sobrescriben salidas existentes
# ni se hacen llamadas a APIs.
#
# Uso: Rscript scripts/pipeline/19_izquierda_v2.R            (paso 1)
#      PASO=2 Rscript scripts/pipeline/19_izquierda_v2.R     (paso 2)

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(fixest)
})

PASO <- Sys.getenv("PASO", "1")

salida <- Sys.getenv("SALIDA", "data/analisis/izquierda_v2")
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Definición vigente (04_indicaciones_bcall.R, líneas 73-76)
# ------------------------------------------------------------

derecha_04 <- c("UDI", "RN", "EVOP", "REP", "PNL", "PSC")
izquierda_04 <- c("PC", "PS", "PPD", "FA", "FRVS", "PL", "PR")
cero_10 <- c("PC", "PS", "FA", "PPD")

# ------------------------------------------------------------
# 1. Inventario de etiquetas por año legislativo (11 mar - 10 mar)
# ------------------------------------------------------------

anio_legislativo <- function(fecha) {
  inicio <- if_else(
    format(fecha, "%m-%d") >= "03-11",
    as.integer(format(fecha, "%Y")),
    as.integer(format(fecha, "%Y")) - 1L
  )
  sprintf("%d-%02d", inicio, (inicio + 1L) %% 100L)
}

votos <- open_dataset("data/origen/votos_analiticos.parquet") |>
  filter(fecha >= as.Date("2010-03-11"), fecha <= as.Date("2026-03-10")) |>
  select(diputado_id, fecha, partido, partido_api) |>
  collect() |>
  mutate(
    partido = str_to_upper(str_squish(as.character(partido))),
    partido = if_else(is.na(partido) | partido == "", "OTRO", partido),
    tiempo = anio_legislativo(fecha)
  )

nombres <- read_parquet("data/origen/diputados_militancias_historicas.parquet") |>
  filter(!is.na(partido_nombre_api), partido_api == partido | partido == "REP") |>
  distinct(partido, nombre = partido_nombre_api) |>
  summarise(nombre = first(nombre), .by = partido)

inventario_largo <- votos |>
  distinct(tiempo, diputado_id, partido) |>
  count(tiempo, partido, name = "diputados")

inventario <- inventario_largo |>
  arrange(tiempo) |>
  pivot_wider(names_from = tiempo, values_from = diputados, values_fill = 0L) |>
  left_join(nombres, by = "partido") |>
  mutate(
    nombre = case_when(
      partido == "IND" ~ "Independientes",
      partido == "OTRO" ~ "Sin etiqueta",
      .default = nombre
    ),
    anios_presente = rowSums(across(matches("^\\d{4}-\\d{2}$")) > 0)
  ) |>
  relocate(partido, nombre, anios_presente) |>
  arrange(desc(anios_presente), partido)

# Etiquetas de la API que la limpieza fundió en IND (PDG e Igualdad en algunos tramos)
ind_por_api <- votos |>
  filter(partido == "IND") |>
  distinct(tiempo, diputado_id, partido_api) |>
  count(partido_api, tiempo, name = "diputados") |>
  arrange(partido_api, tiempo)

write_csv(inventario, file.path(salida, "01_inventario_etiquetas.csv"), na = "")
write_csv(inventario_largo, file.path(salida, "01_inventario_etiquetas_largo.csv"), na = "")
write_csv(ind_por_api, file.path(salida, "01_ind_por_etiqueta_api.csv"), na = "")

# ------------------------------------------------------------
# 2. Propuesta partido -> bloque (editable)
# ------------------------------------------------------------
# bloque_propuesto: izquierda / centro / derecha / IND / a decidir.
# familia_p: grupo que entra en el cero de la escala P (vacío = no entra).
# Las etiquetas "a decidir" llevan una sugerencia que no se aplica hasta aprobarla.

propuesta <- tribble(
  ~partido,  ~bloque_propuesto, ~sugerencia,  ~familia_p, ~nota,
  "PC",      "izquierda",       "",           "PC",       "Sección 4.1.",
  "PS",      "izquierda",       "",           "PS",       "Sección 4.1.",
  "PPD",     "izquierda",       "",           "PPD",      "Sección 4.1.",
  "PR",      "izquierda",       "",           "",         "Sección 4.1.",
  "FRVS",    "izquierda",       "",           "",         "Sección 4.1.",
  "PL",      "izquierda",       "",           "",         "Sección 4.1. Partido Liberal de Chile; misma organización que LIBERAL.",
  "LIBERAL", "izquierda",       "",           "",         "Partido Liberal de Chile con otra etiqueta; hoy queda en centro porque 04 busca PL.",
  "FA",      "izquierda",       "",           "FA",       "Frente Amplio fusionado (desde 2024).",
  "RD",      "izquierda",       "",           "FA",       "Componente del FA antes de la fusión.",
  "PCS",     "izquierda",       "",           "FA",       "Convergencia Social; componente del FA antes de la fusión.",
  "COMUNES", "izquierda",       "",           "FA",       "Componente del FA antes de la fusión.",
  "IC",      "a decidir",       "izquierda",  "",         "Izquierda Ciudadana, Nueva Mayoría 2014-2018; no figura en la sección 4.1.",
  "PH",      "a decidir",       "izquierda",  "",         "Fundador del FA en 2017, salió en 2018.",
  "PEV",     "a decidir",       "izquierda",  "",         "Ecologista Verde; integró el FA entre 2017 y 2020 aprox.",
  "PRO",     "a decidir",       "izquierda",  "",         "Partido Progresista (ME-O).",
  "PAH",     "a decidir",       "izquierda",  "",         "Acción Humanista; escisión del PH.",
  "PDG",     "a decidir",       "centro",     "",         "Partido de la Gente; populista, sin eje claro.",
  "DC",      "a decidir",       "centro",     "",         "Democracia Cristiana.",
  "DEM",     "a decidir",       "centro",     "",         "Demócratas; escisión de la DC (2022), apoyó a la derecha en 2025.",
  "AMA",     "a decidir",       "centro",     "",         "Amarillos por Chile; centro, cercano a la derecha desde 2023.",
  "PRI",     "a decidir",       "centro",     "",         "Regionalista Independiente.",
  "UCP",     "a decidir",       "centro",     "",         "Unión de Centro Progresista.",
  "AMPL",    "a decidir",       "derecha",    "",         "Amplitud; escisión de RN.",
  "PCC",     "a decidir",       "derecha",    "",         "Conservador Cristiano; listas con republicanos en 2021.",
  "PCCH",    "a decidir",       "derecha",    "",         "Partido Cristiano de Chile.",
  "UDI",     "derecha",         "",           "",         "",
  "RN",      "derecha",         "",           "",         "",
  "EVOP",    "derecha",         "",           "",         "",
  "REP",     "derecha",         "",           "",         "PRCh.",
  "PNL",     "derecha",         "",           "",         "",
  "PSC",     "derecha",         "",           "",         "En right_parties de 04.",
  "IND",     "IND",             "",           "",         "Se clasifican con la regla IND-D de 04.",
  "OTRO",    "IND",             "",           "",         "Sin etiqueta; mismo tratamiento que IND en 04."
)

faltan <- setdiff(inventario$partido, propuesta$partido)
if (length(faltan) > 0) {
  propuesta <- bind_rows(
    propuesta,
    tibble(partido = faltan, bloque_propuesto = "a decidir", sugerencia = "",
           familia_p = "", nota = "Etiqueta no prevista.")
  )
}

tabla <- propuesta |>
  left_join(select(inventario, partido, nombre, anios_presente), by = "partido") |>
  mutate(
    bloque_actual_04 = case_when(
      partido %in% derecha_04 ~ "derecha",
      partido %in% izquierda_04 ~ "izquierda",
      partido %in% c("IND", "OTRO") ~ "IND",
      .default = "centro y otros"
    ),
    en_cero_actual = partido %in% cero_10,
    cambia = bloque_actual_04 != bloque_propuesto
  ) |>
  filter(!is.na(anios_presente)) |>
  relocate(partido, nombre, bloque_actual_04, bloque_propuesto, sugerencia,
           familia_p, en_cero_actual, cambia, anios_presente, nota)

# La tabla editable no se sobrescribe si ya existe (puede tener decisiones).
archivo_tabla <- file.path(salida, "tabla_bloques.csv")
if (file.exists(archivo_tabla)) {
  message("Ya existe ", archivo_tabla, "; no se sobrescribe. Propuesta nueva en tabla_bloques_propuesta.csv")
  write_csv(tabla, file.path(salida, "tabla_bloques_propuesta.csv"), na = "")
} else {
  write_csv(tabla, archivo_tabla, na = "")
}

# ------------------------------------------------------------
# 3. Efecto de la propuesta: diputados por bloque y año
# ------------------------------------------------------------

efecto <- inventario_largo |>
  left_join(select(tabla, partido, bloque_actual_04, bloque_propuesto, sugerencia, familia_p),
            by = "partido") |>
  mutate(bloque_si_se_acepta = if_else(bloque_propuesto == "a decidir", sugerencia, bloque_propuesto))

por_bloque <- bind_rows(
  efecto |> mutate(bloque = bloque_actual_04) |>
    summarise(diputados = sum(diputados), .by = c(tiempo, bloque)) |>
    mutate(version = "actual_04"),
  efecto |> mutate(bloque = bloque_si_se_acepta) |>
    summarise(diputados = sum(diputados), .by = c(tiempo, bloque)) |>
    mutate(version = "propuesta_con_sugerencias")
) |>
  pivot_wider(names_from = tiempo, values_from = diputados, values_fill = 0L) |>
  arrange(version, bloque)

cero <- bind_rows(
  efecto |> filter(partido %in% cero_10) |>
    summarise(diputados = sum(diputados), partidos = paste(sort(partido), collapse = " "),
              .by = tiempo) |> mutate(version = "actual (PC PS PPD FA)"),
  efecto |> filter(familia_p != "") |>
    summarise(diputados = sum(diputados), partidos = paste(sort(partido), collapse = " "),
              .by = tiempo) |> mutate(version = "propuesta (PC PS PPD FA+componentes)")
) |>
  arrange(version, tiempo)

write_csv(por_bloque, file.path(salida, "01_diputados_por_bloque.csv"), na = "")
write_csv(cero, file.path(salida, "01_cero_escala_p.csv"), na = "")

options(width = 250)
cat("\nINVENTARIO DE ETIQUETAS (diputados con al menos un voto)\n")
print(inventario, n = Inf)
cat("\nPROPUESTA\n")
print(select(tabla, partido, bloque_actual_04, bloque_propuesto, sugerencia, familia_p, cambia), n = Inf)
cat("\nDIPUTADOS POR BLOQUE\n")
print(por_bloque, n = Inf)
cat("\nCERO DE LA ESCALA P\n")
print(cero, n = Inf)
cat("\nIND por etiqueta de la API\n")
print(ind_por_api |> pivot_wider(names_from = tiempo, values_from = diputados, values_fill = 0L))

if (PASO != "2") quit(save = "no")

# ==============================================================================
# PASO 2. Reestimación con la tabla aprobada
# ==============================================================================

seccion <- function(t) cat("\n============================================================\n", t,
                           "\n============================================================\n", sep = "")
exigir <- function(ok, mensaje) if (!isTRUE(ok)) stop(mensaje, call. = FALSE)

# ---- Tabla aprobada: las etiquetas "a decidir" toman la sugerencia -------------

aprobada <- read_csv(file.path(salida, "tabla_bloques.csv"), show_col_types = FALSE,
                     col_types = cols(.default = "c")) |>
  mutate(
    sugerencia = coalesce(sugerencia, ""),
    familia_p = coalesce(familia_p, ""),
    bloque_final = if_else(bloque_propuesto == "a decidir", sugerencia, bloque_propuesto)
  )
exigir(!any(aprobada$bloque_final %in% c("", NA)), "Hay etiquetas sin bloque final.")

izquierda_v2 <- aprobada |> filter(bloque_final == "izquierda") |> pull(partido)
derecha_v2 <- aprobada |> filter(bloque_final == "derecha") |> pull(partido)
familia_fa <- aprobada |> filter(familia_p == "FA", partido != "FA") |> pull(partido)
write_csv(select(aprobada, partido, nombre, bloque_actual_04, bloque_final, familia_p),
          file.path(salida, "02_tabla_aplicada.csv"), na = "")

cat("Izquierda v2: ", paste(izquierda_v2, collapse = " "), "\n")
cat("Derecha v2:   ", paste(derecha_v2, collapse = " "), "\n")
cat("Familia FA:   ", paste(familia_fa, collapse = " "), "\n")

# ---- Copia aislada del proyecto -----------------------------------------------

vec <- function(x) paste0("c(", paste0('"', x, '"', collapse = ", "), ")")

parchar <- function(texto, viejo, nuevo, archivo) {
  n <- str_count(texto, fixed(viejo))
  exigir(n == 1, paste0(archivo, ": se esperaba una ocurrencia de «", viejo, "», hay ", n))
  str_replace(texto, fixed(viejo), nuevo)
}

izq_04 <- 'c("PC", "PS", "PPD", "FA", "FRVS", "PL", "PR")'
der_04 <- 'c("UDI", "RN", "EVOP", "REP", "PNL", "PSC")'

# Únicos cambios a las copias: listas de partidos y familia FA en la unidad de 04
parches_v2 <- list(
  "04" = list(
    c(paste("right_parties <-", der_04), paste("right_parties <-", vec(derecha_v2))),
    c(paste("left_parties <-", izq_04),
      paste0("left_parties <- ", vec(izquierda_v2), "\n",
             "familia_fa <- setNames(rep(\"FA\", ", length(familia_fa), "), ", vec(familia_fa), ")\n",
             "a_familia <- function(g) unname(coalesce(familia_fa[g], g))")),
    c('known = list(grupo_votacion[grupo_votacion != "OTRO"]),',
      'known = list(a_familia(grupo_votacion[grupo_votacion != "OTRO"])),')
  ),
  "07" = list(c(paste("left_parties <-", izq_04), paste("left_parties <-", vec(izquierda_v2)))),
  "13" = list(c(paste("izquierda <-", izq_04), paste("izquierda <-", vec(izquierda_v2)))),
  "16" = list(c(paste("izquierda <-", izq_04), paste("izquierda <-", vec(izquierda_v2)))),
  "17" = list(
    c('right_parties <- c(partidos_derecha, "PSC")',
      paste0("right_parties <- unique(c(partidos_derecha, ", vec(setdiff(derecha_v2, c("REP", "PNL", "UDI", "RN", "EVOP"))), "))")),
    c(paste("izquierda <-", izq_04), paste("izquierda <-", vec(izquierda_v2)))
  ),
  "18" = list(c(paste("right_parties <-", der_04), paste("right_parties <-", vec(derecha_v2))))
)

scripts_cadena <- c("04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "16", "17", "18")

construir_espejo <- function(dir, parches) {
  dir.create(file.path(dir, "data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "scripts", "pipeline"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "logs"), showWarnings = FALSE)
  for (d in c("origen", "llm")) {
    if (!dir.exists(file.path(dir, "data", d))) {
      exigir(file.copy(file.path("data", d), file.path(dir, "data"), recursive = TRUE),
             paste("No se pudo copiar data/", d))
    }
  }
  writeLines("Raíz de la copia aislada de 19_izquierda_v2.R", file.path(dir, ".here"))
  for (s in scripts_cadena) {
    original <- list.files("scripts/pipeline", pattern = paste0("^", s, "_.*\\.R$"), full.names = TRUE)
    exigir(length(original) == 1, paste("No hay un único script", s))
    texto <- read_file(original)
    for (p in parches[[s]]) texto <- parchar(texto, p[1], p[2], basename(original))
    write_file(texto, file.path(dir, "scripts", "pipeline", basename(original)))
  }
  invisible(dir)
}

correr <- function(dir, s, entorno = character()) {
  script <- list.files(file.path(dir, "scripts", "pipeline"), pattern = paste0("^", s, "_.*\\.R$"))
  log <- file.path("logs", paste0(s, ".log"))
  t0 <- Sys.time()
  entorno <- if (length(entorno)) paste0(names(entorno), "=", entorno) else character()
  estado <- withr::with_dir(dir, system2("Rscript", file.path("scripts", "pipeline", script),
                                         stdout = log, stderr = log, env = entorno))
  cat(sprintf("  %s %-32s %s (%.1f min)\n", s, script, if (estado == 0) "ok" else "FALLA",
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  estado
}

# ---- Tabla 5: mismo modelo que 18 (F2), con los cuatro bloques -----------------

coeficientes_tabla5 <- function(archivo_votos) {
  EJES <- tribble(
    ~eje, ~polo_mas, ~polo_menos,
    "economica", "Pro-mercado", "Pro-Estado",
    "subsidiariedad", "Provisión privada", "Provisión estatal",
    "valorica", "Conservadora", "Progresista",
    "orden", "Orden y castigo", "Garantías",
    "nacion", "Soberanista", "Pluralista"
  )
  GOB <- tibble(inicio = as.Date(c("2010-03-11", "2014-03-11", "2018-03-11", "2022-03-11", "2026-03-11")),
                orientacion = c("derecha", "izquierda", "derecha", "izquierda", "derecha"))
  v <- read_csv(archivo_votos, col_types = cols(.default = "c"), na = "", progress = FALSE) |>
    mutate(fecha = as.Date(fecha), voto_bcall = as.integer(voto_bcall), votacion_id = as.integer(votacion_id),
           diputado_id = as.integer(diputado_id), llm_evaluable = as.logical(llm_evaluable)) |>
    filter(fecha >= as.Date("2016-03-11"), fecha <= as.Date("2026-03-10"), llm_evaluable %in% TRUE,
           !llm_objeto %in% "Admisibilidad", voto_bcall %in% c(-1L, 1L))
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
  )
  rhs <- "autor * gobierno_derecha + objeto + naturaleza + operacion + presupuesto + tema + pol_economica + pol_subsidiariedad + pol_valorica + pol_orden + pol_nacion"
  formula_lpm <- as.formula(paste("si ~", rhs, "| diputado_id"))
  map_dfr(c("Nueva derecha", "Derecha tradicional", "IND con la derecha", "Izquierda"), \(b) {
    d <- filter(v, bloque == b, !is.na(si), !is.na(gobierno_derecha))
    m <- feols(formula_lpm, data = d, vcov = ~votacion_id, warn = FALSE, notes = FALSE)
    ct <- coeftable(m)
    tibble(bloque = b, term = rownames(ct), estimate = ct[, 1], std_error = ct[, 2], p_value = ct[, 4],
           n = nobs(m), r2_r2 = r2(m, "r2"), r2_ar2 = r2(m, "ar2"), r2_wr2 = r2(m, "wr2"), r2_war2 = r2(m, "war2"))
  })
}

# ==============================================================================
# 2.1 Validación: la copia con la definición original reproduce 04 y la Tabla 5
# ==============================================================================

seccion("2.1 VALIDACIÓN DE LA COPIA AISLADA")

ref_t5 <- read_csv("data/analisis/regresiones/ventana_2016_2026/A_coeficientes_llm.csv", show_col_types = FALSE)
t5_base <- coeficientes_tabla5("data/analisis/bcall/votos_codificados.csv.gz")
chequeo_t5 <- inner_join(t5_base, ref_t5, by = c("bloque", "term"), suffix = c("", "_ref"))
exigir(nrow(chequeo_t5) == nrow(ref_t5) &&
         max(abs(chequeo_t5$estimate - chequeo_t5$estimate_ref)) < 1e-8 &&
         max(abs(chequeo_t5$std_error - chequeo_t5$std_error_ref)) < 1e-8,
       "El modelo de la Tabla 5 no reproduce A_coeficientes_llm.csv.")
tipo_r2 <- c("r2", "ar2", "wr2", "war2")[map_lgl(c("r2_r2", "r2_ar2", "r2_wr2", "r2_war2"),
                                                 \(k) max(abs(chequeo_t5[[k]] - chequeo_t5$r2_ref)) < 1e-8)]
exigir(length(tipo_r2) >= 1, "Ningún R² reproduce la columna r2 de A_coeficientes_llm.csv.")
tipo_r2 <- tipo_r2[1]
cat("Tabla 5 reproducida (estimaciones y errores por votación; R² =", tipo_r2, ").\n")

formato_t5 <- function(x) x |>
  transmute(bloque, term, estimate, std_error, p_value, n, r2 = .data[[paste0("r2_", tipo_r2)]])

espejo_base <- file.path(salida, "espejo_base")
if (!file.exists(file.path(espejo_base, "data", "analisis", "bcall", "bcall_unidades.csv"))) {
  construir_espejo(espejo_base, list())
  exigir(correr(espejo_base, "04") == 0, "04 sin cambios falla en la copia aislada.")
}
comparar_archivo <- function(nombre, claves) {
  a <- read_csv(file.path("data/analisis/bcall", nombre), show_col_types = FALSE, guess_max = 1e5)
  b <- read_csv(file.path(espejo_base, "data/analisis/bcall", nombre), show_col_types = FALSE, guess_max = 1e5)
  nrow(a) == nrow(b) && isTRUE(all.equal(arrange(a, across(all_of(claves))), arrange(b, across(all_of(claves))),
                                         check.attributes = FALSE, tolerance = 1e-10))
}
exigir(comparar_archivo("bcall_unidades.csv", c("universo", "eje", "periodo", "tiempo", "diputado_id")),
       "La copia con la definición original no reproduce bcall_unidades.csv.")
exigir(comparar_archivo("independientes_alineamiento.csv", c("periodo", "diputado_id")),
       "La copia con la definición original no reproduce independientes_alineamiento.csv.")
cat("04 sin cambios reproduce bcall_unidades.csv e independientes_alineamiento.csv.\n")

# ==============================================================================
# 2.2 Cadena con la izquierda v2
# ==============================================================================

seccion("2.2 CADENA 04-18 CON LA IZQUIERDA V2")

espejo <- file.path(salida, "espejo")
construir_espejo(espejo, parches_v2)
# 15 usa API; su salida (termómetro por texto) no depende de los partidos y viene en data/llm.
# El modelo de la Tabla 5 no tiene script propio: se reestima aquí y lo leen 11 y 18.
dir.create(file.path(espejo, "data", "analisis", "regresiones", "ventana_2016_2026"),
           recursive = TRUE, showWarnings = FALSE)

estados <- c()
for (s in c("04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "16")) {
  estados[s] <- correr(espejo, s)
  exigir(estados[s] == 0 || s %in% c("05", "06", "08", "09"),
         paste0("Falla ", s, ": ver ", file.path(espejo, "logs", paste0(s, ".log"))))
}

t5_v2 <- coeficientes_tabla5(file.path(espejo, "data/analisis/bcall/votos_codificados.csv.gz"))
write_csv(formato_t5(t5_v2),
          file.path(espejo, "data/analisis/regresiones/ventana_2016_2026/A_coeficientes_llm.csv"))

for (s in c("11", "17", "18")) estados[s] <- correr(espejo, s, c(REPLICAS = Sys.getenv("REPLICAS", "500")))
write_csv(tibble(script = names(estados), estado = unname(estados)), file.path(salida, "02_estado_cadena.csv"))

# ==============================================================================
# 2.3 Tabla antes / después
# ==============================================================================

seccion("2.3 ANTES Y DESPUÉS")

anios_nueva <- c("2022-23", "2023-24", "2024-25", "2025-26")

# unidad "pct": proporción mostrada en puntos (umbral 2); "num": umbral 0,05
extraer <- function(raiz) {
  leer <- function(...) {
    f <- file.path(raiz, ...)
    if (!file.exists(f)) return(NULL)
    read_csv(f, show_col_types = FALSE, guess_max = 1e5)
  }
  fila <- function(d, seccion, medida, clave, valor, unidad) {
    if (is.null(d) || nrow(d) == 0) return(NULL)
    tibble(seccion = seccion, medida = medida, clave = as.character(clave), valor = as.numeric(valor), unidad = unidad)
  }
  out <- list()

  sep <- leer("bloque1", "A_separacion_izquierda_derecha.csv")
  if (!is.null(sep)) out <- c(out, list(
    fila(sep, "1. Separación", "% mayorías distintas", sep$tiempo, sep$pct_mayorias_distintas, "pct"),
    fila(sep, "1. Separación", "Ω (solapamiento)", sep$tiempo, sep$solapamiento, "num"),
    fila(sep, "1. Separación", "δ (distancia estandarizada)", sep$tiempo, sep$distancia, "num"),
    fila(sep, "1. Separación", "Diputados de izquierda en B-Call", sep$tiempo, sep$n_izquierda, "n")
  ))

  pos <- leer("bcall_anio", "01_posicion_partido.csv")
  if (!is.null(pos)) {
    p <- pos |> filter(partido %in% c("REP", "UDI", "RN", "EVOP", "PNL"))
    w <- p |> select(tiempo, partido, mediana) |> pivot_wider(names_from = partido, values_from = mediana)
    esc <- distinct(pos, tiempo, cero, sd_anio, n_izquierda)
    out <- c(out, list(
      fila(p, "2. Tabla 3 (P)", paste("P", p$partido), p$tiempo, p$mediana, "num"),
      fila(w, "2. Tabla 3 (P)", "PRCh − UDI", w$tiempo, w$REP - w$UDI, "num"),
      fila(esc, "2. Tabla 3 (P)", "Cero (mediana d1 de la izquierda)", esc$tiempo, esc$cero, "num"),
      fila(esc, "2. Tabla 3 (P)", "sd de d1 en la Cámara", esc$tiempo, esc$sd_anio, "num"),
      fila(esc, "2. Tabla 3 (P)", "Diputados en el cero", esc$tiempo, esc$n_izquierda, "n")
    ))
    w4 <- filter(w, tiempo %in% anios_nueva)
    out <- c(out, list(fila(w4, "8. Tabla 6 (R)", "R posición", w4$tiempo, (w4$REP - w4$UDI) / w4$UDI, "num")))
  }

  ac <- leer("bloque1", "C_acuerdo_mayorias.csv")
  if (!is.null(ac)) out <- c(out, list(fila(ac, "3. Tabla 4 (acuerdo)", ac$par, ac$tiempo, ac$acuerdo, "pct")))

  rice <- leer("bloque1", "C_rice_conjunto_derecha.csv")
  if (!is.null(rice)) out <- c(out, list(fila(rice, "4. Δ", "Δ", rice$tiempo, rice$perdida, "num")))
  dw <- leer("revision", "A_delta_ponderado.csv")
  if (!is.null(dw)) out <- c(out, list(
    fila(dw, "4. Δ", "Δw", dw$tiempo, dw$delta_w, "num"),
    fila(dw, "4. Δ", "% con algún partido contra el bloque", dw$tiempo, dw$pct_alguno_contra, "pct")
  ))

  ind <- leer("revision", "D1_independientes_por_anio.csv")
  if (!is.null(ind)) out <- c(out, list(
    fila(ind, "5. IND con la derecha", "IND con la derecha (04)", ind$tiempo, ind$original, "n"),
    fila(ind, "5. IND con la derecha", "Con la mitad impar", ind$tiempo, ind$mitad_impar_derecha, "n"),
    fila(ind, "5. IND con la derecha", "Acuerdo mediano en la mitad par", ind$tiempo, ind$acuerdo_par_mediana_impar_derecha, "pct")
  ))

  acg <- leer("bloque2", "B_acuerdo_derecha_grupo.csv")
  pg <- leer("bloque2", "B_posicion_grupo.csv")
  if (!is.null(acg)) {
    a <- filter(acg, grupo %in% c("DEM", "DC", "IND-D"))
    out <- c(out, list(fila(a, "6. Centro", paste("Acuerdo con la derecha,", a$grupo), a$tiempo, a$acuerdo_derecha, "pct")))
  }
  if (!is.null(pg)) {
    a <- filter(pg, grupo %in% c("DEM", "DC"))
    out <- c(out, list(fila(a, "6. Centro", paste("P,", a$grupo), a$tiempo, a$posicion, "num")))
  }

  t5 <- leer("regresiones", "ventana_2016_2026", "A_coeficientes_llm.csv")
  if (!is.null(t5)) {
    a <- filter(t5, str_starts(term, "pol_"))
    out <- c(out, list(
      fila(a, "7. Tabla 5", paste(a$bloque, a$term), "2016-26", a$estimate, "pct"),
      fila(distinct(a, bloque, n), "7. Tabla 5", "Votos", distinct(a, bloque, n)$bloque, distinct(a, bloque, n)$n, "n")
    ))
  }

  r <- leer("revision", "F2_bootstrap_R.csv")
  if (!is.null(r)) out <- c(out, list(
    fila(r, "8. Tabla 6 (R)", "Nueva − tradicional", r$term, r$nueva_menos_tradicional, "pct"),
    fila(r, "8. Tabla 6 (R)", "Tradicional − izquierda", r$term, r$tradicional_menos_izquierda, "pct"),
    fila(r, "8. Tabla 6 (R)", "R (sin redondear)", r$term, r$R, "num"),
    fila(r, "8. Tabla 6 (R)", "R, percentil 2,5", r$term, r$R_p025, "num"),
    fila(r, "8. Tabla 6 (R)", "R, percentil 97,5", r$term, r$R_p975, "num"),
    fila(r, "8. Tabla 6 (R)", "Réplicas con denominador ≤ 0", r$term, r$den_cruza_cero, "pct")
  ))

  idx <- leer("bloque3", "A_indice_grupo_anio.csv")
  if (!is.null(idx)) out <- c(out, list(fila(idx, "9. Termómetro", paste("Índice I (Fig. 13),", idx$grupo), idx$tiempo, idx$indice, "num")))
  sd_ <- leer("bloque3", "B_si_por_direccion.csv")
  if (!is.null(sd_)) out <- c(out, list(fila(sd_, "9. Termómetro", paste("% Sí (Fig. 14),", sd_$grupo, "·", sd_$direccion),
                                             sd_$legislatura, sd_$si, "pct")))
  sen <- leer("bloque3", "B_sensibilidad_grupo_anio.csv")
  if (!is.null(sen)) out <- c(out, list(fila(sen, "9. Termómetro", paste("Sensibilidad b (Fig. A3),", sen$grupo), sen$tiempo, sen$estimate, "num")))
  pe <- leer("revision", "E_pendiente_legislatura.csv")
  if (!is.null(pe)) out <- c(out, list(fila(pe, "9. Termómetro", paste("Pendiente (E),", pe$grupo), pe$legislatura, pe$estimate, "num")))
  re <- leer("bloque3", "D_panel_reelectos.csv")
  if (!is.null(re)) {
    a <- re |> summarise(cambio = median(cambio, na.rm = TRUE), n = n(), .by = trayectoria) |> filter(n >= 5)
    out <- c(out, list(fila(a, "9. Termómetro", paste("Cambio mediano del índice, reelectos", a$trayectoria), "2018-22 → 2022-26", a$cambio, "num")))
  }

  t7 <- leer("bloque3", "C_lpm_mayorias_distintas.csv")
  if (!is.null(t7)) out <- c(out, list(
    fila(t7, "10. Tabla 7", paste(t7$term, "· coef."), "2022-26", t7$estimate, "num"),
    fila(t7, "10. Tabla 7", paste(t7$term, "· IC bajo"), "2022-26", t7$conf.low, "num"),
    fila(t7, "10. Tabla 7", paste(t7$term, "· IC alto"), "2022-26", t7$conf.high, "num")
  ))
  du <- leer("bloque3", "C_divide_une.csv")
  if (!is.null(du)) {
    a <- filter(du, dimension %in% c("Autoría", "Dirección", "Tópico"))
    out <- c(out, list(fila(a, "10. Tabla 7", paste("% dividen,", a$dimension, "·", a$categoria), "2022-26", a$distintas, "pct")))
  }
  v7 <- leer("revision", "B2_tabla7_versiones.csv")
  if (!is.null(v7)) {
    a <- mutate(v7, k = paste0(version, " · ", se_tipo))
    out <- c(out, list(
      fila(a, "10. Tabla 7 (versiones)", paste(a$k, "· coef."), "Ejecutivo", a$estimate, "num"),
      fila(a, "10. Tabla 7 (versiones)", paste(a$k, "· IC bajo"), "Ejecutivo", a$conf_low, "num"),
      fila(a, "10. Tabla 7 (versiones)", paste(a$k, "· IC alto"), "Ejecutivo", a$conf_high, "num"),
      fila(a, "10. Tabla 7 (versiones)", paste(a$k, "· p"), "Ejecutivo", a$p_value, "num")
    ))
  }

  pm <- leer("revision", "F1_permutacion_resumen.csv")
  if (!is.null(pm)) out <- c(out, list(
    fila(pm, "11. Permutación", "Distancia observada", pm$tiempo, pm$observada, "num"),
    fila(pm, "11. Permutación", "Nula, mediana", pm$tiempo, pm$nula_mediana, "num"),
    fila(pm, "11. Permutación", "Nula, percentil 95", pm$tiempo, pm$nula_p95, "num"),
    fila(pm, "11. Permutación", "p de una cola", pm$tiempo, pm$p_una_cola, "num")
  ))

  bind_rows(out)
}

antes <- extraer("data/analisis")
despues <- extraer(file.path(espejo, "data", "analisis"))

comparacion <- full_join(antes, despues, by = c("seccion", "medida", "clave", "unidad"),
                         suffix = c("_antes", "_despues")) |>
  mutate(
    escala = if_else(unidad == "pct", 100, 1),
    antes = valor_antes * escala,
    despues = valor_despues * escala,
    diferencia = despues - antes,
    umbral = case_when(unidad == "pct" ~ 2, unidad == "num" ~ 0.05, unidad == "n" ~ 0.5),
    cambia = !is.na(diferencia) & abs(diferencia) > umbral + 1e-12 |
      xor(is.na(valor_antes), is.na(valor_despues))
  ) |>
  select(seccion, medida, clave, unidad, antes, despues, diferencia, cambia) |>
  arrange(seccion, medida, clave)

write_csv(comparacion, file.path(salida, "03_antes_despues.csv"), na = "")

# Versión legible: negrita en lo que cambia más de 2 puntos (pct) o de 0,05 (num)
fmt <- function(x, u) case_when(is.na(x) ~ "—", u == "n" ~ format(round(x), big.mark = "."),
                                u == "pct" ~ formatC(x, format = "f", digits = 1, decimal.mark = ","),
                                TRUE ~ formatC(x, format = "f", digits = 3, decimal.mark = ","))
md <- comparacion |>
  mutate(
    a = fmt(antes, unidad), d = fmt(despues, unidad),
    dif = case_when(is.na(diferencia) ~ "—",
                    unidad == "num" ~ formatC(diferencia, format = "f", digits = 3, decimal.mark = ","),
                    TRUE ~ formatC(diferencia, format = "f", digits = 1, decimal.mark = ",")),
    across(c(a, d, dif), \(x) if_else(cambia, paste0("**", x, "**"), x)),
    linea = paste("|", seccion, "|", medida, "|", clave, "|", a, "|", d, "|", dif, "|")
  )
write_lines(c("| Sección | Medida | Año / clave | Antes | Después | Diferencia |",
              "|---|---|---|---:|---:|---:|", md$linea),
            file.path(salida, "03_antes_despues.md"))

resumen <- comparacion |> summarise(medidas = n(), cambian = sum(cambia), .by = seccion)
print(resumen, n = Inf)
cat("\nEscrito en", salida, "\n")
