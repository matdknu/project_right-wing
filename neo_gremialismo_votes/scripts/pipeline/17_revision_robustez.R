# ==============================================================================
# 17_revision_robustez.R
#
# Revisión de los hallazgos del artículo. No reestima B-Call ni el LLM: lee las
# salidas de 04, 12, 13, 14, 15 y 16.
#
# 0. Reproducción: los números del texto salen de los CSV de 12, 13 y 16.
# A. Δ ponderado (H2): promedio de los Rice de partido ponderado por votantes
#    (S + N). Prueba unitaria: Δw = 0 si todas las mayorías van al mismo lado.
# B. Efecto del Ejecutivo (Tabla 7, Figura 15): las votaciones de salud del
#    presupuesto 2025, seis versiones del LPM, sacar una partida a la vez,
#    autoría × dirección y bloque de los autores parlamentarios.
# C. Placebo de H3: dispersión y eta² de la derecha y de la izquierda por año
#    y gobierno.
# D. H4 sin circularidad: dos clasificaciones de los independientes, la escala
#    P cruda y el cambio de los reelectos de la UDI y RN.
# E. Termómetro y lógica de gobierno (Figura 14) con la izquierda, por
#    instrumento y por autoría.
#
# Ventana: 11 de marzo de 2016 al 10 de marzo de 2026.
# Salidas:  data/analisis/revision/ y figuras.
# ==============================================================================

pacman::p_load(tidyverse, fixest, scales, patchwork, here)

# ---- Settings ----------------------------------------------------------------

ventana <- c(as.Date("2016-03-11"), as.Date("2026-03-11"))
anios <- c(
  "2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
  "2021-22", "2022-23", "2023-24", "2024-25", "2025-26"
)
anios_nueva <- anios[7:10]
min_miembros <- 3L
semilla <- 20261014L
reps_wild <- 9999L

nueva <- c("REP", "PNL")
tradicional <- c("UDI", "RN", "EVOP")
partidos_derecha <- c(nueva, tradicional)
right_parties <- c(partidos_derecha, "PSC")
izquierda <- c("PC", "PS", "PPD", "FA", "FRVS", "PL", "PR")
izquierda_cero <- c("PC", "PS", "FA", "PPD")
candidatos_alineados <- c("IND", "OTRO")
umbral_alineamiento <- 0.75
min_divisivas <- 20L

etiqueta <- c(REP = "PRCh", PNL = "PNL", UDI = "UDI", RN = "RN", EVOP = "Evópoli",
              `IND-D` = "IND con la derecha", PS = "PS", PC = "PC", FA = "FA", PPD = "PPD")

gobierno_anio <- tibble(
  tiempo = anios,
  gobierno = c("Bachelet II", "Bachelet II", rep("Piñera II", 4), rep("Boric", 4))
)

legislatura <- function(t) case_when(
  t %in% anios[1:2] ~ "2016-18",
  t %in% anios[3:6] ~ "2018-22",
  t %in% anios[7:10] ~ "2022-26"
)

paleta <- c(
  PRCh = "#D55E00", PNL = "#E69F00", UDI = "#08306B", RN = "#2171B5",
  Evópoli = "#6BAED6", `IND con la derecha` = "#009E73",
  PS = "#7F7F7F", PC = "#B2182B", FA = "#7B3294", PPD = "#CC79A7"
)
colores_gobierno <- c(`Bachelet II` = "#F2E6E6", `Piñera II` = "#E3EAF4", Boric = "#F2E6E6")

output_dir <- here("data", "analisis", "revision")
figures_dir <- file.path(output_dir, "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Helpers -----------------------------------------------------------------

section <- function(title) {
  cat("\n============================================================\n", title,
      "\n============================================================\n", sep = "")
}
escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")
guardar <- function(p, nombre, ancho = 10, alto = 5.5) {
  ggsave(file.path(figures_dir, nombre), p, width = ancho, height = alto, dpi = 300, bg = "white")
}
tema <- function() {
  theme_minimal(base_size = 10) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.caption = element_text(hjust = 0, colour = "grey30", size = 8),
          plot.caption.position = "plot")
}
pct <- label_percent(decimal.mark = ",", accuracy = 1)
coma <- label_number(decimal.mark = ",", accuracy = 0.01)
leer <- function(...) read_csv(here("data", "analisis", ...), show_col_types = FALSE)
exigir <- function(ok, mensaje) if (!isTRUE(ok)) stop(mensaje, call. = FALSE)

modal_voto <- function(v) {
  v <- v[v %in% c(-1L, 0L, 1L)]
  if (length(v) < min_miembros) return(NA_integer_)
  as.integer(names(sort(table(v), decreasing = TRUE))[1])
}

# Voto modal como en 04 (desempate por el primer valor en orden)
voto_modal <- function(d, nombre) {
  d |>
    filter(!is.na(voto_bcall)) |>
    count(votacion_id, voto_bcall) |>
    slice_max(n, n = 1, with_ties = FALSE, by = votacion_id) |>
    select(votacion_id, !!nombre := voto_bcall)
}

eta2 <- function(y, g) {
  if (n_distinct(g) < 2) return(NA_real_)
  1 - sum((y - ave(y, g))^2) / sum((y - mean(y))^2)
}

sin_tildes <- function(x) str_to_lower(stringi::stri_trans_general(x, "Latin-ASCII"))

# ---- Datos -------------------------------------------------------------------

section("DATOS")

votos <- read_csv(here("data", "analisis", "bcall", "votos_codificados.csv.gz"),
                  show_col_types = FALSE, progress = FALSE) |>
  mutate(fecha = as.Date(fecha), voto_bcall = as.integer(voto_bcall)) |>
  filter(fecha >= ventana[1], fecha < ventana[2], anio_legislativo %in% anios)

unidades <- leer("bcall", "bcall_unidades.csv") |>
  filter(eje == "todas", tiempo %in% anios, !is.na(d1))

escala <- leer("bcall_anio", "01_posicion_partido.csv") |> distinct(tiempo, cero, sd_anio)

cat("Votos:", nrow(votos), "| votaciones:", n_distinct(votos$votacion_id), "\n")

# ==============================================================================
# 0. REPRODUCCIÓN
# ==============================================================================

section("0. REPRODUCCIÓN DE LOS NÚMEROS DEL TEXTO")

entre <- leer("bloque1", "C_rice_conjunto_derecha.csv")
lpm_original <- leer("bloque3", "C_lpm_mayorias_distintas.csv")
divide <- leer("bloque3", "C_divide_une.csv")
posicion_grupo <- leer("bloque2", "B_posicion_grupo.csv")
clasificacion <- leer("bloque2", "C_clasificacion_eta2.csv")

valor <- function(d, ...) filter(d, ...)
ejec <- filter(lpm_original, term == "autor::Ejecutivo")

reproduccion <- tribble(
  ~cifra,                                   ~texto,                  ~calculado,
  "Δ 2016-17",                              "0,002",                 sprintf("%.3f", valor(entre, tiempo == "2016-17")$perdida),
  "Δ 2022-23 a 2025-26",                    "0,07 0,08 0,12 0,09",   paste(sprintf("%.2f", filter(entre, tiempo %in% anios_nueva)$perdida), collapse = " "),
  "Tabla 7: Ejecutivo",                     "+0,14 [0,07; 0,22]",    sprintf("%+.2f [%.2f; %.2f]", ejec$estimate, ejec$conf.low, ejec$conf.high),
  "División Ejecutivo",                     "22% de 248",            with(valor(divide, dimension == "Autoría", categoria == "Ejecutivo"), sprintf("%.0f%% de %d", 100 * distintas, votaciones)),
  "División parlamentaria",                 "6% de 167",             with(valor(divide, dimension == "Autoría", categoria == "Parlamentaria"), sprintf("%.0f%% de %d", 100 * distintas, votaciones)),
  "Salud divididas",                        "35 de 66",              with(valor(divide, dimension == "Tópico", categoria == "Salud"), sprintf("%.0f de %d", distintas * votaciones, votaciones)),
  "IQR UDI y RN 2021-22",                   "0,37 y 0,44",           with(filter(posicion_grupo, tiempo == "2021-22", grupo %in% c("UDI", "RN")) |> arrange(desc(grupo)), paste(sprintf("%.2f", iqr), collapse = " y ")),
  "η² toda la derecha 2022-26",             "15 35 37 63",           paste(sprintf("%.0f", 100 * filter(clasificacion, tiempo %in% anios_nueva)$eta2), collapse = " ")
)
escribir(reproduccion, "00_reproduccion.csv")
print(reproduccion, width = Inf)

# ==============================================================================
# A. Δ PONDERADO
# ==============================================================================

section("A. Δ PONDERADO POR VOTANTES")

tamano_partido <- votos |>
  distinct(anio_legislativo, partido_votacion, diputado_id) |>
  count(anio_legislativo, partido_votacion, name = "diputados")

# Sí y No de cada partido de derecha en cada votación (partidos con 3 o más
# diputados en el año y 3 o más votando Sí o No en la votación)
sn_partido <- votos |>
  filter(partido_votacion %in% partidos_derecha, voto_bcall %in% c(-1L, 1L)) |>
  left_join(tamano_partido, by = c("anio_legislativo", "partido_votacion")) |>
  filter(diputados >= min_miembros) |>
  summarise(S = sum(voto_bcall == 1L), N = sum(voto_bcall == -1L),
            .by = c(anio_legislativo, votacion_id, partido_votacion)) |>
  filter(S + N >= min_miembros) |>
  mutate(rice = abs(S - N) / (S + N))

delta_w_votacion <- function(d) {
  d |>
    summarise(
      partidos = n(),
      rice_simple = mean(rice),
      rice_ponderado = sum((S + N) * rice) / sum(S + N),
      rice_bloque = abs(sum(S) - sum(N)) / sum(S + N),
      signo_bloque = sign(sum(S) - sum(N)),
      alguno_contra = any(sign(S - N) != 0 & sign(sum(S) - sum(N)) != 0 &
                            sign(S - N) != sign(sum(S) - sum(N))),
      mismo_lado = n_distinct(sign(S - N)[S != N]) <= 1,
      .by = c(anio_legislativo, votacion_id)
    ) |>
    filter(partidos >= 2) |>
    mutate(delta_simple = rice_simple - rice_bloque, delta_w = rice_ponderado - rice_bloque)
}

# Prueba unitaria con datos sintéticos
prueba <- tribble(
  ~anio_legislativo, ~votacion_id, ~partido_votacion, ~S, ~N,
  "t", 1L, "A", 20L, 3L,
  "t", 1L, "B", 8L, 0L,
  "t", 1L, "C", 5L, 5L,
  "t", 2L, "A", 2L, 18L,
  "t", 2L, "B", 0L, 6L,
  "t", 3L, "A", 20L, 3L,
  "t", 3L, "B", 0L, 8L
) |>
  mutate(rice = abs(S - N) / (S + N)) |>
  delta_w_votacion()
exigir(all(abs(prueba$delta_w[prueba$mismo_lado]) < 1e-12), "Prueba unitaria: Δw ≠ 0 con mayorías del mismo lado.")
exigir(prueba$delta_w[prueba$votacion_id == 3L] > 0, "Prueba unitaria: Δw debería ser positivo con mayorías opuestas.")
cat("Prueba unitaria sintética: OK (Δw = 0 en las votaciones 1 y 2; Δw =",
    round(prueba$delta_w[3], 3), "en la 3)\n")

delta_votacion <- delta_w_votacion(sn_partido)
exigir(all(abs(delta_votacion$delta_w[delta_votacion$mismo_lado]) < 1e-12),
       "Prueba unitaria en datos reales: Δw ≠ 0 con mayorías del mismo lado.")
cat("Prueba unitaria en datos reales: OK en", sum(delta_votacion$mismo_lado), "votaciones con mayorías del mismo lado\n")

delta_anio <- delta_votacion |>
  summarise(
    votaciones = n(),
    delta_w_si_contra = mean(delta_w[alguno_contra]),
    delta_w = mean(delta_w),
    delta_simple_mismas = mean(delta_simple),
    pct_alguno_contra = mean(alguno_contra),
    .by = anio_legislativo
  ) |>
  rename(tiempo = anio_legislativo) |>
  left_join(select(entre, tiempo, delta_original = perdida, votaciones_original = votaciones,
                   partidos), by = "tiempo") |>
  left_join(gobierno_anio, by = "tiempo") |>
  relocate(tiempo, gobierno, partidos, delta_original, delta_w) |>
  arrange(tiempo)
escribir(delta_anio, "A_delta_ponderado.csv")
delta_anio |> mutate(across(where(is.double), \(x) round(x, 3))) |> print(n = Inf, width = Inf)

# ==============================================================================
# B. EFECTO DEL EJECUTIVO
# ==============================================================================

section("B. EFECTO DEL EJECUTIVO")

codificadas <- read_csv(here("data", "llm", "resultados", "indicaciones_codificadas.csv"),
                        show_col_types = FALSE, na = "") |>
  select(votacion_id, fecha, boletin, texto_indicacion, llm_resumen, llm_presupuesto)
topicos <- leer("topicos", "topicos_indicaciones.csv") |> select(votacion_id, partida)

por_votacion <- leer("bloque3", "C_votaciones_2022_26.csv") |>
  left_join(codificadas, by = "votacion_id") |>
  left_join(topicos, by = "votacion_id") |>
  mutate(
    distintas = as.integer(distintas),
    direccion = fct_relevel(factor(direccion), "Neutral"),
    presupuesto = fct_relevel(factor(presupuesto), "Otros proyectos"),
    autor = fct_relevel(factor(autor), "Parlamentaria"),
    llm_presupuesto = fct_relevel(factor(coalesce(llm_presupuesto, "No")), "No"),
    mecanismo = fct_relevel(factor(mecanismo), "Gasto y transferencias"),
    partida_txt = case_when(
      presupuesto == "Otros proyectos" ~ "Otros proyectos",
      is.na(partida) ~ "Presupuesto sin partida",
      TRUE ~ sprintf("Partida %02d", as.integer(partida))
    ),
    # Grupo de la partida: los proyectos que no son presupuesto, por boletín;
    # las indicaciones sin partida, por boletín del presupuesto
    cluster_partida = case_when(
      presupuesto == "Otros proyectos" ~ paste("Boletín", boletin),
      is.na(partida) ~ paste("Sin partida", boletin),
      TRUE ~ partida_txt
    )
  )

datos_lpm <- filter(por_votacion, autor != "No identificable") |> droplevels()

# --- B1. Salud en el presupuesto 2025 -----------------------------------------

salud_2025 <- por_votacion |>
  filter(tiempo == "2024-25", presupuesto == "Ley de Presupuestos", topico_principal == "Salud") |>
  mutate(repone = str_detect(sin_tildes(coalesce(llm_resumen, "")), "repon|restitu|restablec|reincorpor|restaur")) |>
  transmute(
    votacion_id, fecha, partida = partida_txt, autor, direccion, mecanismo, llm_presupuesto,
    repone_gasto = autor == "Ejecutivo" & repone,
    resumen_llm = llm_resumen,
    si_tradicional = `si_Derecha tradicional`, si_prch = `si_Nueva derecha`, si_izquierda = si_Izquierda,
    dividen = distintas == 1L
  ) |>
  arrange(desc(repone_gasto), fecha, votacion_id)
escribir(salud_2025, "B1_salud_presupuesto_2025.csv")

cat("Votaciones de salud del presupuesto 2025:", nrow(salud_2025), "\n")
salud_2025 |> count(autor, repone_gasto, dividen) |> print()
repone_ids <- salud_2025$votacion_id[salud_2025$repone_gasto]
cat("Ejecutivo que repone gasto:", length(repone_ids), "| de ellas dividen:",
    sum(salud_2025$dividen[salud_2025$repone_gasto]), "\n")
salud_2025 |>
  filter(repone_gasto) |>
  summarise(si_tradicional = mean(si_tradicional), si_prch = mean(si_prch), dividen = mean(dividen)) |>
  print()

# --- B2. Versiones del LPM ----------------------------------------------------

rhs_base <- "i(direccion, ref = 'Neutral') + i(presupuesto, ref = 'Otros proyectos') + i(autor, ref = 'Parlamentaria')"

lpm <- function(d, rhs = rhs_base, cluster = ~votacion_id) {
  feols(as.formula(paste("distintas ~", rhs, "| tiempo")), data = d, cluster = cluster)
}
coef_ejecutivo <- function(m, version, d, grupos = NA_integer_, se_tipo = "Por votación") {
  ct <- coeftable(m)
  fila <- "autor::Ejecutivo"
  tibble(
    version = version, se_tipo = se_tipo, votaciones = nobs(m), grupos = grupos,
    estimate = ct[fila, 1], se = ct[fila, 2],
    conf_low = ct[fila, 1] - qnorm(0.975) * ct[fila, 2],
    conf_high = ct[fila, 1] + qnorm(0.975) * ct[fila, 2],
    p_value = ct[fila, 4]
  )
}

# Wild cluster bootstrap (pesos de Webb). p: con la nula impuesta (WCR).
# Intervalo: percentil-t sin imponer la nula (WCU).
wild_cluster <- function(d, rhs, cluster_var, termino = "autorEjecutivo", B = reps_wild) {
  f <- as.formula(paste("distintas ~", rhs, "+ factor(tiempo)"))
  X <- model.matrix(f, d)
  X <- X[, colSums(abs(X)) > 0, drop = FALSE]
  y <- d$distintas
  cl <- as.integer(factor(d[[cluster_var]]))
  G <- max(cl); N <- nrow(X); K <- ncol(X)
  k <- which(colnames(X) == termino)
  XtXi <- solve(crossprod(X))
  a <- drop(X %*% XtXi[k, ])
  ajuste <- G / (G - 1) * (N - 1) / (N - K)
  se_k <- function(u) sqrt(ajuste * sum(rowsum(a * u, cl)^2))
  beta <- drop(XtXi %*% crossprod(X, y))
  u <- y - drop(X %*% beta)
  t_obs <- beta[k] / se_k(u)
  Xr <- X[, -k, drop = FALSE]
  br <- qr.coef(qr(Xr), y)
  yr <- drop(Xr %*% br)
  ur <- y - yr
  webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
  set.seed(semilla)
  t_r <- t_u <- numeric(B)
  for (b in seq_len(B)) {
    w <- sample(webb, G, replace = TRUE)[cl]
    ys <- yr + ur * w
    bs <- drop(XtXi %*% crossprod(X, ys))
    t_r[b] <- bs[k] / se_k(ys - drop(X %*% bs))
    ys <- drop(X %*% beta) + u * w
    bs <- drop(XtXi %*% crossprod(X, ys))
    t_u[b] <- (bs[k] - beta[k]) / se_k(ys - drop(X %*% bs))
  }
  q <- quantile(abs(t_u), 0.95)
  tibble(estimate = beta[k], se = se_k(u), conf_low = beta[k] - q * se_k(u),
         conf_high = beta[k] + q * se_k(u), p_value = mean(abs(t_r) >= abs(t_obs)), grupos = G)
}

rhs_lm <- "direccion + presupuesto + autor"
versiones <- list()

# a. Original
m_a <- lpm(datos_lpm)
versiones$a <- coef_ejecutivo(m_a, "a. Original", datos_lpm)
exigir(abs(versiones$a$estimate - ejec$estimate) < 1e-10, "La versión original no reproduce la Tabla 7.")

# b. Sin las votaciones de salud del presupuesto 2025 que reponen gasto
d_b <- filter(datos_lpm, !votacion_id %in% repone_ids)
versiones$b <- coef_ejecutivo(lpm(d_b), "b. Sin salud 2025 que repone gasto", d_b)
ejecutivo_salud_ids <- salud_2025$votacion_id[salud_2025$autor == "Ejecutivo"]
d_b2 <- filter(datos_lpm, !votacion_id %in% ejecutivo_salud_ids)
versiones$b2 <- coef_ejecutivo(lpm(d_b2), "b. Sin toda la salud 2025 del Ejecutivo", d_b2)

# c. Errores por boletín y por partida
for (cv in c("boletin", "cluster_partida")) {
  nombre <- c(boletin = "boletín", cluster_partida = "partida")[[cv]]
  G <- n_distinct(datos_lpm[[cv]])
  versiones[[paste0("c_", cv)]] <- coef_ejecutivo(
    lpm(datos_lpm, cluster = as.formula(paste0("~", cv))),
    paste("c. Agrupado por", nombre), datos_lpm, G, "Analítico (CR1)"
  )
  if (G < 30) {
    versiones[[paste0("c_", cv, "_wild")]] <- wild_cluster(datos_lpm, rhs_lm, cv) |>
      mutate(version = paste("c. Agrupado por", nombre), se_tipo = "Wild cluster bootstrap (Webb, 9.999)",
             votaciones = nrow(datos_lpm))
  }
}

# d. Controles por mecanismo y tipo presupuestario
rhs_d <- paste(rhs_base, "+ i(mecanismo, ref = 'Gasto y transferencias') + i(llm_presupuesto, ref = 'No')")
versiones$d <- coef_ejecutivo(lpm(datos_lpm, rhs_d), "d. Con mecanismo y tipo presupuestario", datos_lpm)

# e. Sin la categoría Comisión
n_autoria <- count(por_votacion, autor, name = "votaciones")
d_e <- filter(datos_lpm, autor != "Comisión") |> droplevels()
versiones$e <- coef_ejecutivo(lpm(d_e), "e. Sin Comisión", d_e)

# f. Solo presupuesto
d_f <- filter(datos_lpm, presupuesto == "Ley de Presupuestos") |> droplevels()
m_f <- feols(distintas ~ i(direccion, ref = "Neutral") + i(autor, ref = "Parlamentaria") | tiempo,
             data = d_f, cluster = ~votacion_id)
versiones$f <- coef_ejecutivo(m_f, "f. Solo Ley de Presupuestos", d_f)

tabla7 <- bind_rows(versiones) |>
  relocate(version, se_tipo, votaciones, grupos)
escribir(tabla7, "B2_tabla7_versiones.csv")
tabla7 |> mutate(across(where(is.double), \(x) round(x, 3))) |> print(n = Inf, width = Inf)

cat("\nVotaciones por autoría (2022-26, con termómetro):\n")
print(n_autoria)
escribir(n_autoria, "B2_n_autoria.csv")

fuera_presupuesto <- datos_lpm |>
  filter(presupuesto == "Otros proyectos") |>
  summarise(votaciones = n(), dividen = sum(distintas), pct_dividen = mean(distintas), .by = autor)
escribir(fuera_presupuesto, "B2_fuera_presupuesto.csv")
cat("\nFuera del presupuesto (solo descriptivo):\n")
print(fuera_presupuesto)

# --- B3. Sacar una partida a la vez -------------------------------------------

partidas <- datos_lpm |> count(partida_txt, name = "votaciones")
loo <- map_dfr(partidas$partida_txt, \(p) {
  d <- filter(datos_lpm, partida_txt != p) |> droplevels()
  m <- tryCatch(lpm(d), error = \(e) NULL)
  if (is.null(m) || !"autor::Ejecutivo" %in% names(coef(m))) return(NULL)
  coef_ejecutivo(m, p, d) |> mutate(partida = p)
}) |>
  left_join(partidas, by = c("partida" = "partida_txt"), suffix = c("", "_fuera")) |>
  transmute(partida, votaciones_fuera = votaciones_fuera, votaciones_modelo = votaciones,
            estimate, conf_low, conf_high, cambio = estimate - versiones$a$estimate) |>
  arrange(cambio)
escribir(loo, "B3_sacar_una_partida.csv")
loo |> mutate(across(where(is.double), \(x) round(x, 3))) |> print(n = Inf, width = Inf)

# --- B4. Autoría × dirección --------------------------------------------------

cruce <- datos_lpm |>
  summarise(votaciones = n(), dividen = sum(distintas), pct_dividen = mean(distintas),
            .by = c(autor, direccion)) |>
  arrange(autor, direccion)
escribir(cruce, "B4_autoria_direccion.csv")
cruce |>
  mutate(celda = sprintf("%d (%.0f%%)", votaciones, 100 * pct_dividen)) |>
  select(autor, direccion, celda) |>
  pivot_wider(names_from = direccion, values_from = celda) |>
  print()

# --- B5. Bloque de los autores parlamentarios ---------------------------------

# El encabezado nombra a los autores ("del diputado Schalper Diego", "de los
# diputados Jorge Alessandri y Luis Sánchez"). Se busca el apellido paterno
# del padrón del año en ese tramo; si dos diputados del año comparten
# apellido, se exige también el nombre.
padron <- votos |>
  distinct(anio_legislativo, diputado_id, legislator, bloque) |>
  slice_head(n = 1, by = c(anio_legislativo, diputado_id)) |>
  mutate(
    palabras = str_split(sin_tildes(legislator), "\\s+"),
    nombre = map_chr(palabras, 1),
    paterno = map_chr(palabras, \(p) if (length(p) >= 3) p[length(p) - 1] else p[length(p)]),
    lado = case_when(
      bloque %in% c("Nueva derecha", "Derecha tradicional", "IND con la derecha", "Otra derecha") ~ "derecha",
      bloque == "Izquierda" ~ "izquierda",
      TRUE ~ "centro"
    )
  ) |>
  add_count(anio_legislativo, paterno, name = "homonimos")

tramo_autores <- function(texto) {
  t <- sin_tildes(str_squish(coalesce(texto, "")))
  t <- str_replace_all(t, "&quot;", "\"")
  inicio <- str_locate(t, "(diputad[oa]s?|senador[ae]?s?|senoras?|senores?)\\s")[, "end"]
  out <- if_else(is.na(inicio), NA_character_, str_sub(t, inicio + 1, inicio + 220))
  str_remove(out, "(\\s-\\s|\\spart\\.?\\s?\\d|\\spartida|\\spara\\s|,?\\sde cuyo|\\sdel siguiente|\\sque\\s|\\sen el\\s|\\sal\\s|\\sa la\\s|\\(|:|\"|\u201c|\\snumeral|\\sart).*$")
}

parlamentarias <- datos_lpm |>
  filter(autor == "Parlamentaria") |>
  mutate(tramo = tramo_autores(texto_indicacion), y_otros = str_detect(coalesce(tramo, ""), "y otr"))

autores <- parlamentarias |>
  select(votacion_id, tiempo, tramo) |>
  filter(!is.na(tramo)) |>
  mutate(tokens = str_extract_all(tramo, "[a-z]{3,}")) |>
  inner_join(padron, by = c("tiempo" = "anio_legislativo"), relationship = "many-to-many") |>
  filter(map2_lgl(paterno, tokens, \(p, tk) p %in% tk)) |>
  filter(homonimos == 1 | map2_lgl(nombre, tokens, \(n, tk) n %in% tk))

bloque_autores <- autores |>
  summarise(autores = n_distinct(diputado_id), lados = list(unique(lado)),
            nombres = paste(unique(legislator), collapse = "; "), .by = votacion_id) |>
  mutate(bloque_autores = map_chr(lados, \(l) case_when(
    setequal(l, "derecha") ~ "Derecha",
    setequal(l, "izquierda") ~ "Izquierda",
    setequal(l, "centro") ~ "Centro",
    TRUE ~ "Mixto"
  ))) |>
  select(-lados)

parlamentarias <- parlamentarias |>
  left_join(bloque_autores, by = "votacion_id") |>
  mutate(bloque_autores = coalesce(bloque_autores, "Sin identificar"))

cat("Indicaciones parlamentarias:", nrow(parlamentarias),
    "| con autor identificado:", sum(parlamentarias$bloque_autores != "Sin identificar"),
    sprintf("(%.0f%%)", 100 * mean(parlamentarias$bloque_autores != "Sin identificar")), "\n")

escribir(select(parlamentarias, votacion_id, tiempo, presupuesto, direccion, tramo, nombres, autores,
                y_otros, bloque_autores, distintas), "B5_autores_parlamentarios.csv")

division_bloque_autor <- bind_rows(
  datos_lpm |> filter(autor != "Parlamentaria") |> mutate(categoria = as.character(autor)),
  parlamentarias |> mutate(categoria = paste("Parlamentaria:", bloque_autores))
) |>
  summarise(votaciones = n(), dividen = sum(distintas), pct_dividen = mean(distintas),
            mas_estado = mean(direccion == "Más Estado"), .by = categoria) |>
  arrange(desc(votaciones))
escribir(division_bloque_autor, "B5_division_por_bloque_autor.csv")
division_bloque_autor |> mutate(across(where(is.double), \(x) round(x, 3))) |> print(n = Inf)

d_b5 <- datos_lpm |>
  left_join(select(parlamentarias, votacion_id, bloque_autores), by = "votacion_id") |>
  mutate(autor5 = if_else(autor == "Parlamentaria", paste("Parl.", bloque_autores), as.character(autor))) |>
  filter(autor5 != "Comisión")
m_b5 <- feols(distintas ~ i(direccion, ref = "Neutral") + i(presupuesto, ref = "Otros proyectos") +
                i(autor5, ref = "Parl. Derecha") | tiempo, data = d_b5, cluster = ~votacion_id)
b5_tab <- broom::tidy(m_b5, conf.int = TRUE) |> filter(str_detect(term, "autor5"))
escribir(b5_tab, "B5_lpm_bloque_autor.csv")
print(b5_tab)

# ==============================================================================
# C. PLACEBO DE H3
# ==============================================================================

section("C. PLACEBO DE H3: DERECHA E IZQUIERDA POR GOBIERNO")

# Grupo del diputado en el año (13): el que más se repite en sus votos
grupo_anio <- votos |>
  filter(!is.na(grupo_votacion), grupo_votacion != "OTRO") |>
  count(anio_legislativo, diputado_id, grupo_votacion) |>
  slice_max(n, n = 1, with_ties = FALSE, by = c(anio_legislativo, diputado_id)) |>
  select(tiempo = anio_legislativo, diputado_id, grupo = grupo_votacion)

camara <- unidades |>
  filter(universo == "camara") |>
  select(tiempo, diputado_id, d1) |>
  left_join(grupo_anio, by = c("tiempo", "diputado_id")) |>
  left_join(escala, by = "tiempo") |>
  mutate(P = (d1 - cero) / sd_anio)

derecha_u <- unidades |>
  filter(universo == "derecha") |>
  select(tiempo, diputado_id, d1) |>
  left_join(grupo_anio, by = c("tiempo", "diputado_id"))

iqr_partido <- camara |>
  filter(grupo %in% c("UDI", "RN", "PS", "PC", "PPD", "FA")) |>
  summarise(diputados = n(), iqr = IQR(P), mediana = median(P), .by = c(tiempo, grupo)) |>
  filter(diputados >= 2)

eta2_version <- function(d, grupos, nombre, minimo = min_miembros) {
  d |>
    filter(grupo %in% grupos) |>
    add_count(tiempo, grupo, name = "n_grupo") |>
    filter(n_grupo >= minimo) |>
    summarise(eta2 = eta2(d1, grupo), diputados = n(),
              grupos = paste(sort(unique(coalesce(etiqueta[grupo], grupo))), collapse = ", "),
              .by = tiempo) |>
    mutate(version = nombre)
}

grupos_toda <- c(nueva, tradicional, "IND-D", "PSC")
eta2_tabla <- bind_rows(
  eta2_version(derecha_u, grupos_toda, "Toda la derecha (universo derecha, Fig. 7)"),
  eta2_version(derecha_u, tradicional, "Tradicionales con 3+ diputados (universo derecha, Fig. 7)"),
  eta2_version(derecha_u, c("UDI", "RN"), "Solo UDI y RN (universo derecha)"),
  eta2_version(derecha_u, tradicional, "UDI, RN y Evópoli con 2+ (universo derecha)", minimo = 2L),
  eta2_version(camara, grupos_toda, "Toda la derecha (Cámara)"),
  eta2_version(camara, c("UDI", "RN"), "Solo UDI y RN (Cámara)"),
  eta2_version(camara, izquierda_cero, "Izquierda: PS, PC, PPD y FA (Cámara)")
) |>
  left_join(gobierno_anio, by = "tiempo") |>
  arrange(version, tiempo)

placebo <- iqr_partido |>
  select(tiempo, grupo, iqr) |>
  pivot_wider(names_from = grupo, values_from = iqr, names_prefix = "iqr_") |>
  left_join(eta2_tabla |> select(tiempo, version, eta2) |>
              pivot_wider(names_from = version, values_from = eta2), by = "tiempo") |>
  left_join(gobierno_anio, by = "tiempo") |>
  relocate(tiempo, gobierno) |>
  arrange(tiempo)
escribir(iqr_partido, "C_iqr_partido.csv")
escribir(eta2_tabla, "C_eta2_versiones.csv")
escribir(placebo, "C_placebo_h3.csv")
placebo |> mutate(across(where(is.double), \(x) round(x, 2))) |> print(n = Inf, width = Inf)

cat("\nEvópoli en el universo derecha, diputados por año:\n")
derecha_u |> filter(grupo == "EVOP") |> count(tiempo) |> print()

comparacion_oposicion <- bind_rows(
  iqr_partido |> rename(valor = iqr) |> mutate(medida = paste("IQR de P,", grupo)),
  eta2_tabla |> rename(valor = eta2) |> mutate(medida = paste("η²,", version))
) |>
  mutate(legislatura = legislatura(tiempo)) |>
  filter(legislatura %in% c("2016-18", "2022-26")) |>
  summarise(promedio = mean(valor, na.rm = TRUE), anios = sum(!is.na(valor)), .by = c(medida, legislatura)) |>
  pivot_wider(names_from = legislatura, values_from = c(promedio, anios)) |>
  mutate(diferencia = `promedio_2022-26` - `promedio_2016-18`)
escribir(comparacion_oposicion, "C_comparacion_2016_18_vs_2022_26.csv")
comparacion_oposicion |> mutate(across(where(is.double), \(x) round(x, 3))) |> print(n = Inf, width = Inf)

# Figura: derecha e izquierda en paneles separados
fondo <- gobierno_anio |>
  mutate(x = match(tiempo, anios)) |>
  summarise(xmin = min(x) - 0.5, xmax = max(x) + 0.5, .by = gobierno)

panel_iqr <- function(grupos, titulo) {
  iqr_partido |>
    filter(grupo %in% grupos) |>
    mutate(grupo = factor(grupo, grupos), x = match(tiempo, anios)) |>
    ggplot() +
    geom_rect(data = fondo, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = gobierno),
              alpha = 0.7, inherit.aes = FALSE) +
    geom_text(data = fondo, aes(x = (xmin + xmax) / 2, y = Inf, label = gobierno), vjust = 1.4,
              size = 2.8, colour = "grey30", inherit.aes = FALSE) +
    geom_line(aes(x, iqr, colour = grupo), linewidth = 0.6) +
    geom_point(aes(x, iqr, colour = grupo), size = 1.8) +
    scale_fill_manual(values = colores_gobierno, guide = "none") +
    scale_colour_manual(values = paleta, name = NULL) +
    scale_x_continuous(breaks = seq_along(anios), labels = anios) +
    scale_y_continuous("Rango intercuartil de P", labels = coma, limits = c(0, NA)) +
    labs(x = NULL, title = titulo) +
    tema() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

panel_eta <- function(versiones_eta, colores, titulo) {
  eta2_tabla |>
    filter(version %in% names(versiones_eta)) |>
    mutate(version = factor(versiones_eta[version], versiones_eta), x = match(tiempo, anios)) |>
    ggplot() +
    geom_rect(data = fondo, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = gobierno),
              alpha = 0.7, inherit.aes = FALSE) +
    geom_line(aes(x, eta2, colour = version), linewidth = 0.6) +
    geom_point(aes(x, eta2, colour = version), size = 1.8) +
    scale_fill_manual(values = colores_gobierno, guide = "none") +
    scale_colour_manual(values = set_names(colores, versiones_eta), name = NULL) +
    scale_x_continuous(breaks = seq_along(anios), labels = anios) +
    scale_y_continuous("Varianza de d1 explicada por el grupo (η²)", labels = pct, limits = c(0, 1)) +
    labs(x = NULL, title = titulo) +
    tema() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(colour = guide_legend(ncol = 1))
}

f_c <- (panel_iqr(c("UDI", "RN"), "Derecha: dispersión interna") |
          panel_iqr(c("PS", "PC", "PPD", "FA"), "Izquierda: dispersión interna")) /
  (panel_eta(c(`Toda la derecha (universo derecha, Fig. 7)` = "Toda la derecha (universo derecha)",
               `Solo UDI y RN (universo derecha)` = "Solo UDI y RN (universo derecha)",
               `Toda la derecha (Cámara)` = "Toda la derecha (Cámara)"),
             c("#D55E00", "#08306B", "#E69F00"), "Derecha: clasificación") |
     panel_eta(c(`Izquierda: PS, PC, PPD y FA (Cámara)` = "PS, PC, PPD y FA (Cámara)"),
               "#7F7F7F", "Izquierda: clasificación")) +
  plot_annotation(caption = str_wrap(paste(
    "Arriba: rango intercuartil de la posición P (escala anual común de la Cámara) por partido, partidos con al menos 2 diputados.",
    "Abajo: η² del grupo en d1, grupos con al menos 3 diputados. El universo derecha es el B-Call estimado solo entre diputados de derecha; la izquierda no tiene un B-Call propio y se mide en el de la Cámara.",
    "Fondo: gobierno de turno. Bachelet II y Boric son gobiernos de izquierda: la derecha es oposición en los dos."
  ), 170))
guardar(f_c, "C_placebo_h3.png", ancho = 12, alto = 8.5)

# ==============================================================================
# D. H4 SIN CIRCULARIDAD
# ==============================================================================

section("D. H4 SIN CIRCULARIDAD")

# --- D1. Dos clasificaciones de los independientes ----------------------------

divisivas_periodo <- inner_join(
  voto_modal(filter(votos, partido_votacion %in% right_parties), "modal_derecha"),
  voto_modal(filter(votos, partido_votacion %in% izquierda), "modal_izquierda"),
  by = "votacion_id"
) |>
  filter(modal_derecha != modal_izquierda) |>
  left_join(distinct(votos, votacion_id, fecha, periodo), by = "votacion_id") |>
  arrange(periodo, fecha, votacion_id) |>
  mutate(orden = row_number(), mitad = if_else(orden %% 2 == 1, "impar", "par"), .by = periodo)

alinear <- function(d) {
  d |>
    summarise(
      votaciones_divisivas = n(),
      acuerdo_derecha = mean(voto_bcall == modal_derecha),
      acuerdo_izquierda = mean(voto_bcall == modal_izquierda),
      .by = c(periodo, diputado_id)
    ) |>
    mutate(con_la_derecha = votaciones_divisivas >= min_divisivas &
             acuerdo_derecha >= umbral_alineamiento & acuerdo_derecha > acuerdo_izquierda)
}

votos_div <- votos |>
  filter(!is.na(voto_bcall)) |>
  inner_join(divisivas_periodo, by = c("votacion_id", "fecha", "periodo"))

candidatos <- votos |>
  filter(partido_votacion %in% candidatos_alineados) |>
  distinct(periodo, anio_legislativo, diputado_id, legislator)

# Original (04): todas las divisivas del período, solo votos como IND u OTRO
original <- votos_div |> filter(partido_votacion %in% candidatos_alineados) |> alinear()
ind_d_datos <- votos |> filter(grupo_votacion == "IND-D") |> distinct(periodo, diputado_id)
exigir(nrow(anti_join(filter(original, con_la_derecha), ind_d_datos, by = c("periodo", "diputado_id"))) == 0 &&
         nrow(anti_join(ind_d_datos, filter(original, con_la_derecha), by = c("periodo", "diputado_id"))) == 0,
       "La clasificación original no reproduce los IND-D de 04.")

# (i) Período anterior: todos los votos del diputado en el período previo
periodos <- sort(unique(votos$periodo))
previo <- votos_div |>
  alinear() |>
  mutate(periodo = periodos[match(periodo, periodos) + 1]) |>
  filter(!is.na(periodo)) |>
  select(periodo, diputado_id, con_derecha_previo = con_la_derecha, acuerdo_previo = acuerdo_derecha)

# (ii) Mitades: se clasifica con las impares y se mide en las pares
impar <- votos_div |> filter(partido_votacion %in% candidatos_alineados, mitad == "impar") |> alinear() |>
  select(periodo, diputado_id, con_derecha_impar = con_la_derecha, acuerdo_impar = acuerdo_derecha)
par_anio <- votos_div |>
  filter(partido_votacion %in% candidatos_alineados, mitad == "par") |>
  summarise(votaciones_par = n(), acuerdo_par = mean(voto_bcall == modal_derecha),
            .by = c(periodo, anio_legislativo, diputado_id))

clasif_ind <- candidatos |>
  left_join(select(original, periodo, diputado_id, con_derecha_original = con_la_derecha), by = c("periodo", "diputado_id")) |>
  left_join(previo, by = c("periodo", "diputado_id")) |>
  left_join(impar, by = c("periodo", "diputado_id")) |>
  left_join(par_anio, by = c("periodo", "anio_legislativo", "diputado_id")) |>
  mutate(across(starts_with("con_derecha"), \(x) coalesce(x, FALSE)),
         tiene_previo = !is.na(acuerdo_previo))
escribir(clasif_ind, "D1_independientes_clasificacion.csv")

conteo_ind <- clasif_ind |>
  summarise(
    independientes = n(),
    original = sum(con_derecha_original),
    con_periodo_anterior = sum(tiene_previo),
    previo_derecha = sum(con_derecha_previo),
    mitad_impar_derecha = sum(con_derecha_impar),
    ambas_original_e_impar = sum(con_derecha_original & con_derecha_impar),
    acuerdo_par_mediana_impar_derecha = median(acuerdo_par[con_derecha_impar], na.rm = TRUE),
    acuerdo_par_mediana_resto = median(acuerdo_par[!con_derecha_impar], na.rm = TRUE),
    .by = anio_legislativo
  ) |>
  rename(tiempo = anio_legislativo) |>
  arrange(tiempo)
escribir(conteo_ind, "D1_independientes_por_anio.csv")
conteo_ind |> mutate(across(where(is.double), \(x) round(x, 2))) |> print(n = Inf, width = Inf)

# --- D2. ¿Se estiró la escala P? ----------------------------------------------

partido_unidad <- unidades |> filter(universo == "camara") |> select(tiempo, diputado_id, partido, d1)
escala_cruda <- partido_unidad |>
  summarise(
    d1_udi = median(d1[partido == "UDI"]),
    d1_rn = median(d1[partido == "RN"]),
    d1_izquierda = median(d1[partido %in% izquierda_cero]),
    sd_camara = sd(d1),
    .by = tiempo
  ) |>
  left_join(escala, by = "tiempo") |>
  mutate(distancia_cruda_udi = d1_udi - d1_izquierda, P_udi = distancia_cruda_udi / sd_camara,
         P_rn = (d1_rn - d1_izquierda) / sd_camara) |>
  left_join(gobierno_anio, by = "tiempo") |>
  arrange(tiempo)
exigir(max(abs(escala_cruda$d1_izquierda - escala_cruda$cero)) < 1e-10 &&
         max(abs(escala_cruda$sd_camara - escala_cruda$sd_anio)) < 1e-10,
       "La escala recalculada no coincide con 01_posicion_partido.csv.")
escribir(select(escala_cruda, -cero, -sd_anio), "D2_escala_cruda.csv")
escala_cruda |> select(-cero, -sd_anio) |> mutate(across(where(is.double), \(x) round(x, 3))) |> print(n = Inf, width = Inf)

# --- D3. Reelectos de la UDI y RN: P y termómetro ------------------------------

p_legislatura <- camara |>
  mutate(legislatura = legislatura(tiempo)) |>
  filter(legislatura %in% c("2018-22", "2022-26")) |>
  summarise(P = mean(P), grupo = names(sort(table(grupo), decreasing = TRUE))[1],
            .by = c(diputado_id, legislatura)) |>
  pivot_wider(names_from = legislatura, values_from = c(P, grupo))

termometro_panel <- leer("bloque3", "D_panel_reelectos.csv") |>
  select(diputado_id, legislator, indice_antes = `indice_2018-22`, indice_despues = `indice_2022-26`,
         grupo_termo_antes = `grupo_2018-22`, grupo_termo_despues = `grupo_2022-26`)

reelectos <- p_legislatura |>
  filter(`grupo_2018-22` %in% c("UDI", "RN")) |>
  inner_join(termometro_panel, by = "diputado_id") |>
  filter(!is.na(`P_2022-26`)) |>
  transmute(
    legislator, partido_antes = `grupo_2018-22`, grupo_despues = coalesce(etiqueta[`grupo_2022-26`], `grupo_2022-26`),
    P_antes = `P_2018-22`, P_despues = `P_2022-26`, cambio_P = P_despues - P_antes,
    indice_antes, indice_despues, cambio_indice = indice_despues - indice_antes
  ) |>
  arrange(partido_antes, grupo_despues, desc(cambio_P))
escribir(reelectos, "D3_reelectos_udi_rn.csv")

reelectos_resumen <- reelectos |>
  mutate(trayectoria = paste(partido_antes, "→", grupo_despues)) |>
  summarise(diputados = n(), cambio_P = median(cambio_P), cambio_indice = median(cambio_indice),
            .by = trayectoria) |>
  arrange(desc(diputados))
escribir(reelectos_resumen, "D3_reelectos_resumen.csv")
correlacion <- cor.test(reelectos$cambio_P, reelectos$cambio_indice)
cat("Correlación entre el cambio en P y el cambio en el termómetro (", nrow(reelectos), " reelectos): ",
    round(correlacion$estimate, 2), " (p = ", round(correlacion$p.value, 3), ")\n", sep = "")
reelectos |> mutate(across(where(is.double), \(x) round(x, 2))) |> print(n = Inf, width = Inf)
reelectos_resumen |> mutate(across(where(is.double), \(x) round(x, 2))) |> print()

# ==============================================================================
# E. TERMÓMETRO Y LÓGICA DE GOBIERNO
# ==============================================================================

section("E. TERMÓMETRO Y LÓGICA DE GOBIERNO (FIGURA 14)")

termometro <- read_csv(here("data", "llm", "termometro", "termometro_indicaciones.csv"), show_col_types = FALSE) |>
  filter(!is.na(termometro_num)) |>
  mutate(signo = as.integer(sign(termometro_num)),
         direccion = factor(c("Más Estado", "Neutral", "Más mercado")[signo + 2],
                            c("Más Estado", "Neutral", "Más mercado"))) |>
  select(votacion_id, signo, direccion)

grupos_e <- c("PRCh", "PNL", "UDI", "RN", "Evópoli", "IND con la derecha", "PS", "PPD", "FA", "PC")
votos_e <- votos |>
  filter(voto_bcall %in% c(-1L, 0L, 1L)) |>
  inner_join(termometro, by = "votacion_id") |>
  left_join(leer("topicos", "topicos_indicaciones.csv") |> select(votacion_id, presupuesto), by = "votacion_id") |>
  mutate(
    legislatura = legislatura(anio_legislativo),
    grupo = coalesce(etiqueta[grupo_votacion], grupo_votacion),
    si = as.integer(voto_bcall == 1L),
    instrumento = if_else(coalesce(presupuesto, FALSE), "Ley de Presupuestos", "Otros proyectos"),
    autoria = autor_heuristico
  ) |>
  filter(grupo %in% grupos_e, !(grupo_votacion %in% nueva & !anio_legislativo %in% anios_nueva))

respuesta <- function(d, por = character()) {
  d |>
    summarise(votos = n(), votaciones = n_distinct(votacion_id), diputados = n_distinct(diputado_id),
              si = mean(si), .by = all_of(c("legislatura", por, "grupo", "direccion"))) |>
    mutate(mostrar = votaciones >= 5 & diputados >= min_miembros)
}

resp_total <- respuesta(votos_e)
resp_instrumento <- respuesta(votos_e, "instrumento")
resp_autoria <- respuesta(filter(votos_e, autoria %in% c("Ejecutivo", "Parlamentaria")), "autoria")
escribir(resp_total, "E_respuesta_termometro.csv")
escribir(resp_instrumento, "E_respuesta_por_instrumento.csv")
escribir(resp_autoria, "E_respuesta_por_autoria.csv")

resp_total |>
  filter(mostrar) |>
  mutate(celda = sprintf("%.0f%% (%d)", 100 * si, votaciones)) |>
  select(legislatura, grupo, direccion, celda) |>
  pivot_wider(names_from = direccion, values_from = celda) |>
  arrange(legislatura, match(grupo, grupos_e)) |>
  print(n = Inf)

# Pendiente de Sí sobre el signo por grupo y legislatura (errores por votación)
pendiente <- votos_e |>
  nest(.by = c(legislatura, grupo)) |>
  mutate(votaciones = map_int(data, \(d) n_distinct(d$votacion_id)),
         diputados = map_int(data, \(d) n_distinct(d$diputado_id)),
         signos = map_int(data, \(d) n_distinct(d$signo))) |>
  filter(votaciones >= 15, signos >= 2, diputados >= min_miembros) |>
  mutate(m = map(data, \(d) feols(si ~ signo, data = d, cluster = ~votacion_id)),
         estimate = map_dbl(m, \(x) coef(x)[["signo"]]), se = map_dbl(m, \(x) se(x)[["signo"]])) |>
  select(legislatura, grupo, votaciones, diputados, estimate, se) |>
  mutate(conf_low = estimate - 1.96 * se, conf_high = estimate + 1.96 * se) |>
  arrange(legislatura, match(grupo, grupos_e))
escribir(pendiente, "E_pendiente_legislatura.csv")
pendiente |> mutate(across(where(is.double), \(x) round(x, 3))) |> print(n = Inf)

figura_respuesta <- function(d, filas = NULL, caption) {
  d <- d |>
    filter(mostrar) |>
    mutate(grupo = factor(grupo, grupos_e),
           lado = if_else(grupo %in% c("PS", "PPD", "FA", "PC"), "Izquierda", "Derecha"))
  n_eje <- d |>
    summarise(n = max(votaciones), .by = c(legislatura, any_of(filas), direccion))
  faceta <- if (is.null(filas)) facet_wrap(~legislatura) else facet_grid(as.formula(paste(filas, "~ legislatura")))
  ggplot(d, aes(direccion, si, colour = grupo, group = grupo)) +
    geom_line(aes(linetype = lado), linewidth = 0.6) +
    geom_point(size = 1.9) +
    geom_text(data = n_eje, aes(direccion, 0.02, label = paste0("n = ", n)), inherit.aes = FALSE,
              size = 2.6, colour = "grey35") +
    faceta +
    scale_colour_manual(values = paleta, name = NULL) +
    scale_linetype_manual(values = c(Derecha = "solid", Izquierda = "dashed"), guide = "none") +
    scale_y_continuous("% Sí", labels = pct, limits = c(0, 1)) +
    labs(x = "Dirección de lo que implica votar Sí", caption = str_wrap(caption, 160)) +
    tema() +
    guides(colour = guide_legend(nrow = 1))
}

base_caption <- paste(
  "% de votos Sí (sobre Sí, No y abstención) por dirección del texto según el termómetro. Línea continua = derecha; discontinua = izquierda.",
  "n = votaciones de la celda (máximo entre grupos). Celdas con al menos 5 votaciones y 3 diputados."
)
guardar(figura_respuesta(resp_total, caption = base_caption), "E_respuesta_con_izquierda.png", alto = 5.5)
guardar(figura_respuesta(resp_instrumento, "instrumento", base_caption), "E_respuesta_por_instrumento.png", alto = 8)
guardar(figura_respuesta(resp_autoria, "autoria", base_caption), "E_respuesta_por_autoria.png", alto = 8)

cat("\nEscrito en", output_dir, "\n")
print(list.files(output_dir, recursive = TRUE))
