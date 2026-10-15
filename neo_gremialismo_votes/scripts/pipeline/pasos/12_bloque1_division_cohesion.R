# ==============================================================================
# 12_bloque1_division_cohesion.R
#
# Bloque 1. ¿Se diferencian la izquierda y la derecha? Y, dado que sí,
# ¿cuán cohesionadas están las derechas, dentro de cada partido y entre ellos?
#
# A. Separación izquierda-derecha con B-Call (universo Cámara, eje de todas
#    las indicaciones, un modelo por año legislativo):
#      - mediana de d1 de cada bloque;
#      - distancia estandarizada entre bloques (diferencia de medianas sobre
#        la desviación combinada);
#      - solapamiento: probabilidad de que un diputado de derecha quede a la
#        izquierda de uno de izquierda (0 = separación total, 0,5 = iguales).
#    Y en las votaciones: % en que la mayoría de la izquierda y la de la
#    derecha votan distinto.
# B. Cohesión dentro de cada partido y bloque, por año:
#      - Rice: |% Sí − % No| entre quienes votan Sí o No, promedio de las
#        votaciones con al menos 3 miembros votando;
#      - d2 de B-Call (variabilidad del voto del diputado), cruda y ajustada
#        por d1, porque los diputados extremos tienden a ser más volátiles.
# C. Cohesión entre partidos, por año:
#      - acuerdo de mayorías: % de votaciones en que el voto mayoritario de
#        dos partidos coincide (Sí, No o abstención);
#      - Rice de la derecha completa frente al Rice promedio de sus partidos:
#        la diferencia es la división que aparece al juntarlos.
#
# Ventana: 11 de marzo de 2016 al 10 de marzo de 2026. La nueva derecha
# cuenta desde que tiene al menos 3 diputados (2022-23).
#
# Entradas: data/analisis/bcall/ (04). No reestima B-Call.
# Salidas:  data/analisis/bloque1/ y figuras.
# ==============================================================================

pacman::p_load(tidyverse, scales, patchwork, here)

# ---- Settings ----------------------------------------------------------------

ventana <- c(as.Date("2016-03-11"), as.Date("2026-03-11"))
anios <- c(
  "2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
  "2021-22", "2022-23", "2023-24", "2024-25", "2025-26"
)
min_miembros <- 3L

partidos_derecha <- c("REP", "PNL", "UDI", "RN", "EVOP")
bloques_division <- c("Izquierda", "Derecha")

# Pares para el acuerdo de mayorías. Los dos últimos son la vara.
pares <- tribble(
  ~a,     ~b,     ~tipo,
  "REP",  "UDI",  "Nueva y tradicional",
  "REP",  "RN",   "Nueva y tradicional",
  "REP",  "EVOP", "Nueva y tradicional",
  "UDI",  "RN",   "Dentro de la tradicional",
  "UDI",  "EVOP", "Dentro de la tradicional",
  "RN",   "EVOP", "Dentro de la tradicional",
  "UDI",  "PS",   "Derecha e izquierda",
  "REP",  "PS",   "Derecha e izquierda"
)

etiqueta <- c(REP = "PRCh", EVOP = "Evópoli", PNL = "PNL", UDI = "UDI", RN = "RN",
              PS = "PS", PC = "PC", PPD = "PPD", FA = "FA")

paleta <- c(
  PRCh = "#D55E00", PNL = "#E69F00", UDI = "#08306B", RN = "#2171B5",
  Evópoli = "#6BAED6", `IND con la derecha` = "#009E73",
  Izquierda = "#4D4D4D", Derecha = "#08306B",
  `Nueva derecha` = "#D55E00", `Derecha tradicional` = "#08306B"
)

analisis_dir <- here("data", "analisis", "bcall")
output_dir <- here("data", "analisis", "bloque1")
figures_dir <- file.path(output_dir, "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Helpers -----------------------------------------------------------------

section <- function(title) {
  cat("\n============================================================\n", title,
      "\n============================================================\n", sep = "")
}

escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")

guardar <- function(p, nombre, ancho = 9, alto = 5.5) {
  ggsave(file.path(figures_dir, nombre), p, width = ancho, height = alto, dpi = 300, bg = "white")
}

tema <- function() {
  theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, colour = "grey30", size = 8),
      plot.caption.position = "plot"
    )
}

coma <- label_number(decimal.mark = ",", accuracy = 0.01)
pct <- label_percent(decimal.mark = ",", accuracy = 1)

rice <- function(v) {
  v <- v[v %in% c(-1L, 1L)]
  if (length(v) < min_miembros) return(NA_real_)
  abs(mean(v))
}

modal_voto <- function(v) {
  v <- v[v %in% c(-1L, 0L, 1L)]
  if (length(v) < min_miembros) return(NA_integer_)
  as.integer(names(sort(table(v), decreasing = TRUE))[1])
}

distancia_estandarizada <- function(x, y) {
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  (median(x) - median(y)) / sqrt((var(x) + var(y)) / 2)
}

# P(un diputado de x queda a la izquierda de uno de y)
solapamiento <- function(x, y) {
  if (length(x) == 0 || length(y) == 0) return(NA_real_)
  mean(outer(x, y, "<")) + 0.5 * mean(outer(x, y, "=="))
}

# ---- Datos -------------------------------------------------------------------

section("DATOS")

votos <- read_csv(file.path(analisis_dir, "votos_codificados.csv.gz"),
                  show_col_types = FALSE, progress = FALSE) |>
  mutate(fecha = as.Date(fecha), voto_bcall = as.integer(voto_bcall)) |>
  filter(fecha >= ventana[1], fecha < ventana[2], anio_legislativo %in% anios)

# Bloque de cada diputado en el año: el que más se repite en sus votos
bloque_diputado <- votos |>
  count(anio_legislativo, diputado_id, bloque) |>
  slice_max(n, n = 1, with_ties = FALSE, by = c(anio_legislativo, diputado_id)) |>
  select(tiempo = anio_legislativo, diputado_id, bloque)

# Partidos con al menos 3 diputados en el año (la nueva derecha, desde 2022-23)
tamano_partido <- votos |>
  distinct(anio_legislativo, partido_votacion, diputado_id) |>
  count(anio_legislativo, partido_votacion, name = "diputados")

unidades <- read_csv(file.path(analisis_dir, "bcall_unidades.csv"), show_col_types = FALSE) |>
  filter(universo == "camara", eje == "todas", tiempo %in% anios, !is.na(d1), !is.na(d2)) |>
  left_join(bloque_diputado, by = c("tiempo", "diputado_id")) |>
  mutate(
    lado = case_when(
      bloque == "Izquierda" ~ "Izquierda",
      bloque %in% c("Derecha tradicional", "Nueva derecha") ~ "Derecha"
    )
  )

cat("Votos:", nrow(votos), "| votaciones:", n_distinct(votos$votacion_id),
    "| diputados-año en B-Call:", nrow(unidades), "\n")

# ==============================================================================
# A. SEPARACIÓN IZQUIERDA-DERECHA
# ==============================================================================

section("A. SEPARACIÓN IZQUIERDA-DERECHA (B-Call Cámara)")

separacion <- unidades |>
  filter(!is.na(lado)) |>
  summarise(
    izquierda = list(d1[lado == "Izquierda"]),
    derecha = list(d1[lado == "Derecha"]),
    .by = tiempo
  ) |>
  mutate(
    n_izquierda = lengths(izquierda),
    n_derecha = lengths(derecha),
    mediana_izquierda = map_dbl(izquierda, median),
    mediana_derecha = map_dbl(derecha, median),
    distancia = map2_dbl(derecha, izquierda, distancia_estandarizada),
    solapamiento = map2_dbl(derecha, izquierda, solapamiento)
  ) |>
  select(-izquierda, -derecha) |>
  arrange(tiempo)

# En las votaciones: ¿la mayoría de cada lado vota distinto?
lado_voto <- votos |>
  mutate(lado = case_when(
    bloque == "Izquierda" ~ "Izquierda",
    bloque %in% c("Derecha tradicional", "Nueva derecha") ~ "Derecha"
  )) |>
  filter(!is.na(lado))

division_votaciones <- lado_voto |>
  summarise(modal = modal_voto(voto_bcall), .by = c(anio_legislativo, votacion_id, lado)) |>
  pivot_wider(names_from = lado, values_from = modal) |>
  filter(!is.na(Izquierda), !is.na(Derecha)) |>
  summarise(
    votaciones = n(),
    pct_mayorias_distintas = mean(Izquierda != Derecha),
    .by = anio_legislativo
  ) |>
  rename(tiempo = anio_legislativo)

separacion <- left_join(separacion, division_votaciones, by = "tiempo")
escribir(separacion, "A_separacion_izquierda_derecha.csv")
separacion |>
  mutate(across(where(is.double), \(x) round(x, 3))) |>
  print(n = Inf, width = Inf)

# ==============================================================================
# B. COHESIÓN DENTRO DE CADA PARTIDO Y BLOQUE
# ==============================================================================

section("B. COHESIÓN DENTRO (Rice y d2)")

# Rice por votación
rice_partido_votacion <- votos |>
  filter(partido_votacion %in% c(partidos_derecha, "PS", "PC", "PPD", "FA")) |>
  summarise(rice = rice(voto_bcall), .by = c(anio_legislativo, votacion_id, partido_votacion))

rice_bloque_votacion <- votos |>
  filter(bloque %in% c("Nueva derecha", "Derecha tradicional", "IND con la derecha", "Izquierda")) |>
  summarise(rice = rice(voto_bcall), .by = c(anio_legislativo, votacion_id, bloque))

rice_partido <- rice_partido_votacion |>
  left_join(tamano_partido, by = c("anio_legislativo", "partido_votacion")) |>
  filter(diputados >= min_miembros) |>
  summarise(
    votaciones = sum(!is.na(rice)),
    rice = mean(rice, na.rm = TRUE),
    diputados = first(diputados),
    .by = c(anio_legislativo, partido_votacion)
  ) |>
  filter(votaciones > 0) |>
  mutate(partido = etiqueta[partido_votacion]) |>
  rename(tiempo = anio_legislativo)

rice_bloque <- rice_bloque_votacion |>
  summarise(votaciones = sum(!is.na(rice)), rice = mean(rice, na.rm = TRUE),
            .by = c(anio_legislativo, bloque)) |>
  filter(votaciones > 0) |>
  rename(tiempo = anio_legislativo)

# d2: la variabilidad del voto. Ajustada por d1 con una curva cuadrática por
# año: el residuo dice si el partido es más o menos volátil de lo esperable
# para su posición.
unidades <- unidades |>
  group_by(tiempo) |>
  mutate(d2_ajustado = residuals(lm(d2 ~ poly(d1, 2, raw = TRUE)))) |>
  ungroup()

d2_partido <- unidades |>
  filter(partido %in% c(partidos_derecha, "PS", "PC", "PPD", "FA")) |>
  summarise(
    diputados = n(),
    d2_mediana = median(d2),
    d2_ajustado = median(d2_ajustado),
    .by = c(tiempo, partido)
  ) |>
  filter(diputados >= min_miembros) |>
  mutate(partido = etiqueta[partido])

d2_bloque <- unidades |>
  filter(bloque %in% c("Nueva derecha", "Derecha tradicional", "IND con la derecha", "Izquierda")) |>
  summarise(diputados = n(), d2_mediana = median(d2), d2_ajustado = median(d2_ajustado),
            .by = c(tiempo, bloque)) |>
  filter(diputados >= min_miembros)

cohesion_partido <- full_join(
  select(rice_partido, tiempo, partido, rice, votaciones),
  d2_partido, by = c("tiempo", "partido")
) |>
  arrange(partido, tiempo)

cohesion_bloque <- full_join(
  select(rice_bloque, tiempo, bloque, rice, votaciones),
  d2_bloque, by = c("tiempo", "bloque")
) |>
  arrange(bloque, tiempo)

escribir(cohesion_partido, "B_cohesion_partido.csv")
escribir(cohesion_bloque, "B_cohesion_bloque.csv")

cohesion_partido |>
  filter(partido %in% c("PRCh", "UDI", "RN", "Evópoli", "PS")) |>
  mutate(across(where(is.double), \(x) round(x, 3))) |>
  print(n = Inf, width = Inf)
cohesion_bloque |>
  mutate(across(where(is.double), \(x) round(x, 3))) |>
  print(n = Inf, width = Inf)

# ==============================================================================
# C. COHESIÓN ENTRE PARTIDOS
# ==============================================================================

section("C. COHESIÓN ENTRE (acuerdo de mayorías y Rice conjunto)")

modal_partido <- votos |>
  filter(partido_votacion %in% unique(c(pares$a, pares$b))) |>
  summarise(modal = modal_voto(voto_bcall), .by = c(anio_legislativo, votacion_id, partido_votacion)) |>
  filter(!is.na(modal))

acuerdo <- pmap_dfr(pares, \(a, b, tipo) {
  modal_partido |>
    filter(partido_votacion %in% c(a, b)) |>
    pivot_wider(names_from = partido_votacion, values_from = modal) |>
    filter(!is.na(.data[[a]]), !is.na(.data[[b]])) |>
    summarise(acuerdo = mean(.data[[a]] == .data[[b]]), votaciones = n(), .by = anio_legislativo) |>
    mutate(par = paste(etiqueta[a], "–", etiqueta[b]), tipo = tipo)
}) |>
  rename(tiempo = anio_legislativo) |>
  arrange(par, tiempo)

escribir(acuerdo, "C_acuerdo_mayorias.csv")
acuerdo |>
  mutate(acuerdo = round(acuerdo, 3)) |>
  select(tiempo, par, acuerdo) |>
  pivot_wider(names_from = par, values_from = acuerdo) |>
  arrange(tiempo) |>
  print(n = Inf, width = Inf)

# Rice de la derecha completa frente al promedio de sus partidos, en las
# mismas votaciones. Si los partidos votan cohesionados pero en direcciones
# distintas, el Rice conjunto cae.
derecha_conjunta <- votos |>
  filter(partido_votacion %in% partidos_derecha) |>
  left_join(tamano_partido, by = c("anio_legislativo", "partido_votacion")) |>
  filter(diputados >= min_miembros)

rice_conjunto <- derecha_conjunta |>
  summarise(rice_conjunto = rice(voto_bcall), .by = c(anio_legislativo, votacion_id))

rice_promedio_partidos <- derecha_conjunta |>
  summarise(rice = rice(voto_bcall), .by = c(anio_legislativo, votacion_id, partido_votacion)) |>
  summarise(rice_partidos = mean(rice, na.rm = TRUE), .by = c(anio_legislativo, votacion_id))

entre <- inner_join(rice_conjunto, rice_promedio_partidos, by = c("anio_legislativo", "votacion_id")) |>
  filter(!is.na(rice_conjunto), !is.na(rice_partidos)) |>
  summarise(
    votaciones = n(),
    rice_partidos = mean(rice_partidos),
    rice_conjunto = mean(rice_conjunto),
    .by = anio_legislativo
  ) |>
  mutate(perdida = rice_partidos - rice_conjunto) |>
  rename(tiempo = anio_legislativo) |>
  arrange(tiempo)

partidos_en_conjunto <- derecha_conjunta |>
  distinct(anio_legislativo, partido_votacion) |>
  summarise(partidos = paste(sort(etiqueta[partido_votacion]), collapse = ", "), .by = anio_legislativo) |>
  rename(tiempo = anio_legislativo)

entre <- left_join(entre, partidos_en_conjunto, by = "tiempo")
escribir(entre, "C_rice_conjunto_derecha.csv")
entre |>
  mutate(across(where(is.double), \(x) round(x, 3))) |>
  print(n = Inf, width = Inf)

# ==============================================================================
# FIGURAS
# ==============================================================================

section("FIGURAS")

# 1. Separación izquierda-derecha
sep_largo <- separacion |>
  select(tiempo, Izquierda = mediana_izquierda, Derecha = mediana_derecha) |>
  pivot_longer(-tiempo, names_to = "lado", values_to = "d1")

f1a <- ggplot(filter(unidades, !is.na(lado)), aes(factor(tiempo, anios), d1, colour = lado)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_jitter(width = 0.18, height = 0, size = 0.8, alpha = 0.45) +
  geom_point(data = sep_largo, aes(y = d1), size = 2.6, shape = 18) +
  scale_colour_manual(values = paleta[c("Izquierda", "Derecha")], name = NULL) +
  scale_y_continuous("Posición en B-Call (d1)", labels = coma) +
  labs(x = NULL) +
  tema() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

f1b <- separacion |>
  select(tiempo, `Mayorías que votan distinto` = pct_mayorias_distintas, Solapamiento = solapamiento) |>
  pivot_longer(-tiempo) |>
  ggplot(aes(factor(tiempo, anios), value, colour = name, group = name)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c(`Mayorías que votan distinto` = "#08306B", Solapamiento = "#D55E00"), name = NULL) +
  scale_y_continuous(NULL, labels = pct, limits = c(0, 1)) +
  labs(x = NULL) +
  tema() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

f1 <- (f1a | f1b) +
  plot_annotation(
    tag_levels = "A",
    caption = str_wrap(paste(
      "A: un punto por diputado; el rombo es la mediana del bloque. Derecha = UDI, RN, Evópoli, PRCh y PNL; izquierda = PC, PS, PPD, FA, PR, FRVS y PL.",
      "B: solapamiento = probabilidad de que un diputado de derecha quede a la izquierda de uno de izquierda; mayorías que votan distinto = % de votaciones en que el voto mayoritario de cada lado difiere."
    ), 150)
  )
guardar(f1, "01_separacion_izquierda_derecha.png", ancho = 10, alto = 5)

# 2. Cohesión dentro: Rice por partido y d2 ajustado
partidos_fig <- c("PRCh", "UDI", "RN", "Evópoli", "PNL")
cp <- cohesion_partido |>
  filter(partido %in% partidos_fig) |>
  mutate(tiempo = factor(tiempo, anios), partido = factor(partido, partidos_fig))

f2a <- ggplot(filter(cp, !is.na(rice)), aes(tiempo, rice, colour = partido, group = partido)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = paleta, name = NULL, limits = partidos_fig) +
  scale_y_continuous("Rice (1 = todos votan igual)", labels = coma, limits = c(0.6, 1)) +
  labs(x = NULL) +
  tema() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

f2b <- ggplot(filter(cp, !is.na(d2_ajustado)), aes(tiempo, d2_ajustado, colour = partido, group = partido)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = paleta, name = NULL, limits = partidos_fig) +
  scale_y_continuous("d2 ajustado por d1 (− = más consistente)", labels = coma) +
  labs(x = NULL) +
  tema() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bloques_fig <- c("Nueva derecha", "Derecha tradicional", "IND con la derecha", "Izquierda")
f2c <- cohesion_bloque |>
  filter(bloque %in% bloques_fig, !is.na(rice)) |>
  ggplot(aes(factor(tiempo, anios), rice, colour = bloque, group = bloque)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = paleta, name = NULL, limits = bloques_fig) +
  scale_y_continuous("Rice del bloque", labels = coma, limits = c(0.6, 1)) +
  labs(x = NULL) +
  tema() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

f2 <- ((f2a | f2b) / f2c) +
  plot_annotation(
    tag_levels = "A",
    caption = str_wrap(paste(
      "A y C: Rice promedio de las votaciones con al menos 3 miembros votando Sí o No; en C el grupo es el bloque completo.",
      "B: mediana del residuo de d2 tras una curva cuadrática en d1 dentro de cada año; negativo = el partido vota de forma más consistente de lo esperable para su posición.",
      "Solo años con al menos 3 diputados: el PRCh desde 2022-23; Evópoli sale en 2022-26, con 2 diputados."
    ), 150)
  )
guardar(f2, "02_cohesion_dentro_partidos.png", ancho = 10, alto = 8.5)

# 3. Cohesión entre: acuerdo de mayorías y Rice conjunto
acuerdo_fig <- acuerdo |>
  mutate(tiempo = factor(tiempo, anios),
         tipo = factor(tipo, c("Nueva y tradicional", "Dentro de la tradicional", "Derecha e izquierda")))

f3a <- ggplot(acuerdo_fig, aes(tiempo, acuerdo, colour = par, group = par)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6) +
  facet_wrap(~tipo, nrow = 1) +
  scale_y_continuous("Acuerdo de mayorías", labels = pct, limits = c(0, 1)) +
  scale_colour_manual(
    values = c(
      "PRCh – UDI" = "#D55E00", "PRCh – RN" = "#E69F00", "PRCh – Evópoli" = "#F0B27A",
      "UDI – RN" = "#08306B", "UDI – Evópoli" = "#2171B5", "RN – Evópoli" = "#6BAED6",
      "UDI – PS" = "#4D4D4D", "PRCh – PS" = "#999999"
    ),
    name = NULL
  ) +
  labs(x = NULL) +
  tema() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

f3b <- entre |>
  select(tiempo, `Promedio de los partidos` = rice_partidos, `Derecha completa` = rice_conjunto) |>
  pivot_longer(-tiempo) |>
  ggplot(aes(factor(tiempo, anios), value, colour = name, group = name)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c(`Promedio de los partidos` = "#08306B", `Derecha completa` = "#D55E00"), name = NULL) +
  scale_y_continuous("Rice", labels = coma, limits = c(0.6, 1)) +
  labs(x = NULL) +
  tema() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

f3 <- (f3a / f3b) +
  plot_layout(heights = c(1.3, 1)) +
  plot_annotation(
    tag_levels = "A",
    caption = str_wrap(paste(
      "A: % de votaciones en que el voto mayoritario de los dos partidos coincide (Sí, No o abstención); solo partidos con al menos 3 diputados votando.",
      "B: Rice de la derecha tomada como un solo grupo frente al Rice promedio de sus partidos, en las mismas votaciones. La distancia entre las dos líneas es la división entre partidos."
    ), 150)
  )
guardar(f3, "03_cohesion_entre_partidos.png", ancho = 10, alto = 8)

cat("\nEscrito en", output_dir, "\n")
print(list.files(output_dir, recursive = TRUE))
