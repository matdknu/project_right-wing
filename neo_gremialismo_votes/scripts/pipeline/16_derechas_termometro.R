# ==============================================================================
# 16_derechas_termometro.R
#
# Bloque 3. ¿Qué movió a las derechas en el eje mercado-Estado? Qué las
# dividió, qué las unió y cómo cambiaron.
#
# Medida (15): signo del termómetro de lo que implica votar SÍ: −1 = más
# Estado, 0 = no mueve la balanza, +1 = más mercado. Se usa el signo porque
# los dos modelos coinciden en la dirección (kappa 0,70) y no en la
# intensidad (kappa ponderado 0,29).
#
# A. Índice mercado-Estado de cada diputado y año: promedio de signo × voto
#    (Sí = +1, No = −1) en los textos con signo distinto de 0. Va de −1
#    (siempre con más Estado) a +1 (siempre con más mercado). Además, por
#    separado, % No ante más Estado y % Sí ante más mercado.
# B. Respuesta al termómetro: % Sí por signo y grupo; sensibilidad = LPM de
#    Sí sobre el signo por grupo y año (errores por votación).
# C. Qué divide y qué une a la nueva y la tradicional (2022-26): brecha de
#    % Sí y % de votaciones con mayorías distintas, por signo, mecanismo,
#    presupuesto, tópico (14) y autoría; y un LPM de las mayorías distintas.
# D. Cómo cambiaron: el índice de la derecha tradicional por gobierno y el de
#    los mismos diputados en 2018-22 y 2022-26.
#
# Cautela: el índice depende de la agenda del año (qué se votó). Comparar
# grupos dentro de un año es más firme que comparar años.
#
# Entradas: data/llm/termometro/termometro_indicaciones.csv (15),
#           data/analisis/bcall/votos_codificados.csv.gz,
#           data/analisis/topicos/topicos_indicaciones.csv (14).
# Salidas:  data/analisis/bloque3/ y figuras.
# ==============================================================================

pacman::p_load(tidyverse, fixest, scales, patchwork, here)

# ---- Settings ----------------------------------------------------------------

ventana <- c(as.Date("2016-03-11"), as.Date("2026-03-11"))
anios <- c(
  "2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
  "2021-22", "2022-23", "2023-24", "2024-25", "2025-26"
)
min_votos_indice <- 10L
min_miembros <- 3L

nueva <- c("REP", "PNL")
tradicional <- c("UDI", "RN", "EVOP")
izquierda <- c("PC", "PS", "PPD", "FA", "FRVS", "PL", "PR")

etiqueta <- c(REP = "PRCh", PNL = "PNL", UDI = "UDI", RN = "RN", EVOP = "Evópoli",
              `IND-D` = "IND con la derecha", PS = "PS", PSC = "PSC")
grupos_fig <- c("PRCh", "UDI", "RN", "Evópoli", "IND con la derecha", "PS")
paleta <- c(
  PRCh = "#D55E00", PNL = "#E69F00", UDI = "#08306B", RN = "#2171B5",
  Evópoli = "#6BAED6", `IND con la derecha` = "#009E73", PS = "#999999",
  `Nueva derecha` = "#D55E00", `Derecha tradicional` = "#08306B", Izquierda = "#999999"
)

GOBIERNOS <- tribble(
  ~desde,                ~gobierno,
  as.Date("2014-03-11"), "Bachelet II",
  as.Date("2018-03-11"), "Piñera II",
  as.Date("2022-03-11"), "Boric"
)

output_dir <- here("data", "analisis", "bloque3")
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
coma <- label_number(decimal.mark = ",", accuracy = 0.1)

legislatura <- function(t) case_when(
  t %in% anios[1:2] ~ "2016-18",
  t %in% anios[3:6] ~ "2018-22",
  t %in% anios[7:10] ~ "2022-26"
)

# ---- Datos -------------------------------------------------------------------

section("DATOS")

termometro <- read_csv(here("data", "llm", "termometro", "termometro_indicaciones.csv"),
                       show_col_types = FALSE) |>
  filter(!is.na(termometro_num)) |>
  mutate(
    signo = as.integer(sign(termometro_num)),
    direccion = factor(c("Más Estado", "Neutral", "Más mercado")[signo + 2],
                       c("Más Estado", "Neutral", "Más mercado"))
  ) |>
  select(votacion_id, termometro_num, signo, direccion, mecanismo, confianza)

topicos <- read_csv(here("data", "analisis", "topicos", "topicos_indicaciones.csv"), show_col_types = FALSE) |>
  select(votacion_id, presupuesto, topico_principal)

votos <- read_csv(here("data", "analisis", "bcall", "votos_codificados.csv.gz"),
                  show_col_types = FALSE, progress = FALSE) |>
  mutate(fecha = as.Date(fecha), voto_bcall = as.integer(voto_bcall)) |>
  filter(fecha >= ventana[1], fecha < ventana[2], anio_legislativo %in% anios,
         voto_bcall %in% c(-1L, 0L, 1L)) |>
  inner_join(termometro, by = "votacion_id") |>
  left_join(topicos, by = "votacion_id") |>
  mutate(
    tiempo = anio_legislativo,
    legislatura = legislatura(tiempo),
    gobierno = GOBIERNOS$gobierno[findInterval(fecha, GOBIERNOS$desde)],
    grupo = coalesce(etiqueta[grupo_votacion], grupo_votacion),
    familia = case_when(
      grupo_votacion %in% nueva ~ "Nueva derecha",
      grupo_votacion %in% tradicional ~ "Derecha tradicional",
      grupo_votacion %in% izquierda ~ "Izquierda"
    ),
    si = as.integer(voto_bcall == 1L),
    autor = autor_heuristico
  ) |>
  # La nueva derecha cuenta desde 2022-23 (al menos 3 diputados)
  filter(!(grupo_votacion %in% nueva & !tiempo %in% anios[7:10]))

votaciones <- distinct(votos, votacion_id, tiempo, direccion, mecanismo)
cat("Votaciones con termómetro:", nrow(votaciones), "| votos:", nrow(votos), "\n")
conteo <- votaciones |> count(tiempo, direccion) |> pivot_wider(names_from = direccion, values_from = n, values_fill = 0)
escribir(conteo, "00_votaciones_por_direccion_anio.csv")
print(conteo)

# ==============================================================================
# A. ÍNDICE MERCADO-ESTADO
# ==============================================================================

section("A. ÍNDICE MERCADO-ESTADO")

indice_diputado <- votos |>
  filter(signo != 0L, voto_bcall != 0L) |>
  summarise(
    votos = n(),
    indice = mean(signo * voto_bcall),
    no_ante_estado = mean(voto_bcall[signo == -1L] == -1L),
    si_ante_mercado = mean(voto_bcall[signo == 1L] == 1L),
    grupo = names(sort(table(grupo), decreasing = TRUE))[1],
    .by = c(tiempo, diputado_id, legislator)
  ) |>
  filter(votos >= min_votos_indice)
escribir(indice_diputado, "A_indice_diputado_anio.csv")

indice_grupo <- indice_diputado |>
  summarise(
    diputados = n(),
    q25 = quantile(indice, 0.25), q75 = quantile(indice, 0.75),
    indice = median(indice),
    no_ante_estado = median(no_ante_estado, na.rm = TRUE),
    si_ante_mercado = median(si_ante_mercado, na.rm = TRUE),
    .by = c(tiempo, grupo)
  ) |>
  filter(diputados >= min_miembros) |>
  arrange(grupo, tiempo)
escribir(indice_grupo, "A_indice_grupo_anio.csv")

indice_grupo |>
  filter(grupo %in% c(grupos_fig, "PNL")) |>
  select(tiempo, grupo, indice) |>
  mutate(indice = round(indice, 2)) |>
  pivot_wider(names_from = grupo, values_from = indice) |>
  arrange(tiempo) |>
  print(n = Inf, width = Inf)

# ==============================================================================
# B. RESPUESTA AL TERMÓMETRO
# ==============================================================================

section("B. RESPUESTA AL TERMÓMETRO")

respuesta <- votos |>
  filter(grupo %in% c(grupos_fig, "PNL"), !is.na(legislatura)) |>
  summarise(votos = n(), votaciones = n_distinct(votacion_id), diputados = n_distinct(diputado_id),
            si = mean(si), .by = c(legislatura, grupo, direccion)) |>
  filter(votaciones >= 5, diputados >= min_miembros) |>
  arrange(legislatura, grupo, direccion)
escribir(respuesta, "B_si_por_direccion.csv")
respuesta |>
  mutate(si = round(si, 2)) |>
  select(legislatura, grupo, direccion, si) |>
  pivot_wider(names_from = direccion, values_from = si) |>
  print(n = Inf)

sensibilidad <- votos |>
  filter(grupo %in% c(grupos_fig, "PNL")) |>
  nest(.by = c(tiempo, grupo)) |>
  mutate(
    votaciones = map_int(data, \(d) n_distinct(d$votacion_id)),
    signos = map_int(data, \(d) n_distinct(d$signo)),
    diputados = map_int(data, \(d) n_distinct(d$diputado_id))
  ) |>
  filter(votaciones >= 15, signos >= 2, diputados >= min_miembros) |>
  mutate(modelo = map(data, \(d) feols(si ~ signo, data = d, cluster = ~votacion_id)),
         estimate = map_dbl(modelo, \(m) coef(m)[["signo"]]),
         se = map_dbl(modelo, \(m) se(m)[["signo"]])) |>
  select(tiempo, grupo, votaciones, diputados, estimate, se) |>
  mutate(low = estimate - 1.96 * se, high = estimate + 1.96 * se) |>
  arrange(grupo, tiempo)
escribir(sensibilidad, "B_sensibilidad_grupo_anio.csv")
sensibilidad |>
  select(tiempo, grupo, estimate) |>
  mutate(estimate = round(estimate, 2)) |>
  pivot_wider(names_from = grupo, values_from = estimate) |>
  arrange(tiempo) |>
  print(n = Inf, width = Inf)

# ==============================================================================
# C. QUÉ DIVIDE Y QUÉ UNE (2022-26)
# ==============================================================================

section("C. QUÉ DIVIDE Y QUÉ UNE, 2022-26")

modal <- function(v) {
  if (length(v) < min_miembros) return(NA_integer_)
  as.integer(names(sort(table(v), decreasing = TRUE))[1])
}

por_votacion <- votos |>
  filter(legislatura == "2022-26", familia %in% c("Nueva derecha", "Derecha tradicional", "Izquierda")) |>
  summarise(
    si = mean(si), modal = modal(voto_bcall), n = n(),
    .by = c(votacion_id, tiempo, familia, direccion, mecanismo, presupuesto, topico_principal, autor, gobierno)
  ) |>
  pivot_wider(names_from = familia, values_from = c(si, modal, n)) |>
  filter(!is.na(`modal_Nueva derecha`), !is.na(`modal_Derecha tradicional`)) |>
  mutate(
    brecha = `si_Nueva derecha` - `si_Derecha tradicional`,
    distintas = `modal_Nueva derecha` != `modal_Derecha tradicional`,
    trad_con_izquierda = `modal_Derecha tradicional` == modal_Izquierda,
    nueva_con_izquierda = `modal_Nueva derecha` == modal_Izquierda,
    presupuesto = if_else(coalesce(presupuesto, FALSE), "Ley de Presupuestos", "Otros proyectos")
  )
escribir(por_votacion, "C_votaciones_2022_26.csv")

resumir <- function(d, por, dimension) {
  d |>
    summarise(
      votaciones = n(),
      distintas = mean(distintas),
      brecha_abs = mean(abs(brecha)),
      brecha = mean(brecha),
      si_nueva = mean(`si_Nueva derecha`),
      si_tradicional = mean(`si_Derecha tradicional`),
      trad_con_izquierda = mean(trad_con_izquierda, na.rm = TRUE),
      .by = all_of(por)
    ) |>
    rename(categoria = all_of(por)) |>
    mutate(dimension = dimension, categoria = as.character(categoria))
}

divide_une <- bind_rows(
  resumir(por_votacion, "direccion", "Dirección del Sí"),
  resumir(filter(por_votacion, mecanismo != "No aplica"), "mecanismo", "Mecanismo"),
  resumir(por_votacion, "presupuesto", "Instrumento"),
  resumir(por_votacion, "autor", "Autoría"),
  resumir(filter(por_votacion, !topico_principal %in% c("Sin tópico")), "topico_principal", "Tópico")
) |>
  filter(votaciones >= 8) |>
  relocate(dimension, categoria) |>
  arrange(dimension, desc(distintas))
escribir(divide_une, "C_divide_une.csv")
divide_une |> mutate(across(where(is.double), \(x) round(x, 2))) |> print(n = Inf, width = Inf)

# LPM: ¿qué predice que las mayorías voten distinto?
lpm_div <- feols(
  as.integer(distintas) ~ i(direccion, ref = "Neutral") + i(presupuesto, ref = "Otros proyectos") +
    i(autor, ref = "Parlamentaria") | tiempo,
  data = filter(por_votacion, autor != "No identificable"), cluster = ~votacion_id
)
lpm_tab <- broom::tidy(lpm_div, conf.int = TRUE)
escribir(lpm_tab, "C_lpm_mayorias_distintas.csv")
print(lpm_tab)

# ==============================================================================
# D. CÓMO CAMBIARON
# ==============================================================================

section("D. CÓMO CAMBIARON")

por_gobierno <- votos |>
  filter(signo != 0L, voto_bcall != 0L, familia %in% c("Nueva derecha", "Derecha tradicional", "Izquierda")) |>
  summarise(
    votaciones = n_distinct(votacion_id),
    indice = mean(signo * voto_bcall),
    no_ante_estado = mean(voto_bcall[signo == -1L] == -1L),
    si_ante_mercado = mean(voto_bcall[signo == 1L] == 1L),
    .by = c(gobierno, familia)
  ) |>
  arrange(familia, factor(gobierno, GOBIERNOS$gobierno))
escribir(por_gobierno, "D_indice_por_gobierno.csv")
por_gobierno |> mutate(across(where(is.double), \(x) round(x, 2))) |> print()

# Mismos diputados en 2018-22 y 2022-26
panel <- indice_diputado |>
  mutate(legislatura = legislatura(tiempo)) |>
  filter(legislatura %in% c("2018-22", "2022-26")) |>
  summarise(indice = weighted.mean(indice, votos), votos = sum(votos),
            grupo = names(sort(table(grupo), decreasing = TRUE))[1],
            .by = c(diputado_id, legislator, legislatura)) |>
  pivot_wider(names_from = legislatura, values_from = c(indice, votos, grupo)) |>
  filter(!is.na(`indice_2018-22`), !is.na(`indice_2022-26`)) |>
  mutate(cambio = `indice_2022-26` - `indice_2018-22`,
         trayectoria = paste(`grupo_2018-22`, "→", `grupo_2022-26`))
escribir(panel, "D_panel_reelectos.csv")
panel |>
  filter(`grupo_2018-22` %in% c("UDI", "RN", "Evópoli", "IND con la derecha", "PRCh") |
           `grupo_2022-26` %in% c("UDI", "RN", "Evópoli", "IND con la derecha", "PRCh", "PNL")) |>
  summarise(diputados = n(), antes = median(`indice_2018-22`), despues = median(`indice_2022-26`),
            cambio = median(cambio), .by = trayectoria) |>
  arrange(desc(diputados)) |>
  mutate(across(where(is.double), \(x) round(x, 2))) |>
  print(n = Inf)

# ==============================================================================
# FIGURAS
# ==============================================================================

section("FIGURAS")

# 1. Índice por grupo y año
f1 <- indice_grupo |>
  filter(grupo %in% c(grupos_fig, "PNL")) |>
  filter(!(grupo %in% c("PRCh", "PNL") & !tiempo %in% anios[7:10])) |>
  mutate(grupo = factor(grupo, c("PRCh", "PNL", "UDI", "RN", "Evópoli", "IND con la derecha", "PS"))) |>
  ggplot(aes(factor(tiempo, anios), indice, colour = grupo, group = grupo)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_linerange(aes(ymin = q25, ymax = q75), position = position_dodge(width = 0.5), alpha = 0.5) +
  geom_line(position = position_dodge(width = 0.5), linewidth = 0.6) +
  geom_point(position = position_dodge(width = 0.5), size = 1.8) +
  scale_colour_manual(values = paleta, name = NULL) +
  scale_y_continuous("Índice mercado-Estado (−1 = más Estado; +1 = más mercado)", limits = c(-1, 1), labels = coma) +
  labs(x = NULL, caption = str_wrap(paste(
    "Promedio, por diputado, de signo × voto (Sí = +1, No = −1) en las indicaciones cuyo Sí mueve la balanza hacia más Estado (−1) o más mercado (+1), según el termómetro LLM (15).",
    "Punto = mediana del grupo; línea = rango intercuartil. Diputados con al menos 10 de esos votos en el año; grupos con al menos 3 diputados."
  ), 160)) +
  tema() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
guardar(f1, "01_indice_mercado_estado.png", alto = 5.5)

# 2. % Sí por dirección del texto, 2018-22 y 2022-26
f2 <- respuesta |>
  filter(legislatura %in% c("2018-22", "2022-26"), grupo %in% grupos_fig) |>
  mutate(grupo = factor(grupo, grupos_fig)) |>
  ggplot(aes(direccion, si, colour = grupo, group = grupo)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~legislatura) +
  scale_colour_manual(values = paleta, name = NULL) +
  scale_y_continuous("% Sí", labels = pct, limits = c(0, 1)) +
  labs(x = "Dirección de lo que implica votar Sí", caption = str_wrap(
    "% de votos Sí (sobre Sí, No y abstención) por dirección del texto según el termómetro. Celdas con al menos 5 votaciones.", 160)) +
  tema()
guardar(f2, "02_respuesta_termometro.png", alto = 5)

# 3. Qué divide y qué une, 2022-26
f3 <- divide_une |>
  filter(dimension != "Tópico" | votaciones >= 15) |>
  mutate(etiqueta = paste0(categoria, " (", votaciones, ")"),
         etiqueta = fct_reorder(etiqueta, distintas)) |>
  ggplot(aes(distintas, etiqueta)) +
  geom_segment(aes(x = 0, xend = distintas, yend = etiqueta), colour = "grey75") +
  geom_point(aes(colour = brecha), size = 2.8) +
  facet_grid(dimension ~ ., scales = "free_y", space = "free_y") +
  scale_colour_gradient2(low = "#08306B", mid = "grey70", high = "#D55E00", midpoint = 0,
                         labels = label_number(accuracy = 0.01, decimal.mark = ","),
                         name = "Brecha de % Sí\n(nueva − tradicional)") +
  scale_x_continuous("% de votaciones en que la mayoría de la nueva y la de la tradicional votan distinto",
                     labels = pct, limits = c(0, NA)) +
  labs(y = NULL, caption = str_wrap(paste(
    "2022-26. Nueva = PRCh y PNL; tradicional = UDI, RN y Evópoli. Entre paréntesis, número de votaciones con termómetro.",
    "Tópico del diccionario (14); se muestran tópicos con al menos 15 votaciones."
  ), 140)) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), strip.text.y = element_text(angle = 0, face = "bold"),
        plot.caption = element_text(hjust = 0, colour = "grey30", size = 8), plot.caption.position = "plot",
        legend.position = "right")
guardar(f3, "03_que_divide_que_une.png", ancho = 10, alto = 8)

# 4. Sensibilidad por año
f4 <- sensibilidad |>
  filter(grupo %in% grupos_fig) |>
  mutate(grupo = factor(grupo, grupos_fig)) |>
  ggplot(aes(factor(tiempo, anios), estimate, colour = grupo, group = grupo)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 0.55), size = 0.25) +
  geom_line(position = position_dodge(width = 0.55), linewidth = 0.4, alpha = 0.6) +
  scale_colour_manual(values = paleta, name = NULL) +
  scale_y_continuous("Cambio en la probabilidad de Sí por paso hacia el mercado", labels = coma) +
  labs(x = NULL, caption = str_wrap(paste(
    "LPM de Sí sobre el signo del termómetro (−1, 0, +1), por grupo y año, errores por votación; intervalo al 95%.",
    "Positivo = el grupo vota más Sí cuanto más hacia el mercado va el texto. Años con al menos 15 votaciones y dos signos."
  ), 160)) +
  tema() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
guardar(f4, "04_sensibilidad_termometro.png", alto = 5.5)

cat("\nEscrito en", output_dir, "\n")
print(list.files(output_dir, recursive = TRUE))
