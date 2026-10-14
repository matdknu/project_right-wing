# ==============================================================================
# 13_bloque2_orden_desorden.R
#
# Bloque 2. ¿La nueva derecha ordenó o desordenó el espacio de la derecha?
#
# A. Desorden de etiquetas: cambios de grupo (partido o IND con la derecha)
#    de cada diputado, dentro de un año legislativo y entre legislaturas.
#    De dónde vienen y adónde van los que entran o salen de la nueva derecha.
# B. ¿A quiénes se llevaron hacia la derecha?
#      - acuerdo con la derecha: en las votaciones donde la mayoría de la
#        derecha y la de la izquierda votan distinto, % de veces que el
#        diputado vota con la derecha. Por grupo y año, y para los mismos
#        diputados antes (2018-22) y después (2022-26);
#      - posición en la escala anual común (cero = mediana de la izquierda,
#        unidad = desviación de la Cámara en el año).
# C. ¿Se ordenó la derecha tradicional?
#      - clasificación: cuánto de la variación de d1 dentro de la derecha
#        explica el grupo (eta², universo derecha). Más alto = cada grupo
#        ocupa su propio lugar;
#      - dispersión interna: rango intercuartil de d1 por partido en la
#        escala común;
#      - atracción: en las votaciones donde la mayoría de su partido y la del
#        PRCh votan distinto, % de diputados UDI, RN y Evópoli que votan con
#        el PRCh.
#
# Cautela: el PRCh entra con la oposición a Boric (2022-26) y la comparación
# previa es con el gobierno de Piñera II (2018-22). El cambio de rol de la
# derecha tradicional y la entrada de la nueva derecha ocurren a la vez.
#
# Ventana: 11 de marzo de 2016 al 10 de marzo de 2026.
# Entradas: data/analisis/bcall/ y data/analisis/bcall_anio/. No reestima B-Call.
# Salidas:  data/analisis/bloque2/ y figuras.
# ==============================================================================

pacman::p_load(tidyverse, scales, patchwork, here)

# ---- Settings ----------------------------------------------------------------

ventana <- c(as.Date("2016-03-11"), as.Date("2026-03-11"))
anios <- c(
  "2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
  "2021-22", "2022-23", "2023-24", "2024-25", "2025-26"
)
min_miembros <- 3L

nueva <- c("REP", "PNL")
tradicional <- c("UDI", "RN", "EVOP")
izquierda <- c("PC", "PS", "PPD", "FA", "FRVS", "PL", "PR")
centro_fig <- c("DC", "DEM", "AMA", "PDG", "IND")

familia <- function(g) {
  case_when(
    g %in% nueva ~ "Nueva derecha",
    g %in% tradicional ~ "Derecha tradicional",
    g == "IND-D" ~ "IND con la derecha",
    g == "PSC" ~ "Otra derecha",
    g %in% izquierda ~ "Izquierda",
    TRUE ~ "Centro y otros"
  )
}

etiqueta <- c(
  REP = "PRCh", PNL = "PNL", UDI = "UDI", RN = "RN", EVOP = "Evópoli",
  `IND-D` = "IND con la derecha", PSC = "PSC", DC = "DC", DEM = "Demócratas",
  AMA = "Amarillos", PDG = "PDG", IND = "IND (otros)"
)

paleta <- c(
  PRCh = "#D55E00", PNL = "#E69F00", UDI = "#08306B", RN = "#2171B5",
  Evópoli = "#6BAED6", `IND con la derecha` = "#009E73", PSC = "#8C6D31",
  DC = "#CC79A7", Demócratas = "#7B3294", Amarillos = "#E6AB02",
  PDG = "#A6761D", `IND (otros)` = "#999999"
)

analisis_dir <- here("data", "analisis", "bcall")
output_dir <- here("data", "analisis", "bloque2")
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
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, colour = "grey30", size = 8),
      plot.caption.position = "plot",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

coma <- label_number(decimal.mark = ",", accuracy = 0.1)
pct <- label_percent(decimal.mark = ",", accuracy = 1)

modal_voto <- function(v) {
  v <- v[v %in% c(-1L, 0L, 1L)]
  if (length(v) < min_miembros) return(NA_integer_)
  as.integer(names(sort(table(v), decreasing = TRUE))[1])
}

eta2 <- function(y, g) {
  if (n_distinct(g) < 2) return(NA_real_)
  1 - sum((y - ave(y, g))^2) / sum((y - mean(y))^2)
}

# ---- Datos -------------------------------------------------------------------

section("DATOS")

votos <- read_csv(file.path(analisis_dir, "votos_codificados.csv.gz"),
                  show_col_types = FALSE, progress = FALSE) |>
  mutate(fecha = as.Date(fecha), voto_bcall = as.integer(voto_bcall)) |>
  filter(fecha >= ventana[1], fecha < ventana[2], anio_legislativo %in% anios,
         !is.na(grupo_votacion), grupo_votacion != "OTRO")

# Grupo de cada diputado en el año: el que más se repite en sus votos
grupo_anio <- votos |>
  count(anio_legislativo, diputado_id, legislator, grupo_votacion) |>
  slice_max(n, n = 1, with_ties = FALSE, by = c(anio_legislativo, diputado_id)) |>
  select(tiempo = anio_legislativo, diputado_id, legislator, grupo = grupo_votacion)

escala <- read_csv(here("data", "analisis", "bcall_anio", "01_posicion_partido.csv"),
                   show_col_types = FALSE) |>
  distinct(tiempo, cero, sd_anio)

unidades <- read_csv(file.path(analisis_dir, "bcall_unidades.csv"), show_col_types = FALSE) |>
  filter(eje == "todas", tiempo %in% anios, !is.na(d1)) |>
  select(universo, tiempo, diputado_id, d1, d2) |>
  left_join(grupo_anio, by = c("tiempo", "diputado_id")) |>
  left_join(escala, by = "tiempo") |>
  mutate(posicion = (d1 - cero) / sd_anio)

# ==============================================================================
# A. DESORDEN DE ETIQUETAS: CAMBIOS DE GRUPO
# ==============================================================================

section("A. CAMBIOS DE GRUPO")

# Secuencia de grupos de cada diputado, en orden de fecha, sin repetir tramos
tramos <- votos |>
  summarise(desde = min(fecha), .by = c(diputado_id, legislator, periodo, grupo_votacion)) |>
  arrange(diputado_id, desde) |>
  mutate(
    anterior = lag(grupo_votacion),
    periodo_anterior = lag(periodo),
    .by = diputado_id
  )

cambios <- tramos |>
  filter(!is.na(anterior), anterior != grupo_votacion) |>
  mutate(
    tiempo = map_chr(desde, \(f) {
      a <- as.integer(format(f, "%Y")) - as.integer(f < as.Date(paste0(format(f, "%Y"), "-03-11")))
      sprintf("%d-%02d", a, (a + 1) %% 100)
    }),
    momento = if_else(periodo == periodo_anterior, "Dentro de la legislatura", "Entre legislaturas"),
    origen = anterior,
    destino = grupo_votacion,
    familia_origen = familia(origen),
    familia_destino = familia(destino),
    toca_derecha = familia_origen %in% c("Nueva derecha", "Derecha tradicional", "IND con la derecha", "Otra derecha") |
      familia_destino %in% c("Nueva derecha", "Derecha tradicional", "IND con la derecha", "Otra derecha")
  ) |>
  select(tiempo, desde, momento, diputado_id, legislator, origen, destino,
         familia_origen, familia_destino, toca_derecha)

escribir(cambios, "A_cambios_de_grupo.csv")

cambios_anio <- cambios |>
  filter(toca_derecha) |>
  count(tiempo, momento, name = "cambios") |>
  complete(tiempo = anios, momento, fill = list(cambios = 0L)) |>
  arrange(tiempo)
escribir(cambios_anio, "A_cambios_por_anio.csv")
cambios_anio |> pivot_wider(names_from = momento, values_from = cambios) |> print(n = Inf)

flujos <- cambios |>
  filter(toca_derecha) |>
  count(familia_origen, familia_destino, origen, destino, sort = TRUE)
escribir(flujos, "A_flujos_derecha.csv")
print(flujos, n = 40)

cat("\nCambios que tocan a la nueva derecha:\n")
cambios |>
  filter(familia_origen == "Nueva derecha" | familia_destino == "Nueva derecha") |>
  select(tiempo, momento, legislator, origen, destino) |>
  arrange(tiempo, legislator) |>
  print(n = Inf)

# ==============================================================================
# B. ¿A QUIÉNES SE LLEVARON HACIA LA DERECHA?
# ==============================================================================

section("B. ACUERDO CON LA DERECHA Y POSICIÓN")

# Votaciones divisivas: la mayoría de la derecha (tradicional + nueva) y la de
# la izquierda votan distinto
mayorias <- votos |>
  mutate(lado = case_when(
    grupo_votacion %in% c(nueva, tradicional) ~ "derecha",
    grupo_votacion %in% izquierda ~ "izquierda"
  )) |>
  filter(!is.na(lado)) |>
  summarise(modal = modal_voto(voto_bcall), .by = c(votacion_id, lado)) |>
  pivot_wider(names_from = lado, values_from = modal) |>
  filter(!is.na(derecha), !is.na(izquierda), derecha != izquierda)

acuerdo_diputado <- votos |>
  filter(voto_bcall %in% c(-1L, 0L, 1L)) |>
  inner_join(mayorias, by = "votacion_id") |>
  summarise(
    votaciones = n(),
    acuerdo_derecha = mean(voto_bcall == derecha),
    acuerdo_izquierda = mean(voto_bcall == izquierda),
    .by = c(anio_legislativo, diputado_id)
  ) |>
  rename(tiempo = anio_legislativo) |>
  filter(votaciones >= 20) |>
  left_join(grupo_anio, by = c("tiempo", "diputado_id"))

acuerdo_grupo <- acuerdo_diputado |>
  summarise(
    diputados = n(),
    acuerdo_derecha = median(acuerdo_derecha),
    acuerdo_izquierda = median(acuerdo_izquierda),
    .by = c(tiempo, grupo)
  ) |>
  filter(diputados >= 2) |>
  mutate(nombre = coalesce(etiqueta[grupo], grupo), familia = familia(grupo)) |>
  arrange(grupo, tiempo)
escribir(acuerdo_grupo, "B_acuerdo_derecha_grupo.csv")

acuerdo_grupo |>
  filter(grupo %in% c(centro_fig, "IND-D", "PSC")) |>
  select(tiempo, nombre, acuerdo_derecha) |>
  mutate(acuerdo_derecha = round(acuerdo_derecha, 2)) |>
  pivot_wider(names_from = nombre, values_from = acuerdo_derecha) |>
  arrange(tiempo) |>
  print(n = Inf, width = Inf)

# Los mismos diputados antes y después: presentes en 2018-22 y en 2022-26
legislatura <- function(t) if_else(t %in% c("2018-19", "2019-20", "2020-21", "2021-22"), "2018-22",
                                   if_else(t %in% c("2022-23", "2023-24", "2024-25", "2025-26"), "2022-26", NA_character_))

panel <- acuerdo_diputado |>
  mutate(legislatura = legislatura(tiempo)) |>
  filter(!is.na(legislatura)) |>
  summarise(
    acuerdo_derecha = weighted.mean(acuerdo_derecha, votaciones),
    grupo = names(sort(table(grupo), decreasing = TRUE))[1],
    .by = c(diputado_id, legislator, legislatura)
  ) |>
  pivot_wider(names_from = legislatura, values_from = c(acuerdo_derecha, grupo)) |>
  filter(!is.na(`acuerdo_derecha_2018-22`), !is.na(`acuerdo_derecha_2022-26`)) |>
  mutate(
    cambio = `acuerdo_derecha_2022-26` - `acuerdo_derecha_2018-22`,
    trayectoria = paste(coalesce(etiqueta[`grupo_2018-22`], `grupo_2018-22`), "→",
                        coalesce(etiqueta[`grupo_2022-26`], `grupo_2022-26`)),
    familia_antes = familia(`grupo_2018-22`)
  )
escribir(panel, "B_panel_reelectos.csv")

panel_resumen <- panel |>
  summarise(
    diputados = n(),
    antes = median(`acuerdo_derecha_2018-22`),
    despues = median(`acuerdo_derecha_2022-26`),
    cambio_mediano = median(cambio),
    .by = trayectoria
  ) |>
  arrange(desc(diputados))
escribir(panel_resumen, "B_panel_resumen.csv")
panel_resumen |> mutate(across(where(is.double), \(x) round(x, 3))) |> print(n = Inf)

# Posición en la escala común, por grupo
posicion_grupo <- unidades |>
  filter(universo == "camara", !is.na(grupo)) |>
  summarise(
    diputados = n(),
    q25 = quantile(posicion, 0.25),
    q75 = quantile(posicion, 0.75),
    posicion = median(posicion),
    .by = c(tiempo, grupo)
  ) |>
  filter(diputados >= 2, !(grupo %in% nueva & !tiempo %in% c("2022-23", "2023-24", "2024-25", "2025-26"))) |>
  mutate(nombre = coalesce(etiqueta[grupo], grupo), iqr = q75 - q25) |>
  arrange(grupo, tiempo)
escribir(posicion_grupo, "B_posicion_grupo.csv")

posicion_grupo |>
  filter(grupo %in% c(nueva, tradicional, "IND-D", "DC", "DEM", "AMA", "IND")) |>
  select(tiempo, nombre, posicion) |>
  mutate(posicion = round(posicion, 2)) |>
  pivot_wider(names_from = nombre, values_from = posicion) |>
  arrange(tiempo) |>
  print(n = Inf, width = Inf)

# ==============================================================================
# C. ¿SE ORDENÓ LA DERECHA TRADICIONAL?
# ==============================================================================

section("C. ORDEN DENTRO DE LA DERECHA")

# Clasificación: eta² del grupo en d1 dentro de la derecha (grupos con 3 o más)
clasificacion <- unidades |>
  filter(universo == "derecha", grupo %in% c(nueva, tradicional, "IND-D", "PSC")) |>
  add_count(tiempo, grupo, name = "n_grupo") |>
  filter(n_grupo >= min_miembros) |>
  summarise(
    diputados = n(),
    grupos = paste(sort(unique(coalesce(etiqueta[grupo], grupo))), collapse = ", "),
    eta2 = eta2(d1, grupo),
    .by = tiempo
  ) |>
  arrange(tiempo)

# Solo los partidos tradicionales, para comparar con y sin PRCh en la mezcla
clasificacion_trad <- unidades |>
  filter(universo == "derecha", grupo %in% tradicional) |>
  add_count(tiempo, grupo, name = "n_grupo") |>
  filter(n_grupo >= min_miembros) |>
  summarise(eta2_tradicional = eta2(d1, grupo), .by = tiempo)

clasificacion <- left_join(clasificacion, clasificacion_trad, by = "tiempo")
escribir(clasificacion, "C_clasificacion_eta2.csv")
clasificacion |> mutate(across(where(is.double), \(x) round(x, 3))) |> print(n = Inf, width = Inf)

# Atracción: cuando la mayoría del partido y la del PRCh votan distinto
modal_grupo <- votos |>
  filter(grupo_votacion %in% c(tradicional, "REP")) |>
  summarise(modal = modal_voto(voto_bcall), .by = c(votacion_id, grupo_votacion)) |>
  filter(!is.na(modal)) |>
  pivot_wider(names_from = grupo_votacion, values_from = modal, names_prefix = "m_")

atraccion <- votos |>
  filter(grupo_votacion %in% tradicional, voto_bcall %in% c(-1L, 0L, 1L)) |>
  inner_join(modal_grupo, by = "votacion_id") |>
  mutate(propio = case_when(
    grupo_votacion == "UDI" ~ m_UDI,
    grupo_votacion == "RN" ~ m_RN,
    grupo_votacion == "EVOP" ~ m_EVOP
  )) |>
  filter(!is.na(propio), !is.na(m_REP), propio != m_REP) |>
  summarise(
    votaciones = n_distinct(votacion_id),
    votos = n(),
    con_prch = mean(voto_bcall == m_REP),
    con_partido = mean(voto_bcall == propio),
    .by = c(anio_legislativo, grupo_votacion)
  ) |>
  rename(tiempo = anio_legislativo) |>
  mutate(partido = etiqueta[grupo_votacion]) |>
  arrange(partido, tiempo)
escribir(atraccion, "C_atraccion_prch.csv")
atraccion |> mutate(across(where(is.double), \(x) round(x, 3))) |> print(n = Inf, width = Inf)

# ==============================================================================
# FIGURAS
# ==============================================================================

section("FIGURAS")

# 1. Cambios de grupo que tocan a la derecha
f1a <- cambios_anio |>
  ggplot(aes(factor(tiempo, anios), cambios, fill = momento)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(`Dentro de la legislatura` = "#D55E00", `Entre legislaturas` = "#6BAED6"), name = NULL) +
  labs(x = NULL, y = "Cambios de grupo") +
  tema()

flujos_familia <- cambios |>
  filter(toca_derecha, tiempo %in% c("2022-23", "2023-24", "2024-25", "2025-26")) |>
  mutate(origen = coalesce(etiqueta[origen], origen), destino = coalesce(etiqueta[destino], destino)) |>
  count(origen, destino) |>
  mutate(flujo = paste(origen, "→", destino), flujo = fct_reorder(flujo, n))

f1b <- ggplot(flujos_familia, aes(n, flujo)) +
  geom_col(fill = "#08306B", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.3, size = 3) +
  labs(x = "Diputados, 2022-26", y = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())

f1 <- (f1a | f1b) +
  plot_layout(widths = c(1, 1.2)) +
  plot_annotation(
    tag_levels = "A",
    caption = str_wrap(paste(
      "Un cambio es un tramo de votos con un grupo distinto del anterior para el mismo diputado; solo se cuentan los que salen o llegan a un grupo de derecha.",
      "Entre legislaturas = el diputado reelecto vuelve con otro grupo. IND con la derecha = independiente que vota con la derecha en las votaciones divisivas del período.",
      "B: flujos de origen a destino en 2022-26."
    ), 160)
  )
guardar(f1, "01_cambios_de_grupo.png", alto = 6)

# 2. Atracción: acuerdo con la derecha y posición común
grupos_fig2 <- c("IND con la derecha", "DC", "Demócratas", "Amarillos", "PDG", "IND (otros)")
f2a <- acuerdo_grupo |>
  filter(nombre %in% grupos_fig2) |>
  mutate(nombre = factor(nombre, grupos_fig2)) |>
  ggplot(aes(factor(tiempo, anios), acuerdo_derecha, colour = nombre, group = nombre)) +
  geom_hline(yintercept = 0.5, colour = "grey60", linewidth = 0.3, linetype = "dashed") +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = paleta, name = NULL, limits = grupos_fig2) +
  scale_y_continuous("Vota con la derecha (votaciones divisivas)", labels = pct, limits = c(0, 1)) +
  labs(x = NULL) +
  tema()

grupos_pos <- c("PRCh", "PNL", "UDI", "RN", "Evópoli", "IND con la derecha", "Demócratas", "DC")
f2b <- posicion_grupo |>
  filter(nombre %in% grupos_pos) |>
  mutate(nombre = factor(nombre, grupos_pos)) |>
  ggplot(aes(factor(tiempo, anios), posicion, colour = nombre, group = nombre)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = paleta, name = NULL, limits = grupos_pos) +
  scale_y_continuous("Posición (0 = mediana de la izquierda; desviaciones del año)", labels = coma) +
  labs(x = NULL) +
  tema()

f2 <- (f2a | f2b) +
  plot_annotation(
    tag_levels = "A",
    caption = str_wrap(paste(
      "A: mediana por grupo del % de votos que coinciden con la mayoría de la derecha (UDI, RN, Evópoli, PRCh y PNL) cuando esta y la de la izquierda votan distinto; diputados con al menos 20 de esas votaciones en el año.",
      "B: mediana de d1 de B-Call (Cámara) en la escala anual común. Grupos con al menos 2 diputados."
    ), 160)
  )
guardar(f2, "02_atraccion_derecha.png", alto = 6)

# 3. Panel de reelectos
panel_fig <- panel |>
  filter(familia_antes %in% c("Derecha tradicional", "Centro y otros", "IND con la derecha", "Nueva derecha", "Otra derecha"))

f3 <- ggplot(panel_fig, aes(`acuerdo_derecha_2018-22`, `acuerdo_derecha_2022-26`)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linetype = "dashed") +
  geom_point(aes(colour = coalesce(etiqueta[`grupo_2022-26`], `grupo_2022-26`)), size = 2.2, alpha = 0.85) +
  ggrepel::geom_text_repel(
    data = filter(panel_fig, `grupo_2018-22` != `grupo_2022-26` | abs(cambio) > 0.15),
    aes(label = paste0(word(legislator, 1), " ", word(legislator, 2), " (", trayectoria, ")")),
    size = 2.4, max.overlaps = 30, min.segment.length = 0
  ) +
  scale_colour_manual(values = paleta, name = "Grupo en 2022-26", na.value = "grey50") +
  scale_x_continuous("Vota con la derecha, 2018-22", labels = pct, limits = c(0, 1)) +
  scale_y_continuous("Vota con la derecha, 2022-26", labels = pct, limits = c(0, 1)) +
  labs(caption = str_wrap(paste(
    "Diputados reelectos con votos en las dos legislaturas, fuera de la izquierda. Sobre la diagonal = vota más con la derecha en 2022-26.",
    "Etiquetados: quienes cambian de grupo o se mueven más de 15 puntos."
  ), 150)) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, colour = "grey30", size = 8),
        plot.caption.position = "plot")
guardar(f3, "03_panel_reelectos.png", ancho = 10, alto = 7.5)

# 4. Orden dentro de la derecha tradicional
partidos_trad <- c("UDI", "RN", "Evópoli", "PRCh")
f4a <- clasificacion |>
  select(tiempo, `Toda la derecha` = eta2, `Solo UDI, RN y Evópoli` = eta2_tradicional) |>
  pivot_longer(-tiempo) |>
  filter(!is.na(value)) |>
  ggplot(aes(factor(tiempo, anios), value, colour = name, group = name)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c(`Toda la derecha` = "#D55E00", `Solo UDI, RN y Evópoli` = "#08306B"), name = NULL) +
  scale_y_continuous("Varianza de d1 explicada por el grupo (eta²)", labels = pct, limits = c(0, 1)) +
  labs(x = NULL) +
  tema()

f4b <- posicion_grupo |>
  filter(nombre %in% partidos_trad) |>
  mutate(nombre = factor(nombre, partidos_trad)) |>
  ggplot(aes(factor(tiempo, anios), iqr, colour = nombre, group = nombre)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = paleta, name = NULL, limits = partidos_trad) +
  scale_y_continuous("Dispersión interna de d1 (rango intercuartil)", labels = coma) +
  labs(x = NULL) +
  tema()

f4c <- atraccion |>
  filter(partido %in% c("UDI", "RN", "Evópoli")) |>
  ggplot(aes(factor(tiempo, anios), con_prch, colour = partido, group = partido)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = paleta, name = NULL, limits = c("UDI", "RN")) +
  scale_y_continuous("Vota con el PRCh cuando su partido no", labels = pct, limits = c(0, NA)) +
  labs(x = NULL) +
  tema()

f4 <- (f4a | f4b | f4c) +
  plot_annotation(
    tag_levels = "A",
    caption = str_wrap(paste(
      "A: eta² del grupo en d1 de B-Call dentro de la derecha (universo derecha), grupos con al menos 3 diputados: UDI, RN, Evópoli, PRCh, PNL, IND con la derecha y PSC según el año.",
      "B: rango intercuartil de d1 en la escala anual común (Cámara). C: votaciones en que la mayoría del partido y la del PRCh votan distinto; % de votos del partido que coinciden con el PRCh. Evópoli tiene 2 diputados en 2022-26: en B su dispersión es la de dos personas y en C no entra."
    ), 170)
  )
guardar(f4, "04_orden_derecha_tradicional.png", ancho = 12, alto = 5.5)

cat("\nEscrito en", output_dir, "\n")
print(list.files(output_dir, recursive = TRUE))
