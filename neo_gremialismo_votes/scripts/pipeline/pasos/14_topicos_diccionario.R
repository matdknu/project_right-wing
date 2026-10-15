# ==============================================================================
# 14_topicos_diccionario.R
#
# Tópicos de las indicaciones votadas, con un diccionario de palabras. Solo
# cuenta qué se discute por año; no mira cómo se vota.
#
# Regla:
#   1. Se normaliza el texto (minúsculas, sin tildes) y se buscan las raíces
#      de cada tópico. Una indicación puede tener varios tópicos.
#   2. Fuente: el texto de la indicación. Si no hay ninguna coincidencia, el
#      nombre del proyecto (marcado como fuente = "proyecto").
#   3. Ley de Presupuestos: se marca aparte y la partida asigna además el
#      sector (Partida 09 = Educación, 16 = Salud, etc.). Interior (05),
#      Hacienda (08) y Tesoro Público (50) mezclan materias y no asignan.
#   4. Tópico principal: el de más coincidencias; en empate, el primero en el
#      orden del diccionario.
#
# Chequeo: coincidencia del tópico principal con el tema que asignó el LLM
# (03), con una tabla de equivalencias. No reemplaza al libro de códigos.
#
# Ventana: 11 de marzo de 2016 al 10 de marzo de 2026.
# Entradas: data/origen/indicaciones_analiticas.parquet, proyectos.parquet,
#           data/llm/resultados/indicaciones_codificadas.csv (solo lectura).
# Salidas:  data/analisis/topicos/ y figuras.
# ==============================================================================

pacman::p_load(tidyverse, arrow, stringi, scales, patchwork, here)

# ---- Settings ----------------------------------------------------------------

ventana <- c(as.Date("2016-03-11"), as.Date("2026-03-11"))
anios <- c(
  "2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
  "2021-22", "2022-23", "2023-24", "2024-25", "2025-26"
)

# Diccionario: raíces sobre texto normalizado (sin tildes, minúsculas).
# \\b marca inicio o fin de palabra.
DICCIONARIO <- tribble(
  ~topico,                          ~patron,
  "Educación",                      "\\beducac|\\beducativ|\\bescolar|\\bescuela|\\bcolegio|\\bliceo|\\buniversidad|\\bestudiant|\\bdocente|\\bprofesor|\\bjardin(es)? infantil|\\bparvular|\\bmatricula|\\bgratuidad|\\bcredito con aval|\\bmineduc|\\bjunaeb|\\bjunji|\\bsostenedor",
  "Salud",                          "\\bsalud\\b|\\bhospital|\\bisapre|\\bfonasa|\\bmedicament|\\bfarmac|\\bpaciente|\\benfermedad|\\bcesfam|\\batencion primaria|\\bcodigo sanitario|\\bminsal|\\bmedic[oa]s?\\b|\\bcancer|\\bvacun|\\bpandemia|\\bcovid",
  "Seguridad y justicia penal",     "\\bdelito|\\bpenal\\b|\\bpenales\\b|\\bcodigo penal|\\bcarabinero|\\bpolicia|\\bpdi\\b|\\bcrimen|\\bcriminal|\\bfiscalia|\\bministerio publico|\\bpresidio|\\bcarcel|\\bgendarmeria|\\bterroris|\\bnarcotrafic|\\barmas\\b|\\barmas de fuego|\\bseguridad publica|\\borden publico|\\bhomicidio|\\brobo\\b|\\bimputad|\\bcondenad|\\breclusion|\\bvictima",
  "Pensiones y vejez",              "\\bpension(es)?\\b(?! de alimentos)|\\bafp\\b|\\bjubilac|\\badultos? mayor|\\bvejez|\\bseguridad social|\\bpilar solidario|\\bpension garantizada|\\bpgu\\b|\\bcotizacion(es)? previsional|\\bprevisional|\\bfondos de pensiones|\\brenta vitalicia|\\bretiro de fondos",
  "Trabajo y empleo",               "\\btrabajador|\\bempleador|\\bcodigo del trabajo|\\bcontrato de trabajo|\\bjornada|\\bsindica|\\bremuneracion|\\bingreso minimo|\\bsalario|\\bdespido|\\bindemnizacion|\\bcesantia|\\bempleo\\b|\\bteletrabajo|\\bnegociacion colectiva|\\bhuelga",
  "Medio ambiente",                 "\\bmedio ?ambient|\\bambiental|\\bcambio climatico|\\bemisiones|\\bcontaminac|\\bbiodiversidad|\\bhumedal|\\bresiduos|\\breciclaj|\\bglaciar|\\bareas? protegid|\\bimpacto ambiental|\\bclimatic|\\bdescarbonizac",
  "Agua, energía y minería",        "\\baguas?\\b|\\bhidric|\\bderechos de aprovechamiento|\\benergia|\\benergetic|\\belectric|\\bmineri|\\bminer[oa]s?\\b|\\blitio|\\bcobre\\b|\\bcodelco|\\broyalty|\\bcombustible|\\bpetroleo|\\bgas natural",
  "Pueblos indígenas",              "\\bindigena|\\bpueblos? originario|\\bmapuche|\\bconadi\\b|\\brapa nui|\\baymara|\\bplurinacional|\\bconvenio 169|\\bcomunidades? indigena|\\bcorporacion nacional de desarrollo indigena",
  "Ciencia y tecnología",           "\\bciencia|\\bcientific|\\binvestigacion y desarrollo|\\binnovacion|\\btecnologi|\\banid\\b|\\bconicyt|\\binteligencia artificial|\\bciberseguridad|\\bdatos personales|\\btelecomunicac|\\binternet|\\bdigital",
  "Vivienda y ciudad",              "\\bvivienda|\\bhabitacional|\\bserviu|\\bminvu|\\barriendo|\\bcampamento|\\burbanis|\\bplan regulador|\\bdeudores habitacionales",
  "Familia, género e infancia",     "\\bnin[oa]s?\\b|\\binfancia|\\badolescen|\\bmujer|\\bgenero\\b|\\bviolencia intrafamiliar|\\bmatrimonio|\\baborto|\\binterrupcion voluntaria|\\bidentidad de genero|\\bfamilia\\b|\\bmaternidad|\\bpaternidad|\\bpension de alimentos|\\bcuidado personal|\\bsename|\\bmejor ninez",
  "Migración",                      "\\bmigra|\\bextranjer|\\brefugiad|\\bexpulsion|\\bingreso clandestino",
  "Impuestos",                      "\\bimpuesto|\\btributari|\\btributo|\\biva\\b|\\bexencion|\\bservicio de impuestos internos|\\bsii\\b|\\belusion|\\bevasion",
  "Sistema político y Estado",      "\\belecci[oó]n|\\belectoral|\\bpartidos politicos|\\bcandidat|\\bplebiscit|\\bconvencion constitucional|\\bconsejo constitucional|\\breforma constitucional|\\bdieta parlamentaria|\\bservel\\b|\\bvoto obligatorio|\\bprobidad|\\blobby|\\bgobernador regional|\\bmunicipal|\\bfuncionarios publicos|\\bcontraloria",
  "Transporte y obras públicas",    "\\btransporte|\\btransantiago|\\bred metropolitana de movilidad|\\bobras publicas|\\bconcesion|\\bcarretera|\\bvialidad|\\bpuerto|\\baeropuerto|\\bferrocarril|\\btren\\b|\\bmetro\\b",
  "Cultura y deporte",              "\\bcultura\\b|\\bculturas\\b|\\bpatrimonio|\\bartist|\\bdeporte|\\bdeportiv|\\bmonumento",
  "Agricultura y pesca",            "\\bagricol|\\bagricultur|\\bcampesin|\\bganad|\\bpesca\\b|\\bpesquer|\\bacuicultur|\\bforestal|\\bsag\\b|\\bindap\\b|\\bconaf\\b|\\briego",
  "Defensa y relaciones exteriores", "\\bfuerzas armadas|\\bdefensa nacional|\\bejercito|\\barmada\\b|\\bfuerza aerea|\\bmilitar|\\brelaciones exteriores|\\bcancilleria|\\btratado|\\bdiplomat",
  "Economía y consumidores",        "\\bconsumidor|\\bsernac|\\bempresa|\\bpymes?\\b|\\bbancari|\\bbanco|\\bdeudor|\\bfomento productivo|\\bcorfo|\\bsercotec|\\blibre competencia|\\bmercado de valores|\\bcomercio\\b"
)

# Partidas de la Ley de Presupuestos y su sector en el diccionario
PARTIDAS <- tribble(
  ~partida, ~topico,
  1L,  "Sistema político y Estado",
  2L,  "Sistema político y Estado",
  3L,  "Seguridad y justicia penal",
  4L,  "Sistema político y Estado",
  6L,  "Defensa y relaciones exteriores",
  7L,  "Economía y consumidores",
  9L,  "Educación",
  11L, "Defensa y relaciones exteriores",
  13L, "Agricultura y pesca",
  20L, "Sistema político y Estado",
  22L, "Sistema político y Estado",
  10L, "Seguridad y justicia penal",
  12L, "Transporte y obras públicas",
  15L, "Trabajo y empleo",
  16L, "Salud",
  17L, "Agua, energía y minería",
  18L, "Vivienda y ciudad",
  19L, "Transporte y obras públicas",
  21L, "Familia, género e infancia",
  23L, "Seguridad y justicia penal",
  24L, "Agua, energía y minería",
  25L, "Medio ambiente",
  26L, "Cultura y deporte",
  27L, "Familia, género e infancia",
  28L, "Sistema político y Estado",
  29L, "Cultura y deporte",
  30L, "Ciencia y tecnología",
  31L, "Seguridad y justicia penal"
)

PATRON_PRESUPUESTO <- "\\bley de presupuestos?|\\bpartida\\s*n?[°o]?\\s*\\d|\\bglosa|\\bsubtitulo\\s*\\d|\\bprograma\\s*\\d+\\b.*\\bcapitulo|\\bcapitulo\\s*\\d+\\b.*\\bprograma"

# Equivalencias con el tema del LLM, para el chequeo
EQUIVALENCIAS <- tribble(
  ~llm_tema,                                    ~topico,
  "Educación",                                  "Educación",
  "Salud",                                      "Salud",
  "Justicia, delito y seguridad",               "Seguridad y justicia penal",
  "Protección social y pensiones",              "Pensiones y vejez",
  "Trabajo y empleo",                           "Trabajo y empleo",
  "Medio ambiente",                             "Medio ambiente",
  "Energía y minería",                          "Agua, energía y minería",
  "Territorio, aguas y recursos naturales",     "Agua, energía y minería",
  "Pueblos indígenas",                          "Pueblos indígenas",
  "Ciencia, tecnología y comunicaciones",       "Ciencia y tecnología",
  "Vivienda y desarrollo urbano",               "Vivienda y ciudad",
  "Familia, género e infancia",                 "Familia, género e infancia",
  "Migración",                                  "Migración",
  "Macroeconomía y tributación",                "Impuestos",
  "Gobierno, Congreso y administración pública", "Sistema político y Estado",
  "Transporte y obras públicas",                "Transporte y obras públicas",
  "Cultura, deporte y patrimonio",              "Cultura y deporte",
  "Agricultura y pesca",                        "Agricultura y pesca",
  "Defensa",                                    "Defensa y relaciones exteriores",
  "Relaciones internacionales y comercio exterior", "Defensa y relaciones exteriores",
  "Empresas, banca y consumidores",             "Economía y consumidores"
)

output_dir <- here("data", "analisis", "topicos")
figures_dir <- file.path(output_dir, "figuras")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Helpers -----------------------------------------------------------------

section <- function(title) {
  cat("\n============================================================\n", title,
      "\n============================================================\n", sep = "")
}

escribir <- function(x, nombre) write_csv(x, file.path(output_dir, nombre), na = "")

guardar <- function(p, nombre, ancho = 10, alto = 6) {
  ggsave(file.path(figures_dir, nombre), p, width = ancho, height = alto, dpi = 300, bg = "white")
}

normalizar <- function(x) {
  x |>
    coalesce("") |>
    stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_squish()
}

anio_legislativo <- function(fecha) {
  a <- as.integer(format(fecha, "%Y")) - as.integer(fecha < as.Date(paste0(format(fecha, "%Y"), "-03-11")))
  sprintf("%d-%02d", a, (a + 1) %% 100)
}

contar <- function(texto, patron) str_count(texto, regex(patron))

# ---- Datos -------------------------------------------------------------------

section("DATOS")

proyectos <- read_parquet(here("data", "origen", "proyectos.parquet")) |>
  distinct(boletin, .keep_all = TRUE) |>
  select(boletin_clean = boletin, nombre_boletin = nombre)

indicaciones <- read_parquet(here("data", "origen", "indicaciones_analiticas.parquet")) |>
  filter(fecha >= ventana[1], fecha < ventana[2]) |>
  left_join(proyectos, by = "boletin_clean") |>
  transmute(
    votacion_id, fecha, boletin = boletin_clean,
    tiempo = anio_legislativo(fecha),
    texto = normalizar(texto_indicacion),
    proyecto = normalizar(coalesce(titulo_proyecto, nombre_proyecto, nombre_boletin))
  ) |>
  filter(tiempo %in% anios)

cat("Indicaciones votadas:", nrow(indicaciones), "| con nombre de proyecto:",
    sum(indicaciones$proyecto != ""), "\n")

# ---- Codificación ------------------------------------------------------------

section("CODIFICACIÓN")

coincidencias <- function(texto) {
  map(set_names(DICCIONARIO$patron, DICCIONARIO$topico), \(p) contar(texto, p)) |>
    as_tibble()
}

en_texto <- coincidencias(indicaciones$texto)
en_proyecto <- coincidencias(indicaciones$proyecto)

presupuesto <- str_detect(indicaciones$texto, regex(PATRON_PRESUPUESTO)) |
  str_detect(indicaciones$proyecto, regex("\\bley de presupuestos?"))
partida <- str_match(indicaciones$texto, "partida\\s*n?[°o]?\\s*(\\d{1,2})")[, 2] |> as.integer()

sin_texto <- rowSums(en_texto) == 0
matriz <- en_texto
matriz[sin_texto, ] <- en_proyecto[sin_texto, ]

# La partida suma su sector
for (k in seq_len(nrow(PARTIDAS))) {
  fila <- presupuesto & !is.na(partida) & partida == PARTIDAS$partida[k]
  matriz[[PARTIDAS$topico[k]]][fila] <- matriz[[PARTIDAS$topico[k]]][fila] + 1L
}

principal <- apply(as.matrix(matriz), 1, \(x) if (max(x) == 0) NA_character_ else names(matriz)[which.max(x)])

codificadas <- indicaciones |>
  select(votacion_id, fecha, tiempo, boletin) |>
  mutate(
    presupuesto = presupuesto,
    partida = if_else(presupuesto, partida, NA_integer_),
    fuente = case_when(!sin_texto ~ "indicación", rowSums(en_proyecto) > 0 ~ "proyecto", TRUE ~ "sin tópico"),
    fuente = if_else(fuente == "sin tópico" & presupuesto & !is.na(partida) & partida %in% PARTIDAS$partida,
                     "partida", fuente),
    topico_principal = coalesce(principal, if_else(presupuesto, "Presupuesto sin sector", "Sin tópico")),
    topicos = apply(as.matrix(matriz) > 0, 1, \(x) paste(names(matriz)[x], collapse = "; "))
  )

escribir(codificadas, "topicos_indicaciones.csv")
escribir(DICCIONARIO, "diccionario.csv")
escribir(PARTIDAS, "partidas_presupuesto.csv")

cat("Fuente del tópico:\n"); print(count(codificadas, fuente))
cat("Presupuesto:", sum(codificadas$presupuesto), "\n")

# ---- Conteos por año ---------------------------------------------------------

section("CONTEOS")

orden_topicos <- c(DICCIONARIO$topico, "Presupuesto sin sector", "Sin tópico")

# Principal: cada indicación cuenta una vez
conteo_principal <- codificadas |>
  count(tiempo, topico_principal, name = "indicaciones") |>
  complete(tiempo = anios, topico_principal = orden_topicos, fill = list(indicaciones = 0L)) |>
  mutate(pct = indicaciones / sum(indicaciones), .by = tiempo) |>
  mutate(topico_principal = factor(topico_principal, orden_topicos)) |>
  arrange(topico_principal, tiempo)
escribir(conteo_principal, "conteo_topico_principal_anio.csv")

# Menciones: una indicación cuenta en cada tópico que toca
menciones <- bind_cols(select(codificadas, tiempo), as_tibble(as.matrix(matriz) > 0)) |>
  pivot_longer(-tiempo, names_to = "topico", values_to = "toca") |>
  summarise(indicaciones = sum(toca), .by = c(tiempo, topico)) |>
  bind_rows(
    codificadas |> summarise(indicaciones = sum(presupuesto), .by = tiempo) |>
      mutate(topico = "Ley de Presupuestos")
  ) |>
  left_join(count(codificadas, tiempo, name = "total"), by = "tiempo") |>
  mutate(pct = indicaciones / total) |>
  arrange(topico, tiempo)
escribir(menciones, "conteo_menciones_anio.csv")

tabla <- conteo_principal |>
  select(tiempo, topico_principal, indicaciones) |>
  pivot_wider(names_from = tiempo, values_from = indicaciones) |>
  mutate(Total = rowSums(across(all_of(anios))))
escribir(tabla, "tabla_topico_principal_anio.csv")
print(tabla, n = Inf, width = Inf)

cat("\nMenciones (una indicación puede tocar varios tópicos):\n")
menciones |>
  select(tiempo, topico, indicaciones) |>
  pivot_wider(names_from = tiempo, values_from = indicaciones) |>
  mutate(Total = rowSums(across(all_of(anios)))) |>
  arrange(desc(Total)) |>
  print(n = Inf, width = Inf)

# ---- Chequeo con el LLM ------------------------------------------------------

section("CHEQUEO CON EL TEMA DEL LLM")

llm <- read_csv(here("data", "llm", "resultados", "indicaciones_codificadas.csv"),
                show_col_types = FALSE) |>
  select(votacion_id, llm_tema) |>
  distinct(votacion_id, .keep_all = TRUE) |>
  left_join(EQUIVALENCIAS, by = "llm_tema") |>
  rename(topico_llm = topico)

chequeo <- codificadas |>
  inner_join(llm, by = "votacion_id") |>
  filter(!is.na(topico_llm), !topico_principal %in% c("Presupuesto sin sector", "Sin tópico")) |>
  mutate(
    coincide_principal = topico_principal == topico_llm,
    coincide_alguno = map2_lgl(topicos, topico_llm, \(t, l) l %in% str_split_1(t, "; "))
  )

resumen_chequeo <- chequeo |>
  summarise(
    indicaciones = n(),
    coincide_principal = mean(coincide_principal),
    coincide_alguno = mean(coincide_alguno),
    .by = topico_llm
  ) |>
  arrange(desc(indicaciones)) |>
  bind_rows(
    chequeo |> summarise(topico_llm = "Total", indicaciones = n(),
                         coincide_principal = mean(coincide_principal),
                         coincide_alguno = mean(coincide_alguno))
  )
escribir(resumen_chequeo, "chequeo_llm.csv")
resumen_chequeo |> mutate(across(where(is.double), \(x) round(x, 2))) |> print(n = Inf)

# ==============================================================================
# FIGURAS
# ==============================================================================

section("FIGURAS")

# 1. Mapa de calor: tópico principal por año
calor <- conteo_principal |>
  filter(topico_principal != "Sin tópico") |>
  mutate(topico_principal = fct_rev(topico_principal))

f1 <- ggplot(calor, aes(factor(tiempo, anios), topico_principal, fill = indicaciones)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = if_else(indicaciones > 0, as.character(indicaciones), "")), size = 2.8,
            colour = if_else(calor$indicaciones > 120, "white", "grey15")) +
  scale_fill_gradient(low = "#F1F5FB", high = "#08306B", name = "Indicaciones", trans = "sqrt") +
  labs(
    x = NULL, y = NULL,
    caption = str_wrap(paste(
      "Cada indicación votada cuenta una vez, en su tópico principal (el de más coincidencias del diccionario en el texto de la indicación;",
      "si no hay ninguna, en el nombre del proyecto). En las indicaciones a la Ley de Presupuestos la partida suma su sector.",
      "Presupuesto sin sector = indicación presupuestaria sin palabras del diccionario y de una partida transversal (Interior, Hacienda, Tesoro Público) o sin partida legible. No se muestran las que quedan sin tópico."
    ), 150)
  ) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(), legend.position = "right",
        plot.caption = element_text(hjust = 0, colour = "grey30", size = 8), plot.caption.position = "plot",
        axis.text.x = element_text(angle = 45, hjust = 1))
guardar(f1, "01_topico_principal_anio.png", alto = 6.5)

# 2. Tendencias de los tópicos que pidió el proyecto, en % de las indicaciones del año
foco <- c("Educación", "Salud", "Seguridad y justicia penal", "Pensiones y vejez",
          "Medio ambiente", "Pueblos indígenas", "Ciencia y tecnología", "Trabajo y empleo")
paleta_foco <- c(
  "Educación" = "#08306B", "Salud" = "#D55E00", "Seguridad y justicia penal" = "#111111",
  "Pensiones y vejez" = "#E69F00", "Medio ambiente" = "#009E73", "Pueblos indígenas" = "#CC79A7",
  "Ciencia y tecnología" = "#56B4E9", "Trabajo y empleo" = "#8C6D31"
)

f2a <- menciones |>
  filter(topico %in% foco) |>
  mutate(topico = factor(topico, foco)) |>
  ggplot(aes(factor(tiempo, anios), indicaciones, colour = topico, group = topico)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = paleta_foco, name = NULL) +
  labs(x = NULL, y = "Indicaciones que tocan el tópico") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

f2b <- menciones |>
  filter(topico %in% foco) |>
  mutate(topico = factor(topico, foco)) |>
  ggplot(aes(factor(tiempo, anios), pct, colour = topico, group = topico)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = paleta_foco, name = NULL) +
  scale_y_continuous(labels = label_percent(decimal.mark = ",", accuracy = 1)) +
  labs(x = NULL, y = "% de las indicaciones del año") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

f2 <- (f2a | f2b) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    caption = str_wrap(paste(
      "Una indicación cuenta en cada tópico que toca. A: número de indicaciones; B: % de las indicaciones votadas ese año.",
      "Incluye las indicaciones presupuestarias asignadas por partida."
    ), 150)
  ) &
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, colour = "grey30", size = 8))
guardar(f2, "02_tendencia_topicos.png", alto = 5.5)

# 3. Peso del presupuesto y tópicos dentro y fuera de él
f3a <- codificadas |>
  count(tiempo, presupuesto) |>
  mutate(presupuesto = if_else(presupuesto, "Ley de Presupuestos", "Otros proyectos")) |>
  ggplot(aes(factor(tiempo, anios), n, fill = presupuesto)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(`Ley de Presupuestos` = "#D55E00", `Otros proyectos` = "#6BAED6"), name = NULL) +
  labs(x = NULL, y = "Indicaciones votadas") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

dentro_fuera <- bind_cols(select(codificadas, tiempo, presupuesto), as_tibble(as.matrix(matriz) > 0)) |>
  pivot_longer(-c(tiempo, presupuesto), names_to = "topico", values_to = "toca") |>
  filter(topico %in% foco) |>
  summarise(indicaciones = sum(toca), .by = c(tiempo, presupuesto, topico)) |>
  mutate(
    presupuesto = if_else(presupuesto, "Ley de Presupuestos", "Otros proyectos"),
    topico = factor(topico, foco)
  )
escribir(dentro_fuera, "conteo_menciones_presupuesto_anio.csv")

f3b <- ggplot(dentro_fuera, aes(factor(tiempo, anios), indicaciones, colour = topico, group = topico)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.4) +
  facet_wrap(~presupuesto, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = paleta_foco, name = NULL) +
  labs(x = NULL, y = "Indicaciones que tocan el tópico") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))

f3 <- (f3a | f3b) +
  plot_layout(widths = c(1, 1.6)) +
  plot_annotation(
    tag_levels = "A",
    caption = str_wrap(paste(
      "Ley de Presupuestos = la indicación menciona la ley, una partida, una glosa o un subtítulo.",
      "B: en el presupuesto el tópico viene sobre todo de la partida (09 = Educación, 16 = Salud, 10, 23 y 31 = seguridad y justicia)."
    ), 150)
  ) &
  theme(plot.caption = element_text(hjust = 0, colour = "grey30", size = 8))
guardar(f3, "03_presupuesto_anio.png", ancho = 11, alto = 6)

cat("\nEscrito en", output_dir, "\n")
print(list.files(output_dir, recursive = TRUE))
