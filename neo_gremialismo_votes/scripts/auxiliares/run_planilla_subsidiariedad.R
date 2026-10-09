# Planilla ciega de los 31 desacuerdos de subsidiariedad y kappa binario de naturaleza.
library(tidyverse)

des <- read_delim(
  "data/llm/validacion/desacuerdos_subsidiariedad_reducido_v04fc6c14.csv",
  delim = ";", show_col_types = FALSE
)
m <- map_dfr(
  list.files("data/llm/muestras", pattern = "^muestra_.*\\.csv$", full.names = TRUE),
  \(f) read_delim(f, delim = ";", show_col_types = FALSE)
) |>
  distinct(texto_id, .keep_all = TRUE)

planilla <- tibble(texto_id = des$texto_id) |>
  left_join(m |> select(texto_id, `texto completo` = texto), by = "texto_id") |>
  mutate(humano = "", nota = "") |>
  select(texto_id, `texto completo`, humano, nota)

stopifnot(nrow(planilla) == 31, all(!is.na(planilla$`texto completo`)))
write_excel_csv2(planilla, "data/llm/validacion/adjudicar_subsidiariedad.csv")

definicion <- tribble(
  ~campo, ~texto,
  "eje", "Subsidiariedad. Dirección del Sí en quién provee los derechos sociales (educación, salud, pensiones, vivienda, cuidados) y en la autonomía de los cuerpos intermedios. Si el texto no regula esa provisión ni la autonomía de prestadores, usa No aplica: no infieras el eje desde macroeconomía o tributación salvo que el texto diga quién provee el servicio.",
  "como_llenar", "En la columna humano escribe exactamente una categoría. La columna nota es libre. No hay respuestas de los modelos en la planilla.",
  "Provisión privada", "votar Sí fortalece a privados o cuerpos intermedios (colegios particulares, universidades, isapres, AFP, gremios, iglesias, organizaciones sociales) en la provisión de derechos sociales, la libertad de elección o de enseñanza, o su autonomía frente al Estado.",
  "Provisión estatal", "votar Sí amplía la provisión pública de derechos sociales o el control del Estado sobre los prestadores privados.",
  "Neutral", "solo si el texto toca la materia de este eje y el cambio no mueve la balanza. Si el texto no toca la materia, usa No aplica.",
  "Mixta", "el cambio empuja explícitamente en ambas direcciones.",
  "No aplica", "el texto no toca la materia de este eje, o el texto no permite saber qué cambia. No uses Neutral por falta de certeza."
)
write_excel_csv2(definicion, "data/llm/validacion/adjudicar_subsidiariedad_definicion.csv")

# Kappa de naturaleza reducida a Sustantiva / No sustantiva
oa <- read_csv("data/llm/cache/votacion__openai-gpt-6-luna__04fc6c14.csv", show_col_types = FALSE)
ds <- read_csv("data/llm/cache/votacion__deepseek-deepseek-v4-flash__04fc6c14.csv", show_col_types = FALSE)
bin <- function(x) case_when(x == "Sustantiva" ~ "Sustantiva", x %in% c("Técnica", "Procedimental") ~ "No sustantiva")
d <- inner_join(
  oa |> filter(codificado_ok %in% TRUE) |> transmute(texto_id, openai = bin(naturaleza)),
  ds |> filter(codificado_ok %in% TRUE) |> transmute(texto_id, deepseek = bin(naturaleza)),
  by = "texto_id"
)
# kappa de Cohen, misma fórmula que 03
kappa_cohen <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  a <- a[ok]; b <- b[ok]
  if (length(a) == 0) return(NA_real_)
  tab <- table(a, b)
  n <- sum(tab)
  pe <- sum(rowSums(tab) * colSums(tab)) / n^2
  po <- sum(diag(tab)) / n
  if (pe == 1) return(NA_real_)
  (po - pe) / (1 - pe)
}
tab <- tibble(
  n = nrow(d),
  acuerdo = mean(d$openai == d$deepseek),
  kappa_binario = kappa_cohen(d$openai, d$deepseek)
)
print(tab)
print(count(d, openai, deepseek))
write_csv(tab, "data/llm/validacion/kappa_naturaleza_binaria.csv")
cat("Planilla:", nrow(planilla), "filas\n")
