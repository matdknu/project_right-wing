library(tidyverse)
oa <- read_csv("data/llm/cache/votacion__openai-gpt-6-luna__04fc6c14.csv", show_col_types = FALSE)
ds <- read_csv("data/llm/cache/votacion__deepseek-deepseek-v4-flash__04fc6c14.csv", show_col_types = FALSE)
d <- inner_join(
  oa |> filter(codificado_ok %in% TRUE) |> transmute(texto_id, openai = subsidiariedad),
  ds |> filter(codificado_ok %in% TRUE) |> transmute(texto_id, deepseek = subsidiariedad),
  by = "texto_id"
)
pol <- function(x) {
  case_when(
    x == "Provisión privada" ~ "1",
    x == "Provisión estatal" ~ "-1",
    TRUE ~ "0"
  )
}
d <- mutate(d, red_o = pol(openai), red_d = pol(deepseek))
cat("n", nrow(d), "desc cat", sum(d$openai != d$deepseek, na.rm = TRUE),
    "desc red", sum(d$red_o != d$red_d, na.rm = TRUE), "\n")
print(count(filter(d, openai != deepseek), openai, deepseek, sort = TRUE), n = 20)
cat("--- reducidos ---\n")
print(count(filter(d, red_o != red_d), red_o, red_d, sort = TRUE))
m <- map_dfr(list.files("data/llm/muestras", full.names = TRUE), \(f) read_delim(f, delim = ";", show_col_types = FALSE))
des <- d |>
  filter(red_o != red_d) |>
  left_join(distinct(m, texto_id, anio, texto), by = "texto_id") |>
  mutate(texto = str_trunc(texto, 240))
write_excel_csv2(des, "data/llm/validacion/desacuerdos_subsidiariedad_reducido_v04fc6c14.csv")
cat("export", nrow(des), "\n")
