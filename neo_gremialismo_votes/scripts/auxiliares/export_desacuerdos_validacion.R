# Exporta desacuerdos OpenAI vs DeepSeek para variables problemáticas (piloto).
# Uso: Rscript export_desacuerdos_validacion.R

pacman::p_load(tidyverse)

vars <- c("naturaleza", "subsidiariedad", "nacion")
origen <- here::here("data", "llm", "validacion", "desacuerdos_openai_vs_deepseek.csv")
dest <- here::here("data", "llm", "validacion")

d <- read_delim(origen, delim = ";", show_col_types = FALSE, na = "")
stopifnot(nrow(d) > 0)

des <- d |>
  filter(variable %in% vars, openai != deepseek) |>
  arrange(variable, anio, muestra)

walk(vars, \(v) {
  sub <- filter(des, variable == v)
  write_excel_csv2(sub, file.path(dest, paste0("desacuerdos_", v, "_openai_deepseek.csv")))
  cat(v, ": ", nrow(sub), " desacuerdos\n", sep = "")
})

resumen <- des |>
  count(variable, openai, deepseek, sort = TRUE) |>
  group_by(variable) |>
  mutate(pct = round(100 * n / sum(n), 1), .groups = "drop")

write_csv(resumen, file.path(dest, "desacuerdos_piloto_resumen_pares.csv"))
cat("-> desacuerdos_piloto_resumen_pares.csv\n")
