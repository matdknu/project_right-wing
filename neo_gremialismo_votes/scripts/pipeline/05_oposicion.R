# ==============================================================================
# 05_oposicion.R  ·  Iniciativa, pivotalidad y presupuesto
#
# Pasos: 09 (presupuesto), 21 (iniciativa con lo que hay en proyectos.parquet),
#        32 (descarga de la Cámara lo que falta), 33 y 37 (cobertura completa),
#        22 y 29 (pivotalidad), 23 (salud 2025).
# 32 es incremental y no usa API de pago.
# Uso: Rscript scripts/pipeline/05_oposicion.R
# ==============================================================================

f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(f), "_correr.R"), local = TRUE)

correr_pasos(c(
  "09_difusion.R",
  "21_revista_iniciativa.R",
  "32_revista_descarga_iniciativa.R",
  "33_revista_iniciativa_completa.R",
  "37_revista_tabla_iniciativa.R",
  "22_revista_pivotalidad.R",
  "29_revista_pivotalidad_muestras.R",
  "23_revista_salud_2025.R"
))
message("\nOposición lista.")
