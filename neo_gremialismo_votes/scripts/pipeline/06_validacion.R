# ==============================================================================
# 06_validacion.R  ·  Chequeos, permutación, validación humana, apéndice E
#
# Pasos: 06, 07 y 08 (cadena original, insumos de 17 y del espejo de 19),
#        17 (robustez), 18 (permutación y bootstrap de R), 30 (planillas humanas),
#        31 (alfa; se omite si aún no hay planillas llenas), 41 (apéndice E).
# Uso: Rscript scripts/pipeline/06_validacion.R
# ==============================================================================

f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(f), "_correr.R"), local = TRUE)

humana <- file.path("data", "analisis", "revista", "a2_validacion_humana", "codificadas")
hay_humana <- length(list.files(humana, pattern = "^ejes_A\\.(xlsx|csv)$")) > 0

correr_pasos(c(
  "06_estrategias_voto.R",
  "07_derechas_estrategias.R",
  "08_chequeos_hallazgos.R",
  "17_revision_robustez.R",
  "18_permutacion_bootstrap.R",
  "30_revista_validacion_humana_materiales.R",
  if (hay_humana) "31_revista_validacion_humana_alfa.R" else {
    message("Se omite 31: no hay planillas humanas en ", humana)
    character()
  },
  "41_revista_apendice_E.R"
))
message("\nValidación lista.")
