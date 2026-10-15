# ==============================================================================
# 03_posicion.R  ·  Posición, cohesión y contrastes
#
# Pasos: 12 y 13 (separación y reacomodo, indicaciones), 24 (W-NOMINATE),
#        25 (Rice corregido por azar), 26 (DiD de H3), 34 (Fábrega),
#        38 (P y R), 39 (B-Call con umbral 10 %), 40 (Demócratas, DC, universo del Rice).
# Uso: Rscript scripts/pipeline/03_posicion.R
#      SOLO=24,34 Rscript scripts/pipeline/03_posicion.R
# ==============================================================================

f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(f), "_correr.R"), local = TRUE)

correr_pasos(c(
  "12_bloque1_division_cohesion.R",
  "13_bloque2_orden_desorden.R",
  "24_revista_wnominate.R",
  "25_revista_rice_grupos_chicos.R",
  "26_revista_did_h3.R",
  "34_revista_fabrega.R",
  "38_revista_posicion_R.R",
  "39_revista_bcall_umbral10.R",
  "40_revista_centro_y_rice.R"
))
message("\nPosición lista.")
