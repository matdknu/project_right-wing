# ==============================================================================
# 02_bcall.R  ·  B-Call de indicaciones y de todas las votaciones
#
# Pasos: 04 (indicaciones), 10 (escala anual P), 19 (izquierda corregida),
#        20 (todas las votaciones, con esa izquierda).
# 19 se corre con SALIDA en data/analisis/revista/a1_izquierda/. PASO=1 solo
# inventario; PASO=2 (omisión) reestima la cadena 04-18 en el espejo.
# Uso: Rscript scripts/pipeline/02_bcall.R
#      PASO=1 Rscript scripts/pipeline/02_bcall.R
# ==============================================================================

f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(f), "_correr.R"), local = TRUE)

correr_pasos(c("04_indicaciones_bcall.R", "10_bcall_anio.R"))
correr_pasos("19_izquierda_v2.R", entorno = c(
  SALIDA = "data/analisis/revista/a1_izquierda",
  PASO = Sys.getenv("PASO", "2")
))
correr_pasos("20_revista_universo.R")
message("\nB-Call listo en data/analisis/bcall/ y data/analisis/revista/a4_universo/")
