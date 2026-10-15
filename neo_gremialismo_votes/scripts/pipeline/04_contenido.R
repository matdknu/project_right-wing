# ==============================================================================
# 04_contenido.R  ·  Qué implica votar Sí
#
# Pasos: 05 (LPM por bloque), 14 (diccionario de tópicos), 16 (termómetro),
#        27 y 28 (modelo conjunto A, B, C y A sin presupuesto), 36 (techo en nación).
# Uso: Rscript scripts/pipeline/04_contenido.R
# ==============================================================================

f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(f), "_correr.R"), local = TRUE)

correr_pasos(c(
  "05_regresiones_votos.R",
  "14_topicos_diccionario.R",
  "16_derechas_termometro.R",
  "27_revista_modelo_conjunto.R",
  "28_revista_modelo_conjunto_C.R",
  "36_revista_techo_nacion.R"
))
message("\nContenido listo.")
