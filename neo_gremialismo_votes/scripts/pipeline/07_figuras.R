# ==============================================================================
# 07_figuras.R  ·  Figuras del artículo
#
# Pasos: 11 (figuras del borrador), 35 (las mismas con la izquierda corregida;
#        copia a data/analisis/revista/figuras_izquierda_corregida/ y, si
#        COPIAR=1, a figuras/).
# Uso: Rscript scripts/pipeline/07_figuras.R
#      COPIAR=1 Rscript scripts/pipeline/07_figuras.R
# ==============================================================================

f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(f), "_correr.R"), local = TRUE)

correr_pasos(c("11_figuras_paper.R", "35_revista_figuras_corregidas.R"))

if (identical(Sys.getenv("COPIAR"), "1")) {
  origen <- file.path("data", "analisis", "revista", "figuras_izquierda_corregida")
  figs <- list.files(origen, pattern = "\\.(png|pdf|csv)$", full.names = TRUE)
  invisible(file.copy(figs, "figuras", overwrite = TRUE))
  message("Copiadas ", length(figs), " figuras a figuras/")
}
message("\nFiguras listas.")
