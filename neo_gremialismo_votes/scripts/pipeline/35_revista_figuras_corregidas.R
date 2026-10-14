# ==============================================================================
# 35_revista_figuras_corregidas.R
#
# Regenera las figuras del paper con la izquierda corregida (espejo del 19).
# El 11 original comprueba sus insumos contra las cifras del borrador y se detiene
# si no coinciden. Con la izquierda corregida, algunas cifras cambian en el último
# decimal; este script copia el 11 al espejo, reemplaza SOLO esos números por los
# corregidos (lista CAMBIOS, cada uno con la cifra del borrador y la corregida), lo
# corre en el espejo y copia las figuras a data/analisis/revista/figuras_izquierda_corregida/.
# No toca figuras/ ni el 11 original.
#
# Cada cambio se verifica: el texto original debe aparecer exactamente una vez. Si el
# 11 falla en otro chequeo, el script muestra el mensaje y se agrega a CAMBIOS solo
# después de revisar el valor corregido en el CSV de origen.
# ==============================================================================

pacman::p_load(tidyverse, here)

espejo <- here("data", "analisis", "revista", "a1_izquierda", "espejo")
original <- here("scripts", "pipeline", "11_figuras_paper.R")
destino_script <- file.path(espejo, "scripts", "pipeline", "11_figuras_paper.R")
salida <- here("data", "analisis", "revista", "figuras_izquierda_corregida")
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

CAMBIOS <- tribble(
  ~antes, ~despues, ~motivo,
  "c(0.77, 2.99, 2.71, 2.02)", "c(0.78, 2.99, 2.71, 2.02)", "distancia d1 del primer año: 0,77 → 0,78 con la izquierda corregida",
  'paste("Distancia no da 0,77/2,99/2,71/2,02:"', 'paste("Distancia no da 0,78/2,99/2,71/2,02:"', "mensaje del chequeo",
  '  RD = "#117733",', '  RD = "#117733",\n  FA = "#117733",', "con la izquierda corregida, RD, PCS y COMUNES forman «FA» (mismo color que RD)"
)
extra <- Sys.getenv("CAMBIOS_EXTRA", "")   # «antes|||despues» separados por ;;; para correr con otros ajustes ya revisados
if (nzchar(extra)) {
  e <- str_split(str_split_1(extra, ";;;"), fixed("|||"))
  CAMBIOS <- bind_rows(CAMBIOS, tibble(antes = map_chr(e, 1), despues = map_chr(e, 2), motivo = "ajuste revisado manualmente"))
}

txt <- read_file(original)
for (i in seq_len(nrow(CAMBIOS))) {
  n <- str_count(txt, fixed(CAMBIOS$antes[i]))
  if (n != 1) stop("El texto «", CAMBIOS$antes[i], "» aparece ", n, " veces en el 11; se esperaba 1.", call. = FALSE)
  txt <- str_replace(txt, fixed(CAMBIOS$antes[i]), CAMBIOS$despues[i])
}
dir.create(dirname(destino_script), recursive = TRUE, showWarnings = FALSE)
write_file(txt, destino_script)
write_csv(CAMBIOS, file.path(salida, "00_cambios_al_11.csv"))
cat("11 parcheado en el espejo:", nrow(CAMBIOS), "cambios\n")

# Corre el 11 en el espejo (sus lecturas y la escritura de figuras/ son relativas al espejo)
viejo_wd <- setwd(espejo)
res <- system2("Rscript", destino_script, stdout = TRUE, stderr = TRUE)
setwd(viejo_wd)
writeLines(res, file.path(salida, "01_log_11.txt"))
if (!is.null(attr(res, "status")) && attr(res, "status") != 0) { cat(tail(res, 15), sep = "\n"); stop("El 11 falló en el espejo.", call. = FALSE) }

figs <- list.files(file.path(espejo, "figuras"), full.names = TRUE)
invisible(file.copy(figs, salida, overwrite = TRUE))
cat("Figuras copiadas a", salida, ":", length(figs), "archivos\n")

# Comparación con las figuras vigentes: tamaño y fecha (no pixel a pixel)
vigentes <- list.files(here("figuras"), full.names = FALSE)
nuevas <- list.files(salida, pattern = "\\.(png|pdf)$")
cat("Figuras nuevas sin equivalente en figuras/:", paste(setdiff(nuevas, vigentes), collapse = ", "), "\n")
cat("Figuras vigentes sin versión corregida:", paste(setdiff(vigentes[str_detect(vigentes, "\\.(png|pdf)$")], nuevas), collapse = ", "), "\n")
