# ==============================================================================
# 37_revista_tabla_iniciativa.R
#
# Arma, con la cobertura completa de iniciativa (33), las cifras de tbl-iniciativa
# y de los párrafos que la comentan:
#   - % Sí del PRCh, la UDI y RN ante mociones y mensajes, 2022-26, todas las votaciones de proyectos;
#   - % de votaciones con las mayorías de las dos derechas distintas, por iniciativa;
#   - caída bruta del Sí (mensaje − moción) y con controles (año + materia + tipo de votación);
#   - coeficientes de Bachelet II y Piñera II con IC 95 %;
#   - % de votaciones con mayorías distintas entre UDI y RN, en total, bajo Bachelet II y Piñera II.
# No reestima nada: lee a3_iniciativa/completo/ y recalcula los promedios brutos.
# Salidas: data/analisis/revista/c_paper/37_*.csv
# ==============================================================================

pacman::p_load(tidyverse, here)
in_dir <- here("data", "analisis", "revista", "a3_iniciativa", "completo")
out_dir <- here("data", "analisis", "revista", "c_paper")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
escribir <- function(x, nombre) write_csv(x, file.path(out_dir, nombre), na = "")

si <- read_csv(file.path(in_dir, "03_si_por_iniciativa.csv"), show_col_types = FALSE)
div <- read_csv(file.path(in_dir, "01_division_por_iniciativa.csv"), show_col_types = FALSE)
mod <- read_csv(file.path(in_dir, "02_modelo_mensaje.csv"), show_col_types = FALSE)

tabla <- bind_rows(
  si |> filter(gobierno == "Boric (2022-26)", muestra == "Todos") |>
    transmute(fila = paste0("% Sí de ", if_else(partido == "REP", "PRCh", partido)), moción = 100 * si_mocion, mensaje = 100 * si_mensaje),
  div |> filter(gobierno == "Boric (2022-26)", muestra == "Todos") |> select(iniciativa, pct_difieren) |>
    pivot_wider(names_from = iniciativa, values_from = pct_difieren) |>
    transmute(fila = "% de votaciones con mayorías de las dos derechas distintas", moción = 100 * `Moción`, mensaje = 100 * Mensaje))
escribir(tabla, "37_tbl_iniciativa.csv"); cat("tbl-iniciativa (2022-26, todas)\n"); print(mutate(tabla, across(where(is.double), \(x) round(x, 1))))

caidas <- si |> filter(gobierno == "Boric (2022-26)") |>
  transmute(partido, muestra, caida_bruta_pp = 100 * (si_mensaje - si_mocion), caida_con_controles_pp = 100 * efecto_mensaje,
            ic_bajo_pp = 100 * conf_low, ic_alto_pp = 100 * conf_high, votaciones = n)
escribir(caidas, "37_caidas_con_controles.csv"); cat("\nCaída del Sí (mensaje − moción): bruta y con controles\n"); print(mutate(caidas, across(where(is.double), \(x) round(x, 1))))

coef <- mod |> filter(efectos_fijos == "tiempo + materia") |>
  transmute(gobierno, comparacion, muestra, efecto_pp = 100 * estimate, ic_bajo_pp = 100 * conf_low, ic_alto_pp = 100 * conf_high, p, votaciones = n, boletines)
escribir(coef, "37_coeficientes_mensaje.csv"); cat("\nEfecto de ser mensaje sobre la división (pp), año + materia\n"); print(mutate(coef, across(where(is.double), \(x) round(x, 3))), width = Inf)

total <- div |> filter(muestra == "Todos") |> summarise(pct_difieren = 100 * weighted.mean(pct_difieren, votaciones), votaciones = sum(votaciones), .by = c(gobierno, comparacion))
por_ini <- div |> filter(muestra == "Todos") |> transmute(gobierno, iniciativa, pct_difieren = 100 * pct_difieren, votaciones)
escribir(total, "37_pct_mayorias_distintas_total.csv"); escribir(por_ini, "37_pct_mayorias_distintas_por_iniciativa.csv")
cat("\n% de votaciones con mayorías distintas (total)\n"); print(mutate(total, pct_difieren = round(pct_difieren, 1)))
cat("\nrazón PRCh-vs-tradicional / UDI-vs-RN bajo Bachelet II (totales)\n")
print(round(total$pct_difieren[total$gobierno == "Boric (2022-26)"] / total$pct_difieren[total$gobierno == "Bachelet II (2016-18)"], 2))
