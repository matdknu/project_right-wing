# ==============================================================================
# 11_figuras_paper.R
#
# Figuras del borrador. No reestima, no llama a la API y no escribe en llm/.
# Lee resultados ya guardados y los grafica en figuras/.
#
# El rango intercuartílico no está en 01_posicion_partido.csv. Se calcula
# desde el d1 individual de llm/analisis/bcall_unidades.csv, con el cero y la
# desviación que ya trae ese CSV. Si la mediana no coincide, el script para.
# ==============================================================================

pacman::p_load(tidyverse, scales, patchwork, ggrepel, here)

dir_out <- here("figuras")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

anios <- c(
  "2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
  "2021-22", "2022-23", "2023-24", "2024-25", "2025-26"
)

# Okabe-Ito. Tradicional en azules, nueva en naranja/bermellón, IND en verde,
# izquierda en gris. La forma separa nueva (triángulo) de tradicional (círculo).
paleta <- c(
  PLR = "#D55E00",
  PNL = "#E69F00",
  UDI = "#08306B",
  RN = "#2171B5",
  Evópoli = "#6BAED6",
  `IND con la derecha` = "#009E73",
  Izquierda = "#4D4D4D"
)
formas <- c(
  PLR = 17, PNL = 17,
  UDI = 16, RN = 16, Evópoli = 16,
  `IND con la derecha` = 15,
  Izquierda = 18
)
formas_huecas <- c(
  PLR = 2, PNL = 2,
  UDI = 1, RN = 1, Evópoli = 1,
  `IND con la derecha` = 0,
  Izquierda = 5
)
lineas <- c(
  PLR = "solid", PNL = "solid",
  UDI = "solid", RN = "dashed", Evópoli = "dotted",
  `IND con la derecha` = "solid", Izquierda = "solid"
)
# Los bloques del coefplot usan el color de su partido ancla.
paleta_bloque <- c(
  Nueva = paleta[["PLR"]],
  Tradicional = paleta[["UDI"]],
  `IND con la derecha` = paleta[["IND con la derecha"]],
  Izquierda = paleta[["Izquierda"]]
)

etiquetar_partido <- function(x) {
  recode(x, REP = "PLR", EVOP = "Evópoli", .default = x)
}

tema <- function() {
  theme_minimal(base_size = 10) +
    theme(
      plot.title = element_blank(),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      plot.caption = element_text(hjust = 0, colour = "grey30", size = 8),
      plot.caption.position = "plot",
      plot.margin = margin(6, 10, 8, 10)
    )
}

num <- label_number(decimal.mark = ",", big.mark = ".", accuracy = 0.01)
num1 <- label_number(decimal.mark = ",", big.mark = ".", accuracy = 0.1)
coma <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, decimal.mark = ",")
}

guardar <- function(plot, nombre, alto) {
  ggsave(
    file.path(dir_out, paste0(nombre, ".pdf")),
    plot, width = 6.5, height = alto, device = cairo_pdf, bg = "white"
  )
  ggsave(
    file.path(dir_out, paste0(nombre, ".png")),
    plot, width = 6.5, height = alto, dpi = 300, bg = "white",
    device = grDevices::png, type = "cairo"
  )
}

exigir <- function(condicion, mensaje) {
  if (!isTRUE(condicion)) stop(mensaje, call. = FALSE)
}

# ------------------------------------------------------------------------------
# Lectura
# ------------------------------------------------------------------------------

posicion <- read_csv(here("data", "analisis", "bcall_anio", "01_posicion_partido.csv"), show_col_types = FALSE)
distancia <- read_csv(here("data", "analisis", "estrategias", "07_2016_2026", "B_distancia_d1.csv"), show_col_types = FALSE)
coef <- read_csv(here("data", "analisis", "regresiones", "ventana_2016_2026", "A_coeficientes_llm.csv"), show_col_types = FALSE)
logit <- read_csv(here("data", "analisis", "chequeos", "05_logit_efectos_marginales.csv"), show_col_types = FALSE)
brecha <- read_csv(here("data", "analisis", "chequeos", "05_brecha_holm.csv"), show_col_types = FALSE)
presupuesto <- read_csv(here("data", "analisis", "difusion", "01_presupuesto_partido_ciclo.csv"), show_col_types = FALSE)
kappa <- read_csv(here("data", "llm", "validacion", "kappa_ejes_reducido_vs_anterior.csv"), show_col_types = FALSE)
indice <- read_csv(here("data", "analisis", "bcall", "indice_llm_partido.csv"), show_col_types = FALSE)
unidades <- read_csv(here("data", "analisis", "bcall", "bcall_unidades.csv"), show_col_types = FALSE)

exigir(all(c("tiempo", "partido", "mediana", "n", "cero", "sd_anio") %in% names(posicion)),
       "01_posicion_partido.csv no trae tiempo, partido, mediana, n, cero o sd_anio.")
exigir(all(c("tiempo", "distancia", "solapamiento", "n_nueva", "n_tradicional") %in% names(distancia)),
       "B_distancia_d1.csv no trae las columnas esperadas.")
exigir(all(c("bloque", "term", "estimate", "std_error", "p_value", "n") %in% names(coef)),
       "A_coeficientes_llm.csv no trae bloque, term, estimate, std_error, p_value o n.")
exigir("efecto marginal" %in% logit$estimador,
       "05_logit_efectos_marginales.csv no trae el estimador 'efecto marginal'.")
exigir("p_holm_cinco_ejes" %in% names(brecha),
       "05_brecha_holm.csv no trae p_holm_cinco_ejes.")

# ------------------------------------------------------------------------------
# Chequeo contra el borrador. Si no cuadra, para: no se ajusta el gráfico.
# ------------------------------------------------------------------------------

no_plr <- presupuesto |> filter(partido == "REP") |> arrange(ciclo) |> pull(pct_no)
exigir(identical(round(no_plr * 100), c(33, 51, 67, 53)),
       paste("PLR % de No no da 33/51/67/53:", paste(round(no_plr * 100), collapse = "/")))

acuerdo_udi <- presupuesto |> filter(partido == "UDI") |> arrange(ciclo) |> pull(pct_igual_rep)
exigir(identical(round(acuerdo_udi * 100), c(82, 76, 73, 91)),
       paste("UDI acuerdo con PLR no da 82/76/73/91:", paste(round(acuerdo_udi * 100), collapse = "/")))

pp <- function(bloque, termino) {
  coef |> filter(.data$bloque == .env$bloque, term == termino) |> pull(estimate) * 100
}
exigir(round(pp("Nueva derecha", "pol_valorica")) == 43 && round(pp("Derecha tradicional", "pol_valorica")) == 19,
       "Conservador no da +43 / +19.")
exigir(round(pp("Nueva derecha", "pol_nacion")) == 16 && round(pp("Derecha tradicional", "pol_nacion")) == 28,
       "Soberanista no da +16 / +28.")

dist_r <- distancia |> arrange(tiempo) |> pull(distancia)
exigir(identical(round(dist_r, 2), c(0.77, 2.99, 2.71, 2.02)),
       paste("Distancia no da 0,77/2,99/2,71/2,02:", paste(round(dist_r, 2), collapse = "/")))

val <- brecha |> filter(term == "pol_valorica")
exigir(nrow(val) == 1 && round(val$estimate * 100) == 17 && round(val$p_value, 3) == 0.024 &&
         round(val$p_holm_cinco_ejes, 3) == 0.118,
       "La brecha valórica no da +17 pp, p = 0,024, Holm = 0,118.")

# ------------------------------------------------------------------------------
# Mapa clásico de B-Call: un punto por diputado, Cámara 2022-2026, eje todas.
# Coordenada del período (ref_d1, ref_d2), no el d1 del año.
# ------------------------------------------------------------------------------

mapa <- unidades |>
  filter(universo == "camara", eje == "todas", periodo == "2022-2026", !is.na(ref_d1), !is.na(ref_d2)) |>
  distinct(diputado_id, .keep_all = TRUE)

exigir(n_distinct(mapa$diputado_id) == nrow(mapa),
       "Hay más de una coordenada de período por diputado.")
exigir(nrow(mapa) > 100, "El mapa de la Cámara 2022-2026 quedó con pocos diputados.")

mediana_udi <- median(mapa$ref_d1[mapa$partido == "UDI"])
mediana_pc <- median(mapa$ref_d1[mapa$partido == "PC"])
exigir(mediana_udi > mediana_pc,
       "En el mapa, la UDI no queda a la derecha del PC. No se grafica.")

n_partido <- count(mapa, partido)
propios <- n_partido |> filter(n >= 3 | partido == "EVOP") |> pull(partido)

mapa_plot <- mapa |>
  mutate(
    partido = if_else(partido %in% propios, partido, "Otro"),
    partido = etiquetar_partido(partido),
    partido = recode(partido, `IND-D` = "IND con la derecha", CAMBIO = "Cambio")
  )

orden_mapa <- mapa_plot |>
  summarise(mediana = median(ref_d1), .by = partido) |>
  arrange(mediana) |>
  pull(partido)

mapa_plot <- mapa_plot |>
  mutate(partido = factor(partido, levels = orden_mapa))

paleta_mapa <- c(
  PC = "#000000",
  PS = "#882255",
  PCS = "#AA4499",
  RD = "#117733",
  PPD = "#CC6677",
  DC = "#984EA3",
  PDG = "#999999",
  LIBERAL = "#DDCC77",
  IND = "#767676",
  Cambio = "#4A5568",
  Otro = "#D0D0D0",
  `IND con la derecha` = paleta[["IND con la derecha"]],
  RN = paleta[["RN"]],
  Evópoli = paleta[["Evópoli"]],
  UDI = paleta[["UDI"]],
  PLR = paleta[["PLR"]]
)
exigir(all(levels(mapa_plot$partido) %in% names(paleta_mapa)),
       paste("Falta color para:", paste(setdiff(levels(mapa_plot$partido), names(paleta_mapa)), collapse = ", ")))

formas_mapa <- setNames(rep(16, length(paleta_mapa)), names(paleta_mapa))
formas_mapa[["PLR"]] <- 17
formas_mapa[["IND con la derecha"]] <- 15

mapa_fig <- ggplot(mapa_plot, aes(ref_d1, ref_d2, colour = partido, shape = partido)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.3) +
  geom_hline(yintercept = median(mapa_plot$ref_d2), linetype = "dashed", colour = "grey45", linewidth = 0.3) +
  geom_point(size = 2, alpha = 0.9) +
  scale_x_continuous("Posición (d1)", labels = num) +
  scale_y_continuous("Variabilidad (d2)", labels = num) +
  scale_colour_manual(values = paleta_mapa, breaks = orden_mapa, drop = FALSE) +
  scale_shape_manual(values = formas_mapa, breaks = orden_mapa, drop = FALSE) +
  guides(
    colour = guide_legend(nrow = 3, byrow = TRUE),
    shape = guide_legend(nrow = 3, byrow = TRUE)
  ) +
  labs(colour = NULL, shape = NULL) +
  tema() +
  theme(legend.text = element_text(size = 8))

write_csv(
  mapa_plot |> transmute(diputado_id, legislator, partido = as.character(partido), ref_d1, ref_d2),
  file.path(dir_out, "fig_mapa_datos.csv")
)
guardar(mapa_fig, "fig_mapa", 5.2)

# ------------------------------------------------------------------------------
# Figura 1
# ------------------------------------------------------------------------------

escala <- posicion |> distinct(tiempo, cero, sd_anio)

individual <- unidades |>
  filter(universo == "camara", eje == "todas", tiempo %in% anios, partido %in% posicion$partido) |>
  inner_join(escala, by = "tiempo") |>
  mutate(d1_sd = (d1 - cero) / sd_anio)

cuartil <- individual |>
  summarise(
    mediana_ind = median(d1_sd),
    q25 = quantile(d1_sd, 0.25),
    q75 = quantile(d1_sd, 0.75),
    n_ind = n(),
    .by = c(tiempo, partido)
  )

fig1a <- posicion |>
  inner_join(cuartil, by = c("tiempo", "partido")) |>
  mutate(partido = etiquetar_partido(partido))

exigir(max(abs(fig1a$mediana - fig1a$mediana_ind)) < 1e-8,
       "La mediana del CSV no coincide con la mediana de los diputados en bcall_unidades.csv.")
exigir(all(fig1a$n == fig1a$n_ind),
       "El n del CSV no coincide con el número de diputados en bcall_unidades.csv.")

omitidos <- fig1a |> filter(n < 3)
fig1a_plot <- fig1a |>
  filter(n >= 3) |>
  arrange(match(tiempo, anios), mediana) |>
  mutate(
    tiempo = factor(tiempo, levels = anios),
    eje_y = factor(paste(partido, tiempo), levels = paste(partido, tiempo))
  )

nota_omitidos <- omitidos |>
  arrange(match(tiempo, anios), partido) |>
  summarise(txt = paste0(partido, " ", tiempo, " (", n, ")", collapse = "; ")) |>
  pull(txt)

pa <- ggplot(fig1a_plot, aes(mediana, eje_y, colour = partido, shape = partido)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.3) +
  geom_segment(aes(x = q25, xend = q75, yend = eje_y), linewidth = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~tiempo, ncol = 5, scales = "free_y") +
  scale_x_continuous(
    "Desviaciones estándar (0 = mediana de la izquierda)",
    labels = num1, breaks = c(0, 1, 2, 3)
  ) +
  scale_y_discrete(labels = function(x) sub(" .*", "", x)) +
  scale_colour_manual(values = paleta, breaks = names(paleta)) +
  scale_shape_manual(values = formas, breaks = names(formas)) +
  coord_cartesian(xlim = c(0, max(3, fig1a_plot$q75) + 0.15)) +
  tema() +
  theme(axis.title.y = element_blank(), legend.title = element_blank())

pb <- distancia |>
  mutate(
    tiempo = factor(tiempo, levels = anios),
    solap_etiq = paste0("solap. ", coma(solapamiento, 2))
  )

pb_plot <- ggplot(pb, aes(tiempo, distancia)) +
  geom_line(group = 1, colour = "grey35", linewidth = 0.4) +
  geom_point(aes(size = n_nueva), colour = paleta[["PLR"]], shape = formas[["PLR"]]) +
  geom_text_repel(
    aes(label = solap_etiq),
    size = 2.7, colour = "grey25", direction = "y", seed = 1,
    min.segment.length = Inf, nudge_y = 0.28, segment.colour = NA
  ) +
  scale_y_continuous("Distancia estandarizada", labels = num, limits = c(0, 3.7)) +
  scale_size_continuous("Diputados de la nueva", range = c(2.2, 4.2), breaks = c(7, 8, 9)) +
  labs(x = "Año legislativo") +
  tema()

fig1 <- pa / pb_plot +
  plot_layout(heights = c(1.55, 1)) +
  plot_annotation(
    tag_levels = "A",
    caption = str_wrap(paste0(
      "A omite partidos con menos de 3 diputados: ", nota_omitidos, ". ",
      "B: parte de la separación puede ser mecánica: 7–9 diputados PLR, pivote PLR."
    ), 108)
  )

fig1_datos <- bind_rows(
  fig1a_plot |>
    transmute(panel = "A", tiempo = as.character(tiempo), partido, mediana, q25, q75, n,
              distancia = NA_real_, solapamiento = NA_real_, n_nueva = NA_integer_, n_tradicional = NA_integer_),
  pb |>
    transmute(panel = "B", tiempo = as.character(tiempo), partido = NA_character_, mediana = NA_real_,
              q25 = NA_real_, q75 = NA_real_, n = NA_integer_, distancia, solapamiento, n_nueva, n_tradicional)
)
write_csv(fig1_datos, file.path(dir_out, "fig1_datos.csv"))
guardar(fig1, "fig1", 5.5)

# ------------------------------------------------------------------------------
# Figura 2
# ------------------------------------------------------------------------------

ejes <- c(
  pol_subsidiariedad = "Provisión privada (provisional)",
  pol_economica = "Pro-mercado",
  pol_orden = "Orden y castigo",
  pol_nacion = "Soberanista",
  pol_valorica = "Conservador"
)
bloques <- c(
  `Nueva derecha` = "Nueva",
  `Derecha tradicional` = "Tradicional",
  `IND con la derecha` = "IND con la derecha",
  Izquierda = "Izquierda"
)

lpm <- coef |>
  filter(term %in% names(ejes)) |>
  mutate(
    eje = factor(unname(ejes[term]), levels = unname(ejes)),
    bloque = unname(bloques[bloque]),
    estimador = "Lineal",
    pp = estimate * 100,
    se_pp = std_error * 100,
    conf_bajo = pp - 1.96 * se_pp,
    conf_alto = pp + 1.96 * se_pp,
    provisional = term == "pol_subsidiariedad"
  )

exigir(all(lpm$bloque %in% names(paleta_bloque)) && !anyNA(lpm$bloque),
       "Hay un bloque en A_coeficientes_llm.csv que no está en la paleta.")
exigir(setequal(logit$estimador, c("LPM", "logit", "efecto marginal")),
       "05_logit_efectos_marginales.csv trae estimadores distintos de LPM, logit y efecto marginal.")
exigir(all(logit$eje %in% c("valorica", "nacion")),
       "El logit trae ejes distintos de valorica y nacion. No se agregan al gráfico.")

marginal <- logit |>
  filter(estimador == "efecto marginal") |>
  mutate(
    term = paste0("pol_", eje),
    eje = factor(unname(ejes[term]), levels = unname(ejes)),
    bloque = unname(bloques[bloque]),
    estimador = "Logit",
    pp = estimate * 100,
    se_pp = std_error * 100,
    conf_bajo = pp - 1.96 * se_pp,
    conf_alto = pp + 1.96 * se_pp
  )

marginal <- marginal |> mutate(forma = formas_huecas[bloque])

# Una sola escala de forma no puede ser llena y hueca a la vez. La posición
# se calcula a mano para que el logit caiga sobre el mismo bloque.
n_bloques <- 4
paso <- 0.7 / n_bloques
orden_bloques <- c("Nueva", "Tradicional", "IND con la derecha", "Izquierda")
desplazar <- function(d) {
  d |>
    mutate(
      fila = as.numeric(eje),
      slot = match(bloque, orden_bloques),
      y = fila + (slot - (n_bloques + 1) / 2) * paso
    )
}

lpm_y <- desplazar(lpm) |> mutate(clave_forma = paste(bloque, "lineal"))
log_y <- desplazar(marginal) |> mutate(clave_forma = paste(bloque, "logit"))

fa <- ggplot() +
  geom_vline(xintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_errorbar(
    data = lpm_y,
    aes(xmin = conf_bajo, xmax = conf_alto, y = y, colour = bloque, linetype = provisional),
    width = 0.12, orientation = "y", linewidth = 0.4
  ) +
  geom_point(data = lpm_y, aes(pp, y, colour = bloque, shape = clave_forma), size = 2.3) +
  geom_point(data = log_y, aes(pp, y, colour = bloque, shape = clave_forma), size = 2.3, stroke = 0.7) +
  scale_y_continuous(breaks = seq_along(levels(lpm$eje)), labels = levels(lpm$eje)) +
  scale_x_continuous("Puntos porcentuales", labels = num1) +
  scale_colour_manual(values = paleta_bloque, breaks = orden_bloques) +
  scale_shape_manual(
    values = c(
      `Nueva lineal` = 17, `Nueva logit` = 2,
      `Tradicional lineal` = 16, `Tradicional logit` = 1,
      `IND con la derecha lineal` = 15, `IND con la derecha logit` = 0,
      `Izquierda lineal` = 18, `Izquierda logit` = 5
    ),
    guide = "none"
  ) +
  scale_linetype_manual(values = c(`TRUE` = "dashed", `FALSE` = "solid"), guide = "none") +
  guides(colour = guide_legend(override.aes = list(shape = c(17, 16, 15, 18)))) +
  labs(y = NULL, colour = NULL, shape = NULL) +
  tema() +
  theme(axis.title.y = element_blank())

brecha_plot <- brecha |>
  filter(term %in% names(ejes)) |>
  mutate(
    eje = factor(unname(ejes[term]), levels = unname(ejes)),
    pp = estimate * 100,
    se_pp = std_error * 100,
    conf_bajo = pp - 1.96 * se_pp,
    conf_alto = pp + 1.96 * se_pp,
    provisional = term == "pol_subsidiariedad",
    holm_etiq = paste0("Holm ", coma(p_holm_cinco_ejes, 3))
  )

fb <- ggplot(brecha_plot, aes(pp, eje)) +
  geom_vline(xintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_errorbar(
    aes(xmin = conf_bajo, xmax = conf_alto, linetype = provisional),
    width = 0.15, orientation = "y", linewidth = 0.4, colour = "grey20"
  ) +
  geom_point(size = 2.3, colour = "grey20") +
  geom_text(aes(x = 36, label = holm_etiq), hjust = 0, size = 2.6, colour = "grey25") +
  scale_x_continuous("Puntos porcentuales", labels = num1, limits = c(-20, 52)) +
  scale_linetype_manual(values = c(`TRUE` = "dashed", `FALSE` = "solid"), guide = "none") +
  labs(y = NULL) +
  tema() +
  theme(axis.title.y = element_blank())

fig2 <- fa / fb +
  plot_layout(heights = c(1.35, 1)) +
  plot_annotation(
    tag_levels = "A",
    caption = str_wrap("A: punto lleno = modelo lineal; punto hueco = efecto marginal del logit, solo en valórico y nación. Provisión privada con intervalo discontinuo: subsidiariedad provisional. B: brecha nueva − tradicional, 531 votaciones; el número es el p de Holm de los cinco ejes.", 108)
  )

fig2_datos <- bind_rows(
  transmute(lpm, panel = "A", bloque, eje = as.character(eje), estimador, pp, se_pp, conf_bajo, conf_alto,
            p_value, holm = NA_real_, n, provisional),
  transmute(marginal, panel = "A", bloque, eje = as.character(eje), estimador, pp, se_pp, conf_bajo, conf_alto,
            p_value, holm = NA_real_, n, provisional = FALSE),
  transmute(brecha_plot, panel = "B", bloque = "Nueva − tradicional", eje = as.character(eje),
            estimador = "Lineal", pp, se_pp, conf_bajo, conf_alto, p_value,
            holm = p_holm_cinco_ejes, n = n_votaciones, provisional)
)
write_csv(fig2_datos, file.path(dir_out, "fig2_datos.csv"))
guardar(fig2, "fig2", 5.5)

# ------------------------------------------------------------------------------
# Figura 3
# ------------------------------------------------------------------------------

n_ciclo <- c(`2023` = 290, `2024` = 517, `2025` = 601, `2026` = 110)
exigir(all(presupuesto |> filter(partido == "REP") |> arrange(ciclo) |> pull(n_votaciones) == unname(n_ciclo)),
       "Las votaciones por ciclo del PLR no son 290, 517, 601 y 110.")

medidas <- c(
  pct_no = "% de No",
  pct_igual_rep = "% igual a la mayoría del PLR",
  pct_si_rebaja = "% de Sí a rebajas simbólicas"
)

fig3_datos <- presupuesto |>
  mutate(partido = etiquetar_partido(partido), ciclo = factor(ciclo, levels = names(n_ciclo))) |>
  pivot_longer(c(pct_no, pct_igual_rep, pct_si_rebaja), names_to = "medida", values_to = "proporcion") |>
  mutate(
    panel = unname(medidas[medida]),
    panel = factor(panel, levels = unname(medidas)),
    valor = proporcion * 100,
    valor = if_else(panel == "% igual a la mayoría del PLR" & partido == "PLR", NA_real_, valor)
  )

fig3 <- ggplot(fig3_datos, aes(ciclo, valor, colour = partido, shape = partido, linetype = partido, group = partido)) +
  geom_line(linewidth = 0.45, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  facet_wrap(~panel, nrow = 1) +
  scale_y_continuous(labels = num1, limits = c(0, 100)) +
  scale_x_discrete(labels = function(x) paste0(x, "\n(", n_ciclo[x], ")")) +
  scale_colour_manual(values = paleta, breaks = c("PLR", "PNL", "UDI", "RN", "Evópoli")) +
  scale_shape_manual(values = formas, breaks = c("PLR", "PNL", "UDI", "RN", "Evópoli")) +
  scale_linetype_manual(values = lineas, breaks = c("PLR", "PNL", "UDI", "RN", "Evópoli")) +
  labs(x = "Ley de Presupuestos", y = NULL, colour = NULL, shape = NULL, linetype = NULL) +
  tema()

fig3 <- fig3 +
  labs(caption = str_wrap("En el acuerdo no se grafica al PLR (queda en 99–100%). En las rebajas, 2026 queda vacío: no hubo. PNL solo en 2026. El número bajo el año es el de votaciones del proyecto; Evópoli vota en 286, 504, 589 y 106.", 108))

write_csv(
  fig3_datos |> transmute(panel = as.character(panel), ciclo = as.character(ciclo), partido, valor, n_votaciones),
  file.path(dir_out, "fig3_datos.csv")
)
guardar(fig3, "fig3", 4.2)

# ------------------------------------------------------------------------------
# Apéndice
# ------------------------------------------------------------------------------

kappa_plot <- kappa |>
  mutate(
    version = recode(version, anterior = "Código anterior", nueva = "Código actual"),
    eje = recode(
      eje,
      economica = "Económico",
      subsidiariedad = "Subsidiariedad (provisional)",
      valorica = "Valórico",
      orden = "Orden",
      nacion = "Nación"
    ),
    eje = factor(eje, levels = c("Subsidiariedad (provisional)", "Económico", "Orden", "Valórico", "Nación"))
  )

fa1 <- ggplot(kappa_plot, aes(kappa_reducido, eje, colour = version, shape = version, group = eje)) +
  geom_vline(xintercept = c(0.4, 0.6), linetype = "dashed", colour = "grey50", linewidth = 0.3) +
  geom_line(colour = "grey70", linewidth = 0.3) +
  geom_point(size = 2.4) +
  scale_x_continuous("Kappa en la escala reducida (+1 / −1 / 0)", labels = num, limits = c(0.3, 0.9)) +
  scale_colour_manual(values = c(`Código anterior` = "#0072B2", `Código actual` = "#D55E00")) +
  scale_shape_manual(values = c(`Código anterior` = 16, `Código actual` = 17)) +
  labs(y = NULL, colour = NULL, shape = NULL) +
  tema()

write_csv(
  kappa_plot |> select(version, eje, n, kappa, kappa_reducido),
  file.path(dir_out, "figA1_datos.csv")
)
guardar(fa1, "figA1", 4)

indice_plot <- indice |>
  filter(
    eje %in% c("valorica", "economica"),
    partido %in% c("REP", "PNL", "UDI", "RN", "EVOP"),
    tiempo %in% anios,
    n_votos >= 10
  ) |>
  mutate(
    partido = etiquetar_partido(partido),
    tiempo = factor(tiempo, levels = anios),
    eje = factor(
      recode(eje, valorica = "Valórico", economica = "Económico"),
      levels = c("Valórico", "Económico")
    )
  )

fa2 <- ggplot(indice_plot, aes(tiempo, indice, colour = partido, shape = partido, size = n_votos)) +
  geom_point() +
  facet_wrap(~eje, ncol = 1) +
  scale_y_continuous("Índice hacia el polo de la derecha", labels = num, limits = c(0, 1)) +
  scale_colour_manual(values = paleta, breaks = c("PLR", "PNL", "UDI", "RN", "Evópoli")) +
  scale_shape_manual(values = formas, breaks = c("PLR", "PNL", "UDI", "RN", "Evópoli")) +
  scale_size_continuous("Votos", range = c(1.5, 5)) +
  labs(
    x = "Año legislativo",
    caption = str_wrap("No se grafican celdas con menos de 10 votos. Valórico 2024-25: composición; los boletines no se repiten respecto de 2022-23.", 108)
  ) +
  tema() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write_csv(
  indice_plot |> transmute(eje = as.character(eje), tiempo = as.character(tiempo), partido, diputados, n_votos, indice),
  file.path(dir_out, "figA2_datos.csv")
)
guardar(fa2, "figA2", 5.5)

cat("\nEscrito en", dir_out, "\n")
cat(paste(list.files(dir_out), collapse = "\n"), "\n")
