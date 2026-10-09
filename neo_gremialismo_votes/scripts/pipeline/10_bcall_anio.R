# ==============================================================================
# 10_bcall_anio.R
#
# No toca 03 a 09. No reestima el B-Call.
#
# Toma el B-Call de la Cámara, eje todas, un modelo por año legislativo
# (llm/analisis/bcall_unidades.csv). Dentro de cada año el d1 solo está
# identificado hasta una escala. Se expresa en desviaciones estándar de los
# diputados de ese año, con el cero en la mediana de la izquierda
# (PC, PS, FA, PPD). Positivo = derecha, como ya dejó el 04.
#
# La serie de la pregunta es la mediana de UDI menos la de REP.
# Chequeo: en 2022-2026 esa brecha, en valor absoluto, tiene que moverse
# en el mismo sentido que la distancia estandarizada del universo derecha
# (llm/estrategias/07_2016_2026/B_distancia_d1.csv).
#
# Salidas: llm/bcall_anio/
# ==============================================================================

pacman::p_load(tidyverse, here)

anios <- c(
  "2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
  "2021-22", "2022-23", "2023-24", "2024-25", "2025-26"
)
izquierda <- c("PC", "PS", "FA", "PPD")
partidos <- c("REP", "PNL", "UDI", "RN", "EVOP")

dir_out <- here("data", "analisis", "bcall_anio")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

unidades <- read_csv(here("data", "analisis", "bcall", "bcall_unidades.csv"), show_col_types = FALSE) |>
  filter(universo == "camara", eje == "todas", tiempo %in% anios)

ancla <- unidades |>
  filter(partido %in% izquierda) |>
  summarise(
    cero = median(d1),
    n_izquierda = n(),
    partidos_izquierda = paste(sort(unique(partido)), collapse = " "),
    .by = tiempo
  )

escala <- unidades |>
  summarise(sd_anio = sd(d1), n_camara = n(), .by = tiempo)

posicion <- unidades |>
  filter(partido %in% partidos) |>
  inner_join(ancla, by = "tiempo") |>
  inner_join(escala, by = "tiempo") |>
  mutate(d1_sd = (d1 - cero) / sd_anio) |>
  summarise(
    mediana = median(d1_sd),
    n = n(),
    .by = c(tiempo, partido, cero, sd_anio, n_izquierda, n_camara, partidos_izquierda)
  ) |>
  arrange(match(tiempo, anios), match(partido, partidos))

brecha <- posicion |>
  select(tiempo, partido, mediana) |>
  pivot_wider(names_from = partido, values_from = mediana) |>
  mutate(udi_menos_rep = UDI - REP, distancia = abs(udi_menos_rep))

distancia_derecha <- read_csv(
  here("data", "analisis", "estrategias", "07_2016_2026", "B_distancia_d1.csv"),
  show_col_types = FALSE
) |>
  rename(tiempo = tiempo, distancia_tabla2 = distancia)

chequeo <- brecha |>
  left_join(distancia_derecha, by = "tiempo") |>
  filter(tiempo >= "2022-23") |>
  mutate(
    sube_esta = distancia > lag(distancia),
    sube_tabla2 = distancia_tabla2 > lag(distancia_tabla2),
    mismo_sentido = sube_esta == sube_tabla2
  )

write_csv(posicion, file.path(dir_out, "01_posicion_partido.csv"))
write_csv(chequeo, file.path(dir_out, "01_chequeo_tabla2.csv"))

cat("\nPosición, en sd de la Cámara, cero = mediana PC PS FA PPD\n\n")
print(posicion |> select(tiempo, partido, mediana, n, n_izquierda, partidos_izquierda), n = 50)
cat("\nBrecha UDI − REP y distancia de la tabla 2\n\n")
print(chequeo |> select(tiempo, UDI, REP, udi_menos_rep, distancia, distancia_tabla2, mismo_sentido))
cat("\nEscrito en", dir_out, "\n")
