# ==============================================================================
# 1.subset.R
#
# Create manually inspectable subsets of the final analytical databases.
#
# Sampling unit:
#   - indication voting event (`votacion_id`)
#   - the same IDs are used across every exported database
#   - all variables and all associated deputy-vote rows are retained
# ==============================================================================

pacman::p_load(
  arrow,
  dplyr,
  openxlsx,
  here
)

# ------------------------------------------------------------------------------
# 1. Settings
# ------------------------------------------------------------------------------

ids_per_year <- 50L

sampling_seed <- 20260831

subset_folder <- here::here(
  "subset", "data"
)

dir.create(
  subset_folder,
  showWarnings = FALSE
)

# Change these paths only if the final databases are stored elsewhere.

votaciones_file <- here::here(
  "data",
  "origen",
  "votaciones_analiticas.parquet"
)

indicaciones_file <- here::here(
  "data",
  "origen",
  "indicaciones_analiticas.parquet"
)

votos_file <- here::here(
  "data",
  "origen",
  "votos_analiticos.parquet"
)

# ------------------------------------------------------------------------------
# 2. Read the complete databases
# ------------------------------------------------------------------------------

votaciones_analiticas <- read_parquet(
  votaciones_file
)

indicaciones_analiticas <- read_parquet(
  indicaciones_file
)

votos_analiticos <- read_parquet(
  votos_file
)

# ------------------------------------------------------------------------------
# 3. Select indication voting IDs
# ------------------------------------------------------------------------------

set.seed(
  sampling_seed
)

ids_sub <- indicaciones_analiticas |>
  distinct(
    votacion_id,
    anio
  ) |>
  arrange(
    anio,
    votacion_id
  ) |>
  group_by(
    anio
  ) |>
  slice_sample(
    n = ids_per_year
  ) |>
  ungroup() |>
  arrange(
    anio,
    votacion_id
  )

# ------------------------------------------------------------------------------
# 4. Recreate every database using the same voting IDs
# ------------------------------------------------------------------------------

votaciones_analiticas_sub <- votaciones_analiticas |>
  semi_join(
    ids_sub,
    by = "votacion_id"
  ) |>
  arrange(
    fecha,
    votacion_id
  )

indicaciones_analiticas_sub <- indicaciones_analiticas |>
  semi_join(
    ids_sub,
    by = "votacion_id"
  ) |>
  arrange(
    fecha,
    votacion_id
  )

votos_analiticos_sub <- votos_analiticos |>
  semi_join(
    ids_sub,
    by = "votacion_id"
  ) |>
  arrange(
    fecha,
    votacion_id,
    diputado_id
  )

# ------------------------------------------------------------------------------
# 5. Verify that the selected IDs match
# ------------------------------------------------------------------------------

id_verification <- ids_sub |>
  mutate(
    in_votaciones = votacion_id %in%
      votaciones_analiticas_sub$votacion_id,
    
    in_indicaciones = votacion_id %in%
      indicaciones_analiticas_sub$votacion_id,
    
    in_votos = votacion_id %in%
      votos_analiticos_sub$votacion_id
  ) |>
  arrange(
    anio,
    votacion_id
  )

verification_summary <- id_verification |>
  summarise(
    selected_ids = n(),
    
    ids_in_votaciones = sum(
      in_votaciones
    ),
    
    ids_in_indicaciones = sum(
      in_indicaciones
    ),
    
    ids_in_votos = sum(
      in_votos
    ),
    
    ids_present_in_every_database = sum(
      in_votaciones &
        in_indicaciones &
        in_votos
    )
  )

cat(
  "\n============================================================\n"
)

cat(
  "SUBSET VERIFICATION\n"
)

cat(
  "============================================================\n"
)

print(
  verification_summary,
  width = Inf
)

cat(
  "\nSelected IDs by year:\n"
)

ids_sub |>
  count(
    anio,
    name = "selected_ids"
  ) |>
  arrange(
    anio
  ) |>
  print(
    n = Inf,
    width = Inf
  )

cat(
  "\nRows in each subset:\n"
)

tibble(
  database = c(
    "votaciones_analiticas_sub",
    "indicaciones_analiticas_sub",
    "votos_analiticos_sub"
  ),
  
  rows = c(
    nrow(votaciones_analiticas_sub),
    nrow(indicaciones_analiticas_sub),
    nrow(votos_analiticos_sub)
  ),
  
  distinct_votacion_ids = c(
    n_distinct(
      votaciones_analiticas_sub$votacion_id
    ),
    
    n_distinct(
      indicaciones_analiticas_sub$votacion_id
    ),
    
    n_distinct(
      votos_analiticos_sub$votacion_id
    )
  )
) |>
  print(
    n = Inf,
    width = Inf
  )

# ------------------------------------------------------------------------------
# 6. Export the common ID list
# ------------------------------------------------------------------------------

write.xlsx(
  ids_sub,
  file = file.path(
    subset_folder,
    "ids_sub.xlsx"
  ),
  sheetName = "ids_sub",
  overwrite = TRUE
)

# ------------------------------------------------------------------------------
# 7. Export each complete subset
# ------------------------------------------------------------------------------

write.xlsx(
  votaciones_analiticas_sub,
  file = file.path(
    subset_folder,
    "votaciones_analiticas_sub.xlsx"
  ),
  sheetName = "votaciones",
  overwrite = TRUE
)

write.xlsx(
  indicaciones_analiticas_sub,
  file = file.path(
    subset_folder,
    "indicaciones_analiticas_sub.xlsx"
  ),
  sheetName = "indicaciones",
  overwrite = TRUE
)

write.xlsx(
  votos_analiticos_sub,
  file = file.path(
    subset_folder,
    "votos_analiticos_sub.xlsx"
  ),
  sheetName = "votos",
  overwrite = TRUE
)

write.xlsx(
  id_verification,
  file = file.path(
    subset_folder,
    "id_verification_sub.xlsx"
  ),
  sheetName = "verification",
  overwrite = TRUE
)

cat(
  "\n============================================================\n"
)

cat(
  "SUBSET FILES CREATED IN:\n",
  subset_folder,
  "\n",
  sep = ""
)

cat(
  "============================================================\n"
)