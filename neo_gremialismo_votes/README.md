# Votaciones de la Cámara y las dos derechas

Indicaciones de la Cámara de Diputados, 2010–2026. El paper está en esta carpeta: `articulo.qmd` y `articulo.html`.

Todo se corre **desde esta carpeta**. Las claves van en `~/.Renviron` (`OPENAI_API_KEY`, `DEEPSEEK_API_KEY`), no en el repositorio.

## Cómo repetirlo

```bash
Rscript scripts/install.R
Rscript scripts/pipeline/00_datos.R      # solo si falta data/origen/
Rscript scripts/pipeline/01_llm.R        # lee la caché si ya está
Rscript scripts/pipeline/02_bcall.R
Rscript scripts/pipeline/03_posicion.R
Rscript scripts/pipeline/04_contenido.R
Rscript scripts/pipeline/05_oposicion.R
Rscript scripts/pipeline/06_validacion.R
Rscript scripts/pipeline/07_figuras.R
python3 scripts/pipeline/08_articulo.py
quarto render articulo.qmd
```

`01_llm.R` no vuelve a pagar la codificación si `data/llm/cache/` ya tiene las versiones `04fc6c14` (cinco ejes) y `59195313` (termómetro).

Un paso suelto: `SOLO=24,34 Rscript scripts/pipeline/03_posicion.R`.

## Dónde está cada cosa

```
articulo.qmd          el paper
figuras/              figuras 1, 2, 3 y el apéndice

data/raw/             descarga de la Cámara, sin editar
data/origen/          votos e indicaciones ya limpios
data/llm/             lo que el modelo escribió sobre cada texto
data/analisis/        B-Call, regresiones, chequeos y la versión de revista

scripts/pipeline/     00 a 08, en orden
scripts/pipeline/pasos/  cada estimación, numerada como antes
scripts/auxiliares/   planillas y reanudación del batch
```

| Carpeta | Qué es | Lo produce |
|---|---|---|
| `data/raw/` | XML y parquet de la API | `00_datos` (paso `0_query`) |
| `data/origen/` | `votos_analiticos.parquet`, `indicaciones_analiticas.parquet` | `00_datos` (paso `0_clean`) |
| `data/llm/resultados/` | `indicaciones_codificadas.csv` | `01_llm` (paso `03`) |
| `data/llm/cache/` | respuestas ya pagadas; no se borran | `01_llm` |
| `data/analisis/bcall/` | posiciones e índice LLM | `02_bcall` (paso `04`) |
| `data/analisis/revista/` | universo, iniciativa, pivotalidad, W-NOMINATE | `02` a `06` |
| `figuras/` | las del paper | `07_figuras` |

## Scripts, en orden

| Script | Hace |
|---|---|
| `00_datos.R` | Descarga y limpia la Cámara |
| `01_llm.R` | Codifica el texto (cinco ejes y termómetro). Lee la caché si ya existe |
| `02_bcall.R` | B-Call de indicaciones y de todas las votaciones, izquierda corregida |
| `03_posicion.R` | Cohesión, W-NOMINATE, Fábrega, Rice, DiD, umbral 10 % |
| `04_contenido.R` | LPM, tópicos, termómetro, modelo conjunto, techo en nación |
| `05_oposicion.R` | Iniciativa, pivotalidad, presupuesto, salud 2025 |
| `06_validacion.R` | Permutación, planillas humanas, apéndice E |
| `07_figuras.R` | Escribe `figuras/` |
| `08_articulo.py` | Rellena `articulo.qmd` con las cifras |

Los 44 archivos de `pasos/` son las estimaciones. Los 9 de arriba son lo que se corre. `1_indications.R` y `1_subset.R` son exploraciones y no entran.

Paquetes: `tidyverse`, `arrow`, `ellmer`, `bcall`, `fixest`, `here`, `pacman`. Los declara `R/dependencies.R`.
