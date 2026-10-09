# Votaciones de la Cámara y las dos derechas

Indicaciones de la Cámara de Diputados, 2010–2026. El paper está en esta carpeta: `articulo.qmd` y `articulo.html`.

Todo se corre **desde esta carpeta**. Las claves van en `~/.Renviron` (`OPENAI_API_KEY`, `DEEPSEEK_API_KEY`), no en el repositorio.

## Cómo repetirlo

```bash
Rscript scripts/install.R          # una vez
Rscript scripts/run_datos.R        # solo si falta data/origen/
Rscript scripts/run_analisis.R     # LLM, B-Call y regresiones
Rscript scripts/pipeline/11_figuras_paper.R
quarto render articulo.qmd
```

`run_analisis.R` no vuelve a pagar la codificación si `data/llm/cache/` ya tiene la versión `04fc6c14`.

## Dónde está cada cosa

```
articulo.qmd          el paper
figuras/              figuras 1, 2, 3 y el apéndice

data/raw/             descarga de la Cámara, sin editar
data/origen/          votos e indicaciones ya limpios
data/llm/             lo que el modelo escribió sobre cada texto
data/analisis/        B-Call, regresiones, chequeos y difusión

scripts/pipeline/     00 a 11, en orden
scripts/auxiliares/   planillas y reanudación del batch; no hacen falta para el paper
presentation/         la presentación de B-Call
archive/              lo que ya no corre
```

| Carpeta | Qué es | Lo produce |
|---|---|---|
| `data/raw/` | XML y parquet de la API | `0_query.R` |
| `data/origen/` | `votos_analiticos.parquet`, `indicaciones_analiticas.parquet` | `0_clean.R` |
| `data/llm/resultados/` | `indicaciones_codificadas.csv` | `03` |
| `data/llm/cache/` | respuestas ya pagadas; no se borran | `03` |
| `data/analisis/bcall/` | posiciones e índice LLM | `04` |
| `data/analisis/regresiones/` | qué mueve el Sí | `05` |
| `data/analisis/estrategias/` | repertorio y brecha, ventana 2016–2026 | `06` y `07` |
| `data/analisis/chequeos/` | los cinco chequeos | `08` |
| `data/analisis/difusion/` | presupuesto y texto fijo | `09` |
| `data/analisis/bcall_anio/` | escala de la Cámara por año | `10` |
| `figuras/` | las del paper | `11` |

## Scripts, en orden

| Script | Hace |
|---|---|
| `scripts/pipeline/0_query.R` | Descarga la Cámara a `data/raw/` |
| `scripts/pipeline/0_clean.R` | Deja los parquet en `data/origen/` |
| `scripts/pipeline/03_indicaciones_llm.R` | Codifica el texto. Lee la caché si ya existe |
| `scripts/pipeline/04_indicaciones_bcall.R` | B-Call de la Cámara y de la derecha |
| `scripts/pipeline/05_regresiones_votos.R` | Modelo lineal del Sí |
| `scripts/pipeline/06_estrategias_voto.R` | Configuraciones de voto |
| `scripts/pipeline/07_derechas_estrategias.R` | Repertorio, brecha y difusión, marzo 2016 a marzo 2026 |
| `scripts/pipeline/08_chequeos_hallazgos.R` | Chequeos antes de interpretar |
| `scripts/pipeline/09_difusion.R` | Presupuesto y brecha con el texto fijo |
| `scripts/pipeline/10_bcall_anio.R` | Posición anual en desviaciones de la Cámara |
| `scripts/pipeline/11_figuras_paper.R` | Escribe `figuras/` |

`1_indications.R` y `1_subset.R` son exploraciones. No entran en la cadena del paper.

Paquetes: `tidyverse`, `arrow`, `ellmer`, `bcall`, `fixest`, `here`, `pacman`. Los declara `R/dependencies.R`.
