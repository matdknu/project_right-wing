# Neogremialismo

Investigación Fondecyt sobre la derecha chilena en el gobierno de Kast: cómo votan **REP, UDI, RN y PNL**, y qué repertorios circulan en prensa y discursos.

![Léxico discursos Kast](outputs/imagenes/discursos_kast_top_palabras.png)

Documentación: [`docs/PROYECTO.qmd`](docs/PROYECTO.qmd).

## Estructura

```
scrapers/
  prensa/      → core/ + fuentes/ (emol, biobio, t13, clinic, …)
  congreso/    → Cámara, Senado, BCN
  discursos/   → Presidencia
data/raw/      → congreso.db · prensa/<fuente>/ · discursos/
data/processed/prensa/prensa_unificada.parquet
analysis/
  prensa/ · parlamento/ · discursos_presidenciales/
outputs/imagenes/
docs/PROYECTO.qmd
```

## Correr

```bash
python3 data/scripts/unify_prensa.py
python3 data/scripts/build_derived.py

Rscript analysis/prensa/prensa_tendencias.R
Rscript analysis/discursos_presidenciales/descriptivo.R
Rscript analysis/parlamento/agencia_cohesion.R
```
