# Neogremialismo

Investigación Fondecyt sobre la derecha chilena en el gobierno de Kast: cómo votan **REP, UDI, RN y PNL**, y qué repertorios circulan en prensa y discursos.

![Kast](kast-flag.jpg)

## En GitHub

- `analysis/` — scripts R
- `outputs/imagenes/` — figuras
- `docs/` — documentación del proyecto ([inventario carpetas y export](docs/ESTRUCTURA_EXPORT.md))

Scrapers, `data/raw/` y `data/processed/` son **locales** (gitignore).

## Estructura

```
analysis/
  prensa/          01…06  (01 canónico)
  parlamento/      01…09  (01–02 canónicos)
  discursos_presidenciales/
  puente/          01_agenda
  qualmer/         sample codificación
  run_todo.R       orquestador canónico
outputs/imagenes/
docs/PROYECTO.qmd · informe_analisis.qmd · qualmer_propuesta.qmd

# solo local
scrapers/          → prensa · congreso · discursos
data/raw/          → congreso.db · prensa/ · discursos/
data/processed/canon/
```

```bash
make analisis   # ETL canónico + run_todo.R
```

