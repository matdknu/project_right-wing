# Neogremialismo

Investigación Fondecyt sobre la derecha chilena en el gobierno de Kast: cómo votan **REP, UDI, RN y PNL**, y qué repertorios circulan en prensa y discursos.

![Kast](kast-flag.jpg)

## Documentación

- **Presentación de datos** — [Link](https://matdknu.github.io/project_right-wing/presentacion_hallazgos.html)
- **[`docs/REPOSITORIO.md`](docs/REPOSITORIO.md)** — inventario completo: qué hay, qué hace, hipótesis H1–H4
- [`docs/PROYECTO.qmd`](docs/PROYECTO.qmd) — método, diccionario A–D, contrastación
- [`docs/ESTRUCTURA_EXPORT.md`](docs/ESTRUCTURA_EXPORT.md) — qué va a GitHub vs local

## En GitHub

- `analysis/` — scripts R
- `outputs/imagenes/` — figuras
- `docs/` — documentación e informes

Scrapers, `data/raw/` y `data/processed/` son **locales** (gitignore).

## Paquete para analizar (sin scrapers)

```
exports/neo_gremialismo_analisis_hipotesis.zip   # ~370 MB; WinRAR/Explorer lo abren
```

Incluye `analysis/`, `data` canónico + `congreso.db` + discursos, `docs/`, figuras.  
Ver `LEEME_PAQUETE.md` dentro del ZIP.

## Estructura

```
analysis/          prensa · parlamento · discursos · puente · qualmer
outputs/imagenes/
docs/              REPOSITORIO · PROYECTO · informe · Qualmer

# solo local
scrapers/          prensa · congreso · discursos
data/raw/          congreso.db · prensa/ · discursos/
data/processed/canon/
```

```bash
make analisis        # ETL canónico + run_todo.R
make presentacion    # slides hallazgos + hipótesis
```

