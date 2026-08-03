# analysis/

Pipeline de análisis. Una dirección: `data/processed/canon/` → scripts → `outputs/imagenes/`.

## Orden canónico (`make analisis`)

```bash
Rscript analysis/run_todo.R
```

| # | Script | Dominio |
|---|--------|---------|
| 1 | `prensa/01_descriptivo.R` | Prensa Kast (total) |
| 2 | `discursos_presidenciales/01_repertorios.R` | Discursos A–D |
| 3 | `parlamento/01_cohesion_bloque.R` | Rice / REP–PNL |
| 4 | `parlamento/02_proyectos_watchlist.R` | Proyectos `-05` |
| 5 | `puente/01_agenda.R` | Eco mediático ↔ legislativo |

## Carpetas

| Carpeta | Contenido |
|---------|-----------|
| [`prensa/`](prensa/) | Medios 2026 + baseline Fondecyt |
| [`parlamento/`](parlamento/) | Votaciones, redes, PDL |
| [`discursos_presidenciales/`](discursos_presidenciales/) | Discursos Presidencia |
| [`puente/`](puente/) | Cruce prensa–votos–eventos |
| [`qualmer/`](qualmer/) | Sample codificación cualitativa |

Shared: `_helpers.R`, `_diccionario.R` (códigos A–D).
