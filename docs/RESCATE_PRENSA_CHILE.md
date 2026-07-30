# Rescate prensa-chile → neo_gremialismo

Fecha: 2026-07-29

Fuente: `/Users/matdknu/Dropbox/social-data-science/prensa-chile`  
Destino: `data/raw/prensa/` + corpus `total/` / `derecha/`

## Decisión

**No borrar** `prensa-chile`. Hay material recuperable (~19k artículos 2025 con cuerpo) y scrapers útiles. La condición “si no hay NADA, borrar” no se cumple.

## Qué se rescató

Importador: [`scrapers/prensa/import_prensa_chile.py`](../../scrapers/prensa/import_prensa_chile.py)

| Fuente | Artículos 2025 importados | Archivo |
|---|---:|---|
| CNN | 7.229 | `data/raw/prensa/cnn/cnn_prensa_chile_2025.parquet` |
| Meganoticias | 6.006 | `…/meganoticias/meganoticias_prensa_chile_2025.parquet` |
| Radio UChile | 4.945 | `…/radiouchile/radiouchile_prensa_chile_2025.parquet` |
| El Dínamo | 548 | `…/eldinamo/eldinamo_prensa_chile_2025.parquet` |
| 24 Horas | 375 | `…/24horas/24horas_prensa_chile_2025.parquet` |
| CIPER | 193 | `…/ciper/ciper_prensa_chile_2025.parquet` |
| CHV Noticias | 83 | `…/chvnoticias/chvnoticias_prensa_chile_2025.parquet` |
| El Mostrador | 37 | `…/elmostrador/elmostrador_prensa_chile_2025.parquet` |
| El Desconcierto | 23 | `…/eldesconcierto/eldesconcierto_prensa_chile_2025.parquet` |
| **Total** | **19.439** | |

Corpus tras reconstrucción (`run.py --corpus-only`):

- `prensa_total`: ~55k URLs únicas (0 duplicados)
- `prensa_derecha`: ~19k (~34%)
- Cobertura 2025 en total: ~19,5k artículos
- Serie: Fondecyt (hist.) + rescate 2025 + scrapers 2026

## Qué se descartó (no importar)

| Artefacto | Motivo | Tamaño aprox. |
|---|---|---|
| `data/maestra/prensa_chile_maestra*.csv/parquet` | Columnas desplazadas (autores EMOL como `medio`), fechas inválidas | ~9 GB (varias copias) |
| `bbdd_backup/` | Copia casi íntegra de `data/procesados` | ~8,6 GB |
| RDS (`emol_*.rds`, `cnn.rds`, …) | Duplican CSV/parquet; no hacen falta tras el import | ~6+ GB |
| `.venv/`, `.RData*`, `.git` | Entorno / sesión, no datos de análisis | ~1,7 GB |

## Scrapers portados

Probados en vivo (listado + fecha + cuerpo) y registrados en `run.py`:

| Medio | Estado | Archivo |
|---|---|---|
| CIPER | OK | `fuentes/ciper.py` |
| Radio UChile | OK (69 arts / 3 días) | `fuentes/radiouchile.py` |
| Cooperativa | OK (24 arts) | `fuentes/cooperativa.py` |
| El Desconcierto | OK (33 arts) | `fuentes/eldesconcierto.py` |
| 24 Horas | **No portado** — feed JS / Playwright | — |
| CHV Noticias | **No portado** — feed JS / Playwright | — |

## Limpieza propuesta (requiere confirmación explícita)

No se ha borrado nada en `prensa-chile`. Si se confirma después de validar el análisis:

1. **Seguro / alto impacto**: borrar `bbdd_backup/` (~8,6 GB) — es duplicado de `data/`.
2. **Seguro**: borrar `.venv/` y `.RData*` si no se usa esa sesión R.
3. **Después de validar import**: borrar CSV maestros duplicados (`prensa_chile_maestra.csv` ×3) y RDS si los parquet/CSV por medio ya están en neo_gremialismo.
4. **No borrar aún** la carpeta completa ni los CSV por medio fuente hasta tener un análisis 2021–2026 estable.

Comando sugerido (solo tras confirmación):

```bash
# Ejemplo — NO ejecutar sin OK explícito
rm -rf /Users/matdknu/Dropbox/social-data-science/prensa-chile/bbdd_backup
```

## Cómo reproducir

```bash
python3 scrapers/prensa/import_prensa_chile.py
python3 scrapers/prensa/run.py --corpus-only
python3 scrapers/prensa/run.py -s ciper,radiouchile,cooperativa,eldesconcierto --since 2026-07-01
```
