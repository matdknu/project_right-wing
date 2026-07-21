# Neogremialismo — votaciones, BCN y análisis

Programa Fondecyt sobre **neogremialismo** en Chile. Este repo separa recolección (local) de análisis (compartible).

## Estructura (qué se comparte)

```
scrapers/          ← NO compartir. Descarga APIs → congreso.db
data/
  raw/congreso.db  ← fuente única (local, no git)
  scripts/         ← SÍ compartir. Tablas derivadas desde la BD
analysis/          ← SÍ compartir. Scripts R que leen la BD → figuras
outputs/imagenes/  ← figuras (PNG)
docs/              ← hipótesis, diccionario, fichas
```

| Capa | Rol | Compartir |
|------|-----|-----------|
| `scrapers/` | Cámara, Senado, BCN → `congreso.db` | No |
| `data/scripts/` | Matching / agregados **en la BD** | Sí |
| `analysis/` | Hipótesis H3/H4 → gráficos | Sí |

**Regla:** no exportar `votos_nominales_*.csv` ni dumps por boletín. Todo el análisis lee `data/raw/congreso.db`. Solo se guardan objetos nuevos si se reutilizan (tablas en la BD) o figuras finales.

## Flujo de trabajo

```bash
# 1) (local) Scrapers → BD
python3 scrapers/congreso/camara_votaciones.py --year 2026 --skip-existing
python3 scrapers/congreso/bcn_leychile.py --from-congreso --skip-existing
python3 scrapers/congreso/match_bcn_diputados.py --proyectos   # autores/ministerios vía API

# 2) (compartible) Tablas derivadas
python3 data/scripts/build_derived.py

# 3) (compartible) Análisis
Rscript analysis/agencia_cohesion.R
Rscript analysis/red_ecos_partidos.R
```

## Análisis principal: agencia vs cohesión (H3/H4)

```bash
python3 data/scripts/build_derived.py
Rscript analysis/agencia_cohesion.R
```

Figura: `outputs/imagenes/agencia_vs_cohesion.png`

- **Agencia:** quién pide votación separada / firma mociones  
- **Cohesión:** Rice en indicaciones + acuerdo posicional REP–PNL en mensajes `-05`

## Cómo compartir el proyecto

Empaquetar **sin** `scrapers/` ni `.env`:

```
analysis/  data/scripts/  docs/  outputs/imagenes/  README.md
+ snapshot de congreso.db (o instrucción para regenerarlo)
```

Quien recibe regenera figuras con `build_derived.py` + los Rscripts; no necesita las APIs.

## Hipótesis

Ver [`docs/HIPOTESIS.md`](docs/HIPOTESIS.md).
