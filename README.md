# Neogremialismo

Investigación Fondecyt sobre la derecha chilena en el gobierno de Kast: cómo votan **REP, UDI, RN y PNL**, y qué repertorios circulan en prensa y discursos oficiales.

![Léxico más frecuente en discursos de Kast (Prensa Presidencia)](outputs/imagenes/discursos_kast_top_palabras.png)

**Discursos Presidencia:** léxico de gestión territorial y orden público (*personas*, *trabajo*, *estado*, *región*, *seguridad*, *educación*, *salud*, *carabineros*) más que de guerra cultural.

## Datos

| Capa | Ruta | Rol |
|------|------|-----|
| Raw | `data/raw/` | Scrapers → `congreso.db`, `prensa/<fuente>/`, discursos |
| Procesada (única de prensa) | `data/processed/prensa/prensa_unificada.parquet` | Se nutre de raw con `unify_prensa.py` |
| Figuras | `outputs/imagenes/` | Solo plots; sin CSV descriptivos intermedios |


