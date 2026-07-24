# Neogremialismo

Investigación Fondecyt sobre la derecha chilena en el gobierno de Kast: cómo votan **REP, UDI, RN y PNL**, y qué repertorios circulan en prensa y discursos.


![Kast](kast-flag.jpg)



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

