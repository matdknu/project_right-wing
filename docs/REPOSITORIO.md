# Documentación completa del repositorio neo_gremialismo

Inventario de **todo** el proyecto: qué hay en cada carpeta, qué hace, y qué se necesita para analizar e hipótesis H1–H4.

---

## 1. Qué es este proyecto

Investigación **Fondecyt** sobre la derecha chilena en el gobierno de Kast (2026+):

- Cómo votan **REP, UDI, RN y PNL** en el Congreso.
- Qué **repertorios discursivos A–D** circulan en **prensa** y **discursos presidenciales**.
- Cómo se articulan (o no) mercado + orden/valores (**neogremialismo**) frente a derecha radical y libertarismo.

Flujo de datos (una sola dirección):

```
scrapers/  →  data/raw/  →  data/scripts/  →  data/processed/  →  analysis/  →  outputs/
(recolectar)   (crudo)       (ETL)              (tablas listos)     (R)         (figuras)
```

---

## 2. Mapa del repositorio

```
neo_gremialismo/
├── README.md                 # Portada corta
├── Makefile                  # make analisis | make informe
├── neo_gremialismo.Rproj     # Proyecto RStudio
├── kast-flag.jpg
├── .gitignore                # data/, scrapers/, secrets fuera de GitHub
│
├── analysis/                 # ★ Scripts R (GitHub)
├── data/                     # ★ Datos + ETL (LOCAL; en paquete de análisis)
├── docs/                     # ★ Documentación e informes (GitHub)
├── outputs/
│   ├── imagenes/             # ★ Figuras (GitHub)
│   └── volatilidad/          # CSV intermedios (local)
├── scrapers/                 # Recolección Python (LOCAL; NO va al paquete)
├── config/                   # settings.py con API keys (local)
├── bibliography/             # PDFs de referencia
└── neogremialismo-data/      # Stub companion de datos
```

| Capa | ¿GitHub? | ¿Paquete análisis? | Rol |
|------|----------|--------------------|-----|
| `analysis/` | Sí | **Sí** | Hipótesis + figuras |
| `data/processed/canon/` | No | **Sí** | Tablas canónicas |
| `data/raw/congreso.db` | No | **Sí** | Votaciones |
| `data/raw/discursos/` | No | **Sí** | Discursos |
| `data/scripts/` | No | **Sí** | Rebuild ETL |
| `docs/` | Sí | **Sí** | Método + H1–H4 |
| `outputs/imagenes/` | Sí | **Sí** | Figuras ya generadas |
| `scrapers/` | No | **No** | Solo recolección |
| `data/raw/prensa/<outlet>/` | No | **No** | Lotes crudos (GB) |
| `outputs/volatilidad/` | No | **No** | Dumps intermedios |

---

## 3. Hipótesis (núcleo científico)

Definidas en `docs/PROYECTO.qmd` y contrastadas vía `analysis/` + diccionario A–D.

| Código | Hipótesis | Idea | Se refuta si… |
|--------|-----------|------|----------------|
| **H1** | Racionalidad normativa del neogremialismo | Mercado + orden/familia/valores (no solo eficiencia) | Discurso REP ≈ LyD técnico o ≈ PNL libertario |
| **H2** | Repertorios de derecha radical | Núcleo gremialista + securitización / migración / guerra cultural / anti-élite | Lo radical reemplaza al gremialista, o solo aparece en redes |
| **H3** | Republicanos vs PNL | Distinción doctrinaria visible en votos (Rice, convergencia) | Votan siempre igual |
| **H4** | Selección liberal-individual | Gobierno privilegia subsidiariedad-como-abstención; deja cuerpos intermedios | Hay iniciativas comunitaristas sistemáticas |

**Regla clave:** el indicador de neogremialismo no es la frecuencia aislada de una palabra, sino la **co-ocurrencia** (p. ej. A5+A3) en la misma unidad.

Resultados agregados del puente: `data/processed/canon/resultados_hipotesis.csv`.

---

## 4. `analysis/` — qué hace cada carpeta y script

Entrada típica: `data/processed/canon/` (+ a veces `congreso.db`).  
Salida: `outputs/imagenes/*.png` (y algunos CSV en `canon/`).

### Raíz

| Archivo | Función |
|---------|---------|
| `_helpers.R` | Rutas del proyecto (`congreso.db`, canon, imágenes, Fondecyt) |
| `_diccionario.R` | Códigos A1–A5, C1–C5, D1; regex; flags co-ocurrencia H1/H2/H4 |
| `run_todo.R` | Orquesta el pipeline **canónico** (5 scripts) |

```bash
make analisis          # ETL canónicos + run_todo.R
Rscript analysis/run_todo.R
```

### Orden canónico

1. `prensa/01_descriptivo.R` → repertorios en prensa Kast  
2. `discursos_presidenciales/01_repertorios.R` → A–D + co-ocurrencias  
3. `parlamento/01_cohesion_bloque.R` → Rice REP/UDI/RN/PNL  
4. `parlamento/02_proyectos_watchlist.R` → mensajes `-05`  
5. `puente/01_agenda.R` → eco mediático ↔ legislativo (±7 días)

### `analysis/prensa/`

| Script | Qué hace |
|--------|----------|
| **01_descriptivo.R** | Volumen, actores, familias A–D (canónico) |
| **02_tendencias.R** | Tendencias 2026 |
| 03_fondecyt_build.R | Importa baseline Fondecyt externo → parquet |
| 04_fondecyt_2020_2022.R | Baseline derecha 2020–22 |
| 05_comparar_periodos.R | 2020–22 vs 2026 |
| 06_prensa_votos.R | Cruce prensa ↔ votaciones |
| 07_serie_2021_2026.R | Serie longitudinal empalmada |
| 08_disputa_actores_repertorios.R | Actores × repertorios |
| 09_emol_tendencia.R | EMOL 2015–2026 |

### `analysis/parlamento/`

| Script | Qué hace |
|--------|----------|
| **01_cohesion_bloque.R** | Índice Rice y convergencia del bloque (H3) |
| **02_proyectos_watchlist.R** | Boletines `-05` (agenda formal del Ejecutivo) |
| 03_agencia_cohesion.R | Agencia vs cohesión |
| 04_red_votaciones.R | Red convergencia/volatilidad |
| 05_red_ecos.R | Ecos partidarios por boletín |
| 06_indicaciones.R | Indicaciones vs voto general |
| 07_mensajes_pdl.R | Casos `18216-05` / `18296-05` |
| 08_hemiciclo_pdl.R | Hemiciclos de polarización |
| 09_piloto_votantes.R | Red a nivel diputado |
| 10_coalicion_disruptivos.R | Disruptivos + Kaiser |
| 11_bcall.R | B-Call: ideología (d1) × volatilidad (d2) |

### `analysis/discursos_presidenciales/`

| Script | Qué hace |
|--------|----------|
| **01_repertorios.R** | Codificación A–D + H1/H2/H4 |
| 02_descriptivo.R | Top palabras (exploratorio) |

### `analysis/puente/`

| Script | Qué hace |
|--------|----------|
| **01_agenda.R** | Ventanas de eventos PDL/endeudamiento; escribe `resultados_hipotesis.csv` |

### `analysis/qualmer/`

| Script | Qué hace |
|--------|----------|
| 00_sample.R | Muestra estratificada para codificación cualitativa humana |

---

## 5. `data/` — qué hay (local; núcleo del paquete)

### `data/raw/` — fuente de verdad cruda

| Ruta | Contenido |
|------|-----------|
| **`congreso.db`** | SQLite: votaciones, votos, diputados, proyectos, BCN, tablas derivadas |
| **`discursos/presidencia/`** | Corpus discursos (`discursos.csv` / `.parquet`, lista IDs) |
| `prensa/<medio>/` | Lotes por outlet (biobio, emol, t13, …) — **pesado; no hace falta para H1–H4 canónicas** |
| `prensa/total/` | Agregados `prensa_total` / `prensa_derecha` |
| `prensa/derecha/` | Filtro keyword derecha |
| `bcn/` | JSON normas Ley Chile |
| `congreso_logs/` | Logs de scrapers Cámara/BCN |
| `prensa/logs/` | Logs daily / EMOL |

**Tablas clave en `congreso.db`:** `votaciones`, `votos`, `diputados`, `proyectos_ley`, `bcn_boletines`, `normas`, `votacion_solicitantes`, `analisis_agencia`.

### `data/processed/canon/` — capa para análisis replicable

| Archivo | Dominio |
|---------|---------|
| `prensa.parquet` | Prensa Kast + columnas repertorio |
| `discursos.parquet` | Discursos + repertorios |
| `votaciones.parquet` / `votos.parquet` / `diputados.parquet` | Parlamento |
| `proyectos.parquet` | Boletines / watchlist |
| `menciones_repertorio.parquet` | Hits A–D por unidad |
| `resultados_hipotesis.csv` | Resumen contraste H1–H4 |
| `puente_eventos.csv` | Eventos del puente |
| `bcall_*.csv` | Coordenadas B-Call |
| `qualmer_sample.csv` | Muestra cualitativa |
| `manifest.csv` | Metadatos del build |

### `data/processed/prensa/`

| Archivo | Uso |
|---------|-----|
| `prensa_unificada.parquet` | Todos los medios deduplicados |
| `fondecyt_*.parquet` | Baseline histórico (opcional; pesado) |
| `prensa_serie_2021_2026.parquet` | Serie larga (opcional) |
| `emol_serie_*.csv` | Agregados EMOL |

### `data/scripts/` — ETL offline (sin APIs)

```bash
python3 data/scripts/build_derived.py     # tablas derivadas en congreso.db
python3 data/scripts/unify_prensa.py      # → prensa_unificada.parquet
python3 data/scripts/build_canonicos.py   # → processed/canon/*.parquet
```

---

## 6. `docs/` — documentación e informes

| Archivo | Contenido |
|---------|-----------|
| **`REPOSITORIO.md`** | Este inventario completo |
| **`ESTRUCTURA_EXPORT.md`** | Qué va a GitHub vs qué queda local |
| **`PROYECTO.qmd`** | Biblia: datos, scrapers, análisis, **H1–H4**, diccionario A–D |
| `informe_analisis.qmd` | Informe con figuras `canon_*.png` |
| `presentacion_hallazgos.qmd` | Slides Reveal.js: H1–H4 × proyectos, votos/fisuras, prensa |
| `qualmer_propuesta.qmd` | Protocolo de codificación cualitativa |
| `RESCATE_PRENSA_CHILE.md` | Migración desde proyecto prensa-chile |

```bash
quarto render docs/informe_analisis.qmd
quarto render docs/PROYECTO.qmd
```

---

## 7. `outputs/`

| Carpeta | Contenido | Versionar |
|---------|-----------|-----------|
| **`imagenes/`** | PNG/GIF del análisis (`canon_*`, `bcall_*`, `disputa_*`, …) | Sí |
| `volatilidad/` | CSV por boletín, catálogos, dumps | No |

Prefijos útiles: `canon_prensa_`, `canon_discursos_`, `canon_camara_`, `canon_pdl_`, `canon_puente_`, `bcall_`, `coalicion_`, `emol_`, `disputa_`.

---

## 8. `scrapers/` — solo recolección (fuera del paquete)

| Subcarpeta | Función | Destino de datos |
|------------|---------|------------------|
| `prensa/` | Multi-medio (`run.py`, `fuentes/`, `core/`) | `data/raw/prensa/` |
| `congreso/` | Cámara, Senado, BCN, match | `data/raw/congreso.db` |
| `discursos/` | Presidencia | `data/raw/discursos/` |
| `diario_oficial/` | Pendiente | — |

Quien **analiza** no necesita scrapers: usa el snapshot de `data/` del paquete.

---

## 9. Otras carpetas

| Ruta | Rol |
|------|-----|
| `config/settings.py` | API keys BCN, watchlist, fechas (gitignore) |
| `bibliography/` | Papers de referencia |
| `neogremialismo-data/` | Plantilla de repo companion de datos |
| `.env` | Secretos locales |

Dependencia externa opcional: proyecto **`derechas-fondecyt`** (baseline 2015–24) vía `FONDECYT_ROOT`.

---

## 10. Cómo reproducir el análisis (sin scrapers)

1. Descomprimir el paquete `neo_gremialismo_analisis_hipotesis.zip`.
2. Abrir `neo_gremialismo.Rproj` o `cd` a la carpeta.
3. Requisitos: R (≥4.x) con tidyverse, arrow, DBI, RSQLite, ggplot2, etc.; Python 3 si querés re-correr ETL.
4. Correr:

```bash
Rscript analysis/run_todo.R
# o, si reconstruís canon:
make analisis
```

5. Leer hipótesis y diccionario: `docs/PROYECTO.qmd`.  
6. Ver resultados: `outputs/imagenes/canon_*.png` y `data/processed/canon/resultados_hipotesis.csv`.

### Qué incluye el paquete de análisis

- `analysis/` completo  
- `data/processed/canon/`  
- `data/processed/prensa/prensa_unificada.parquet` (+ baseline 2020–22 si cabe)  
- `data/raw/congreso.db`  
- `data/raw/discursos/`  
- `data/scripts/`  
- `docs/`  
- `outputs/imagenes/`  
- `Makefile`, `README.md`, `Rproj`

### Qué NO incluye (a propósito)

- `scrapers/`  
- Lotes crudos por medio (`data/raw/prensa/biobio`, `emol_hist`, logs, …)  
- Parquets Fondecyt gigantes (`fondecyt_total` ~3.4 GB)  
- `outputs/volatilidad/`  
- Claves API / `.env`

---

## 11. Diccionario A–D (resumen)

| Familia | Códigos | Tema |
|---------|---------|------|
| **A** Gremialista | A1…A5 | Subsidiariedad, orden, familia, antiestatismo, economía libre |
| **B** (vía co-ocurrencia) | A5+A3, A5+A2… | Articulación neogremialista (H1) |
| **C** Radical | C1…C5 | Seguridad, migración, cultura, anti-élite, civilización |
| **D** Libertario | D1 | Individualismo / Estado mínimo sin A1 |

Implementación: `analysis/_diccionario.R`.

---

## 12. Comandos rápidos

```bash
# Análisis canónico
make analisis

# Informe Quarto
make informe

# Scripts sueltos
Rscript analysis/parlamento/11_bcall.R
Rscript analysis/prensa/02_tendencias.R
```

---

*Documentación maestra del inventario. Detalle metodológico ampliado: [`PROYECTO.qmd`](PROYECTO.qmd). Inventario export GitHub: [`ESTRUCTURA_EXPORT.md`](ESTRUCTURA_EXPORT.md).*
