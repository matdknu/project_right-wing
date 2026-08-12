# Estructura del proyecto: qué hay en cada carpeta y qué se exporta

Guía de inventario para **neo_gremialismo** — proyecto Fondecyt sobre la derecha chilena bajo Kast (votaciones de REP/UDI/RN/PNL y repertorios discursivos A–D en prensa y discursos presidenciales).

---

## Resumen: qué va a GitHub y qué queda local

El repositorio separa **código + figuras + documentación** (público) de **datos crudos y scrapers** (local).

| Carpeta / artefacto | En GitHub | Rol |
|---------------------|-----------|-----|
| `analysis/` | **Sí** | Scripts R de análisis |
| `outputs/imagenes/` | **Sí** | Figuras PNG/GIF (producto visual) |
| `docs/` | **Sí** | Documentación, informes Quarto |
| `data/` | **No** | Datos raw + procesados + ETL |
| `scrapers/` | **No** | Recolección Python |
| `outputs/volatilidad/` | **No** | CSV intermedios, dumps por boletín |
| `config/settings.py` | **No** | Claves API, rutas locales |
| `*.db` | **No** | `congreso.db` y similares |

**Flujo de trabajo:**

```
scrapers/  →  data/raw/  →  data/scripts/  →  data/processed/  →  analysis/  →  outputs/
   (local)      (local)         (local)           (local)          (GitHub)     (imagenes → GitHub)
```

**Comando habitual:**

```bash
make analisis    # build_canonicos.py + run_todo.R
make informe     # + render docs/informe_analisis.qmd
```

Si compartes el proyecto con un colaborador, lo versionado en GitHub alcanza para ver **cómo** se analiza; para **reproducir** hace falta tener `data/` local (o un paquete de datos aparte, p. ej. `neogremialismo-data/`).

---

## `analysis/` — scripts de análisis (exportado a GitHub)

Pipeline unidireccional: lee tablas en `data/processed/canon/` (o `congreso.db` directo) y escribe figuras en `outputs/imagenes/`.

### Archivos compartidos (raíz de `analysis/`)

| Archivo | Qué hace |
|---------|----------|
| `_helpers.R` | Detecta la raíz del proyecto, rutas a `congreso.db`, `prensa_unificada.parquet`, `canon/`, `outputs/imagenes/`, proyecto Fondecyt externo |
| `_diccionario.R` | Diccionario de repertorios A1–A5, C1–C5, D1; regex de detección; flags de co-ocurrencia (H1, H2, H4) |
| `run_todo.R` | Orquestador **canónico**: ejecuta los 5 scripts marcados como núcleo del informe |
| `README.md` | Índice del pipeline |

### Orden canónico (`make analisis` / `run_todo.R`)

| # | Script | Dominio |
|---|--------|---------|
| 1 | `prensa/01_descriptivo.R` | Prensa período Kast (corpus total) |
| 2 | `discursos_presidenciales/01_repertorios.R` | Discursos presidenciales A–D |
| 3 | `parlamento/01_cohesion_bloque.R` | Cohesión Rice REP/UDI/RN/PNL |
| 4 | `parlamento/02_proyectos_watchlist.R` | Mensajes de estado (`-05`) |
| 5 | `puente/01_agenda.R` | Puente prensa ↔ discursos ↔ votos |

Los demás scripts son **exploratorios o complementarios**; se corren a mano según necesidad.

---

### `analysis/prensa/` — medios de comunicación

Análisis del corpus de prensa: descriptivos 2026, tendencias, baseline Fondecyt 2020–22, series largas, puente con votaciones, foco EMOL.

| Script | Rol | Entrada principal | Salida |
|--------|-----|-------------------|--------|
| **`01_descriptivo.R`** | Canónico — volumen, actores, repertorios A–D | `canon/prensa.parquet` | `canon_prensa_*.png` |
| **`02_tendencias.R`** | Tendencias 2026 (volumen, actores, cámara) | `canon/prensa.parquet` | `tendencia_*.png` |
| `03_fondecyt_build.R` | Importa RDS del proyecto `derechas-fondecyt` → parquet | Externo Fondecyt | `processed/prensa/fondecyt_*.parquet` |
| `04_fondecyt_2020_2022.R` | Baseline derecha 2020–22 | `fondecyt_derecha_2020_2022.parquet` | `derecha_2020_22_*.png` |
| `05_comparar_periodos.R` | Compara 2020–22 vs 2026 | Fondecyt + `prensa_total` | `comparar_*_periodos.png` |
| `06_prensa_votos.R` | Cruce prensa ↔ votaciones Cámara | `congreso.db` + prensa unificada | `01_rice.png`…`08_*.png`, `panel_votaciones.png` |
| `07_serie_2021_2026.R` | Serie unificada 2021–26 (Fondecyt + neo) | `fondecyt_total` + `prensa_total` | `prensa_serie_2021_2026.parquet` |
| `08_disputa_actores_repertorios.R` | Disputa actores × familias A–D | serie 2021–2026 | `disputa_*.png` |
| `09_emol_tendencia.R` | EMOL largo plazo 2015–2026 | Fondecyt + `emol_by_id` / `emol_hist` | `emol_*.png`, `emol_serie_*.csv` |

---

### `analysis/parlamento/` — votaciones y proyectos de ley

Comportamiento legislativo de la derecha: cohesión, redes, PDL, indicaciones, B-Call (ideología × volatilidad).

| Script | Rol | Entrada principal | Salida |
|--------|-----|-------------------|--------|
| **`01_cohesion_bloque.R`** | Canónico — índice Rice por partido | `canon/votaciones.parquet`, `votos.parquet` | `canon_camara_*.png` |
| **`02_proyectos_watchlist.R`** | Canónico — boletines `-05` (mensajes presidenciales) | `canon/proyectos.parquet` | `canon_pdl_*.png` |
| `03_agencia_cohesion.R` | Agencia parlamentaria vs cohesión | `congreso.db` (`analisis_agencia`) | `agencia_vs_cohesion.png` |
| `04_red_votaciones.R` | Red de convergencia / volatilidad entre partidos | `congreso.db` | `red_convergencia_*.png` |
| `05_red_ecos.R` | “Ecos” partidarios por boletín | `congreso.db` | `eco_camaras_*.png`, HTML interactivo |
| `06_indicaciones.R` | Voto en indicaciones vs voto general | `congreso.db` | `indicaciones_*.png`, hemiciclo HTML |
| `07_mensajes_pdl.R` | Casos `18216-05` (PDL) y `18296-05` (endeudamiento) | `congreso.db` | `mensajes_*.png` |
| `08_hemiciclo_pdl.R` | Hemiciclos de polarización en PDL | `congreso.db` | `hemiciclo_*.png` / HTML |
| `09_piloto_votantes.R` | Red a nivel diputado (exploratorio) | `congreso.db` | `red_votantes_piloto.png` |
| `10_coalicion_disruptivos.R` | Disruptivos del bloque + alineación Kaiser | `congreso.db` | `coalicion_*.png` |
| `11_bcall.R` | B-Call: dimensión ideológica (d1) × volatilidad (d2) | `congreso.db` | `bcall_*.png`, `bcall_evolucion_derecha.gif`, `canon/bcall_*.csv` |

**Boletines watchlist recurrentes:** `18216-05` (PDL urbanismo), `18296-05` (endeudamiento 2026), más mensajes `-05` de reajuste IMM.

---

### `analysis/discursos_presidenciales/` — discursos de la Presidencia

| Script | Rol | Entrada | Salida |
|--------|-----|---------|--------|
| **`01_repertorios.R`** | Canónico — códigos A–D + co-ocurrencias H1/H2/H4 | `canon/discursos.parquet` | `canon_discursos_*.png` |
| `02_descriptivo.R` | Top palabras (exploratorio, gobierno Kast) | `data/raw/discursos/presidencia/` | `discursos_kast_top_palabras.png` |

---

### `analysis/puente/` — cruce entre dominios

| Script | Rol | Entrada | Salida |
|--------|-----|---------|--------|
| **`01_agenda.R`** | Canónico — ventanas ±7 días en PDL / endeudamiento | canon prensa, discursos, votos | `canon_puente_*.png`, `puente_eventos.csv`, `resultados_hipotesis.csv` |

---

### `analysis/qualmer/` — codificación cualitativa

| Script | Rol | Salida |
|--------|-----|--------|
| `00_sample.R` | Muestra estratificada para codificación humana (Qualmer) | `canon/qualmer_sample.csv` |

Protocolo detallado en `docs/qualmer_propuesta.qmd`.

---

## `data/` — datos (local, no en GitHub)

Todo `data/` está en `.gitignore`. Es la **fuente de verdad** para reproducir análisis en tu máquina. Si exportas datos a un colaborador, este es el paquete relevante.

```
data/
├── raw/           ← salida directa de scrapers e importaciones
├── processed/     ← tablas listas para R (canon + prensa unificada)
└── scripts/       ← ETL Python offline (sin llamadas a API)
```

---

### `data/raw/` — datos crudos

#### Raíz de `data/raw/`

| Archivo / carpeta | Qué contiene | Origen |
|-------------------|--------------|--------|
| **`congreso.db`** | SQLite: votaciones, votos, diputados, proyectos, normas BCN, tablas derivadas | `scrapers/congreso/` |
| `diputados.csv` | Export auxiliar de diputados | derivado |
| `cohesion_rice_por_partido.csv` | Cohesión precalculada | derivado |
| `convergencia_pares_partidos.csv` | Pares partido–partido | derivado |
| `bcn/` | JSON de normas Ley Chile | `scrapers/congreso/bcn_leychile.py` |
| `congreso_logs/` | Logs de corridas Cámara / BCN / Senado | launchd / manual |

**Tablas principales en `congreso.db`:**

| Tabla | Contenido |
|-------|-----------|
| `votaciones` | Metadatos de cada votación (fecha, boletín, resultado, tipo) |
| `votos` | Voto individual por diputado (`+1`/`-1`/abstención) |
| `diputados` | Catálogo diputados (partido, distrito, período) |
| `proyectos_ley` / `proyectos_detalle` | Proyectos y detalle de tramitación |
| `bcn_boletines` | Match boletín ↔ norma Ley Chile (estado, `norma_id`) |
| `normas` / `normas_articulos` | Texto de leyes publicadas (cuando hay match) |
| `votacion_solicitantes` | Quién pidió cada votación | derivado |
| `analisis_agencia` | Métricas de agencia vs cohesión | derivado |
| `votaciones_senado` | Votaciones Senado (cuando el scraper funciona) |

---

#### `data/raw/discursos/presidencia/`

| Archivo | Qué contiene |
|---------|--------------|
| `discursos.csv` / `discursos.parquet` | Corpus de discursos (texto, fecha, URLs) |
| `lista_discursos.csv` | Índice de IDs scrapeados |

Origen: `scrapers/discursos/run.py` → `prensa.presidencia.cl`.

---

#### `data/raw/prensa/` — prensa por outlet

Cada subcarpeta guarda lotes de scrape con patrón `{fuente}_{YYYYMMDD_HHMMSS}.csv/.parquet`.

| Subcarpeta | Medio / corpus |
|------------|----------------|
| **`total/`** | **`prensa_total.parquet`** y **`prensa_derecha.parquet`** — agregados del daily |
| **`derecha/`** | Corpus filtrado por keywords derecha (vía `scrapers/prensa/core/filtros.py`) |
| **`emol/`** | EMOL por ID: `emol_by_id.parquet` (diario), `emol_hist.parquet` (histórico 2018+) |
| `biobio/` | BioBioChile |
| `t13/` | Teletrece |
| `meganoticias/` | Meganoticias |
| `theclinic/` | The Clinic |
| `exante/` | Ex-Ante |
| `cnn/` | CNN Chile |
| `elmostrador/` | El Mostrador |
| `ciper/` | CIPER |
| `radiouchile/` | Radio Universidad de Chile |
| `cooperativa/` | Cooperativa |
| `eldesconcierto/` | El Desconcierto |
| `24horas/` | 24 Horas (rescate 2025) |
| `chvnoticias/` | CHV Noticias (rescate 2025) |
| `eldinamo/` | El Dínamo (rescate 2025) |
| `elmercurio/` | El Mercurio / Google News |
| `google_news/` | Agregador Google News |
| `logs/` | Logs daily, EMOL histórico, discursos |

Los lotes `*_prensa_chile_2025.parquet` son **rescates** del proyecto anterior `prensa-chile` (ver `docs/RESCATE_PRENSA_CHILE.md`).

---

### `data/processed/` — tablas para análisis

#### `data/processed/canon/` — capa canónica (entrada principal de R)

Una tabla por dominio; generada por `build_canonicos.py`. Es lo que consumen casi todos los scripts `01_*.R`.

| Archivo | Contenido |
|---------|-----------|
| `prensa.parquet` | Prensa período Kast con columnas de repertorio |
| `discursos.parquet` | Discursos presidenciales codificados |
| `votaciones.parquet` | Votaciones Cámara normalizadas |
| `votos.parquet` | Votos individuales normalizados |
| `diputados.parquet` | Diputados |
| `proyectos.parquet` | Proyectos / boletines |
| `menciones_repertorio.parquet` | Detalle de menciones A–D por documento |
| `manifest.csv` | Registro de build (fechas, conteos) |
| `puente_eventos.csv` | Eventos para análisis puente |
| `resultados_hipotesis.csv` | Resultados H1–H4 agregados |
| `qualmer_sample.csv` | Muestra Qualmer |
| `bcall_*.csv` | Coordenadas B-Call exportadas |
| `votaciones_ley_bcn.csv` | Cruce votaciones ↔ BCN |

#### `data/processed/prensa/` — prensa unificada y series

| Archivo | Contenido |
|---------|-----------|
| `prensa_unificada.parquet` | Todos los outlets deduplicados (salida de `unify_prensa.py`) |
| `prensa_serie_2021_2026.parquet` | Serie longitudinal Fondecyt + neo |
| `fondecyt_*.parquet` | Importaciones del proyecto baseline |
| `emol_serie_anual.csv` / `emol_serie_mensual.csv` | Series EMOL agregadas |

---

### `data/scripts/` — ETL offline (local)

No llama APIs. Orden de ejecución:

```bash
python3 data/scripts/build_derived.py    # tablas derivadas en congreso.db
python3 data/scripts/unify_prensa.py     # → prensa_unificada.parquet
python3 data/scripts/build_canonicos.py  # → processed/canon/*.parquet
Rscript analysis/run_todo.R              # → outputs/imagenes/
```

| Script | Entrada | Salida |
|--------|---------|--------|
| `build_derived.py` | `congreso.db` | `votacion_solicitantes`, `analisis_agencia` |
| `unify_prensa.py` | `data/raw/prensa/**` | `processed/prensa/prensa_unificada.parquet` |
| `build_canonicos.py` | raw prensa + discursos + `congreso.db` | `processed/canon/*.parquet` + menciones A–D |

---

## `outputs/` — productos del análisis

### `outputs/imagenes/` — **exportado a GitHub**

Figuras finales del pipeline. Convención de prefijos:

| Prefijo | Origen | Ejemplos |
|---------|--------|----------|
| `canon_prensa_` | `prensa/01` | volumen, repertorios, serie_familias |
| `canon_discursos_` | `discursos/01` | repertorios, coocurrencias |
| `canon_camara_` | `parlamento/01` | rice, rep_pnl |
| `canon_pdl_` | `parlamento/02` | watchlist_votos |
| `canon_puente_` | `puente/01` | eco, prensa_diaria |
| `tendencia_` | `prensa/02` | volumen, actores |
| `disputa_` | `prensa/08` | actores, heatmap |
| `emol_` | `prensa/09` | volumen_anual, pct_derecha |
| `coalicion_` | `parlamento/10` | disruptivos, kaiser |
| `bcall_` | `parlamento/11` | mapa, densidades, gif evolución |
| `mensajes_`, `hemiciclo_`, `indicaciones_`, `red_` | scripts exploratorios parlamento | — |

Algunos scripts generan también HTML interactivo (`red_ecos_*.html`); las carpetas `*_files/` de dependencias **no** se versionan.

### `outputs/volatilidad/` — **local**

CSV intermedios, catálogos de votaciones, dumps por boletín (`pdl_18216_05/`, etc.). Útil para debugging y scripts de congreso; no forma parte del export público.

---

## Otras carpetas del repo (contexto)

| Carpeta | GitHub | Rol |
|---------|--------|-----|
| `docs/` | Sí | `PROYECTO.qmd` (biblia del proyecto), `informe_analisis.qmd`, Qualmer |
| `scrapers/` | No | Python: prensa, congreso, discursos |
| `config/` | Parcial | `settings.py` local (API keys, watchlist) |
| `bibliography/` | Sí | PDFs de referencia |
| `neogremialismo-data/` | No | Plantilla repo companion para datos |

---

## Dependencia externa

Varios scripts de prensa usan el proyecto **`derechas-fondecyt`** (baseline 2015–2024). Ruta por defecto en `_helpers.R` o variable de entorno `FONDECYT_ROOT`.

---

## Checklist de exportación

### Lo que subes a GitHub (repo público)

- [ ] `analysis/` completo
- [ ] `outputs/imagenes/*.png` (y GIF si aplica)
- [ ] `docs/` (QMD + HTML renderizado)
- [ ] `README.md`, `Makefile`, `neo_gremialismo.Rproj`

### Lo que mantienes local (o empaquetas aparte)

- [ ] `data/raw/` — especialmente `congreso.db` y `prensa/`
- [ ] `data/processed/` — canon y prensa unificada
- [ ] `data/scripts/` — ETL
- [ ] `scrapers/` — para actualizar datos
- [ ] `outputs/volatilidad/` — si lo necesitas para replicar exploratorios

### Para reproducir desde cero en otra máquina

1. Clonar repo GitHub
2. Copiar o generar `data/` (scrapers + ETL)
3. Opcional: clonar `derechas-fondecyt` para baseline
4. `make analisis` → `make informe`

---

*Documento generado para el inventario del proyecto. Detalle metodológico ampliado en [`PROYECTO.qmd`](PROYECTO.qmd).*
