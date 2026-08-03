# analysis/prensa/

Numeración = orden sugerido. Prefijo `01`–`02` son canónicos / frecuentes.

| Script | Rol | Entrada | Figuras |
|--------|-----|---------|---------|
| **`01_descriptivo.R`** | Canónico Kast (total, sin filtro) | `canon/prensa.parquet` | `canon_prensa_*.png` |
| **`02_tendencias.R`** | Tendencias 2026 (volumen, actores, A–D, cámara) | `canon/prensa.parquet` | `tendencia_*.png` |
| `03_fondecyt_build.R` | Importa RDS Fondecyt → parquet | `derechas-fondecyt` | — |
| `04_fondecyt_2020_2022.R` | Baseline derecha 2020–22 | `fondecyt_derecha_2020_2022.parquet` | `derecha_2020_22_*.png` |
| `05_comparar_periodos.R` | 2020–22 vs 2026 | Fondecyt + `prensa_total` | `comparar_*_periodos.png` |
| `06_prensa_votos.R` | Puente prensa ↔ votaciones | `congreso.db` + unificada | `01_rice`…`08_*.png`, panel |
| **`07_serie_2021_2026.R`** | Empalme Fondecyt+neo, filtro derecha uniforme | `fondecyt_total` + `prensa_total` | — → `prensa_serie_2021_2026.parquet` |
| **`08_disputa_actores_repertorios.R`** | Disputa actores × A–D | serie 2021–2026 | `disputa_*.png` |

```bash
Rscript analysis/prensa/07_serie_2021_2026.R
Rscript analysis/prensa/08_disputa_actores_repertorios.R
```
