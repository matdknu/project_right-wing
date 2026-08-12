# analysis/parlamento/

Numeración = orden sugerido. `01`–`02` entran en `run_todo.R`.

| Script | Rol | Figuras / salida |
|--------|-----|------------------|
| **`01_cohesion_bloque.R`** | Canónico — Rice REP/UDI/RN/PNL + convergencia | `canon_camara_*.png` |
| **`02_proyectos_watchlist.R`** | Canónico — mensajes `-05` y repertorios | `canon_pdl_*.png` |
| `03_agencia_cohesion.R` | Agencia vs cohesión (tabla derived) | `agencia_vs_cohesion.png` |
| `04_red_votaciones.R` | Red convergencia / volatilidad | `red_convergencia_*.png` |
| `05_red_ecos.R` | Ecos por boletín + heatmap | `eco_camaras_*.png`, HTML |
| `06_indicaciones.R` | Indicaciones vs voto general | indicaciones + hemiciclo HTML |
| `07_mensajes_pdl.R` | Casos `18216-05` / `18296-05` | `mensajes_*.png`, heatmaps |
| `08_hemiciclo_pdl.R` | Hemiciclos polarización PDL | `hemiciclo_*.png` / HTML |
| `09_piloto_votantes.R` | Red a nivel diputado (exploratorio) | `red_votantes_piloto.png` |
| `10_coalicion_disruptivos.R` | Disruptivos del bloque + Kaiser | `coalicion_*.png` |
| `11_bcall.R` | B-Call: ideología (d1) × volatilidad (d2) | `bcall_*.png`, `canon/bcall_*.csv` |

```bash
Rscript analysis/parlamento/01_cohesion_bloque.R
Rscript analysis/parlamento/07_mensajes_pdl.R
Rscript analysis/parlamento/08_hemiciclo_pdl.R
```
