# analysis/discursos_presidenciales/

| Script | Rol | Figuras |
|--------|-----|---------|
| **`01_repertorios.R`** | Canónico — códigos A–D + co-ocurrencias H1/H2/H4 | `canon_discursos_*.png` |
| **`03_kast_alusiones.py`** | CSV: léxico, alusiones, Kast vs Kaiser (cuerpo oral + prensa unida) | — |
| **`03_kast_alusiones.R`** | ggplot2 de esos CSV | `discursos_kast_*.png`, `prensa_kast_kaiser_*.png` |

```bash
python3 analysis/discursos_presidenciales/03_kast_alusiones.py
Rscript analysis/discursos_presidenciales/03_kast_alusiones.R
```

`03` lee `data/raw/discursos/presidencia/discursos.csv` (fechas tipo `21 JUL. 2026`), filtra gobierno Kast (`2026-03-11+`, sin Boric) y busca alusiones en el **cuerpo**, no en el titular de Presidencia. La prensa usa `prensa_unida.parquet` (títulos, 2015–2026).
