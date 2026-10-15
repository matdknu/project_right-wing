"""08_articulo.py · Rellena articulo.qmd con las cifras de los análisis 28-41.

Equivale al paso 42. Cada reemplazo exige que el texto aparezca una sola vez.
No toca los [PENDIENTE] ni los [ACTUALIZAR] sin fuente (Servel, Fondecyt, Mixta).

Uso, desde neo_gremialismo_votes/:
    python3 scripts/pipeline/08_articulo.py
"""
from pathlib import Path
import runpy

runpy.run_path(str(Path(__file__).resolve().parent / "pasos" / "42_revista_aplicar_cambios_qmd.py"),
               run_name="__main__")
