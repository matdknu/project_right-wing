# Análisis canónico neo_gremialismo
# Uso:
#   make canonicos   # ETL data/processed/canon/
#   make analisis    # ETL + R módulos + (opcional) Quarto

.PHONY: canonicos analisis informe presentacion all docs-imagenes

canonicos:
	python3 data/scripts/build_canonicos.py

docs-imagenes:
	mkdir -p docs/imagenes
	cp outputs/imagenes/*.png outputs/imagenes/*.gif docs/imagenes/

analisis: canonicos
	Rscript analysis/run_todo.R

informe: analisis docs-imagenes
	quarto render docs/informe_analisis.qmd

presentacion: docs-imagenes
	cd docs && quarto render presentacion_hallazgos.qmd

all: informe presentacion
