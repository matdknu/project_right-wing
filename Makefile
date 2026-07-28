# Análisis canónico neo_gremialismo
# Uso:
#   make canonicos   # ETL data/processed/canon/
#   make analisis    # ETL + R módulos + (opcional) Quarto

.PHONY: canonicos analisis informe all

canonicos:
	python3 data/scripts/build_canonicos.py

analisis: canonicos
	Rscript analysis/run_todo.R

informe: analisis
	quarto render docs/informe_analisis.qmd

all: informe
