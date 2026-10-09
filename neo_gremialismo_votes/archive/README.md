# Archivo (no usar en el pipeline)

Material obsoleto o de desarrollo, conservado por referencia. **No ejecutar** estos scripts salvo comparación histórica.

| Carpeta / archivo | Qué era |
|-------------------|---------|
| `scripts/scrip antiguos/` | Versiones previas de `03_indicaciones_llm.R` y `2_bcall.R` |
| `scripts/analisis_llm_indicaciones.R` | Codificación LLM **modular** (núcleo, economía, cultura, orden) — reemplazada por `03_indicaciones_llm.R` unificado |
| `scripts/notes.R` | Notas sueltas |
| `subset_dev/` | Submuestras y `2_bcall_subset.R` para pruebas |
| `llm/cache_modular/` | Cachés del esquema modular (versiones distintas de libro de códigos) |
| `llm/validacion_modular/` | Estabilidad y categorías del piloto modular |
| `llm/resultados_legacy/` | `indicaciones_llm.csv`, `votos_llm.csv` (nombres antiguos) |
| `llm/analisis_legacy/` | Tablas `A_*`…`E_*` del B-Call anual antiguo |
| `llm/_anterior/` | Respaldo automático de una migración previa de `llm/` |
| `data/processed_llm/` | Piloto Ollama/OpenAI en `data/processed/llm/` |

El pipeline activo vive en la raíz del proyecto (`0_*.R`, `1_*.R`, `03`–`05`) y en `llm/` (sin prefijo `_anterior`).
