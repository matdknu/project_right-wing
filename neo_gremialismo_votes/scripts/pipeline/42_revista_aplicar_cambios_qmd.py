"""
42_revista_aplicar_cambios_qmd.py

Parte del texto entregado por el equipo (data/analisis/revista/c_paper/42_articulo_base_usuario.qmd)
y resuelve los marcadores [ACTUALIZAR] y <!-- VERIFICAR --> que se pueden resolver con los análisis
de los scripts 28-41. Cada reemplazo exige que el texto original aparezca exactamente una vez.
No toca los marcadores [PENDIENTE] ni los [ACTUALIZAR] sin fuente (Servel, Mixta).
Salida: articulo.qmd.
"""
import csv
import pathlib

RAIZ = pathlib.Path(__file__).resolve().parents[2]
C = RAIZ / "data" / "analisis" / "revista" / "c_paper"
base = (C / "42_articulo_base_usuario.qmd").read_text(encoding="utf-8")
M = "\u2212"


def f(x, d=2, signo=False):
    """Número con coma decimal y signo menos tipográfico."""
    s = f"{abs(x):.{d}f}".replace(".", ",")
    if x < 0 and float(f"{abs(x):.{d}f}") != 0:
        return M + s
    return ("+" + s) if (signo and float(f"{abs(x):.{d}f}") != 0) else s


def leer(nombre):
    with open(C / nombre, encoding="utf-8") as fh:
        return list(csv.DictReader(fh))


def reemplazar(texto, viejo, nuevo):
    n = texto.count(viejo)
    assert n == 1, f"se esperaba 1 aparición y hay {n}: {viejo[:80]!r}"
    return texto.replace(viejo, nuevo)


t = base

# ---- 1. Nota del umbral del 10 % ------------------------------------------------------
t = reemplazar(
    t,
    "[ACTUALIZAR: resultado de la repetición con 10%.]",
    "Con el 10%, entran 9 diputados-año más en las indicaciones y 2 más en todas las votaciones; $d1$ correlaciona 1,00 "
    "con el del 15%, la posición $P$ de ningún partido cambia más de 0,02, y el solapamiento, el porcentaje de mayorías "
    "distintas, $\\Delta$ y el porcentaje con algún partido contra el bloque no cambian. La distancia entre izquierda y "
    "derecha cambia como máximo 0,3 desviaciones, y la de la derecha radical a la convencional, solo en 2022-23 en las "
    "indicaciones (de 0,78 a 0,69).",
)

# ---- 2. tbl-posicion: R con todas las votaciones --------------------------------------
pos = {(r["universo"], r["tiempo"]): r for r in leer("38_posicion_P_R.csv")}
anios = ["2022-23", "2023-24", "2024-25", "2025-26"]
for a in anios:
    r = pos[("Todas las votaciones", a)]
    # la fila ya trae d1 y W-NOMINATE; solo se completa la última columna
    import re
    patron = re.compile(rf"(\| {a} \| [^|]+ \| [^|]+ \| )\[ACTUALIZAR\] \|")
    assert len(patron.findall(t)) == 1, a
    t = patron.sub(lambda m: m.group(1) + f(float(r["R"])) + " |", t)
r_ind = [float(pos[("Solo indicaciones", a)]["R"]) for a in anios]
t = reemplazar(
    t,
    "[ACTUALIZAR: calcular $R$ con todas las votaciones y la izquierda corregida; con solo indicaciones va de −0,02 a 0,24, @tbl-posicion-p en el apéndice.]",
    f"Con solo indicaciones, $R$ va de {f(min(r_ind))} a {f(max(r_ind))} (@tbl-posicion-p en el apéndice).",
)

# ---- 3. Las cinco escalas -------------------------------------------------------------
esc = leer("38_cinco_escalas.csv")
lo = min(float(r["brecha_en_sd"]) for r in esc)
hi = max(float(r["brecha_en_sd"]) for r in esc)
t = reemplazar(
    t,
    "el PRCh queda entre 0,5 y 0,9 a la derecha de la UDI y RN en las cinco escalas que comparamos, incluido el punto ideal dinámico de Fábrega (2025).",
    f"el PRCh queda entre 0,5 y 0,9 a la derecha de la UDI y RN en las cinco escalas que comparamos (W-NOMINATE, $d1$ y los tres puntos ideales de Fábrega, 2025: dinámico, DW-NOMINATE y bayesiano; estimaciones de {f(lo)} a {f(hi)}).",
)

# ---- 4. Rice: universo ----------------------------------------------------------------
t = reemplazar(
    t,
    "<!-- VERIFICAR: universo (todas las votaciones o indicaciones) de los Rice corregidos. -->En 2022-26, el Rice corregido por azar del PRCh es 0,99, el de la UDI 0,90 y el de RN 0,92 (@fig-cohesion-dentro, apéndice).",
    "En 2022-26, en todas las votaciones, el Rice corregido por azar del PRCh es 0,99, el de la UDI 0,90 y el de RN 0,91 (@fig-cohesion-dentro, apéndice).",
)
t = reemplazar(t, "(de 0,94 a 0,90 y de 0,93 a 0,92)", "(de 0,94 a 0,90 y de 0,93 a 0,91)")

# ---- 5. Demócratas y DC ---------------------------------------------------------------
ac = leer("40_acuerdo_dem_dc.csv")
dem = [float(r["acuerdo_corregido"]) for r in ac if r["grupo"] == "DEM"]
dc = {r["tiempo"]: float(r["acuerdo_corregido"]) for r in ac if r["grupo"] == "DC"}
pp = {(r["tiempo"], r["partido"]): r for r in leer("40_posicion_P_dem_dc.csv")}
assert round(100 * min(dem)) == 36 and round(100 * max(dem)) == 53
assert round(100 * dc["2022-23"]) == 44 and round(100 * dc["2025-26"]) == 9
pi = lambda a, p: float(pp[(a, p)]["P_Solo indicaciones"])
pt = lambda a, p: float(pp[(a, p)]["P_Todas las votaciones"])
t = reemplazar(
    t,
    "Demócratas vota con la derecha entre 35% y 54% de las veces y la DC baja de 43% a 8% (@fig-atraccion, apéndice).<!-- VERIFICAR con la izquierda corregida: acuerdo y P de Demócratas y DC. -->",
    "Demócratas vota con la derecha entre 36% y 53% de las veces y la DC baja de 44% a 9% (@fig-atraccion, apéndice). "
    f"En la escala de posición, Demócratas queda a {f(pi('2024-25', 'DEM'), 1)} y {f(pi('2025-26', 'DEM'), 1)} desviaciones de la izquierda en las indicaciones de 2024-25 y 2025-26 "
    f"({f(pt('2024-25', 'DEM'), 1)} en los dos años con todas las votaciones), y la DC pasa de {f(pi('2022-23', 'DC'), 1)} a {f(pi('2025-26', 'DC'), 1)} en las indicaciones "
    f"y de {f(pt('2022-23', 'DC'), 1)} a {f(pt('2025-26', 'DC'), 1)} con todas.",
)

# ---- 6. tbl-brechas: fila de orden ----------------------------------------------------
t = reemplazar(
    t,
    "| Orden | +0,6 (6,1) | [ACTUALIZAR] | [ACTUALIZAR] | [ACTUALIZAR] |",
    "| Orden | +0,6 (6,1) | −3,1 (7,1) | −3,3 (6,3) | +3,9 (7,0) |",
)

# ---- 7. Nación: techo -----------------------------------------------------------------
techo = {(r["bloque"], r["eje"]): r for r in leer("36_techo_sobre_textos_del_polo.csv")}
n = techo[("Nueva derecha", "nacion")]
c = techo[("Derecha tradicional", "nacion")]
assert int(n["votos"]) == 108 and int(c["votos"]) == 531 and int(n["votaciones"]) == 15
t = reemplazar(
    t,
    "Una explicación es un techo: si el PRCh ya vota Sí casi siempre ante cualquier texto de esa materia, el texto soberanista no puede subirlo mucho más. [ACTUALIZAR: probabilidades predichas de Sí de cada bloque ante un texto soberanista y ante uno que no toca el eje.] Si el techo se confirma, la brecha no indica una derecha convencional más soberanista.",
    "Una explicación es un techo, y los datos la respaldan. Ante los textos soberanistas, las dos derechas votan Sí casi siempre: el PRCh en sus "
    f"{int(n['votos'])} votos (100%) y la derecha convencional en el {f(float(c['si_observado_pct']), 1)}% de {int(c['votos'])} (15 votaciones). Lo que cambia es el punto de partida. "
    f"Para esos mismos textos, la especificación A predice un Sí de {f(float(n['si_predicho_pct']), 0)}% en el PRCh y {f(float(c['si_predicho_pct']), 0)}% en la convencional (esta última, sobre 100%, "
    f"porque el modelo es lineal); si no tocaran el eje, {f(float(n['si_sin_tocar_eje_pct']), 0)}% y {f(float(c['si_sin_tocar_eje_pct']), 0)}%. "
    f"Los aumentos, de {f(float(n['aumento_pp']), 0)} y {f(float(c['aumento_pp']), 0)} puntos, llegan al mismo tope desde puntos de partida distintos. "
    "La brecha no indica una derecha convencional más soberanista.",
)

# ---- 8. Iniciativa --------------------------------------------------------------------
tabla = {r["fila"]: r for r in leer("37_tbl_iniciativa.csv")}
fila = lambda k: (float(tabla[k]["moción"]), float(tabla[k]["mensaje"]))
cabeza = "| | Moción | Mensaje |\n|---|---:|---:|\n"
viejo_tabla = (
    cabeza + "| % Sí del PRCh | 59,0 | 41,8 |\n| % Sí de la UDI | 64,1 | 56,3 |\n| % Sí de RN | 70,8 | 59,6 |\n"
    "| % de votaciones con mayorías de las dos derechas distintas | 20,3 | 23,5 |"
)
nuevas = []
for etiqueta, k in [("% Sí del PRCh", "% Sí de PRCh"), ("% Sí de la UDI", "% Sí de UDI"), ("% Sí de RN", "% Sí de RN"),
                    ("% de votaciones con mayorías de las dos derechas distintas", "% de votaciones con mayorías de las dos derechas distintas")]:
    a, b = fila(k)
    nuevas.append(f"| {etiqueta} | {f(a, 1)} | {f(b, 1)} |")
t = reemplazar(t, viejo_tabla, cabeza + "\n".join(nuevas))
t = reemplazar(
    t,
    "[ACTUALIZAR: recalcular con la cobertura completa de iniciativa de 2022-23; hoy es del 60% en ese año.]",
    "La iniciativa está identificada en todas las votaciones de proyectos, con los datos abiertos de la Cámara.",
)

cai = {(r["partido"], r["muestra"]): r for r in leer("37_caidas_con_controles.csv")}
g = lambda p, campo: float(cai[(p, "Todos")][campo])
ic = lambda p, pref="": f"{pref}{f(g(p, 'ic_bajo_pp'), 1)} a {f(g(p, 'ic_alto_pp'), 1)}"
mens = [r for r in leer("37_coeficientes_mensaje.csv") if r["gobierno"].startswith("Boric") and r["muestra"] == "Todos"][0]
assert round(float(mens["efecto_pp"]), 1) == 0.6
t = reemplazar(
    t,
    "difieren en el 20% de las votaciones de mociones y en el 24% de las de mensajes, y el efecto de ser mensaje sobre la división, con año, tipo de votación y materia controlados, es de +0,6 puntos, sin distinguirse de cero.",
    f"difieren en el {f(fila('% de votaciones con mayorías de las dos derechas distintas')[0], 0)}% de las votaciones de mociones y en el "
    f"{f(fila('% de votaciones con mayorías de las dos derechas distintas')[1], 0)}% de las de mensajes, y el efecto de ser mensaje sobre la división, con año, tipo de votación y materia "
    f"controlados, es de +0,6 puntos ({'IC 95%: ' + f(float(mens['ic_bajo_pp']), 1) + ' a +' + f(float(mens['ic_alto_pp']), 1)}), sin distinguirse de cero.",
)
t = reemplazar(
    t,
    "La caída bruta es mayor en el PRCh (17 puntos, frente a 8 en la UDI y 11 en RN), pero con los mismos controles las tres caídas son parecidas: 7,4 puntos en el PRCh, 4,9 en la UDI y 8,5 en RN. [ACTUALIZAR: con la cobertura completa de iniciativa.]",
    f"La caída bruta es mayor en el PRCh ({f(-g('PRCh', 'caida_bruta_pp'), 0)} puntos, frente a {f(-g('UDI', 'caida_bruta_pp'), 0)} en la UDI y {f(-g('RN', 'caida_bruta_pp'), 0)} en RN), "
    f"pero con los mismos controles las tres caídas son parecidas: {f(-g('PRCh', 'caida_con_controles_pp'), 1)} puntos en el PRCh ({ic('PRCh', 'IC 95%: ')}), "
    f"{f(-g('UDI', 'caida_con_controles_pp'), 1)} en la UDI ({ic('UDI')}) y {f(-g('RN', 'caida_con_controles_pp'), 1)} en RN ({ic('RN')}), y ninguna se distingue de cero.",
)

co = {(r["gobierno"][:8], r["muestra"]): r for r in leer("37_coeficientes_mensaje.csv")}
bach, pin = co[("Bachelet", "Todos")], co[("Piñera I", "Todos")]
por = {(r["gobierno"][:8], r["iniciativa"]): float(r["pct_difieren"]) for r in leer("37_pct_mayorias_distintas_por_iniciativa.csv")}
tot = {r["gobierno"][:8]: float(r["pct_difieren"]) for r in leer("37_pct_mayorias_distintas_total.csv")}
assert round(float(bach["efecto_pp"]), 1) == -3.9 and round(float(pin["efecto_pp"]), 1) == -8.1
t = reemplazar(
    t,
    "Bajo Bachelet II, ser mensaje no cambia la división entre la UDI y RN (−3,9 puntos, no significativo); bajo Piñera, la reduce (−8,1), porque se alinean con su propio gobierno. [ACTUALIZAR: intervalos de confianza de ambos coeficientes.]",
    f"Bajo Bachelet II, ser mensaje no cambia la división entre la UDI y RN ({f(float(bach['efecto_pp']), 1)} puntos; IC 95%: {f(float(bach['ic_bajo_pp']), 1)} a +{f(float(bach['ic_alto_pp']), 1)}); "
    f"bajo Piñera, la reduce ({f(float(pin['efecto_pp']), 1)}; IC 95%: {f(float(pin['ic_bajo_pp']), 1)} a {f(float(pin['ic_alto_pp']), 1)}), porque se alinean con su propio gobierno: "
    f"sus mayorías difieren en el {f(por[('Piñera I', 'Mensaje')], 0)}% de las votaciones de mensajes y en el {f(por[('Piñera I', 'Moción')], 0)}% de las de mociones, y en el {f(tot['Piñera I'], 0)}% del total.",
)

# ---- 9. tbl-posicion-p ----------------------------------------------------------------
vieja_p = (
    "| 2022-23 | 2,44 | 2,14 | 1,99 | 1,70 | +0,30 | 0,14 |\n| 2023-24 | 2,13 | 2,18 | 2,07 | 2,00 | −0,05 | −0,02 |\n"
    "| 2024-25 | 2,47 | 2,00 | 1,99 | 1,70 | +0,47 | 0,24 |\n| 2025-26 | 2,10 | 2,15 | 2,02 | 2,05 | −0,05 | −0,02 |"
)
filas = []
for a in anios:
    r = pos[("Solo indicaciones", a)]
    filas.append(f"| {a} | {f(float(r['REP']))} | {f(float(r['UDI']))} | {f(float(r['RN']))} | {f(float(r['EVOP']))} | "
                 f"{f(float(r['PRCh_menos_UDI']), 2, True)} | {f(float(r['R']))} |")
t = reemplazar(t, vieja_p, "\n".join(filas))
t = reemplazar(
    t,
    "[ACTUALIZAR con la izquierda corregida: las posiciones cambian como máximo 0,14 y la brecha PRCh − UDI no cambia.]",
    "Con la izquierda corregida.",
)

# ---- 10. tbl-si: columna de la izquierda y N -----------------------------------------
si = {(r["bloque"], r["eje"]): float(r["coef_pp"]) for r in leer("36_tabla5_coeficientes_por_bloque.csv")}
tam = {r["bloque"]: r for r in leer("36_tabla5_tamanos.csv")}
izq = lambda e: f(si[("Izquierda", e)], 0, True)
t = reemplazar(t, "| Conservador | +43 | +19 | +25 | [ACTUALIZAR] |", f"| Conservador | +43 | +19 | +25 | {izq('valorica')} |")
t = reemplazar(t, "| Soberanista | +16 | +28 | +17 | [ACTUALIZAR] |", f"| Soberanista | +16 | +28 | +17 | {izq('nacion')} |")
t = reemplazar(t, "| Pro-mercado | +4 | +4 | +4 | [ACTUALIZAR] |", f"| Pro-mercado | +4 | +4 | +4 | {izq('economica')} |")
assert izq("orden") == M + "3"
vot = {b: int(tam[b]["votos"]) for b in tam}
fmt_n = lambda x: f"{x:,}".replace(",", ".")
t = reemplazar(
    t,
    "[ACTUALIZAR: columna de la izquierda y número de votos con la izquierda corregida (36.342 votos de izquierda).]",
    f"Votos: {fmt_n(vot['Nueva derecha'])} de la derecha radical, {fmt_n(vot['Derecha tradicional'])} de la convencional, {fmt_n(vot['IND con la derecha'])} de los independientes con la derecha y {fmt_n(vot['Izquierda'])} de la izquierda (con la izquierda corregida).",
)

# ---- 11. tbl-cuanto: R e intervalos con la izquierda corregida -------------------------
def R(e):
    a = lambda b: si[(b, e)]
    return (a("Nueva derecha") - a("Derecha tradicional")) / (a("Derecha tradicional") - a("Izquierda"))
Rv = {e: R(e) for e in ["valorica", "nacion", "orden", "economica"]}
ci = {  # percentiles 2,5 y 97,5 del bootstrap, con la izquierda corregida (a1_izquierda/03_antes_despues.csv, "Tabla 6 (R)")
    "valorica": (-0.05809279027922493, 1.45957120819806),
    "nacion": (-0.3716481885378792, -0.04047078135052448),
    "orden": (-2.203230665700445, 2.566682401715015),
    "economica": (-0.6086477359936617, 0.8156621100283672),
}
t = reemplazar(t, "| Valórico | +24,0 | 0,47 | −0,06 a 1,43 |", f"| Valórico | +24,0 | {f(Rv['valorica'])} | {f(ci['valorica'][0])} a {f(ci['valorica'][1])} |")
t = reemplazar(t, "| Nación | −12,0 | −0,17 | −0,37 a −0,04 |", f"| Nación | −12,0 | {f(Rv['nacion'])} | {f(ci['nacion'][0])} a {f(ci['nacion'][1])} |")
t = reemplazar(t, "| Orden | +0,6 | 0,04 | no informativo |", f"| Orden | +0,6 | {f(Rv['orden'])} | {f(ci['orden'][0])} a {f(ci['orden'][1])} (no informativo) |")
t = reemplazar(t, "| Económico | −0,1 | 0,00 | −0,59 a 0,83 |", f"| Económico | −0,1 | {f(Rv['economica'])} | {f(ci['economica'][0])} a {f(ci['economica'][1])} |")
t = reemplazar(
    t,
    "[ACTUALIZAR: intervalos del bootstrap con la izquierda corregida; los actuales son de la definición anterior.]",
    "Los intervalos son los percentiles 2,5 y 97,5 del bootstrap, recalculado con la izquierda corregida.",
)

# ---- 12. Apéndice E -------------------------------------------------------------------
def leer_txt(n):
    return (C / n).read_text(encoding="utf-8").rstrip("\n")

p3, p3d = leer_txt("41_prompt_cinco_ejes.txt"), leer_txt("41_prompt_cinco_ejes_deepseek.txt")
p15, p15d = leer_txt("41_prompt_termometro.txt"), leer_txt("41_prompt_termometro_deepseek.txt")
assert p3d.startswith(p3) and p15d.startswith(p15)
cola3, cola15 = p3d[len(p3):].strip("\n"), p15d[len(p15):].strip("\n")


def tabla_esquema(nombre):
    out = ["| Campo | Tipo | Valores permitidos |", "|---|---|---|"]
    for r in leer(nombre):
        tipo = {"texto": "texto libre", "bool": "lógico", "enum": "categoría"}[r["tipo"]]
        out.append(f"| {r['campo']} | {tipo} | {r['valores'] if r['tipo'] == 'enum' else r['valores']} |")
    return "\n".join(out)


cor = {(r["codificacion"], r["rol"]): r for r in leer("41_corridas.csv")}
c5p, c5v = cor[("Cinco ejes (versión definitiva)", "principal")], cor[("Cinco ejes (versión definitiva)", "validador")]
c5pil = cor[("Cinco ejes (piloto, versión anterior)", "principal")]
ctp, ctv = cor[("Termómetro", "principal")], cor[("Termómetro", "validador")]
usd = lambda r: float(r["costo_estimado_usd"])
assert (c5p["version"], ctp["version"]) == ("04fc6c14", "59195313")
miles = lambda x: fmt_n(int(x))

apendice = f"""## E. Método {{.unnumbered}}

Este apéndice documenta las dos codificaciones con el modelo de lenguaje (scripts 03 y 15 del proyecto), para que puedan replicarse. La versión de cada codificación es un código de ocho caracteres calculado sobre el prompt y los campos: cualquier cambio de redacción genera una versión nueva y una caché nueva, de modo que ningún resultado mezcla versiones.

: Corridas con el modelo de lenguaje. La fecha es la del registro de cada respuesta. {{#tbl-corridas}}

| Codificación | Rol | Modelo y versión | Parámetros | Textos | Fecha (2026) |
|---|---|---|---|---:|---|
| Cinco ejes | Codificador | OpenAI gpt-6-luna, versión {c5p['version']} | razonamiento «bajo»; máximo de 4.000 tokens de salida, que incluyen el razonamiento; sin temperatura; salida estructurada con el esquema de la @tbl-esquema-ejes; API Batch en la corrida completa | {miles(c5p['textos'])} textos únicos | 7 y 8 de octubre |
| Cinco ejes | Validador | DeepSeek deepseek-v4-flash, versión {c5v['version']} | temperatura 0; respuesta en JSON; razonamiento desactivado; máximo de 2.000 tokens | {miles(c5v['textos'])} | 7 de octubre |
| Termómetro | Codificador | OpenAI gpt-6-luna, versión {ctp['version']} | razonamiento «bajo»; máximo de 3.000 tokens; salida estructurada con el esquema de la @tbl-esquema-termometro | {miles(ctp['textos'])} textos únicos | 14 de octubre |
| Termómetro | Validador | DeepSeek deepseek-v4-flash, versión {ctv['version']} | temperatura 0; respuesta en JSON; razonamiento desactivado; máximo de 1.000 tokens | {miles(ctv['textos'])} | 14 de octubre |

**Entrada.** El mensaje que recibe el modelo es el texto sometido a votación, con la autoría reemplazada por «[autoría omitida]» mediante una regla sobre el encabezado, y nada más: ni fecha, ni boletín, ni resultado. La plantilla es `Texto sometido a votación:`, el texto entre las etiquetas `<texto>` y `</texto>`. Los textos de más de 8.000 caracteres se recortan; un texto votado varias veces se codifica una sola vez; en el termómetro no se envían los de menos de 60 caracteres. El enmascarado de autores falló en parte de los textos (@sec-llm). Para la validación se extraen muestras de 100 textos estratificadas por año, con semilla 20260928 (cinco ejes) y 20261014 (termómetro).

**Versiones.** Los resultados del texto usan la versión {c5p['version']} del libro de cinco ejes y la {ctp['version']} del termómetro. El libro de cinco ejes tuvo antes una versión piloto ({c5pil['version']}; {miles(c5pil['textos'])} textos, del 30 de septiembre al 7 de octubre).

**Costo.** Estimado con los tokens de cada corrida y los precios de lista, en dólares por millón de tokens: gpt-6-luna, 0,10 de entrada y 0,50 de salida, con la mitad en el Batch; deepseek-v4-flash, 0,30 y 1,20. La corrida completa de cinco ejes costó unos {f(usd(c5p), 2)} USD; su validación con DeepSeek, {f(usd(c5v), 2)}; el termómetro con gpt-6-luna, {f(usd(ctp), 2)}; el piloto, {f(usd(c5pil) + usd(cor[('Cinco ejes (piloto, versión anterior)', 'validador')]), 2)}. El total es del orden de 1 a 2 USD. No incluye el validador del termómetro, cuyo registro no guardó tokens, ni el descuento por los tokens de entrada en caché de OpenAI.

: Esquema de salida de la codificación de cinco ejes. {{#tbl-esquema-ejes}}

{tabla_esquema("41_esquema_cinco_ejes.csv")}

: Esquema de salida del termómetro. {{#tbl-esquema-termometro}}

{tabla_esquema("41_esquema_termometro.csv")}

**Prompt de sistema de la codificación de cinco ejes** (OpenAI y DeepSeek; los campos de la lista de variables son los de la @tbl-esquema-ejes).

```
{p3}
```

Para DeepSeek, que no tiene salida estructurada, se agrega al final:

```
{cola3}
```

**Prompt de sistema del termómetro** (OpenAI y DeepSeek).

```
{p15}
```

Para DeepSeek se agrega al final:

```
{cola15}
```

[PENDIENTE: resultados de la validación humana por eje.]"""

viejo_E = ("## E. Método {.unnumbered}\n\n[ACTUALIZAR: prompt completo de las dos codificaciones; modelos, versiones, fechas y parámetros; esquema de salida; costo.]\n\n"
           "[PENDIENTE: resultados de la validación humana por eje.]")
t = reemplazar(t, viejo_E, apendice)

# ---- 13. Opción de formato para que los prompts largos se lean en el PDF ----------------
t = reemplazar(t, "    format-links: false\n", "    format-links: false\n    code-overflow: wrap\n")
t = reemplazar(
    t,
    "    fig-pos: \"H\"\n---",
    "    fig-pos: \"H\"\n    include-in-header:\n      text: |\n        \\usepackage{fvextra}\n        \\RecustomVerbatimEnvironment{verbatim}{Verbatim}{breaklines=true}\n---",
)

(RAIZ / "articulo.qmd").write_text(t, encoding="utf-8")
import re as _re
quedan = _re.findall(r"\[(?:ACTUALIZAR|PENDIENTE)[^\]]*\]|<!-- VERIFICAR[^>]*-->", t)
print("Marcadores que quedan:")
for q in quedan:
    print(" -", q[:110])
