#!/usr/bin/env python3
"""Discursos presidenciales de Kast: léxico, alusiones y tendencia Kaiser vs Kast.

Usa el *cuerpo* oral (no el titular de Presidencia, que casi siempre dice “Kast”).
Gobierno Kast = 2026-03-11 en adelante. Prensa = corpus unido 2015–2026 (títulos).

CSV para ggplot2 (`03_kast_alusiones.R`). Uso (desde la raíz):
  python3 analysis/discursos_presidenciales/03_kast_alusiones.py
  Rscript analysis/discursos_presidenciales/03_kast_alusiones.R
"""
from __future__ import annotations

import re
import sys
import unicodedata
from datetime import date
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "data" / "scripts"))
from build_canonicos import GOBIERNO_KAST, fold, parse_fecha_es  # noqa: E402

OUT_CANON = ROOT / "data" / "processed" / "canon"
OUT_PRENSA = ROOT / "data" / "processed" / "prensa"
RAW_DISC = ROOT / "data" / "raw" / "discursos" / "presidencia" / "discursos.csv"
PRENSA_UNIDA = ROOT / "data" / "processed" / "prensa" / "prensa_unida.parquet"

STOP = {
    "de", "la", "el", "en", "y", "a", "los", "del", "las", "se", "un", "una",
    "que", "por", "con", "para", "como", "al", "lo", "su", "sus", "es", "mas",
    "no", "o", "tambien", "este", "esta", "estos", "estas", "hay", "son", "ser",
    "ha", "han", "hemos", "muy", "ya", "si", "pero", "cuando", "donde", "sobre",
    "entre", "desde", "hasta", "sin", "nos", "hoy", "aqui", "asi", "todo",
    "todos", "todas", "cada", "fue", "era", "tiene", "tienen", "hacer", "puede",
    "pueden", "debe", "estan", "estamos", "esto", "porque", "pues", "entonces",
    "ademas", "solo", "mismo", "chile", "chilenos", "chilenas", "pais",
    "gobierno", "presidente", "republica", "antonio", "jose", "boric",
    "gabriel", "tenemos", "nosotros", "ustedes", "usted", "anos", "ano", "dia",
    "dias", "vez", "veces", "menos", "mejor", "mayor", "sido", "siendo", "hace",
    "mucho", "estar", "bien", "tiempo", "poder", "punto", "caso", "parte",
    "manera", "forma", "hecho", "decir", "ahora", "despues", "antes", "siempre",
    "gran", "gracias", "vamos", "voy", "tema", "temas", "bueno", "buena",
    "muchas", "muchos", "quiero", "creo", "claro", "senor", "senora", "ministros",
    "ministro", "companeros", "amigo", "amigos", "queridos", "estimados",
    "algo", "alguien", "algunos", "algunas", "cosas", "cosa", "decia", "dice",
    "esos", "esas", "ese", "esa", "tanto", "distintas", "distintos", "nuestra",
    "nuestro", "nuestros", "nuestras", "tener", "podemos", "importante",
    "aqui", "alli", "ahi", "hacia", "traves", "mediante", "durante",
    "queremos", "querer", "queria", "querian",
    "necesitamos", "necesito", "necesita", "necesitan", "necesario",
    "haciendo", "haber", "habia", "habian", "habra",
    "quien", "quienes", "otro", "otros", "otra", "otras",
    "adelante", "solamente", "tambien",
    "nuevo", "nueva", "nuevos", "nuevas",
    "vida", "hacerlo", "podamos", "debemos", "tenemos",
}

# Prefijos: “necesita*”, “querem*”, etc. (ruido oral, no léxico de política)
STOP_PREFIX = (
    "necesit", "querem", "haciend", "haber", "habia", "adelant",
    "solament", "quien", "otro", "otra", "nuevo", "nueva",
)

# Cuerpo oral — no el titular de sala de prensa.
ALUSIONES = [
    ("empleo / trabajo", r"\bempleo\b|\btrabajo\b|\btrabajadores?\b"),
    ("seguridad", r"seguridad|delincuencia|narcotrafico|crimen organizado"),
    ("familia", r"\bfamilias?\b"),
    ("inversión", r"inversion|emprendedor|emprendimiento"),
    ("Carabineros", r"carabineros"),
    ("Fuerzas Armadas", r"fuerzas armadas|ejercito"),
    ("libertad", r"\blibertades?\b|libre mercado|libertad economica"),
    ("migración", r"migracion|migrantes|frontera"),
    ("orden", r"\borden\b|estado de derecho"),
    ("Dios / fe", r"\bdios\b|fe cristiana|\biglesia\b"),
    ("impuestos", r"impuesto"),
    ("Araucanía", r"araucania|macrozona"),
    ("sociedad civil", r"sociedad civil|cuerpos intermedios|juntas? de vecinos"),
    ("Venezuela", r"venezuela|\bmaduro\b"),
    ("gremios", r"gremialismo|gremialista|\bgremios\b"),
    ("Kast (se nombra)", r"\bkast\b"),
    ("Piñera", r"\bpinera\b"),
    ("Boric", r"\bboric\b"),
    ("permisología", r"permisolog"),
    ("Matthei", r"\bmatthei\b"),
    ("Kaiser", r"\bkaiser\b"),
    ("subsidiariedad", r"subsidiariedad"),
    ("Pinochet", r"\bpinochet\b"),
    ("Jaime Guzmán", r"jaime guzman"),
]

DICC = [
    ("A1", "A", "Subsidiariedad / cuerpos intermedios",
     r"subsidiariedad|cuerpos intermedios|gremios|bien comun|juntas? de vecinos|"
     r"gremialismo|gremialista|sociedad civil|tejido social|"
     r"organizaciones intermedias|asociatividad|principio de subsidiariedad"),
    ("A2", "A", "Orden institucional",
     r"orden publico|seguridad juridica|estado de derecho|estabilidad|autoridad|"
     r"mano dura|orden institucional|certeza juridica"),
    ("A3", "A", "Familia / moral",
     r"familia|matrimonio|valores|tradicion cristiana|dignidad humana|"
     r"familia tradicional|valores familiares"),
    ("A4", "A", "Antimarxismo / antiestatismo",
     r"asistencialismo|dependencia estatal|populismo|marxismo|estatismo|estado interventor"),
    ("A5", "A", "Economía libre",
     r"libre mercado|propiedad privada|emprendimiento|inversion|"
     r"subsidiariedad economica|libertad economica|economia libre"),
    ("C1", "C", "Securitización",
     r"narcotrafico|mano dura|tolerancia cero|delincuencia|crisis de seguridad|"
     r"seguridad|crimen organizado|terrorismo"),
    ("C2", "C", "Migración",
     r"migracion ilegal|crisis migratoria|frontera|migrantes|migracion|"
     r"inmigracion|extranjeros ilegales|control fronterizo"),
    ("C3", "C", "Guerra cultural",
     r"ideologia de genero|woke|adoctrinamiento|identidad de genero|"
     r"enfoque de genero|cultura woke|cancelacion|educacion sexual integral"),
    ("C4", "C", "Anti-élite",
     r"elite globalista|academia desconectada|casta politica|\bcasta\b|"
     r"globalismo|las elites|elite politica|pueblo versus elite"),
    ("C5", "C", "Civilización",
     r"valores occidentales|chile primero|occidente cristiano|hispanidad|"
     r"identidad nacional|nacion chilena"),
    ("D1", "D", "Libertario puro",
     r"estado minimo|libertad individual|libertario|partido nacional libertario|"
     r"\bpnl\b|anarcocapitalismo"),
]


def load_kast_speeches() -> pd.DataFrame:
    df = pd.read_csv(RAW_DISC)
    df["fecha"] = df["fecha"].map(parse_fecha_es)
    df["titulo"] = df["titulo"].fillna("").astype(str)
    df["discurso"] = df["discurso"].fillna("").astype(str)
    df = df[df["fecha"].notna()]
    df = df[df["fecha"] >= GOBIERNO_KAST]
    df = df[~df["titulo"].str.contains("Boric", case=False, regex=False)]
    df = df[df["discurso"].str.len() > 80]
    df["txt"] = df["discurso"].map(fold)
    df["mes"] = pd.to_datetime(df["fecha"]).dt.to_period("M").dt.to_timestamp()
    return df.reset_index(drop=True)


def top_palabras(kast: pd.DataFrame, n: int = 30) -> pd.DataFrame:
    tok_re = re.compile(r"[a-z]{4,}")
    cnt: dict[str, int] = {}
    for t in kast["txt"]:
        for w in tok_re.findall(t):
            if w in STOP or w.startswith(STOP_PREFIX):
                continue
            cnt[w] = cnt.get(w, 0) + 1
    rows = sorted(cnt.items(), key=lambda x: -x[1])[:n]
    return pd.DataFrame(rows, columns=["palabra", "n"])


def alusiones(kast: pd.DataFrame) -> pd.DataFrame:
    rows = []
    n = len(kast)
    for etiqueta, pat in ALUSIONES:
        hit = kast["txt"].str.contains(pat, regex=True)
        rows.append({"etiqueta": etiqueta, "n": int(hit.sum()), "pct": 100 * hit.mean(),
                     "n_discursos": n})
    return pd.DataFrame(rows)


def repertorios(kast: pd.DataFrame) -> pd.DataFrame:
    rows = []
    n = len(kast)
    for codigo, familia, etiqueta, pat in DICC:
        hit = kast["txt"].str.contains(pat, regex=True)
        rows.append({
            "codigo": codigo, "familia": familia, "etiqueta": etiqueta,
            "n": int(hit.sum()), "pct": 100 * hit.mean(), "n_discursos": n,
        })
    return pd.DataFrame(rows)


def mes_kast_kaiser(kast: pd.DataFrame) -> pd.DataFrame:
    g = kast.groupby("mes", as_index=False).agg(
        n_disc=("txt", "size"),
        n_kast=("txt", lambda s: int(s.str.contains(r"\bkast\b").sum())),
        n_kaiser=("txt", lambda s: int(s.str.contains(r"\bkaiser\b").sum())),
    )
    g["pct_kast"] = 100 * g["n_kast"] / g["n_disc"]
    g["pct_kaiser"] = 100 * g["n_kaiser"] / g["n_disc"]
    return g


def prensa_kast_kaiser() -> pd.DataFrame:
    pr = pd.read_parquet(PRENSA_UNIDA, columns=["year", "titulo"])
    pr = pr[pr["year"].between(2015, 2026)]
    tit = pr["titulo"].fillna("").map(fold)
    pr = pr.assign(
        dice_kast=tit.str.contains(r"\bkast\b", regex=True),
        dice_kaiser=tit.str.contains(r"\bkaiser\b", regex=True),
    )
    anio = (
        pr.groupby("year", as_index=False)
        .agg(n=("year", "size"), n_kast=("dice_kast", "sum"), n_kaiser=("dice_kaiser", "sum"))
    )
    anio["pct_kast"] = 100 * anio["n_kast"] / anio["n"]
    anio["pct_kaiser"] = 100 * anio["n_kaiser"] / anio["n"]
    return anio


def kaiser_context(kast: pd.DataFrame) -> str:
    hit = kast[kast["txt"].str.contains(r"\bkaiser\b")]
    if hit.empty:
        return ""
    t = hit.iloc[0]["txt"]
    i = t.find("kaiser")
    return t[max(0, i - 120): i + 140]


def main() -> int:
    OUT_CANON.mkdir(parents=True, exist_ok=True)
    OUT_PRENSA.mkdir(parents=True, exist_ok=True)

    kast = load_kast_speeches()
    print(f"Discursos Kast: {len(kast)}  {kast['fecha'].min()} → {kast['fecha'].max()}")

    top = top_palabras(kast)
    al = alusiones(kast)
    rep = repertorios(kast)
    mes = mes_kast_kaiser(kast)

    al.to_csv(OUT_CANON / "discursos_kast_alusiones.csv", index=False)
    mes.to_csv(OUT_CANON / "discursos_kast_kaiser_mes.csv", index=False)
    rep.to_csv(OUT_CANON / "discursos_kast_repertorios.csv", index=False)
    top.to_csv(OUT_CANON / "discursos_kast_top_palabras.csv", index=False)

    ctx = kaiser_context(kast)
    print("\nAlusiones (% discursos):")
    print(al.sort_values("pct", ascending=False).to_string(index=False, formatters={"pct": "{:.1f}".format}))
    print("\nRepertorios:")
    print(rep.sort_values("pct", ascending=False).to_string(index=False, formatters={"pct": "{:.1f}".format}))
    print("\nKast vs Kaiser por mes (cuerpo):")
    print(mes.to_string(index=False, formatters={"pct_kast": "{:.1f}".format, "pct_kaiser": "{:.1f}".format}))
    if ctx:
        print("\nÚnica mención a Kaiser:", ctx.replace("\n", " "))

    if PRENSA_UNIDA.exists():
        print("\nPrensa unida: Kast vs Kaiser por año (título)…")
        anio = prensa_kast_kaiser()
        anio.to_csv(OUT_PRENSA / "prensa_kast_kaiser_anio.csv", index=False)
        print(anio.to_string(index=False, formatters={
            "pct_kast": "{:.2f}".format, "pct_kaiser": "{:.2f}".format,
        }))
    else:
        print("No hay prensa_unida.parquet — corre python3 data/scripts/merge_fondecyt_neo.py")

    print("CSV listos → ggplot2: Rscript analysis/discursos_presidenciales/03_kast_alusiones.R")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
