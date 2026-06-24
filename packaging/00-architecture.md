# Architektura natywnego bundla jUPWR

Ten dokument opisuje **jak zbudowana jest natywna aplikacja jamovi/jUPWR** na desktopie i co
musi znaleźć się w paczce instalacyjnej. Jest wspólny dla macOS i Windows — różnice platformowe
są zaznaczone.

> Źródło ustaleń: `electron/app/main.js`, `docker/jamovi-Dockerfile`, `platform/ubuntu.conf`,
> `platform/env.conf`.

---

## 1. Z czego składa się działająca aplikacja

jamovi to **nie jeden plik wykonywalny**, tylko zestaw współpracujących komponentów spiętych
powłoką Electrona:

```
┌─────────────────────────────────────────────────────────┐
│ Electron (powłoka GUI)                                   │
│   • okno aplikacji + frontend (klient JS)                │
│   • czyta env.conf, startuje serwer, ładuje http://127…  │
└───────────────┬─────────────────────────────────────────┘
                │ spawn (JAMOVI_SERVER_CMD)
                ▼
┌─────────────────────────────────────────────────────────┐
│ Serwer (Python 3.12, tornado/aiohttp)                    │
│   • jamovi.server — HTTP + protokół coms                 │
│   • jamovi.core   — moduł C/Cython (linkuje nanomsg)     │
│   • readstat      — import SPSS/SAS/Stata                 │
└───────────────┬─────────────────────────────────────────┘
                │ uruchamia procesy silnika
                ▼
┌─────────────────────────────────────────────────────────┐
│ Silnik (C++, jamovi-engine)                              │
│   • wykonuje analizy w R przez RInside/Rcpp              │
│   • linkuje: R, Boost, nanomsg                           │
└───────────────┬─────────────────────────────────────────┘
                │ wywołuje funkcje modułów
                ▼
┌─────────────────────────────────────────────────────────┐
│ R + moduły                                               │
│   • R runtime (ten sam, pod który skompilowano moduły)  │
│   • modules/base/R — jmvcore + WSZYSTKIE zależności      │
│   • modules/<nazwa> — jmv, plots, jperm, jCI, jboot,    │
│     jdistrACTION (skompilowane przez jmc)               │
└─────────────────────────────────────────────────────────┘
```

**Konsekwencja kluczowa:** R, którym budujemy moduły, MUSI trafić do paczki w tej samej wersji.
Nie da się zbudować modułów pod R 4.6 i liczyć, że zadziałają na cudzym R 4.3. Dlatego bundle
wozi własny, kompletny R.

---

## 2. Docelowy układ plików (payload)

W Dockerze wszystko ląduje pod `/usr/lib/jamovi/`. W buildzie natywnym ten sam zestaw pakujemy
**względnie** wobec binarki Electrona. Logiczna zawartość payloadu (niezależna od OS):

| Katalog/plik | Zawartość | Jak powstaje |
|---|---|---|
| `bin/` | binarka Electrona + `env.conf` + `jamovi-engine` | npm electron + `make` (silnik) |
| `client/` | frontend (HTML/JS/CSS) | `vite build` w `client/` |
| `server/` | kod serwera Python (`jamovi/`) | `setup.py` z `server/` + `readstat` |
| `python/` | relokowalny interpreter Python 3.12 + pakiety z `requirements.txt` | python-build-standalone / framework |
| `R/` | relokowalny runtime R | kopia R.framework (macOS) / R-portable (Win) |
| `modules/base/R/` | `jmvcore` + wszystkie zależności pakietowe modułów | `R CMD INSTALL` + `install.packages` |
| `modules/<nazwa>/` | jmv, plots, jperm, jCI, jboot, jdistrACTION | `jmc --install` |
| `i18n/json/` | skompilowane tłumaczenia | `i18n/index.js --build` |
| `version` | numer wersji (np. `2.7.35.0`) | plik `version` z repo |
| ikona | `app-icon.icns` (macOS) / `.ico` (Win) | z `platform/app-icon.*` |

### Układ fizyczny — macOS (`.app`)

```
jUPWR.app/
└── Contents/
    ├── Info.plist            ← CFBundleName=jUPWR, ikona, typy plików .omv
    ├── MacOS/
    │   ├── jUPWR             ← binarka Electrona (przemianowana)
    │   └── env.conf          ← (alternatywnie w Resources/)
    └── Resources/
        ├── env.conf          ← main.js szuka tu, jeśli nie ma obok binarki
        ├── app-icon.icns
        ├── default_app.asar  ← kod aplikacji Electrona (electron/app)
        └── jamovi/           ← payload (client, server, python, R, modules, i18n, bin/jamovi-engine, version)
```

### Układ fizyczny — Windows (NSIS / portable .zip)

```
jUPWR\
├── jUPWR.exe                 ← binarka Electrona (przemianowana)
├── env.conf                  ← obok .exe
├── default_app.asar
├── jamovi-engine.exe
├── resources\               ← electron resources
└── jamovi\                   ← payload (client, server, python, R, modules, i18n, version)
```

---

## 3. Jak Electron znajduje komponenty: `env.conf`

`electron/app/main.js` (`readConfig`, linie ~31–90) szuka `env.conf`:
1. obok binarki: `<execDir>/env.conf`
2. fallback macOS: `<execDir>/../Resources/env.conf`

Następnie **rozwija wszystkie zmienne kończące się na `PATH`, `HOME`, `LIBS`** względem katalogu
binarki (`main.js` linie ~281–290 — `path.resolve(bin, p)`). To jest mechanizm relokowalności:
paczka działa niezależnie od tego, gdzie użytkownik ją rozpakuje.

### Wzór `env.conf` (desktop) — na bazie `platform/ubuntu.conf`

```ini
[ENV]
JAMOVI_HOME=../jamovi
JAMOVI_MODULES_PATH=../jamovi/modules
JAMOVI_CLIENT_PATH=../jamovi/client
JAMOVI_VERSION_PATH=../jamovi/version
JAMOVI_SERVER_CMD=../jamovi/python/bin/python3 -m jamovi.server 0 --slave
JAMOVI_R_VERSION=4.6.0-arm64
R_HOME=../jamovi/R
R_LIBS=../jamovi/modules/base/R
PYTHONPATH=../jamovi/server
# macOS dodatkowo (dynamiczny linker):
DYLD_FALLBACK_LIBRARY_PATH=../jamovi/R/lib
```

> Uwaga: dokładne ścieżki względne zależą od finalnego układu i są dopinane w fazie montażu
> (`50-assemble-app.sh`). Powyższe to wzorzec — wartości potwierdzane empirycznie przy montażu.

Różnica względem `platform/env.conf` (wariant Docker): tamten używa ścieżek **bezwzględnych**
(`/usr/lib/jamovi/...`) i startuje serwer inaczej (`python3 -m jamovi.server 41337 --if=*`).
Desktop używa ścieżek **względnych** + `JAMOVI_SERVER_CMD` z portem `0` (auto) i flagą `--slave`.

---

## 4. Branding jUPWR

- **Tytuł okna** — ustawiany w warstwie klienta/aplikacji (już obecny we forku).
- **Nazwa aplikacji w systemie** — `CFBundleName`/`CFBundleDisplayName` w `Info.plist` (macOS),
  `productName`/metadane NSIS (Windows).
- **Ikona** — `platform/app-icon.icns` (macOS, do wygenerowania z `app-icon.svg`/`.ico`),
  `platform/app-icon.ico` (Windows, już jest).
- **Skojarzenie plików `.omv`** — `platform/jamovi-dataset.xml` + wpisy w `Info.plist`/NSIS.

---

## 5. Najtrudniejsze elementy (mapa ryzyka)

| Element | Dlaczego trudny | Strategia |
|---|---|---|
| Relokowalny R | R.framework ma wkompilowane bezwzględne ścieżki (`R_HOME`, dyld) | kopia framework + `install_name_tool` + względny `R_HOME` w env.conf |
| Kompilacja silnika | linkuje RInside/Rcpp + Boost + nanomsg; arm64/Tahoe | `engine/configure --rhome=… --rpath=…`, potem `otool -L` weryfikacja |
| Pełna biblioteka R | w Dockerze z obrazu `jamovi-deps`; natywnie instalujemy sami | zebranie zależności z `DESCRIPTION` modułów, `install.packages` |
| Relokowalny Python 3.12 | system ma 3.9; potrzebny przenośny 3.12 | brew `python@3.12` lub python-build-standalone |
| Brak podpisu | Gatekeeper/SmartScreen blokują | dokumentacja obejść (zob. `30-distribution.md`) |
