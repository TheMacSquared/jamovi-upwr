# Build natywny macOS (Apple Silicon / arm64)

Instrukcja budowania `jUPWR.app` (i `.dmg`) na macOS arm64. Komendy zebrane w skryptach
`packaging/scripts/macos/*.sh` — ten dokument je objaśnia i opisuje napotkane problemy.

> **Status:** przetestowane end-to-end w sesji 2026-06 na macOS 26.5 (Tahoe), arm64, R 4.6.0.
> Potwierdzono (wariant DEV, z systemowym R):
> - wszystkie komponenty budują się (klient, 6 modułów, i18n, silnik C++, serwer Python),
> - `jUPWR.app` startuje: serwer nasłuchuje (`listening across origin(s)`), klient i i18n
>   serwowane przez HTTP (200), brak tracebacków,
> - silnik ładuje R + RInside + moduł `jmv` (+ zależności) — `library(jmv)` OK,
> - modyfikacje forka obecne w buildzie (np. opcja `boxMean`),
> - `jUPWR-<wersja>-arm64.dmg` tworzy się i poprawnie montuje (~180 MB, bez wbudowanego R).
>
> **Relokowalność (Faza R) — ZROBIONE i przetestowane** (`55-relocate.sh`): wbudowany R +
> python-build-standalone + dyliby Homebrew; w trybie wymuszonego bundla silnik ładuje R+jmv i
> wszystkie natywne dyliby z bundla (potwierdzone `DYLD_PRINT_LIBRARIES`). `.app` ~1.5 GB, `.dmg`
> ~600 MB. Pozostaje walidacja na FIZYCZNIE czystym Macu + ograniczenie eksportu wektorowego (X11) —
> patrz sekcja „Relokowalność".

## TL;DR

```bash
cd packaging/scripts/macos
./00-prereqs.sh        # Homebrew: boost, nanomsg, protobuf, python@3.12, create-dmg
./10-client.sh         # vite build -> stage/jamovi/client
./20-modules.sh        # jmvcore + jmc install (jmv, scatr, jperm, jCI, jboot, distrACTION)
./25-i18n.sh           # kompilacja tłumaczeń
./40-engine.sh         # kompilacja jamovi-engine (C++)
./30-server.sh         # venv 3.12 + requirements + jamovi.core/server/readstat
./50-assemble-app.sh   # montaż jUPWR.app (wariant DEV: systemowy R/Homebrew)
./55-relocate.sh       # RELOKACJA: wbuduj R + Python-standalone + dyliby (czysta maszyna)
./60-package-dmg.sh    # jUPWR.dmg
```

> `55-relocate.sh` jest WYMAGANY do dystrybucji studentom. Pomiń go tylko do szybkiego testu na
> maszynie deweloperskiej.

Katalog roboczy: `packaging/build/` (ignorowany przez git). Wynik: `packaging/build/dist/`.

---

## Wymagania wstępne (Faza A)

`00-prereqs.sh` instaluje przez Homebrew i weryfikuje. Zweryfikowane wersje (ta sesja):

| Narzędzie | Wersja | Skąd |
|---|---|---|
| R | 4.6.0 (R.framework) | instalator z r-project.org |
| Python | 3.12.13 | `brew install python@3.12` |
| boost | 1.90.0 | brew |
| nanomsg | 1.2.2 | brew |
| protobuf | 35.1 (+ abseil) | brew |
| node / npm | 24.17 / 11 | — |
| clang | 21 (Xcode CLT) | `xcode-select --install` |
| create-dmg | 1.2.3 | brew |

Dodatkowo do biblioteki **systemowego** R doinstalowane (binarki CRAN arm64):
`RInside, emmeans, vcd, vcdExtra, multcomp` (reszta zależności modułów była już obecna —
lokalny R miał 301 pakietów). RInside dostarcza `libRInside.dylib` wymagane przez silnik.

```bash
Rscript -e 'install.packages(c("RInside","emmeans","vcd","vcdExtra","multcomp"), repos="https://cloud.r-project.org")'
```

---

## Klient (Faza B) — `10-client.sh`

`npm install` w `client/`, potem `build:coms` (kopiuje `jamovi.proto` → `assets/coms.proto`)
i `vite build --outDir stage/jamovi/client`. Wynik ~5.6 MB, zawiera `index.html`,
`analysisui.html`, `resultsview.html`. Bezproblemowe.

## Moduły R (Faza C) — `20-modules.sh`

1. `R CMD INSTALL jmvcore --library=stage/jamovi/modules/base/R`
2. dla każdego modułu: `node jamovi-compiler/index.js --install <mod> --to stage/jamovi/modules
   --rhome <R.framework/Resources> --rlibs stage/jamovi/modules/base/R --patch-version --skip-deps`

`--skip-deps` zakłada, że zależności (ggplot2, car, multcomp, emmeans, vcd…) są w bibliotece
systemowego R — tak jak w Dockerze pochodzą z obrazu `jamovi-deps`. Moduł `plots` (submoduł
jmvplots) instaluje się pod nazwą **`scatr`**. Zainstalowane: `jmv, scatr, jperm, jCI, jboot,
distrACTION` + `base` (jmvcore).

### Pułapka: `jmc` izoluje `R_LIBS` — zależności modułu trzeba doinstalować do `base/R`

`jamovi-compiler/compilerr.js` ustawia przy `R CMD INSTALL` modułu `R_LIBS_SITE=<--rlibs>` i
`R_LIBS_USER=notthere`, więc **systemowa biblioteka R.framework jest niewidoczna**, mimo że
`00-prereqs.sh` każe doinstalować tam brakujące pakiety. Dla modułu `plots` (ridge/hexbin) to
oznacza, że `ggridges`/`hexbin` muszą trafić do `stage/jamovi/modules/base/R` (obok `jmvcore`),
nie do systemowego R — inaczej `jmc --install plots` pada z `dependency 'hexbin' is not available
for package 'scatr'`, a moduł `scatr` buduje się bez katalogu `R/` (silnik potem zgłasza `nie ma
pakietu o nazwie 'scatr'` przy pierwszym użyciu wykresu). `20-modules.sh` robi to automatycznie
przed instalacją modułów i sam sprawdza na końcu, że każdy zainstalowany moduł ma `modules/<m>/R`.
Przy dodawaniu kolejnego modułu z zależnościami spoza `00-prereqs.sh`/systemowego R — dopisz
analogiczny `install.packages(..., lib=$BASE_R)` w `20-modules.sh`.

---

## Silnik C++ (Faza E) — `40-engine.sh` — NAJTRUDNIEJSZE

`engine/Makefile.in` ma gotowy blok OSX (clang++, `-framework Foundation`, RInside), ale jest
pisany pod starsze środowisko. Trzy problemy i ich rozwiązania (BEZ modyfikacji plików `engine/`
— są na liście „NIE MODYFIKOWAĆ"):

### Problem 1 — protobuf 35 wymaga C++17, Makefile wymusza `-std=c++11`
protobuf ≥ 22 ciągnie **abseil**, którego nagłówki wymagają C++17. Kompilacja waliła się na
`no member named 'conjunction' in namespace 'std'`.
**Fix:** nadpisanie `CXXFLAGS` w wierszu poleceń `make` — w GNU make zmienna z wiersza poleceń
wygrywa, więc `CXXFLAGS += -std=c++11` z Makefile jest ignorowane. `LDFLAGS` z `configure`
(R, boost, RInside, frameworks) zostają nietknięte.

### Problem 2 — abseil/protobuf wymaga wielu bibliotek przy linkowaniu
**Fix:** `pkg-config --cflags --libs protobuf` zwraca pełną listę (`-labsl_*`, `-DPROTOBUF_USE_DLLS`).
Dorzucamy ją do `CXXFLAGS` (jest też na linii linkowania).

### Problem 3 — `boost_system` i `boost_nowide`
Boost ≥ 1.69 ma **boost_system jako header-only** — brew Boost 1.90 nie ma `libboost_system`,
a Makefile linkuje `-lboost_system` (`ld: library 'boost_system' not found`).
Kod używa też `boost::nowide::setenv` (Makefile linkuje `-lboost_nowide` tylko w bloku Windows).
**Fix:** (a) pusty stub `libboost_system.a` na ścieżce linkera (symbole są inline):
```bash
echo 'static int s;' > stub.c && clang -c stub.c -o stub.o && ar rcs libboost_system.a stub.o
```
(b) dodanie `-lboost_nowide` do flag linkowania.

Po tych poprawkach `make` przechodzi i powstaje `jamovi-engine`. `otool -L` pokazuje zależności
od Homebrew (abseil, protobuf, boost_filesystem, nanomsg), R.framework (`libR.dylib` po ścieżce
absolutnej) oraz `libRInside.dylib` (sama nazwa — rozwiązywana przez `DYLD_FALLBACK_LIBRARY_PATH`).

---

## Serwer Python (Faza D) — `30-server.sh`

`server/setup.py` (blok Darwin) też linkuje `boost_system` i wskazuje nieistniejący `../Frameworks`.
**Fix:** zmienne środowiskowe przy buildzie: `CFLAGS=-I/opt/homebrew/include`,
`LDFLAGS="-L/opt/homebrew/lib -L<stublib>"` (stub boost_system jak wyżej).

Kroki (mirror Dockerfile):
1. `python3.12 -m venv --copies stage/jamovi/python`
2. `pip install -r server/requirements.txt`
3. `readstat`: `setup.py install --install-lib=stage/jamovi/server`
4. `jamovi.core`: `SETUP_CORE_ONLY=1 setup.py install …` (Cython → `core.cpython-312-darwin.so`)
5. `jamovi.server`: `SETUP_SERVER_ONLY=1 setup.py install …`

Weryfikacja: `PYTHONPATH=stage/jamovi/server python -c "import jamovi.server, jamovi.core"` → OK.

> **Uwaga o venv:** venv `--copies` jest przenośny między katalogami (pyvenv.cfg wskazuje bazowy
> Python z Homebrew, site-packages jadą razem). Działa po skopiowaniu do `.app`. Dla pełnej
> niezależności od Homebrew → python-build-standalone (patrz Relokowalność).

---

## ⚠️ Pułapka: pusta wstążka analiz/wykresów — `JAMOVI_R_VERSION`

Objaw: aplikacja startuje, dane się wczytują, ale zakładki **Analizy** i **Wykresy** są PUSTE
(tylko przycisk „Moduły").

Przyczyna: serwer oznacza moduł jako `incompatible`, gdy `rVersion` modułu ≠ `JAMOVI_R_VERSION`
aplikacji (`server/.../modules/modules.py` ~l.487; `appinfo.py: determine_r_version`). Gdy
`JAMOVI_R_VERSION` **nie jest ustawione** w env.conf, na macOS przyjmuje domyślnie `3.3.0`, a moduły
mają np. `4.5.0-arm64` → wszystkie niezgodne → `in_menu=False` → pusta wstążka. (Build Docker ustawia
`JAMOVI_R_VERSION`, więc o tym łatwo zapomnieć w buildzie natywnym.)

Fix: env.conf MUSI zawierać `JAMOVI_R_VERSION` równe `rVersion` z `modules/jmv/jamovi-full.yaml`
(skrypty `50`/`55` czytają to automatycznie). Weryfikacja serwerową logiką:
```python
from jamovi.server.utils import conf; conf.init()
from jamovi.server.appinfo import app_info
print(app_info.r_version)   # musi == rVersion modułów (np. 4.5.0-arm64)
```

## ⚠️ Pułapka: każda analiza pada („limit zasobów") — RProtoBuf

Objaw: wstążka pełna, ale URUCHOMIENIE dowolnej analizy → „Analiza przerwana, prawdopodobnie z powodu
przekroczenia limitu zasobów" (silnik ginie i restartuje). `jmv::ANOVA()` w czystym R działa — pada
tylko w aplikacji. To **najtrudniejszy** problem natywnego buildu.

Przyczyna: silnik serializuje wyniki przez pakiet R **`RProtoBuf`** (`jmvcore: RProtoBuf_serialize <-
RProtoBuf::serialize`). Trzy osobne wymogi, wszystkie muszą być spełnione:

1. **RProtoBuf musi istnieć** — bez niego: `Error: Could not load RProtoBuf`.
2. **RProtoBuf musi dzielić TEN SAM protobuf co silnik.** Binarka CRAN statycznie bundluje własny
   protobuf → w procesie silnika (protobuf 35 z C++) są DWIE instancje → **SIGSEGV** (crash report
   pełen „protobuf"). Fix: budować RProtoBuf **ze źródła** przeciw protobuf Homebrew (dynamiczny
   `libprotobuf.35.dylib`), z `-std=c++17` i flagami abseil; pominąć wadliwe testy nagłówków configure
   (`ac_cv_header_google_protobuf_*=yes`). Wtedy `otool -L RProtoBuf.so` pokazuje
   `/opt/homebrew/.../libprotobuf.35.dylib` (wspólny z silnikiem).
3. **RProtoBuf musi być zainstalowany PRZED jmvcore.** jmvcore pieczętuje `RProtoBuf_serialize` przy
   instalacji (lazy-load). Jeśli RProtoBuf jeszcze nie było → binding = NULL na stałe → przy analizie
   `could not find function "RProtoBuf_serialize"`. Po dodaniu RProtoBuf trzeba **przeinstalować jmvcore**.

RProtoBuf instalujemy do `modules/base/R` (pewna ścieżka `.libPaths` silnika). Wszystko to robi
`20-modules.sh` (RProtoBuf → base/R, potem jmvcore). `55-relocate.sh` zbiera dyliby RProtoBuf do `libs/`.
Weryfikacja: `library(RProtoBuf)` OK + `is.function(jmvcore:::RProtoBuf_serialize)` == TRUE +
analiza w GUI liczy się i renderuje.

> Docker tego nie ma, bo używa JEDNEGO systemowego protobuf (3.21) dzielonego przez silnik, Pythona
> i RProtoBuf, a deps (w tym RProtoBuf) instaluje z repo jamovi przed kompilacją modułów.

## Montaż .app (Faza F) — `50-assemble-app.sh`

Kluczowe ustalenia o układzie (z `electron/app/main.js` + `server/.../utils/conf.py` + `engine.py`):

- `main.js` czyta `env.conf` względem katalogu binarki (`Contents/MacOS`), fallback `../Resources/env.conf`.
- `conf.py` (serwer) czyta `JAMOVI_HOME/Resources/env.conf` i rozwija ścieżki względem `JAMOVI_HOME/bin`.
- **Oba zgadzają się**, gdy `JAMOVI_HOME=..` (czyli `Contents`) a ścieżki zaczynają się od
  `../Resources/...`: bo `MacOS/..` == `bin/..` == `Contents` (leksykalnie, normpath).
- `engine.py` na Darwin szuka silnika w `JAMOVI_HOME/MacOS/jamovi-engine` → silnik kładziemy
  w `Contents/MacOS/jamovi-engine` (obok binarki Electrona).

Kroki:
1. `npm install electron@<ver>` → `Electron.app`.
2. kopia → `dist/jUPWR.app`, binarka `MacOS/Electron` → `MacOS/jUPWR`, Info.plist
   (`CFBundleExecutable/Name/DisplayName=jUPWR`, `CFBundleIdentifier=pl.upwr.jupwr`).
3. nasz kod: `@electron/asar pack electron/app Contents/Resources/app.asar` (zastępuje `default_app.asar`).
4. payload → `Contents/Resources/jamovi/{client,server,python,modules,i18n,version}`;
   `jamovi-engine` → `Contents/MacOS/`.
5. `env.conf` → `Contents/Resources/env.conf` (wariant DEV: `R_HOME` = absolutna ścieżka
   systemowego R, `DYLD_FALLBACK_LIBRARY_PATH` → `R/lib` + `RInside/lib`).
6. `codesign --force --deep --sign -` (podpis ad-hoc, bez certyfikatu).

Uruchomienie do debugowania (widać logi serwera/silnika):
```bash
"packaging/build/dist/jUPWR.app/Contents/MacOS/jUPWR"
```

---

## Relokowalność (Faza R) — `55-relocate.sh` — ZAIMPLEMENTOWANE I PRZETESTOWANE

Wariant DEV zależy od ścieżek tej maszyny (`/opt/homebrew`, `/Library/Frameworks/R.framework`).
`55-relocate.sh` przekształca gotową `.app` w **samowystarczalną**, działającą na czystym Macu
(bez Homebrew, bez R). Uruchamiać PO `50-assemble-app.sh`, PRZED `60-package-dmg.sh`.

### Mechanizm: DYLD zamiast install_name_tool
Kluczowa obserwacja: gdy dyld nie znajdzie zależności pod jej **ścieżką absolutną** (bo na czystej
maszynie `/opt/homebrew/...` ani `/Library/Frameworks/R.framework/...` nie istnieją), szuka jej w
katalogach z `DYLD_LIBRARY_PATH` / `DYLD_FALLBACK_LIBRARY_PATH` **po samej nazwie pliku**. Wystarczy
więc wrzucić wszystkie potrzebne dyliby do katalogów bundla i wskazać je w env.conf — bez przepisywania
setek `install_name` (co byłoby konieczne dla ~hundred plików `.so` pakietów R linkujących `libR`).

env.conf (relokowalny) ustawia (ścieżki względne wobec binarki):
```
DYLD_LIBRARY_PATH=../Resources/jamovi/R/lib:../Resources/jamovi/R/library/RInside/lib:../Resources/jamovi/libs
DYLD_FALLBACK_LIBRARY_PATH=...te same...:/usr/lib:/usr/local/lib
```
`DYLD_LIBRARY_PATH` (a nie tylko FALLBACK) **wymusza nasze wersje** libów — chroni przed maszyną,
która ma Homebrew/R w tych samych ścieżkach, ale w innej wersji (ryzyko ABI). Działa, bo `main.js`
przekazuje env do serwera, a `engine.py` kopiuje `os.environ` do silnika; podpis **ad-hoc bez
hardened runtime** nie blokuje zmiennych `DYLD_*`.

### Co skrypt wbudowuje
1. **Python** → `python-build-standalone` 3.12.13 (relokowalny, bez zależności od Homebrew),
   z zainstalowanym `requirements.txt` + `protobuf`. Ten sam ABI co `core.so` (3.12.13) → bez przebudowy.
2. **Dyliby Homebrew** (rekurencyjnie z `jamovi-engine` i `core.so`) → `Resources/jamovi/libs`
   (~85 plików, 9.5 MB: abseil, protobuf, boost_filesystem, **boost_atomic** [zależność
   `@loader_path`], boost_nowide, nanomsg). Kolektor obsługuje `/opt/homebrew/*`, `@loader_path/*`, `@rpath/*`.
3. **R.framework** → `Resources/jamovi/R` (~886 MB; rsync z wykluczeniem doc/tests/html). `R_HOME`
   względny. Silnik używa R przez **RInside** (respektuje env `R_HOME`) — uwaga: `bin/R`/`bin/Rscript`
   IGNORUJĄ env `R_HOME` (mają wbudowaną ścieżkę), ale jamovi ich nie używa.

### Weryfikacja (przeprowadzona)
- **Wymuszony bundle** (`DYLD_LIBRARY_PATH` = tylko katalogi bundla) — silnik inicjalizuje R,
  ładuje `jmv` i wszystkie natywne dyliby **z bundla** (potwierdzone `DYLD_PRINT_LIBRARIES`:
  `libR`, `libRInside`, `libabsl_*`, `libprotobuf`, `libboost_*` ładowane z `Resources/jamovi/...`).
- Pełna `.app` w trybie wymuszonego bundla: serwer nasłuchuje, klient + i18n HTTP 200.
- Statyczne domknięcie: **0** pakietów R linkuje `/opt/homebrew`; wszystkie linkują `R/lib` (w bundlu).
- Render PNG przez `ragg::agg_png` działa bez X11.

### Znane ograniczenie (bez podpisu/X11)
- **Eksport wykresów do SVG/PDF/EPS** używa `cairo` (`grDevices::svg`, `cairo_pdf`), który linkuje
  **X11** (`/opt/X11`) — niedostępne bez XQuartz. **Wyświetlanie** wykresów w wynikach działa (PNG/ragg).
  Opcje: (a) zostawić jako ograniczenie, (b) dobundlować XQuartz, (c) zmienić eksport na `svglite`.
- Pakiet `tcltk` linkuje X11/Tcl — jamovi go nie używa (nieistotne).

### Rozmiar
`.app` ~1.5 GB (R 886M + Python 322M), `.dmg` ~600 MB. Optymalizacja: przyciąć bibliotekę R do
domknięcia zależności modułów (`tools::package_dependencies`) zamiast wszystkich ~300 pakietów.
