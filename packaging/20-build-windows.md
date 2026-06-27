# Build natywny Windows x64 (R 4.6) — jUPWR

> **STATUS: DZIAŁA, PRZETESTOWANE END-TO-END** (czerwiec 2026). GUI startuje, dane się
> wczytują, analizy liczą się i renderują, wszystkie moduły obecne. Produkuje relokowalny
> `jUPWR\` (portable) działający bez Dockera. Cały proces jest zautomatyzowany w
> [`scripts/windows/build.ps1`](scripts/windows/build.ps1) — ten dokument tłumaczy CO robi,
> CO było trudne i JAK to zweryfikować na nowej maszynie.

Toolchain jest **mieszany** (jak na macOS — różne komponenty, różne kompilatory):
- `jamovi.core` (rozszerzenie Cython) → **MSVC** (VS2022) + Boost 1.84 `vc143`
- silnik C++ (`jamovi-engine.exe`) → **mingw** (RTools45, gcc 14 static.posix) + Boost 1.84 mingw
- moduły R → **R 4.6** + jmc (jamovi-compiler)
- Python bundla → **python-build-standalone 3.12** (relokowalny)

---

## 1. Wymagania (dokładne wersje/ścieżki)

| Składnik | Wersja / lokalizacja | Po co |
|---|---|---|
| **VS2022 Build Tools** | workload „Desktop C++" (MSVC v143, Windows SDK) | kompilacja `jamovi.core` |
| **RTools45** | `C:\rtools45` (gcc 14.2 w `x86_64-w64-mingw32.static.posix\bin`) | silnik, nanomsg, RProtoBuf, **protobuf 29.3 + abseil + protoc** |
| **R 4.6.0** | `C:\Program Files\R\R-4.6.0` | runtime + kompilacja modułów; trafia do bundla |
| **R user-lib** | `%LOCALAPPDATA%\R\win-library\4.6` | tu instalujemy ciężkie zależności (ggplot2, car, lavaan…) |
| **Node.js** | 20+ | vite (klient), jmc, @electron/asar |
| **cmake** | dowolny (np. ze Strawberry Perl) | build nanomsg |
| **Boost 1.84** | `C:\local\boost_1_84_0` (prebuilt **MSVC** `boost_1_84_0-msvc-14.3-64.exe` z SourceForge) | core (vc143) + źródła do buildu mingw |
| Internet | — | CRAN/posit (pakiety R), GitHub (Electron, nanomsg, PBS Python) |
| NSIS (opcjonalnie) | `makensis` | installer .exe (portable .zip nie wymaga) |

**Czego NIE trzeba:** Dockera; osobnego Pythona (bundlowy PBS); ręcznego budowania protobuf/abseil
(są w RTools45); zainstalowanego jamovi (jmc po patchu nie wymaga — patrz pułapka #1).

> **Instalacja Boost MSVC:** pobierz `boost_1_84_0-msvc-14.3-64.exe` (≈197 MB) z
> SourceForge boost-binaries i rozpakuj do `C:\local\boost_1_84_0`
> (`/VERYSILENT /DIR=C:\local\boost_1_84_0`). Daje to nagłówki + `lib64-msvc-14.3` (dla core)
> ORAZ pełne źródła (do buildu wariantu mingw dla silnika).

---

## 2. Szybki start

```powershell
# 1. Sprawdź/ustaw zmienne na górze build.ps1 (RHome, Vcvars, BoostRoot, RtoolsMingw)
# 2. Uruchom (z PowerShell, NIE w trybie -NonInteractive jeśli pierwszy raz npm install)
powershell -ExecutionPolicy Bypass -File packaging\scripts\windows\build.ps1
```

Wynik: `packaging\build\dist\jUPWR\` + `packaging\build\dist\jUPWR-<wersja>-portable-win64.zip`.
Uruchomienie: `packaging\build\dist\jUPWR\bin\jUPWR.exe`.

Katalogi robocze (ignorowane przez git): `packaging\build\{stage,dist,deps,dl}`.

---

## 3. Co robi build (fazy)

1. **Boost mingw** — `bootstrap.bat gcc` + `b2 --layout=system --with-filesystem/system/nowide`
   z tych samych źródeł co prebuilt MSVC → `stage-mingw\lib\libboost_*.a`.
2. **nanomsg** — cmake + mingw → `deps\nanomsg\{bin\libnanomsg.dll, lib\libnanomsg.dll.a, include}`.
3. **Klient** — `npm run build:coms` + `vite build` → `stage\jamovi\client`.
4. **Moduły R** — RInside/Rcpp/**RProtoBuf** (binarne) do `base\R`, **potem** `jmvcore`
   (kolejność: patrz pułapka #6), brakujące zależności do user-lib, `jmc --install` dla
   `jmv plots jperm jCI jboot jdistrACTION`.
5. **i18n** — `i18n\index.js --build` → `stage\jamovi\i18n\json`.
6. **Silnik** — generacja `Makefile` z `Makefile.in`, build w środowisku RTools (make+sh,
   gcc/g++/windres/protoc) ze ścieżkami deps przez zmienne środowiskowe.
7. **Python + core + server** — rozpakowanie PBS 3.12 → `stage\jamovi\python`; `pip install`
   requirements (BEZ linii nanomsg!), `protobuf`; build core (MSVC/vcvars) + server (protoc)
   → `stage\jamovi\server`; vendorowanie + patch bindingu nanomsg; patch formatio.
8. **Montaż** — Electron → `bin\jUPWR.exe` + `resources\app.asar`; silnik + DLL do `bin`;
   kopia R 4.6 + scalenie user-lib do `Frameworks\R\library`; przeniesienie payloadu do
   `Frameworks`/`Resources`; zapis `bin\env.conf`.
9. **Pakowanie** — `Compress-Archive` → portable `.zip`.

Docelowy układ (jak instalka jamovi 2.7.35):
```
jUPWR\
├─ bin\         jUPWR.exe, jamovi-engine.exe, env.conf, libnanomsg.dll, R*.dll, resources\app.asar, (electron DLL)
├─ Frameworks\  R\ (R 4.6 + scalona biblioteka), python\ (PBS 3.12 + nanomsg.dll)
└─ Resources\   modules\{base,jmv,scatr,jperm,jCI,jboot,distrACTION}, client\, i18n\*.json, server\, version
```

---

## 4. Pułapki i rozwiązania (wszystkie napotkane przy pierwszym buildzie)

### #1 — jmc na Windows ignorował `--rhome` i wymagał zainstalowanego jamovi
`jamovi-compiler/index.js` w gałęzi `win32` zawsze wołał `installer.find()` → „jamovi could not be
found!". **Fix (w repo):** gałąź win32 honoruje `--rhome` (jak linux). Dodatkowo `snapshots.js`
nie miał wpisu dla R 4.6 (`snapshot=undefined` → crash) — **dodany wpis `'4.6.0'`**. Wywołanie:
`jmc --install <mod> --rhome <R-4.6> --rlibs "<base/R>;<user-lib>" --assume-app-version <ver> --skip-deps`.

### #2 — jmc nie widział zależności modułów (R_LIBS_USER='notthere')
jmc wyłącza user-lib, a tam są ggplot2/car/… (biblioteka systemowa R 4.6 jest niemal pusta).
**Fix:** user-lib przekazujemy jawnie w `--rlibs`. Brakujące 13 pakietów (multcomp, emmeans, vcd,
vcdExtra, GGally, BayesFactor, psych, GPArotation, afex, mvnormtest, lavaan, ROCR, Hmisc) dociągamy
z posit PPM do user-lib.

### #3 — Boost/nanomsg: batniki nie znajdują sąsiednich `.bat`
Zmienna środowiskowa `NoDefaultCurrentDirectoryInExePath=1` psuje `bootstrap.bat`/`build.bat`/cmake.
**Fix:** czyścić ją w sesji (`set NoDefaultCurrentDirectoryInExePath=` przed komendą). Boost mingw
bootstrapować jako `bootstrap.bat gcc`.

### #4 — nanomsg 1.2 nie kompiluje się pod GCC 14
GCC 14 promuje `-Wincompatible-pointer-types` do błędu (stary kod nanomsg). **Fix:** cmake
`-DCMAKE_C_FLAGS="-fpermissive -w"`.

### #5 — silnik: protobuf 29.3 wymaga C++17 + abseil + bibliotek systemowych
- protobuf ≥22 ciągnie **abseil**, nagłówki wymagają **C++17** (Makefile miał `-std=c++11`).
- linkowanie wymaga **wszystkich** `libabsl_*.a` (cykliczne zależności → `-Wl,--start-group … --end-group`)
  + `utf8_range/utf8_validity` + `-lbcrypt -ldbghelp -lws2_32 -lmswsock -ladvapi32`.

**Fix (engine/Makefile.in, blok Windows):** nazwy boost `layout=system` + `-lboost_atomic`,
`-std=gnu++17`, katalogi `-L` przez zmienne `BOOST_LIBDIR/NANOMSG_DIR/PROTOBUF_DIR`, globalny
`LDFLAGS += $(EXTRA_LIBS)`. Wartości (R_HOME short-path bez spacji, INCLUDES, EXTRA_LIBS z listą
abseil) podaje `build.ps1` przez środowisko. Build URUCHAMIAĆ w środowisku RTools (make z `usr\bin`
+ sh, bo Makefile używa `mkdir -p`).
Wynik importuje tylko `libnanomsg.dll` + `R.dll` (boost/protobuf/abseil/RInside/libstdc++ — statycznie,
dzięki wariantowi `static.posix`).

### #6 — RProtoBuf przed jmvcore (pieczęć serializacji)
`jmvcore` przy instalacji „pieczętuje" `RProtoBuf_serialize`. Bez RProtoBuf na ścieżce → binding = NULL.
**Fix:** instalować RProtoBuf do `base\R` **przed** jmvcore. Weryfikacja:
`is.function(get('RProtoBuf_serialize', asNamespace('jmvcore')))` == TRUE.
> **Windows vs macOS:** na macOS RProtoBuf musiał być ZE ŹRÓDŁA (wspólny dynamiczny protobuf, inaczej
> SIGSEGV). **Na Windows wystarczy binarka** (posit) — dwie statyczne kopie protobuf współistnieją
> (tak jak w oficjalnej instalce jamovi 2.7.35). Nie komplikuj buildem ze źródła.

### #7 — pip urywa instalację requirements na `nanomsg`
`nanomsg==1.0` próbuje budować ze źródła (ładuje DLL przy buildzie) i **przerywa całe `pip install -r`**
— przez co aiohttp/certifi/cryptography (dalej na liście) NIE instalują się. **Fix:** instalować
requirements z **pominiętą linią nanomsg**, a binding nanomsg vendorować ręcznie (niżej).

### #8 — binding nanomsg: `str` vs `bytes` → KAŻDA analiza wisi  ⚠️ NAJWAŻNIEJSZE
`nanomsg==1.0` (PyPI, ctypes) jest z ery Py2 — `Socket.bind/connect(address)` przekazuje `str` do
`nn_bind` (ctypes chce `bytes`) → `ctypes.ArgumentError`. Serwer **nie binduje gniazda silnika** →
silniki stoją, każda analiza wisi na wiecznym spinnerze (bez błędu w GUI). Oficjalne jamovi ma
załatany binding. **Fix:** vendorować źródła `nanomsg`/`nanomsg_wrappers`/`_nanomsg_ctypes` z sdistu
PyPI do `Resources\server` i w `nanomsg\__init__.py` w `bind` i `connect` dodać:
`if isinstance(address, str): address = address.encode('utf-8')`.

### #9 — nanomsg DLL: ctypes nie szuka po PATH (Py3.8+)
Binding robi `ctypes.windll.nanomsg` → szuka **`nanomsg.dll`** (nie `libnanomsg.dll`), a Py3.8+ ignoruje
PATH. **Fix:** skopiować `libnanomsg.dll` → `nanomsg.dll` **obok `python.exe`** (`Frameworks\python\`).
Dodatkowo: `_nanomsg_ctypes/__init__.py` ma `except OSError` przy ładowaniu opcjonalnego `nanoconfig` —
brak DLL daje `AttributeError`, więc zmienić na `except (OSError, AttributeError)`.

### #10 — readstat/librdata (import .sav/.dta/.RData) — pominięte
Wymagają `iconv.h` (libiconv) niedostępnego dla MSVC. **Decyzja:** pominąć (niekrytyczne dla dydaktyki).
`formatio/__init__.py` ładuje wtyczki formatów przy starcie — bez `librdata`/`readstat` serwer by nie
wstał. **Fix:** w pętli ładowania wtyczek owinąć import w `try/except ImportError: continue`.

### #11 — pusta wstążka / niezgodność wersji R
`JAMOVI_R_VERSION` w `env.conf` MUSI równać się `rVersion` modułów (np. `4.6.0-x64`, czytane z
`modules\jmv\jamovi-full.yaml`) — inaczej moduły „incompatible" i wstążka pusta. R DLL-e
(`R.dll, Rblas, Rgraphapp, Riconv, Rlapack`) kopiować do `bin` + `PATH` w env.conf z `..\Frameworks\R\bin\x64`.
Ciężkie pakiety R są w user-lib → scalić user-lib do `Frameworks\R\library`.

### ⚠️ Ślepy trop: `.skipOption` w descriptives.b.R — NIE naprawiać
W trakcie diagnozy wieszania „naprawiono" `.skipOption` (crashuje przez `jmv::descriptives()` z poziomu
R). **To ślepy trop:** w aplikacji jUPWR descriptives działa BEZ tej zmiany — błąd dotyczy tylko
bezpośredniego R-API, nie ścieżki silnika. Zmianę wycofano. Globalną przyczyną wieszania był wyłącznie
binding nanomsg (#8). **Wniosek:** nie ufać wrapperom `jmv::xxx()` do reprodukcji zachowania silnika.

---

## 5. Weryfikacja na nowej maszynie

1. **Importy bundla:**
   `Frameworks\python\python.exe -X utf8 -c "import jamovi.core, jamovi.server, nanomsg; print('OK')"`
   (z `PYTHONPATH=Resources\server`, `nanomsg.dll` obok python.exe). Ostrzeżenie o „cpy wrapper" jest OK.
2. **Pieczęć RProtoBuf:** `is.function(get('RProtoBuf_serialize', asNamespace('jmvcore')))` == TRUE.
3. **Procesy po starcie `jUPWR.exe`:** widoczne `jUPWR.exe` (Electron) + `python.exe -m jamovi.server`
   + pula `jamovi-engine.exe`.
4. **Analiza w GUI:** Eksploracja → Zmienne ilościowe + zmienna → tabela (nie wieczny spinner).

## 6. Diagnostyka wieszania
Serwer/silnik logują na stdout/stderr przechwytywany przez Electron — NIE ma osobnego pliku logu.
Uruchom z przechwyceniem:
```powershell
Start-Process bin\jUPWR.exe -WorkingDirectory bin `
  -RedirectStandardOutput out.log -RedirectStandardError err.log
```
Następnie uruchom analizę i czytaj `out.log`/`err.log`. Globalne wieszanie wszystkich analiz → podejrzewaj
**komunikację nanomsg** (#8) w pierwszej kolejności, nie kod R pojedynczej analizy.

## 7. Powiązane
- [`scripts/windows/build.ps1`](scripts/windows/build.ps1) — automatyzacja całości.
- [`10-build-macos.md`](10-build-macos.md) — odpowiednik macOS (źródło wielu lekcji: RProtoBuf, protobuf/abseil, env.conf).
- [`00-architecture.md`](00-architecture.md) — z czego składa się bundle.
- Patche źródłowe na gałęzi `windows-native-build`: `jamovi-compiler/{index.js,snapshots.js}`, `engine/Makefile.in`.
