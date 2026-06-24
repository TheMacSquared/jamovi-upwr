# Build natywny Windows (x64)

> **STATUS: DO WALIDACJI NA MASZYNIE WINDOWS.** Ten dokument powstał z analizy kodu
> (`engine/Makefile.in` blok Windows, `server/setup.py` blok `nt`, komentarze NSIS w
> `electron/app/main.js`, `docker/jamovi-Dockerfile`) oraz z doświadczeń ze sprawdzonego
> buildu macOS. Komendy należy zweryfikować i poprawić podczas pierwszego buildu na Windows.

Skrypty szkieletowe: `scripts/windows/` (`build.ps1`, `jUPWR.nsi`).

## Wymagania (zgodnie z README forka)

| Składnik | Wersja | Uwaga |
|---|---|---|
| Visual Studio 2022 | Build Tools (MSVC v143) | kompilacja `jamovi.core` (Cython) i ew. silnika |
| RTools 4.5 | `C:\rtools45` | dostarcza mingw-w64 + `protoc.exe` (patrz setup.py) |
| R | 4.5/4.6 x64 | `C:\Program Files\R\R-4.x` — moduły budowane pod tę wersję |
| Python | 3.12 x64 | python.org installer lub embeddable |
| Node.js | 22+ | vite, asar, electron |
| Boost | 1.84 | `C:\local\boost_1_84_0` (układ z `setup.py`: `lib64-msvc-14.3`) |
| NSIS | 3.x | `makensis` — installer |
| nanomsg, protobuf | — | biblioteki dla silnika/serwera |

> Uwaga o Boost: `engine/Makefile.in` (blok Windows) zakłada **mingw** Boost 1.79
> (`-lboost_*-mgw8-mt-x64-1_79`), a `server/setup.py` zakłada **MSVC** Boost 1.84
> (`libboost_*-vc143-mt-x64-1_84`). Trzeba ustalić jeden toolchain — analogicznie do macOS
> najprościej trzymać się jednego (MSVC dla setup.py, mingw/RTools dla silnika) i dopasować
> nazwy bibliotek/flagi przy pierwszym buildzie.

## Kolejność (analogicznie do macOS)

1. **Klient** — identycznie: `npm install` + `npm run build:coms` + `vite build --outDir build\stage\jamovi\client`.
2. **Moduły R** — `R CMD INSTALL jmvcore --library=...\modules\base\R`, potem `jmc --install`
   dla każdego modułu z `--rhome "C:\Program Files\R\R-4.x"`. (jmc = `jamovi-compiler`, `npm install`).
3. **i18n** — `node i18n\index.js --build src --dest build\stage\jamovi\i18n\json`.
4. **Serwer** — Python 3.12: `venv`, `pip install -r server\requirements.txt`,
   potem `readstat`, `SETUP_CORE_ONLY=1`/`SETUP_SERVER_ONLY=1 python setup.py install`.
   **Pamiętaj o dopasowaniu `protobuf` (python) do wersji `protoc`** — na macOS to był realny
   błąd (`VersionError gencode/runtime`); `pip install --upgrade protobuf`.
5. **Silnik** — `engine\configure.bat` + `make` (przez powłokę RTools/mingw). Bloki Windows w
   Makefile linkują boost mingw + `windres` osadza manifest UTF-8 (`engine/jamovi-engine.res.o`).
   Wynik: `jamovi-engine.exe`.
6. **Montaż** — Electron Windows (`electron-v<ver>-win32-x64.zip`), binarka → `jUPWR.exe`,
   nasz kod jako `resources\app.asar`, payload do `jUPWR\jamovi\...`, `jamovi-engine.exe` obok `.exe`,
   `env.conf` **obok `.exe`** (na Windows `main.js`/`conf.py` czytają `env.conf` z katalogu binarki / `home\bin`).
7. **Installer** — `makensis jUPWR.nsi` → `jUPWR-<wersja>-x64-setup.exe`; portable = spakowany katalog `jUPWR\` jako `.zip`.

## Różnice względem macOS (z kodu)

- **Ścieżki env.conf:** Windows używa układu „obok `.exe`" (nie `Contents/Resources`). `conf.py`
  (linia ~41) na nie-Darwin czyta `home\bin\env.conf`; `main.js` czyta `env.conf` z katalogu binarki.
  Ścieżki w env.conf względne do katalogu `.exe` (np. `JAMOVI_CLIENT_PATH=jamovi\client`).
- **Silnik:** `engine.py` (linia ~114) na nie-Darwin szuka `home\bin\jamovi-engine` (rozszerzenie `.exe`
  dodaje system). Ustaw `JAMOVI_HOME` tak, by `home\bin` = katalog z `.exe`.
- **Zmienne R:** `engine.py` ustawia `TZDIR=%R_HOME%\share\zoneinfo` (lubridate). DYLD nie dotyczy —
  Windows szuka DLL w katalogu `.exe` + `PATH`; dorzuć do `PATH` katalog z `R.dll`, `RInside`, boost, nanomsg, protobuf.
- **Relokowalność:** prostsza niż macOS — wystarczy mieć wszystkie DLL na `PATH` (ustawionym w env.conf
  względnie). Brak odpowiednika `install_name_tool`.

## Sandbox sieciowy / SmartScreen

`main.js` (linie ~230–245) włącza NetworkServiceSandbox tylko dla buildu NSIS (`%ProgramFiles%`,
gdzie ACE pozwalają) — `env.conf` NSIS powinien zawierać `JAMOVI_NETWORK_SANDBOX=1`; portable `.zip`
**bez** tego markera (inaczej crash przy rozpakowaniu do Downloads/Pulpit). Patrz komentarz w `main.js`.

## Weryfikacja (Windows)
- `jUPWR.exe` uruchamia się, serwer nasłuchuje, okno „jUPWR".
- Descriptives (boxMean/histFacet/V), moduł jCI — wyniki + wykres.
- Test na **czystej** maszynie bez devtoolów (wykrycie brakujących DLL).
