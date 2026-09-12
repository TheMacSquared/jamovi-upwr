# jUPWR Legacy — wariant dla Windows 8.1 x64

> **Status (2026-09-12, wieczór): Faza 1 (Electron 22, R 4.6) i Faza 3 (R 4.1.3,
> Rtools40) ZBUDOWANE i sprawdzone na Windows 11 — portable + instalatory
> w `dist-legacy` i `dist-legacy-r41`; Faza 0 w sali NIE wykonana.** Dokument utrzymywać na obu gałęziach (`main`
> i `legacy/win81`). Konsensus dwóch wcześniejszych planów z 2026-09-07
> (`40-jupwr-old-plan.md`, `40-legacy-win81.md`); fakty zweryfikowane na
> `main` = e94edc57 (jUPWR 1.0.4).

## Kontekst i twardy termin

Sale wykładowe UPWr mają Windows 8.1 x64. jUPWR 1.0.x (jamovi 28.2, Electron 43)
się tam nie instaluje; ten sam problem miało samo jamovi — ostatnia działająca
wersja to 2.3.28. Serwer sieciowy dla pracowni odpada (polityka IT), więc
rozwiązaniem jest lokalna instalka. Legacy to stopgap z datą wygaszenia
(modernizacja sal), nie druga linia rozwojowa.

**Semestr zaczyna się w październiku 2026. Na pierwszych zajęciach musi
działać COŚ** — jeśli IT nie może zaktualizować sal, żaden wariant techniczny
nie jest „kategorycznie odrzucony": ani cofnięcie Electrona, ani cofnięcie R
(decyzja 2026-09-12). Plan ma drabinę wariantów od najtańszego do
najgłębszego, a na dole drabiny leży wariant, który **na pewno** działa.

Decyzje (2026-09-12):
- gałąź `legacy/win81` w tym repo; `main` nietknięty; synchronizacja tylko
  `main → legacy` przez `git merge`;
- wariant główny: **Electron 22.3.27** (ostatnia linia dla 8.1, EOL 2023-10);
- brak VM 8.1 — diagnostyka i testy wyłącznie w sali (skrypt na pendrive);
- **downgrade R / MSVC / Pythona zostaje w planie jako Faza 3**, gotowa do
  uruchomienia, gdy backend 1.0.4 nie ładuje się na 8.1;
- ze względu na termin **build Fazy 1 robimy od razu, równolegle z Fazą 0**,
  żeby jedna wizyta w sali przetestowała oba pakiety;
- **jeden interfejs wszędzie** (2026-09-12): nie da się uczyć na różnych
  interfejsach w różnych tygodniach, więc „wariant zerowy" to NIE jamovi
  2.3.28 stock, tylko jUPWR po pełnym downgradzie (Faza 3). Faza 3 startuje
  **równolegle** z Fazą 1, nie warunkowo — jeśli Faza 1 wystarczy, Fazę 3 się
  odkłada; jeśli nie, jest już w toku;
- **cała praca Legacy poza drzewem głównym**: worktree
  `D:\praca\jamovi-upwr-legacy` (`git worktree add ..\jamovi-upwr-legacy
  legacy/win81` + `git submodule update --init --recursive`). Buildy, cache
  `packaging/build`, artefakty i instalacje R 4.1/Rtools40 nie dotykają
  `D:\praca\jamovi-upwr`, które zostaje na `main`.

## Drabina wariantów (od góry: najtańszy → najgłębszy)

| # | Wariant | Co zmienia | Funkcje jUPWR | Nakład | Kiedy |
|---|---|---|---|---|---|
| 1 | **Electron 22** (Faza 1) | tylko powłoka | ~100 % | 2–3 dni | backend 1.0.4 działa na 8.1 |
| 2 | **Launcher bez Electrona** (Faza 2) | powłoka = przeglądarka sali | ~90 % (bez PDF, drag&drop) | 1–2 dni | brak KB2919355 / czarne okno / weto IT |
| 3 | **Downgrade toolchainu** (Faza 3) = wariant zerowy | R 4.1.3 + Rtools40 (MSVCRT), Electron 22, ew. starszy Python | ~100 % (analizy identyczne) | 1–2 tygodnie | startuje równolegle z 1; wydawany, gdy 1 nie wystarczy |

Wariant 3 jest gwarancją na dzień 1 zajęć: to ten sam jUPWR (klient, moduły,
wyniki), tylko zbudowany na linii, na której jamovi 2.3 działało w tych salach.
jamovi 2.3.28 stock **nie jest** wariantem dydaktycznym — służy w Fazie 0 tylko
jako test, czy system w ogóle uruchamia stary toolchain.

## Konsensus dwóch planów

| Kwestia | old-plan | legacy-win81 | Decyzja |
|---|---|---|---|
| Baza | odbudować z jamovi 2.3.4 | obecny jUPWR + Electron 22 | obecny jUPWR (kod), toolchain jamovi 2.3 (R 4.1) tylko jako Faza 3; sam jamovi 2.3 nie uniesie modułów (fork jmvcore `metodyNew()`, motywy, `jmvcore >= 2.4.2`) |
| Zakres | minimum dydaktyczne po macierzy | wszystkie wbudowane, opcjonalne poza zakresem | wszystkie wbudowane; macierz = tabela wyników testów |
| Bramka | lista pytań | procedura 0a–0d na portable | procedura w sali przez `legacy-diag.ps1`, **ale build Fazy 1 równolegle** (termin) |
| Bez Electrona | brak | równoprawna Faza 2 | rezerwa (wariant 2) |
| Downgrade R/Electron | dopuszczony (baza 2.3) | odrzucony („eskalować do IT") | **dopuszczony jako Faza 3** (2026-09-12) |
| Izolacja | osobna nazwa/katalog/deinstalator | + osobny `UNINST_KEY` | oba: `jUPWR-legacy.nsi` |
| Rygor wydania | SHA-256, manifest, licencje, pilot, wygaszenie | nota EOL, antywirus | suma obu |
| Wersja | „Old 0.1" | `JUPWR_VERSION` + `JUPWR_BUILD_VARIANT` | bez sufiksu — sufiks psuje `release-check.sh:20` i `build.ps1:38` |

## Diagnoza — co psuje zgodność

| Komponent | U nas | Minimum | Na 8.1 |
|---|---|---|---|
| Electron | 43.4.1 | Win 10 od linii 23 (Chromium 109) | **BLOKER** |
| CPython (PBS) | 3.12.11 | Win 8.1 + UCRT | prawdopodobnie OK |
| R / Rtools45 | 4.6.0 | Win 8.1 + UCRT (KB2999226); CRAN oficjalnie wspiera R ≥ 4.2 tylko na Win 10 | prawdopodobnie OK, **niepewne** |
| VC++ redist | 14.4x (VS2022) | **instalator wymaga Win 10** | DLL app-local (Faza 1) |
| NSIS | brak `WinVer` | — | instaluje, nie blokuje |

Objaw z forum jamovi („nie znaleziono punktu wejścia *DiscardVirtualMemory*
w KERNEL32.dll") to Chromium wołające API, którego 8.1 nie ma. Na korzyść:
`electron/app/main.js` używa tylko klasycznych API obecnych w Electronie 22;
serwer ma tryb bez Electrona (`__main__.py:53`, `--start-wb`). W repo **nie ma**
kroku kopiującego VC++ runtime — `vcruntime140*.dll` trafia do bundla tylko
z tarballa Pythona, `jamovi-engine.exe` nie ma go obok siebie.

Fakty ważne dla Fazy 3 (sprawdzone 2026-09-12): `jamovi-compiler/snapshots.js`
ma gotowe wpisy dla R 4.0.2–4.1.3 (epoka jamovi 2.3); `jmv`, `jmvcore` i moduły
jUPWR deklarują `R (>= 3.2)`; w kodzie R modułów nie ma składni wymagającej
R ≥ 4.1 (`|>`, `\(x)`). R 4.1.3 + Rtools40 to ostatnia linia MSVCRT — nie
potrzebuje UCRT i **jest dokładnie tym, na czym jamovi 2.3 działało w sali**.

## Faza 0 — diagnoza w sali (na portable 1.0.4 **i** paczce legacy)

Pendrive: rozpakowany `packaging/build/dist/jUPWR-1.0.4-portable-win64.zip` jako
`<pendrive>\jUPWR\`, **rozpakowana paczka legacy** (Faza 1, jeśli zdążyła się
zbudować) jako `<pendrive>\jUPWR-legacy\`, `legacy-diag.ps1`, plik `.omv`,
instalator jamovi 2.3.28 (tylko test toolchainu). W sali (bez admina, PowerShell 4.0):

```
powershell -ExecutionPolicy Bypass -File D:\legacy-diag.ps1
powershell -ExecutionPolicy Bypass -File D:\legacy-diag.ps1 -AppDir D:\jUPWR-legacy
```

Skrypt loguje do `legacy-diag-<komputer>-<data>.log` obok siebie:

1. maszyna: wersja OS, `KB2919355` (Windows 8.1 Update — bez niego Electron nie
   ruszy), `KB2999226` (UCRT — bez niego nie ruszy nic zbudowane pod UCRT),
   wersje `ucrtbase.dll` i `msvcp140/vcruntime140` w System32, RAM/CPU/GPU,
   antywirus, przeglądarka, prawa konta;
2. środowisko z `bin\env.conf` (jak `readConfig()` w main.js);
3. `python.exe -VV`;
4. `import jamovi.core, jamovi.server, nanomsg` — test rozstrzygający dla backendu;
5. `Rscript --vanilla sessionInfo()`;
6. `require()` pakietów rdzenia (jmvcore, ggplot2, car, emmeans, afex, lavaan,
   BayesFactor, psych, Hmisc, lpSolve, boot, mvtnorm, ragg, systemfonts);
7. `jamovi-engine.exe` bez argumentów — liczy się **jak** pada (skrypt dekoduje
   `STATUS_ENTRYPOINT_NOT_FOUND` / `STATUS_DLL_NOT_FOUND`);
8. `python -m jamovi.server 41337 --start-wb` + HTTP 200 + scenariusz ręczny
   w przeglądarce (otwórz `.omv` → Eksploracja → Zmienne ilościowe → tabela i wykres);
9. `jUPWR.exe` — dla 1.0.4 oczekiwany błąd (treść okna przepisać), dla paczki
   legacy oczekiwane okno aplikacji;
10. jamovi 2.3.28 ręcznie — instalacja i jedna analiza (potwierdza, że linia R 4.1/MSVCRT z Fazy 3 na tej maszynie chodzi).

Jeśli pojawi się okno „Nie znaleziono punktu wejścia…", **najpierw przepisać
treść**, potem OK — skrypt czeka. Równolegle zapytać IT (pisemnie) o: możliwość
doinstalowania KB2919355 / KB2999226, politykę wobec EOL-owego Chromium,
i czy istnieje termin modernizacji sal.

Brama decyzyjna:

| Wynik | Decyzja |
|---|---|
| paczka legacy (krok 9) startuje i liczy analizę | **Faza 1 gotowa** → wydanie pilotażowe |
| 3–8 na 1.0.4 przechodzą, legacy `jUPWR.exe` pada | sprawdzić KB2919355 i GPU; → Faza 2 (launcher) |
| 8 działa, 6 pada na części pakietów | Faza 1/2 + decyzja per analiza (nie blokuje) |
| 4 pada (`import jamovi.core`) lub 5 pada (`R.dll`) mimo UCRT | **→ Faza 3** staje się wariantem wydawanym (już w toku) |
| brak KB2999226 / ucrtbase i IT nie doinstaluje | **→ Faza 3** (R 4.1.3/Rtools40 nie potrzebuje UCRT; Python → sprawdzić PBS 3.10 albo oficjalny embeddable 3.8, ostatni z obsługą 8.1 bez UCRT… — do ustalenia w Fazie 3) |
| jamovi 2.3.28 (krok 10) też nie działa | problem w systemie, nie w toolchainie — Faza 3 nie pomoże, eskalacja do IT z logiem |

Opcjonalnie na maszynie buildowej (0a): audyt importów PE (`pefile`) wszystkich
`*.exe/*.dll/*.pyd` bundla wobec eksportów DLL-i systemowych 8.1 — wymaga kopii
tych DLL-i z sali (`kernel32`, `ucrtbase`, `api-ms-win-*`…). Podejrzani:
`SetThreadDescription`, `VirtualAlloc2`, `MapViewOfFile3`,
`PathCchCanonicalizeEx`, `SetProcessMitigationPolicy`. Skrypt diagnostyczny
może przy okazji skopiować te DLL-e z sali na pendrive (do dopisania, jeśli
Faza 3 stanie się realna).

## Faza 1 — gałąź `legacy/win81`, Electron 22.3.27 (build OD RAZU)

Zmiany są **już na gałęzi** (2026-09-12):

| Plik | Zmiana |
|---|---|
| `client/common/jupwr.ts` | `JUPWR_VERSION` bez sufiksu; nowy `JUPWR_BUILD_VARIANT = 'legacy'` |
| `client/common/icon.ts` | sufiks „Legacy" po numerze wersji (`jUPWR 1.0.4 Legacy (jamovi …)`) |
| `client/vite.config.mts` | `build.target = ['chrome108']` |
| `electron/app/main.js` | `JAMOVI_DISABLE_GPU=1` w env.conf → `app.disableHardwareAcceleration()` |
| `packaging/scripts/windows/build.ps1` | blok `LEGACY OVERRIDES`: `$ElectronVer=22.3.27`, `@electron/asar@3.2.8`, `$Dist=dist-legacy` (nie nadpisuje `dist\jUPWR`), VC++ runtime app-local do `bin\` i `Frameworks\python\`, `JAMOVI_DISABLE_GPU=1` w env.conf, zip `…-legacy-portable-win81.zip` |
| `packaging/scripts/windows/jUPWR-legacy.nsi` | nowy: `WinVer` (<8.1 Abort, ≥10 ostrzeżenie), `InstallDir …\jUPWR-Legacy`, osobny `UNINST_KEY`, skróty „jUPWR Legacy", payload z `dist-legacy` |
| `.github/workflows/release-check.yml` | `legacy/win81` w `push`/`pull_request` |

Nie ruszać: `jUPWR.nsi` (czyta go `release-check.sh` i `test_release_check.py`),
`$AppName`, `$Modules` (porównywane z Dockerem i macOS). Kroki 4e/4f/4g (`.jmo`
opcjonalne) zostają w skrypcie, ale Legacy ich nie wydaje.

Do **przetestowania**, nie przepisania: `printToPDF({})` (`main.js:312`) —
Electron 21 przepisał tę metodę, marginesy/format mogą się różnić od 43.

### Kolejność (build przed wizytą w sali)

1. W worktree `D:\praca\jamovi-upwr-legacy`: `git merge main` (jeśli `main`
   poszedł dalej). **Nigdy nie budować w `D:\praca\jamovi-upwr`.**
2. `packaging\scripts\windows\build.ps1` w worktree, na dev-maszynie Windows 11
   (toolchain bez zmian). Wynik: `D:\praca\jamovi-upwr-legacy\packaging\build\
   dist-legacy\jUPWR\` + portable zip. Pierwszy build w worktree buduje
   zależności od zera (nanomsg, Python, moduły) — ok. 1 h.
3. **Weryfikacja na Windows 11** wg `20-build-windows.md` sekcja 5 + schowek
   (`clipboard.readHTML`), eksport PDF (porównać z paczką `main`), dialogi,
   drag&drop `.omv`, zmiana języka, napis „jUPWR 1.0.4 Legacy" w oknie „O programie".
4. `makensis jUPWR-legacy.nsi`; instalacja **obok** zwykłego jUPWR na jednej
   maszynie: oba w „Dodaj/usuń programy", deinstalacja jednego nie rusza drugiego;
   ostrzeżenie `AtLeastWin10` faktycznie się pokazuje.
5. `bash packaging/scripts/release-check.sh --metadata-only` — bez ostrzeżeń.
6. Sala: Faza 0 na obu paczkach + pełny scenariusz jednych zajęć (patrz Wydanie).

### Dziennik wykonania (fakty z buildów, 2026-09-12)

- **Worktree** `D:\praca\jamovi-upwr-legacy` (gałąź `legacy/win81`), submoduły
  zainicjowane; drzewo główne na `main` nietknięte.
- **Faza 1 zbudowana** (`build.ps1`, ~1 h): `dist-legacy\jUPWR-1.0.4-legacy-portable-win81.zip`
  (754 MB) i `jUPWR-1.0.4-legacy-win81-x64-setup.exe` (makensis `jUPWR-legacy.nsi`).
  W `bin\`: Electron 22.3.27, `msvcp140/vcruntime140/concrt140` obok
  `jamovi-engine.exe`; `env.conf` ma `JAMOVI_DISABLE_GPU=1`, `JAMOVI_R_VERSION=4.6.0-x64`.
  Smoke test na Windows 11: `jUPWR.exe` startuje (4 procesy Electrona), serwer
  Python nasłuchuje na 127.0.0.1, 4 procesy `jamovi-engine.exe`, instancja
  otwiera się. Checklista ręczna (schowek, PDF, dialogi, język) — jeszcze nie.
- **`legacy-diag.ps1` sprawdzony na paczce legacy** (Win 11, `-NoGui`): kroki 3–7
  OK, krok 8 HTTP 200. Pułapka naprawiona: `jamovi.server <port>` **ignoruje
  podany port** — losuje trzy własne i wypisuje
  `accessible from: 127.0.0.1:<port>/?access_key=<klucz>`; bez klucza HTTP
  odmawia. Skrypt czyta adres z logu stdout serwera.
- **Toolchain r41 zainstalowany**: R 4.1.3 (`C:\Program Files\R\R-4.1.3`),
  Rtools40 (`C:\rtools40`, gcc 8.3) + `pacman -S mingw-w64-x86_64-protobuf`
  (3.21.12, bez abseil) `mingw-w64-x86_64-make`. Biblioteka użytkownika R 4.1 to
  `Dokumenty\R\win-library\4.1` (R < 4.2; skrypt pyta `R_LIBS_USER`); 39 pakietów
  Imports modułów (+ zależności, 178 razem) ze snapshotu PPM 2023-04-07 — komplet.
- **Build r41 = `$env:JUPWR_TOOLCHAIN='r41'; build.ps1`** (osobne `stage-r41`,
  `deps-r41`, `dist-legacy-r41`, Boost `stage-mingw-gcc8`). Po drodze:
  - nanomsg: `CMakeCache` w `dl\nanomsg-1.2\build` pamiętał gcc z rtools45 →
    r41 buduje w `build-r41`;
  - **R 4.1 nie czyta komentarzy `#` w `DESCRIPTION`** („error reading file",
    obsługa od R 4.3) → usunięte z 12 modułów jUPWR na gałęzi legacy
    (kandydat do cherry-picka na `main`);
  - jmv 2.8.4, plots 2.9.1 i jmvcore kompilują się pod R 4.1.3 bez zmian w kodzie;
  - **silnik**: `make` jest przyrostowy, a obiekty leżą w `engine\engine\` **i**
    `server\jamovi\common\` — pierwszy build r41 przelinkował/skopiował silnik
    z R 4.6 (import `R_getVarEx`, R ≥ 4.5 → `STATUS_ENTRYPOINT_NOT_FOUND`, 0 silników;
    diagnoza: `objdump -p` importy vs eksporty `R.dll`). Teraz `build.ps1` kasuje
    wszystkie `*.o` i `jamovi-engine.exe` przed `make` (koszt ~2–3 min);
  - `engine\jamovi.pb.cc/.h` (niesledzone, generowane) z protoc 29 wymagają
    `runtime_version.h`, którego protobuf 3.21 nie ma → `build.ps1` kasuje je
    przed `make`, reguła Makefile odtwarza je protoc-em z PATH toolchainu;
  - wyjście `make` silnika idzie do `packaging\build\engine-make-<toolchain>.log`
    (wcześniej `Out-Null` — po błędzie nie było czego czytać).
- **Faza 3 zbudowana**: `dist-legacy-r41\jUPWR-1.0.4-legacy-r41-portable-win81.zip`
  (592 MB) + `jUPWR-1.0.4-legacy-r41-win81-x64-setup.exe` (`makensis /DDISTDIR=
  dist-legacy-r41 /DTAG=legacy-r41 jUPWR-legacy.nsi`). `env.conf`:
  `JAMOVI_R_VERSION=4.1.3`. **Silnik r41 importuje tylko `msvcrt.dll`, `R.dll`,
  `libnanomsg.dll` + systemowe** (r46: zestaw `api-ms-win-crt-*`, czyli UCRT) —
  to jest sedno Fazy 3 na 8.1. `legacy-diag.ps1` na paczce r41 (Win 11): 3–8 OK,
  HTTP 200, 4 silniki; `jUPWR.exe` startuje (4 procesy Electrona, serwer, 4 silniki).
  Kontrola kodu analiz pod R 4.1.3 (Rscript paczki, `jmv::descriptives`,
  `jmv::ttestIS`, `jEksplor:::ilosciowe`, `jCzest:::tabela`, `jTestyT:::ttesttwo`,
  `jANOVA:::anova`, `jRegr:::liniowa`, `jCI:::cionemean`, `jperm:::permtesttwo`):
  identyczne zachowanie jak pod R 4.6 (analizy z wykresem poza silnikiem padają
  na „niepoprawny typ czcionki" w OBU wersjach — brak rejestracji czcionek
  jUPWR poza aplikacją, nie problem R 4.1; z `plot = FALSE` liczą).
- **Co dalej dla obu paczek**: checklista ręczna na Win 11 (schowek, PDF, dialogi,
  język, „O programie" z dopiskiem Legacy), a potem Faza 0 w sali z obiema
  paczkami na pendrive. Kolejność testu w sali: r41 najpierw (najgłębszy
  fallback), potem r46 (Electron 22 + R 4.6) — jeśli r46 działa, jest lżejszy
  w utrzymaniu (ten sam R co `main`).

## Faza 2 — rezerwa: launcher bez Electrona

Gdy Electron 22 nie startuje (brak KB2919355), renderuje wadliwie mimo
`disableHardwareAcceleration`, albo IT zawetuje EOL-owe Chromium. 1–2 dni.

- `launcher/launcher.py` (~50 linii): wczytać `bin\env.conf`, rozwinąć ścieżki
  względne dla kluczy `*PATH`/`*HOME`/`*LIBS` wobec `bin\` (dokładnie tak robi
  krok 2 `legacy-diag.ps1`), wstrzyknąć do `os.environ`,
  `runpy.run_module('jamovi.server')` z `41337 --start-wb`.
- Skrót NSIS → `Frameworks\python\python.exe bin\launcher.py`, `Start in = bin`;
  świadomie `python.exe`, nie `pythonw.exe` — okno konsoli to jedyny sposób
  zamknięcia serwera („nie zamykaj czarnego okna").
- Straty (`isElectron=false`): drag&drop `.omv` (`main.ts:431` używa `file.path`),
  sideload `.jmo` (`pagesideload.ts:32` — nieistotne), eksport PDF (`main.js:294`,
  zostaje „Drukuj"), „zapisać zmiany?" przy zamknięciu, `showMessageBox`
  (`host.ts:305` bez fallbacku → TypeError przy zmianie języka).
- Dwie łatki warte cherry-picku na `main` niezależnie od wariantu (dotyczą też
  jamovi cloud): fallback `showMessageBox` → `confirm`/`alert`; w `ondrop` przy
  `file.path === undefined` ścieżka uploadu z `instance.ts`.

## Faza 3 — downgrade toolchainu (gdy backend 1.0.4 nie ładuje się na 8.1)

Cel: ten sam kod modułów i klienta, ale binaria zbudowane pod linię, która na
8.1 **na pewno** działa — tę, na której działało jamovi 2.3 (R 4.1, MSVCRT).
Nakład 1–2 tygodnie, głównie buildy i testy; ryzyko: kompilacja pakietów R.
**Startuje równolegle z Fazą 1** (decyzja 2026-09-12), w tym samym worktree,
jako drugi zestaw nadpisań w `build.ps1` (`$Toolchain = 'r41'` obok bloku
LEGACY OVERRIDES; wyjście do `dist-legacy-r41`), żeby oba warianty dało się
zbudować z jednej gałęzi. R 4.1.3 i Rtools40 instalują się obok R 4.6/Rtools45
(osobne katalogi, osobny `$UserLib` `win-library\4.1`) — nie ruszają toolchainu
`main`.

Zasada: **schodzić po jednym komponencie**, w kolejności od najbardziej
podejrzanego, i po każdym kroku wozić paczkę do sali (albo prosić IT o jedną
maszynę testową / obraz VM — przy Fazie 3 to już nie prośba, a warunek).

### 3a. R 4.1.3 + Rtools40 (najbardziej prawdopodobny sprawca: UCRT)

- `build.ps1`: `$RHome = C:\Program Files\R\R-4.1.3`, `$UserLib = …\win-library\4.1`,
  `$RtoolsMingw = C:\rtools40\mingw64`, `$RtoolsUsr = C:\rtools40\usr\bin`.
  Rtools40 = gcc 8.3 (C++17 OK dla silnika i protobuf; **Boost 1.84 mingw
  przebudować** pod gcc 8). `JAMOVI_R_VERSION` i `rVersion` w `jamovi-full.yaml`
  biorą się automatycznie z R, pod którym `jmc` buduje moduły; `snapshots.js`
  ma wpis `'4.1.3'`.
- Pakiety R: `$CranRepo` → snapshot Posit Package Manager z daty ~2023-04
  (ostatnie binaria dla R 4.1: `https://packagemanager.posit.co/cran/2023-04-20`)
  — nie `latest`. Sprawdzić wersje minimalne, których moduły faktycznie
  używają (emmeans, afex, car, ggplot2 3.4.x, ragg, systemfonts — wszystkie
  istniały na R 4.1). Jeśli któryś pakiet nowszy jest niezbędny (np. funkcja
  ggplot2 ≥ 3.5 w `plots/`), decyzja per wykres/analiza.
- Silnik `jamovi-engine.exe`: Rtools40 mingw → linkuje MSVCRT, bez UCRT.
- Testy modułów: `packaging/scripts/test-statistics.sh` z R 4.1.3 (testthat
  jperm/jCI/jRegr) + porównanie wyników kilku analiz z paczką 1.0.4.

### 3b. Python / jamovi.core (jeśli krok 4 diagnostyki pada, a 3a nie pomogło)

- PBS `cpython-3.12` → starszy tarball PBS (`3.10.x`/`3.11.x`, releases
  20230507 lub wcześniejsze) — serwer deklaruje Python ≥ 3.5, `requirements.txt`
  sprawdzić pod 3.10; `jamovi.core` (Cython, MSVC v143) rekompiluje się
  `setup.py install` pod nowego Pythona — v143 emituje kod działający na 8.1,
  potrzebne tylko DLL-e runtime app-local (Faza 1 to już robi).
- Jeśli sam `core*.pyd` pada na brakującym API: `_WIN32_WINNT=0x0603` w
  `server/setup.py` (`extra_compile_args`), a w ostateczności toolset v142
  (VS2019 Build Tools) — Boost 1.84 ma prebuilt `lib64-msvc-14.2`.

### 3c. Klient / Electron — bez zmian wobec Fazy 1

Electron 22.3.27 nie zależy od R ani Pythona. Jeśli i on pada mimo KB2919355,
Faza 2 (launcher) zamyka temat po stronie powłoki.

### Co Faza 3 pociąga za sobą

- Osobne `$UserLib` i `$RHome` na maszynie buildowej (R 4.1.3 obok 4.6 — bez konfliktu).
- **Wszystkie** moduły przebudowane pod R 4.1 (`.jmo` opcjonalne nadal poza zakresem).
- `MODULES.md`: kolumna/adnotacja „Legacy: R 4.1.3", bo binaria nie są zamienne
  z `main`; `.omv` pozostają zgodne (ten sam kod analiz).
- `release-check.sh` nie porównuje wersji R — nic do zmiany.
- Docker i macOS nietknięte (Faza 3 żyje tylko w `build.ps1` na gałęzi legacy).

## Dwie wersje jUPWR, jeden interfejs

`main` = wersja domyślna, o którą prosimy IT wszędzie. Legacy = ta sama
aplikacja (klient, moduły, wyniki, pliki `.omv`) na starszym toolchainie dla
sal, których IT nie zaktualizuje na czas. Student i materiały do zajęć nie
widzą różnicy poza dopiskiem „Legacy" w oknie „O programie". jamovi 2.3.28
stock nie jest wariantem dydaktycznym; nie próbować sideloadować modułów jUPWR
do 2.3.28 (niezgodne jmvcore).

## Harmonogram do 1 października 2026

| Termin | Krok | Wyjście |
|---|---|---|
| do 2026-09-15 | build Fazy 1 w worktree; weryfikacja na Win 11; pendrive (1.0.4, legacy, 2.3.28 jako test, skrypt); **start Fazy 3a** (R 4.1.3 + Rtools40 obok 4.6) | paczka Fazy 1 + pendrive |
| do 2026-09-17 | **wizyta w sali nr 1**: Faza 0 na obu paczkach + krok 10; pismo do IT | log, decyzja z bramy |
| 09-18 → 09-22 | A (Faza 1 OK): instalator, pilot na jednej maszynie; B: launcher; C: dokończyć 3a (już w toku) | paczka pilotażowa albo paczka 3a |
| do 2026-09-25 | **wizyta w sali nr 2**: pilot/scenariusz zajęć (A/B) albo Faza 0 na paczce 3a (C) | wynik pilotażu |
| 09-26 → 09-30 | poprawki; przy C ewentualnie 3b; instalacja w całej sali (IT) | sala gotowa |
| 2026-10-01 | zajęcia na jUPWR Legacy (Faza 1 albo 3) — ten sam interfejs co `main` | — |

Reguła: **każda wizyta w sali testuje wszystko, co jest gotowe** (nie po jednym
wariancie na wizytę). Brak dostępu do sali w danym tygodniu przesuwa decyzję,
nie plan — wtedy prosić IT o maszynę testową lub obraz VM.

## Wydanie pilotażowe i utrzymanie

- Wydać dopiero po przejściu w sali: instalacja na zwykłym koncie, restart,
  otwarcie `.omv`, po jednej analizie z każdego modułu wbudowanego (jmv, plots,
  jperm, jCI, jdistrACTION, jDane, jANOVA, jTestyT, jCzest, jEksplor, jRegr),
  wykres, schowek, PDF, polskie znaki i ścieżki ze spacjami, zapis i ponowne
  otwarcie `.omv`, jeden `.omv` w obie strony między Legacy a zwykłym jUPWR,
  praca offline, antywirus pracowni (brak podpisu kodu: `python.exe` + pula
  `jamovi-engine.exe` to klasyczny fałszywy alarm), deinstalacja bez naruszenia danych.
- Do paczki: SHA-256 obu artefaktów (`jUPWR-<ver>-legacy-win81-x64-setup.exe`,
  `jUPWR-<ver>-legacy-portable-win81.zip`), manifest komponentów (Electron
  22.3.27 / Chromium 108 / Node 16.17, Python, R — w Fazie 3 R 4.1.3, wersje
  modułów z `MODULES.md`), wymagania (Win 8.1 x64 + KB2919355 + KB2999226 —
  w Fazie 3a bez KB2999226 po stronie R), instrukcja, lista ograniczeń, licencje.
- Nota dla IT: Electron 22 EOL od 2023-10; serwer słucha tylko na `127.0.0.1`
  (`__main__.py:61-64`), `contextIsolation`, sandbox renderera; wariant
  przejściowy **z zadeklarowaną datą wygaszenia** = modernizacja sal.
- Próba na jednych zajęciach przed instalacją w całej sali; portable nie dotyka
  instalacji, więc powrót jest natychmiastowy.
- **Legacy jest ZAMROŻONE (decyzja 2026-09-12): bez merge `main → legacy`, bez
  rozwoju, bez podbijania wersji.** Sale 8.1 dostają jUPWR 1.0.4 Legacy do czasu
  modernizacji. Dotykać gałęzi tylko przy błędzie blokującym zajęcia: commit
  wprost na `legacy/win81`, przebudowa wydanej paczki (A lub B), nowe sumy,
  test w sali. Usunięte komentarze `#` z DESCRIPTION nie wymagają więc
  cherry-picka na `main` (nie będzie merge'a, który by je przywrócił).
- Zgodność `.omv`: pliki z Legacy otwierają się w nowszym jUPWR; pliki z nowszego
  jUPWR z analizami nowszych modułów mogą nie otworzyć się w Legacy.
- Zakończyć linię po modernizacji sal: odinstalować Legacy (osobny wpis),
  zainstalować zwykły jUPWR.

## Ryzyka (malejąco)

- **R1** backend 1.0.4 nie ładuje się na 8.1 (niskie–średnie / wysokie):
  odpowiedź = **Faza 3** (nie eskalacja). Koszt 1–2 tygodnie mieści się
  w harmonogramie tylko przy wizycie w sali do 09-17; wariant zerowy
  zabezpiecza dzień 1.
- **R2** VC++ 14.4x nie działa na 8.1 (wysokie / średnie): DLL app-local
  (Faza 1); jeśli same DLL-e też niezgodne → redist 14.3x (VS 2022 17.9)
  bez zmiany kompilatora, a w Fazie 3b toolset v142.
- **R3** pakiety R (niskie w Fazie 1, **średnie w Fazie 3a**): snapshot PPM
  z epoki R 4.1; pojedynczy pakiet pada → decyzja per analiza.
- **R4** EOL Chromium (pewne / polityczne): nota dla IT, data wygaszenia.
- **R5** brak podpisu kodu / antywirus (pewne / uciążliwe): test w sali.
- **R6** brak VM 8.1 i dostępu do sali: każda iteracja = wizyta; skrypt zbiera
  wszystko za jednym razem; przy Fazie 3 maszyna testowa od IT jest warunkiem.
- **R7** termin (pewne): mitygacja = Faza 1 i Faza 3 równolegle od pierwszego
  dnia, każda wizyta w sali testuje wszystko, co gotowe.

## Źródła

- Electron: https://www.electronjs.org/blog/windows-7-to-8-1-deprecation-notice
- Python 3.12 / Windows: https://docs.python.org/3.12/using/windows.html
- R / Windows: https://stat.ethz.ch/CRAN/bin/windows/base/rw-FAQ.R-devel.html
- Rtools40 (R 4.0–4.1, MSVCRT): https://cran.r-project.org/bin/windows/Rtools/rtools40.html
- Posit Package Manager, snapshoty CRAN: https://packagemanager.posit.co/client/#/repos/cran/setup
- jamovi 2.6 na Windows 7 (analogiczny objaw): https://forum.jamovi.org/viewtopic.php?t=3935
