# jUPWR Legacy — wariant dla Windows 8.1 x64

> **Status (2026-09-12): gałąź `legacy/win81` przygotowana, Faza 0 NIE wykonana.**
> Zmiany źródłowe Fazy 1 są na gałęzi, ale **build nie był uruchamiany** — czeka
> na wynik diagnostyki w sali. Dokument utrzymywać na obu gałęziach (`main`
> i `legacy/win81`). Konsensus dwóch wcześniejszych planów z 2026-09-07
> (`40-jupwr-old-plan.md`, `40-legacy-win81.md`); fakty zweryfikowane na
> `main` = e94edc57 (jUPWR 1.0.4).

## Kontekst

Sale wykładowe UPWr mają Windows 8.1 x64. jUPWR 1.0.x (jamovi 28.2, Electron 43)
się tam nie instaluje; ten sam problem miało samo jamovi — ostatnia działająca
wersja to 2.3.28. Serwer sieciowy dla pracowni odpada (polityka IT), więc
rozwiązaniem jest lokalna instalka. **Legacy to stopgap z datą wygaszenia**
(modernizacja sal), nie druga linia rozwojowa.

Decyzje (2026-09-12):
- gałąź `legacy/win81` w tym repo; `main` nietknięty; synchronizacja tylko
  `main → legacy` przez `git merge`;
- wariant główny: **Electron 22.3.27** (ostatnia linia dla 8.1, EOL 2023-10);
  launcher przeglądarkowy jako rezerwa;
- brak VM 8.1 — diagnostyka i testy wyłącznie w sali (skrypt na pendrive).

## Konsensus dwóch planów

| Kwestia | old-plan | legacy-win81 | Decyzja |
|---|---|---|---|
| Baza | odbudować z jamovi 2.3.4 | obecny jUPWR + Electron 22 | obecny jUPWR: baza 2.3 nie uniesie modułów (fork jmvcore `metodyNew()`, motywy, R 4.6, `jmvcore >= 2.4.2`) |
| Zakres | minimum dydaktyczne po macierzy | wszystkie wbudowane z `$Modules`, opcjonalne poza zakresem | wszystkie wbudowane; macierz = tabela wyników testów, nie selekcja |
| Bramka | lista pytań | procedura 0a–0d na portable | procedura, wykonana w sali przez `legacy-diag.ps1` |
| Bez Electrona | brak | równoprawna Faza 2 | rezerwa („jak najwięcej ma działać") |
| Izolacja | osobna nazwa/katalog/deinstalator | + osobny `UNINST_KEY` | oba: `jUPWR-legacy.nsi` |
| Rygor wydania | SHA-256, manifest, licencje, pilot, wygaszenie | nota EOL, antywirus | suma obu |
| `.omv` między wersjami | testować, nie obiecywać | — | ten sam backend i moduły ⇒ zgodne; jeden test potwierdzający |
| Wersja | „Old 0.1" | `JUPWR_VERSION` + `JUPWR_BUILD_VARIANT` | bez sufiksu — sufiks psuje `release-check.sh:20` i `build.ps1:38` |

jamovi 2.3.28 zostaje **tylko** punktem odniesienia w sali: jeśli ono też nie
działa, winny jest system (UCRT/KB), nie Electron.

## Diagnoza — co psuje zgodność

| Komponent | U nas | Minimum | Na 8.1 |
|---|---|---|---|
| Electron | 43.4.1 | Win 10 od linii 23 (Chromium 109) | **BLOKER** |
| CPython (PBS) | 3.12.11 | Win 8.1 + UCRT | prawdopodobnie OK |
| R / Rtools45 | 4.6.0 | Win 8.1 + UCRT (KB2999226) | prawdopodobnie OK |
| VC++ redist | 14.4x (VS2022) | **instalator wymaga Win 10** | DLL app-local (Faza 1) |
| NSIS | brak `WinVer` | — | instaluje, nie blokuje |

Objaw z forum jamovi („nie znaleziono punktu wejścia *DiscardVirtualMemory*
w KERNEL32.dll") to Chromium wołające API, którego 8.1 nie ma. Na korzyść:
`electron/app/main.js` używa tylko klasycznych API obecnych w Electronie 22
(`build.ps1` pinuje 43, bo 44+ zmienił schowek — schodzimy w stronę, w którą kod
jest napisany); serwer ma tryb bez Electrona (`__main__.py:53`, `--start-wb`).
W repo **nie ma** kroku kopiującego VC++ runtime — `vcruntime140*.dll` trafia do
bundla tylko z tarballa Pythona, `jamovi-engine.exe` nie ma go obok siebie.

## Faza 0 — diagnoza w sali (bez rebuildu, na portable 1.0.4)

**Nie zaczynać buildu Fazy 1 bez przejścia bramy.**

Pendrive: rozpakowany `packaging/build/dist/jUPWR-1.0.4-portable-win64.zip` jako
`<pendrive>\jUPWR\`, `packaging/scripts/windows/legacy-diag.ps1`, plik `.omv`
do testu, instalator jamovi 2.3.28. W sali (bez admina, PowerShell 4.0):

```
powershell -ExecutionPolicy Bypass -File D:\legacy-diag.ps1
```

Skrypt loguje do `legacy-diag-<komputer>-<data>.log` obok siebie:

1. maszyna: wersja OS, `KB2919355` (Windows 8.1 Update — bez niego Electron nie
   ruszy), `KB2999226` (UCRT — bez niego nic nie ruszy), wersje `ucrtbase.dll`
   i `msvcp140/vcruntime140` w System32, RAM/CPU/GPU, antywirus, przeglądarka,
   prawa konta;
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
9. `jUPWR.exe` — oczekiwany błąd; treść okna przepisać, dziennik zdarzeń;
10. jamovi 2.3.28 ręcznie.

Jeśli pojawi się okno „Nie znaleziono punktu wejścia…", **najpierw przepisać
treść**, potem OK — skrypt czeka.

Brama decyzyjna:

| Wynik | Decyzja |
|---|---|
| 3–8 przechodzą, 9 pada | → Faza 1 |
| 8 działa, 6 pada na części pakietów | Faza 1 + decyzja per analiza (nie blokuje) |
| 4 pada lub 3 pada mimo UCRT | R1: **nie cofać toolchainu**, eskalacja do IT |
| brak KB2919355 | Electron 22 nie ruszy → Faza 2 zamiast Fazy 1 |
| brak KB2999226 / ucrtbase | nic nie ruszy; IT musi doinstalować UCRT |

Opcjonalnie na maszynie buildowej (0a): audyt importów PE (`pefile`) wszystkich
`*.exe/*.dll/*.pyd` bundla wobec eksportów DLL-i systemowych 8.1 — wymaga kopii
tych DLL-i z sali (`kernel32`, `ucrtbase`, `api-ms-win-*`…). Podejrzani:
`SetThreadDescription`, `VirtualAlloc2`, `MapViewOfFile3`,
`PathCchCanonicalizeEx`, `SetProcessMitigationPolicy`.

## Faza 1 — gałąź `legacy/win81`, Electron 22.3.27

Zmiany są **już na gałęzi** (2026-09-12), build czeka na bramę:

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

### Kolejność po przejściu bramy

1. `git checkout legacy/win81`, `git merge main` (jeśli `main` poszedł dalej).
2. `packaging\scripts\windows\build.ps1` na dev-maszynie Windows 11 (toolchain
   bez zmian). Wynik: `packaging/build/dist-legacy/jUPWR/` + portable zip.
3. **Weryfikacja na Windows 11** wg `20-build-windows.md` sekcja 5 + schowek
   (`clipboard.readHTML`), eksport PDF (porównać z paczką `main`), dialogi,
   drag&drop `.omv`, zmiana języka, napis „jUPWR 1.0.4 Legacy" w oknie „O programie".
4. `makensis jUPWR-legacy.nsi`; instalacja **obok** zwykłego jUPWR na jednej
   maszynie: oba w „Dodaj/usuń programy", deinstalacja jednego nie rusza drugiego;
   ostrzeżenie `AtLeastWin10` faktycznie się pokazuje.
5. `bash packaging/scripts/release-check.sh --metadata-only` — bez ostrzeżeń.
6. Sala: pełny scenariusz jednych zajęć (patrz Wydanie).

## Faza 2 — rezerwa: launcher bez Electrona

Tylko gdy Electron 22 nie startuje (brak KB2919355), renderuje wadliwie mimo
`disableHardwareAcceleration`, albo IT zawetuje EOL-owe Chromium. 1–2 dni.

- `launcher/launcher.py` (~50 linii): wczytać `bin\env.conf`, rozwinąć ścieżki
  względne dla kluczy `*PATH`/`*HOME`/`*LIBS` wobec `bin\` (dokładnie tak robi
  krok 2 `legacy-diag.ps1`), wstrzyknąć do `os.environ`,
  `runpy.run_module('jamovi.server')` z `41337 --start-wb`.
- Skrót NSIS → `Frameworks\python\python.exe bin\launcher.py`, `Start in = bin`;
  świadomie `python.exe`, nie `pythonw.exe` — okno konsoli to jedyny sposób
  zamknięcia serwera („nie zamykaj czarnego okna").
- Straty (`isElectron=false`): drag&drop `.omv` (`main.ts:431` używa `file.path`),
  sideload `.jmo` (`pagesideload.ts:32` — nieistotne, opcjonalne poza zakresem),
  eksport PDF (`main.js:294`, zostaje „Drukuj"), „zapisać zmiany?" przy zamknięciu,
  `showMessageBox` (`host.ts:305` bez fallbacku → TypeError przy zmianie języka).
- Dwie łatki warte cherry-picku na `main` niezależnie od wariantu (dotyczą też
  jamovi cloud): fallback `showMessageBox` → `confirm`/`alert`; w `ondrop` przy
  `file.path === undefined` ścieżka uploadu z `instance.ts`.

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
  22.3.27 / Chromium 108 / Node 16.17, Python 3.12.11, R 4.6.0, wersje modułów
  z `MODULES.md`), wymagania (Win 8.1 x64 + KB2919355 + KB2999226), instrukcja,
  lista ograniczeń, licencje komponentów.
- Nota dla IT: Electron 22 EOL od 2023-10; serwer słucha tylko na `127.0.0.1`
  (`__main__.py:61-64`), `contextIsolation`, sandbox renderera; wariant
  przejściowy **z zadeklarowaną datą wygaszenia** = modernizacja sal.
- Próba na jednych zajęciach przed instalacją w całej sali; portable nie dotyka
  instalacji, więc powrót jest natychmiastowy.
- Przy każdym wydaniu `main`: `git merge main` na legacy → konflikt w `build.ps1`
  zawsze „weź `main`, nałóż blok LEGACY OVERRIDES" → zrównać `!define VERSION`
  w obu `.nsi` → `release-check.sh --metadata-only` → build → test w sali tylko
  gdy zmiana dotyczy zajęć w tej sali. W `CHANGELOG.md` wariant nie dostaje
  własnego nagłówka `## x.y.z` (regex `release-check.sh:35`) — punkt w sekcji
  bieżącej wersji.
- Zakończyć linię po modernizacji sal.

## Ryzyka (malejąco)

- **R1** backend nie ładuje się na 8.1 (niskie / krytyczne): cofanie MSVC
  (`_WIN32_WINNT=0x0603`, v142) i R 4.6→4.3 = przebudowa wszystkich modułów,
  `snapshots.js`, `JAMOVI_R_VERSION` — **nie robić**, eskalować do IT.
- **R2** VC++ 14.4x nie działa na 8.1 (wysokie / średnie): DLL app-local
  (Faza 1); jeśli same DLL-e też niezgodne → redist 14.3x (VS 2022 17.9),
  bez zmiany kompilatora.
- **R3** pakiety R (niskie): rdzeń to C/Fortran z Rtools45; `sf`/`terra`
  (jSpace) poza zakresem. Pada pojedynczy pakiet → decyzja per analiza.
- **R4** EOL Chromium (pewne / polityczne): nota dla IT, data wygaszenia.
- **R5** brak podpisu kodu / antywirus (pewne / uciążliwe): test w sali.
- **R6** brak VM 8.1: każda iteracja = wizyta w sali; skrypt zbiera wszystko
  za jednym razem; prosić IT o maszynę testową lub obraz VM z sali.

## Źródła

- Electron: https://www.electronjs.org/blog/windows-7-to-8-1-deprecation-notice
- Python 3.12 / Windows: https://docs.python.org/3.12/using/windows.html
- R / Windows: https://stat.ethz.ch/CRAN/bin/windows/base/rw-FAQ.R-devel.html
- jamovi 2.6 na Windows 7 (analogiczny objaw): https://forum.jamovi.org/viewtopic.php?t=3935
