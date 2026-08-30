# Pakowanie i dystrybucja jUPWR

Dokumentacja i skrypty budowania **natywnych installek** jUPWR dla Windows i macOS,
do rozdania studentom na ich własnych komputerach (bez Dockera).

## Strategia

Pełny natywny build z brandingiem jUPWR i wszystkimi modułami wbudowanymi (jmv z modyfikacjami
forka, jCI, jboot, jperm, distrACTION, scatr; jRISK osobno jako .jmo). Aplikacja = powłoka Electron + serwer Python +
silnik C++ + runtime R + moduły. Szczegóły anatomii: [`00-architecture.md`](00-architecture.md).

## Macierz deliverables

| OS | Arch | Format | Status (2026-06) |
|---|---|---|---|
| macOS | arm64 (Apple Silicon) | `.dmg` | **Relokowalny build przetestowany** — wbudowany R + Python + dyliby; w trybie wymuszonego bundla silnik ładuje R+jmv z bundla, serwer+klient działają. `.dmg` ~517 MB. Pozostaje walidacja na fizycznie czystym Macu + eksport wektorowy wymaga X11. |
| Windows | x64 | `.exe` (NSIS) + portable `.zip` | Udokumentowane (analogia macOS + Dockerfile). **Do przetestowania na maszynie Windows.** |

Podpisywanie kodu: **na razie brak** — installki będą wywoływać ostrzeżenia Gatekeeper (macOS)
i SmartScreen (Windows). Obejścia dla studentów: [`30-distribution.md`](30-distribution.md).

## Dokumenty

- [`00-architecture.md`](00-architecture.md) — z czego składa się aplikacja, układ bundla, `env.conf`.
- [`10-build-macos.md`](10-build-macos.md) — build macOS arm64 krok po kroku (przetestowany).
- [`20-build-windows.md`](20-build-windows.md) — build Windows x64 (do walidacji).
- [`30-distribution.md`](30-distribution.md) — wydanie studentom, obejścia ostrzeżeń, instrukcja instalacji.

## Skrypty (macOS)

`scripts/macos/` — uruchamiane po kolei (numeracja = kolejność):

```
00-prereqs.sh      Homebrew + weryfikacja toolchainu
10-client.sh       vite build (frontend)
20-modules.sh      jmvcore + jmc install (wszystkie moduły R)
25-i18n.sh         kompilacja tłumaczeń
30-server.sh       venv 3.12 + requirements + jamovi.core/server/readstat
40-engine.sh       kompilacja silnika C++
50-assemble-app.sh montaż jUPWR.app (wariant DEV)
55-relocate.sh     RELOKACJA: wbuduj R + Python-standalone + dyliby (dla czystej maszyny)
60-package-dmg.sh  jUPWR.dmg
lib.sh             wspólne ścieżki/zmienne (sourcowane przez resztę)
```

`55-relocate.sh` jest WYMAGANY do dystrybucji (czyni `.app` niezależną od Homebrew/R). Pomiń tylko
do szybkiego testu na maszynie deweloperskiej.

Katalog roboczy: `packaging/build/` (ignorowany przez git). Wyniki: `packaging/build/dist/`.

`scripts/windows/` — `build.ps1` (natywny build) + NSIS. Używane w praktyce: `.jmo`
jRISK/jSpace dla win64 buduje się tą ścieżką (patrz macierz w `MODULES.md`).

## TODO / znane braki

- **Pełne zależności R modułu jmv — dotyczy TYLKO buildów natywnych.** W buildach
  natywnych (macOS, Windows) zależności `Imports` z `jmv/DESCRIPTION` trzeba doinstalować
  ręcznie do `modules/base/R` — listę utrzymują `20-modules.sh` (macOS) i `build.ps1`
  krok 4b (Windows). Braki objawiają się dopiero przy uruchomieniu analizy („no package …"),
  nie przy budowaniu, więc po dołożeniu nowej analizy sprawdź, czy jej pakiety są na liście.

  **Docker tego problemu nie ma**: 21 z 24 pakietów `Imports` przychodzi z obrazu bazowego
  `jamovi/jamovi-deps`, a `jamovi-Dockerfile` dokłada jawnie tylko brakujące trzy
  (`ggridges`, `hexbin`, `GGally`). Zweryfikowane na obrazie 0.8.6: wszystkie 24 pakiety
  `Imports` ładują się, a 16 analiz (t-test, ANOVA, MANCOVA, regresja logistyczna,
  tabele kontyngencji, EFA/PCA, BayesFactor …) liczy się poprawnie.
- **Walidacja na fizycznie czystym Macu** (bez Homebrew/R) — relokowalność potwierdzona w trybie
  wymuszonego bundla; do potwierdzenia na maszynie bez devtoolów.
- **Eksport wykresów do SVG/PDF/EPS** — używa `cairo`→X11 (brak na czystym Macu); wyświetlanie PNG
  (ragg) działa. Rozważyć `svglite` albo dobundlowanie XQuartz.
- **Podpis/notaryzacja** — instalki wywołują ostrzeżenia (świadomie pominięte; patrz `30-distribution.md`).
- **Build Windows — instalator** — `build.ps1` przechodzi i produkuje `.jmo` (potwierdzone
  2026-08-26, macierz w `MODULES.md`); pełny instalator NSIS wymaga jeszcze walidacji
  na czystej maszynie Windows.
- **Optymalizacja rozmiaru** — przyciąć bibliotekę R do domknięcia zależności modułów (~886 MB → mniej).

## Szybki start (macOS arm64)

```bash
cd packaging/scripts/macos
for s in 00-prereqs 10-client 20-modules 25-i18n 30-server 40-engine 50-assemble-app 55-relocate 60-package-dmg; do
    ./$s.sh || { echo "FAIL: $s"; break; }
done
open ../../build/dist/jUPWR.app     # uruchom zbudowaną aplikację
```
