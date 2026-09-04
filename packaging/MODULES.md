# Rejestr modułów jUPWR

Jedno miejsce z odpowiedzią na pytania: *jakie moduły istnieją, które są wbudowane
w instalator, które doinstalowuje się osobno, w jakiej są wersji i pod którą wersję jUPWR
zostały ostatnio zbudowane*. Lista wydań: [`../CHANGELOG.md`](../CHANGELOG.md).
Automatyczna kontrola spójności: `packaging/scripts/release-check.sh`.

## Moduły

| Moduł (katalog) | Nazwa w jamovi | Rola | Dystrybucja | Wersja |
|---|---|---|---|---|
| `jmv/` (submoduł) | jmv | rdzeń analiz jamovi z modyfikacjami forka | wbudowany | 2.8.4 |
| `plots/` (submoduł) | scatr | wykresy (jmvplots; kategorie dydaktyczne, 26 wykresów) | wbudowany | 2.9.0 |
| `jdistrACTION/` | distrACTION | rozkłady prawdopodobieństwa (fork, PL, 11 rozkładów) | wbudowany | 1.3.2 |
| `jCI/` | jCI | przedziały ufności | wbudowany | 0.1.0 |
| `jperm/` | jperm | testy permutacyjne | wbudowany | 0.1.0 |
| `jboot/` | jboot | bootstrap | wbudowany | 0.3.0 |
| `jDane/` | jDane | zbiory danych do zajęć (Biblioteka, bez analiz) | wbudowany | 0.7.0 |
| `jCzest/` | jCzest | częstości: tabela kontyngencji, test zgodności, próby zależne (McNemar, Q Cochrana) | wbudowany | 0.1.0 |
| `jANOVA/` | jANOVA | ANOVA jUPWR: ANOVA z blokami i kowariantami, ANOVA powtórzonych pomiarów (format długi), litery/NIR; zastępuje w menu ANOVĘ jmv (ukrytą w kliencie) | wbudowany | 0.3.0 |
| `jTestyT/` | jTestyT | testy t (jedna próba, dwie grupy, sparowane) z prostym panelem, d Cohena z CI, wykresem estymacyjnym; zastępuje w menu testy t jmv (ukryte w kliencie) | wbudowany | 0.2.0 |
| `jRISK/` | jRISK | ryzyko i niezawodność (jeden kurs) | **opcjonalny — `.jmo` (sideload)** | 0.3.3 |
| `jSpace/` | jSpace | statystyka danych kosmicznych: orbity TLE/SGP4, mapy sf, rastry terra, klasyfikacja (jeden kurs) | **opcjonalny — `.jmo` (sideload)** | 0.3.0 |
| `jRol/` | jRol | doświadczalnictwo rolnicze: układy CRD/RCBD/kwadrat łaciński/split-plot, porównania wielokrotne z literami i NIR, plan doświadczenia, 2 zbiory danych (jeden kurs) | **opcjonalny — `.jmo` (sideload)** | 0.1.0 |

Zasada: **wbudowane** są moduły używane w większości kursów statystyki; moduł obsługujący
jeden kurs jest **opcjonalny** i trafia do studentów jako plik `.jmo` (Moduły → Sideload).
Dzięki temu poprawka w takim module nie wymaga reinstalacji aplikacji.

Gdzie jest zdefiniowana lista wbudowanych (musi być identyczna w trzech miejscach):
`docker/jamovi-Dockerfile` (bloki `COPY`+`jmc --install`), `packaging/scripts/macos/20-modules.sh`
(`MODULES=`), `packaging/scripts/windows/build.ps1` (`$Modules`).

## Moduły opcjonalne — jak się je buduje

Plik `.jmo` zawiera pakiet R **skompilowany pod platformę hosta**, więc każda platforma
wymaga osobnego builda, na maszynie z tą platformą:

| Moduł | Platforma | Skrypt | Wynik |
|---|---|---|---|
| jRISK | macOS arm64 | `packaging/scripts/macos/70-jmo-jrisk.sh` | `packaging/build/dist/jRISK_<wersja>-macos-arm64.jmo` |
| jRISK | Windows x64 | `packaging/scripts/windows/build.ps1` (krok 4e) | `packaging\build\dist\jRISK_<wersja>-win64.jmo` |
| jSpace | macOS arm64 | `packaging/scripts/macos/71-jmo-jspace.sh` | `packaging/build/dist/jSpace_<wersja>-macos-arm64.jmo` |
| jSpace | Windows x64 | `packaging/scripts/windows/build.ps1` (krok 4f) | `packaging\build\dist\jSpace_<wersja>-win64.jmo` |
| jRol | macOS arm64 | `packaging/scripts/macos/72-jmo-jrol.sh` | `packaging/build/dist/jRol_<wersja>-macos-arm64.jmo` |
| jRol | Windows x64 | `packaging/scripts/windows/build.ps1` (krok 4g) | `packaging\build\dist\jRol_<wersja>-win64.jmo` |

Nowy moduł opcjonalny = kopia tych dwóch kroków z podmienioną nazwą + wiersz w tabeli wyżej.

**jSpace buduje się BEZ `--skip-deps`** (inaczej niż jRISK): jmc doinstalowuje wtedy
`sf`, `terra` i `asteRisk` (z zależnościami) do `jSpace/build/R<ver>-<platforma>/`
z przypiętego snapshotu CRAN i pakuje je do `.jmo`. Moduł jest przez to samowystarczalny
po sideloadzie (binaria CRAN sf/terra zawierają GDAL/GEOS/PROJ), ale plik `.jmo` jest
dużo większy niż jRISK — to oczekiwane.

## Macierz zgodności (moduły opcjonalne ↔ wydania jUPWR)

`.jmo` jest budowany tym samym `jmc`/`jmvcore` co aplikacja, więc **przy każdym wydaniu
jUPWR moduły opcjonalne trzeba przebudować i dołączyć do wydania**, nawet gdy ich kod się
nie zmienił. Ta tabela mówi, czy to zrobiono.

### jRISK

| jUPWR | jamovi | jRISK | `.jmo` macOS | `.jmo` Windows |
|---|---|---|---|---|
| 0.9.5.6 | 28.2 | 0.3.3 | ⬜ do zbudowania | ✅ 2026-09-04 |
| 0.9.5.5 | 28.2 | 0.3.3 | ⬜ do zbudowania | ✅ 2026-09-04 |
| 0.9.5 | 28.2 | 0.3.3 | ⬜ do zbudowania | ✅ 2026-09-04 |
| 0.9.4 | 28.2 | 0.3.3 | ✅ 2026-09-03 | ⬜ do zbudowania |
| 0.9.4 | 28.2 | 0.3.2 | ✅ 2026-09-03 | ✅ 2026-09-03 |
| 0.9.2.2 | 28.2 | 0.3.2 | ✅ 2026-08-31 | ✅ 2026-08-31 |
| 0.9.2.1 | 28.2 | 0.3.2 | ⬜ do zbudowania | ✅ 2026-08-31 |
| 0.9.2 | 28.2 | 0.3.2 | ✅ 2026-08-30 | ✅ 2026-08-30 |
| 0.9.1 | 28.2 | 0.3.2 | ⬜ do zbudowania | ⬜ do zbudowania |
| 0.9.0 | 28.2 | 0.3.2 | ⬜ do zbudowania | ✅ 2026-08-30 |
| 0.8.6 | 28.1 | 0.3.2 | ⬜ do zbudowania | ✅ 2026-08-26 |
| 0.8.5 | 28.1 | 0.3.2 | ⬜ do zbudowania | ⬜ do zbudowania |
| 0.8.0 | 28.1 | 0.3.2 | ✅ 2026-08-23 (z 0.7.8, bez zmian) | ⬜ do zbudowania |
| 0.7.8 | 28.1 | 0.3.2 | ✅ 2026-08-23 | ⬜ do zbudowania |

(Wcześniejsze wydania: jRISK był wbudowany — macierz nie dotyczy.)

### jSpace

| jUPWR | jamovi | jSpace | `.jmo` macOS | `.jmo` Windows |
|---|---|---|---|---|
| 0.9.5.6 | 28.2 | 0.3.0 | ⬜ do zbudowania | ✅ 2026-09-04 |
| 0.9.5.5 | 28.2 | 0.2.2 | ⬜ do zbudowania | ✅ 2026-09-04 |
| 0.9.5 | 28.2 | 0.2.2 | ⬜ do zbudowania | ✅ 2026-09-04 |
| 0.9.4 | 28.2 | 0.2.2 | ✅ 2026-09-03 | ⬜ do zbudowania |
| 0.9.4 | 28.2 | 0.2.1 | ✅ 2026-09-03 (zastąpiony przez 0.2.2) | ⬜ |
| 0.9.4 | 28.2 | 0.2.0 | ✅ 2026-09-03 (wadliwy na macOS — nie ładuje sf/asteRisk) | ✅ 2026-09-03 |
| 0.9.2.2 | 28.2 | 0.2.0 | ✅ 2026-08-31 | ✅ 2026-08-31 |
| 0.9.2.1 | 28.2 | 0.2.0 | ⬜ do zbudowania | ✅ 2026-08-31 |
| 0.9.2 | 28.2 | 0.2.0 | ✅ 2026-08-30 | ✅ 2026-08-30 |
| 0.9.1 | 28.2 | 0.2.0 | ⬜ do zbudowania | ⬜ do zbudowania |
| 0.9.0 | 28.2 | 0.2.0 | ⬜ do zbudowania | ✅ 2026-08-30 |
| 0.8.6 | 28.1 | 0.2.0 | ⬜ do zbudowania | ✅ 2026-08-26 |
| 0.8.5 | 28.1 | 0.2.0 | ⬜ do zbudowania | ⬜ do zbudowania |
| 0.8.0 | 28.1 | 0.2.0 | ⬜ do zbudowania | ⬜ do zbudowania |

### jRol

| jUPWR | jamovi | jRol | `.jmo` macOS | `.jmo` Windows |
|---|---|---|---|---|
| 0.9.5.6 | 28.2 | 0.1.0 | ⬜ do zbudowania | ✅ 2026-09-04 |
| 0.9.5.5 | 28.2 | 0.1.0 | ⬜ do zbudowania | ✅ 2026-09-04 |
| 0.9.5 | 28.2 | 0.1.0 | ⬜ do zbudowania | ✅ 2026-09-04 |
| 0.9.4 | 28.2 | 0.1.0 | ✅ 2026-09-03 | ✅ 2026-09-03 |
| 0.9.2.2 | 28.2 | 0.1.0 | ⬜ do zbudowania | ⬜ do zbudowania |

## Procedura wydania jUPWR — checklist

1. Bump wersji: `client/common/jupwr.ts` i tag w `docker-compose.yaml` (skrypty packagingu
   czytają wersję z `jupwr.ts`); wersje modułów bumpuj tylko tam, gdzie zmienił się kod.
2. Wpis w `CHANGELOG.md` (wersja, data, zmiany, wersje modułów).
3. Docker: `docker compose --profile main build` + smoke test.
4. Build natywny dla bieżącej maszyny (macOS: `00`→`60`, **plus `70-jmo-*` dla każdego modułu
   opcjonalnego**; Windows: `build.ps1`, który robi `.jmo` w krokach 4e i 4f).
   Pełna procedura z testami: Windows — [`docs/handouts/build-windows.md`](../docs/handouts/build-windows.md),
   macOS — [`docs/handouts/build-macos.md`](../docs/handouts/build-macos.md).
5. Uzupełnij macierz zgodności wyżej (które `.jmo` zbudowano, na jakiej platformie).
6. `packaging/scripts/release-check.sh` — zero ostrzeżeń.
7. `git status` bez artefaktów builda → commit → push.
