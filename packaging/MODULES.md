# Rejestr modułów jUPWR

Jedno miejsce z odpowiedzią na pytania: *jakie moduły istnieją, które są wbudowane
w instalator, które doinstalowuje się osobno, w jakiej są wersji i pod którą wersję jUPWR
zostały ostatnio zbudowane*. Lista wydań: [`../CHANGELOG.md`](../CHANGELOG.md).
Automatyczna kontrola spójności: `packaging/scripts/release-check.sh`.

## Moduły

| Moduł (katalog) | Nazwa w jamovi | Rola | Dystrybucja | Wersja |
|---|---|---|---|---|
| `jmv/` (submoduł) | jmv | rdzeń analiz jamovi z modyfikacjami forka | wbudowany | 2.8.4 |
| `plots/` (submoduł) | scatr | wykresy (jmvplots) | wbudowany | 2.8.3 |
| `jdistrACTION/` | distrACTION | rozkłady prawdopodobieństwa (fork, PL, 11 rozkładów) | wbudowany | 1.3.1 |
| `jCI/` | jCI | przedziały ufności | wbudowany | 0.1.0 |
| `jperm/` | jperm | testy permutacyjne | wbudowany | 0.1.0 |
| `jboot/` | jboot | bootstrap | wbudowany | 0.2.2 |
| `jRISK/` | jRISK | ryzyko i niezawodność (jeden kurs) | **opcjonalny — `.jmo` (sideload)** | 0.3.2 |

Zasada: **wbudowane** są moduły używane w większości kursów statystyki; moduł obsługujący
jeden kurs jest **opcjonalny** i trafia do studentów jako plik `.jmo` (Moduły → Sideload).
Dzięki temu poprawka w takim module nie wymaga reinstalacji aplikacji.

Gdzie jest zdefiniowana lista wbudowanych (musi być identyczna w trzech miejscach):
`docker/jamovi-Dockerfile` (bloki `COPY`+`jmc --install`), `packaging/scripts/macos/20-modules.sh`
(`MODULES=`), `packaging/scripts/windows/build.ps1` (`$Modules`).

## Moduły opcjonalne — jak się je buduje

Plik `.jmo` zawiera pakiet R **skompilowany pod platformę hosta**, więc każda platforma
wymaga osobnego builda, na maszynie z tą platformą:

| Platforma | Skrypt | Wynik |
|---|---|---|
| macOS arm64 | `packaging/scripts/macos/70-jmo-jrisk.sh` | `packaging/build/dist/jRISK_<wersja>-macos-arm64.jmo` |
| Windows x64 | `packaging/scripts/windows/build.ps1` (krok 4e) | `packaging\build\dist\jRISK_<wersja>-win64.jmo` |

Nowy moduł opcjonalny = kopia tych dwóch kroków z podmienioną nazwą + wiersz w tabeli wyżej.

## Macierz zgodności (moduły opcjonalne ↔ wydania jUPWR)

`.jmo` jest budowany tym samym `jmc`/`jmvcore` co aplikacja, więc **przy każdym wydaniu
jUPWR moduły opcjonalne trzeba przebudować i dołączyć do wydania**, nawet gdy ich kod się
nie zmienił. Ta tabela mówi, czy to zrobiono.

| jUPWR | jamovi | jRISK | `.jmo` macOS | `.jmo` Windows |
|---|---|---|---|---|
| 0.8.0 | 28.1 | 0.3.2 | ✅ 2026-08-23 (z 0.7.8, bez zmian) | ⬜ do zbudowania |
| 0.7.8 | 28.1 | 0.3.2 | ✅ 2026-08-23 | ⬜ do zbudowania |

(Wcześniejsze wydania: jRISK był wbudowany — macierz nie dotyczy.)

## Procedura wydania jUPWR — checklist

1. Bump wersji: `client/common/jupwr.ts` i tag w `docker-compose.yaml` (skrypty packagingu
   czytają wersję z `jupwr.ts`); wersje modułów bumpuj tylko tam, gdzie zmienił się kod.
2. Wpis w `CHANGELOG.md` (wersja, data, zmiany, wersje modułów).
3. Docker: `docker compose --profile main build` + smoke test.
4. Build natywny dla bieżącej maszyny (macOS: `00`→`60`, **plus `70-jmo-*` dla każdego modułu
   opcjonalnego**; Windows: `build.ps1`, który robi `.jmo` w kroku 4e).
5. Uzupełnij macierz zgodności wyżej (które `.jmo` zbudowano, na jakiej platformie).
6. `packaging/scripts/release-check.sh` — zero ostrzeżeń.
7. `git status` bez artefaktów builda → commit → push.
