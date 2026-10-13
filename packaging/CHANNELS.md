# Linie wydań jUPWR (kanały)

Ten plik jest identyczny na wszystkich aktywnych gałęziach. Zmiany wprowadzaj na
`main` i przenoś cherry-pickiem na gałęzie `release/*`.

## Aktualne linie

| Kanał | Gałąź | jUPWR | jamovi | Submoduły jmv/plots/i18n | Co tu trafia |
|---|---|---|---|---|---|
| stable | `release/1.0` | 1.0.x | 28.2 | `release/1.0` | tylko poprawki błędów |
| latest | `main` | 1.1.x | 28.11 | `main` | nowe funkcje, nowy upstream, poprawki |
| legacy | `legacy/win81` | 1.0.4 | 28.2 | (stan z gałęzi) | zamrożona; Windows 8.1, osobny Electron i R; bez przenoszenia poprawek w żadną stronę (`packaging/30-legacy-win81.md`) |

Kanał to etykieta, gałąź to nazwa stała. Gdy latest dojrzeje do wydania dla studentów:
z `main` zakłada się `release/1.1` (w głównym repo i w submodułach jmv, plots, i18n),
ta gałąź staje się stable, a `release/1.0` zostaje zamrożona. Wtedy aktualizuje się
tabelę powyżej (na `main` i nowej `release/*`).

`readstat` jest wspólny dla wszystkich linii (gałąź `sync-28`).

## Katalogi robocze: jedna linia = jeden katalog

Nigdy nie przełączaj linii przez `git checkout` w jednym katalogu: zostają artefakty
builda (`packaging/build`, `node_modules`, `engine/jamovi-engine.exe`) i submoduły
z poprzedniej linii. Każda linia ma własny worktree:

```
<repo>/            main          (latest)
<repo>-1.0/        release/1.0   (stable)
```

Jednorazowo na każdej maszynie (Linux/WSL, macOS, Windows — ścieżki analogiczne):

```
cd <repo>
git fetch origin
git checkout main && git pull --ff-only
git submodule update --init --recursive
git worktree add ../<repo>-1.0 release/1.0
cd ../<repo>-1.0
git submodule update --init --recursive
```

Kontener Dockera ma stałą nazwę `jamovi` i stałe porty — nie uruchamiaj obu linii
naraz (`docker compose --profile main down` przed przejściem do drugiego katalogu).
Obrazy różnią się tagiem (`jupwr/jupwr:<wersja>`), więc build nie nadpisuje drugiej linii.

## Przenoszenie poprawek

- Nowa funkcja: tylko `main`.
- Poprawka błędu: commit na linii, na której błąd znaleziono, potem od razu
  `git cherry-pick -x <sha>` na drugą aktywną linię. `-x` zostawia w opisie
  „(cherry picked from commit …)”, po którym `release-check.sh` rozpoznaje przeniesienie.
- Commit, który z założenia dotyczy tylko stable (np. `.gitmodules`, wersja 1.0.x,
  wpis CHANGELOG wydania 1.0.x), dostaje w opisie linię `Tylko-stable: <powód>`.
- Hurtowa synchronizacja stable → main (gdy poprawek jest dużo): zwykły
  `git merge release/1.0` na `main`. Nigdy force push na `main` ani `release/*`.
- Zmiana w submodule (jmv, plots, i18n): commit w submodule na jego gałęzi tej samej
  nazwy co linia (`release/1.0` albo `main`), push submodułu, dopiero potem commit
  wskaźnika w głównym repo. Poprawkę w submodule przenosi się cherry-pickiem tak samo.

## Wersje i tagi

- Wersja jUPWR musi należeć do serii gałęzi: na `release/1.0` tylko `1.0.x`.
  Na `main` seria wyższa niż każda istniejąca `release/*`.
- Tag wydania: `jupwr-<wersja>` (np. `jupwr-1.0.8`) na commicie wydania. Tagi `v2.7.x`
  i `v28.x` należą do upstreamu jamovi — nie używać tego schematu.
- GitHub Releases: wydanie stable jako „Latest release”, wydanie latest jako „Pre-release”.
- CHANGELOG.md każda linia prowadzi u siebie. Poprawka przeniesiona ze stable dostaje
  wpis w obu CHANGELOG-ach (w sekcji wydania danej linii).
- `.jmo` modułów opcjonalnych buduje się osobno dla każdej linii (inna wersja jamovi);
  macierz zgodności w `packaging/MODULES.md` podaje wersję jUPWR, więc wiadomo, z której
  linii pochodzi artefakt.

## Checklista wydania (każda linia)

1. Jesteś w katalogu tej linii (`git rev-parse --abbrev-ref HEAD`).
2. `git pull --ff-only` w głównym repo, `git submodule update --init --recursive`.
3. Wersja jUPWR w `client/common/jupwr.ts`, `docker-compose.yaml`, `jUPWR.nsi`
   (i skryptach macOS, jeśli zawierają wersję) — z serii tej linii.
4. Wpis w `CHANGELOG.md`, wiersze w `packaging/MODULES.md`.
5. `packaging/scripts/release-check.sh` bez ostrzeżeń — sprawdza też gałąź vs wersja,
   jamovi vs tabela powyżej, wskaźniki submodułów i nieprzeniesione poprawki.
6. Docker build, potem build natywny w katalogu tej linii.
7. Tag `jupwr-<wersja>`, push gałęzi i tagu, GitHub Release.
