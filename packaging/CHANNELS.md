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

Zagnieżdżony fork `plots/jamovi/i18n` (TheMacSquared/jmvplots-i18n) ma ten sam układ:
`release/1.0` dla stable, `main` (i śledzona w `plots/.gitmodules` gałąź `jupwr`,
utrzymywana równo z `main`) dla latest. `jmv/jamovi/i18n` to repo upstreamu (jamovi/jmv-i18n),
nie nasz fork — tam nic nie zakładamy.

Przy zakładaniu nowej `release/X.Y` gałąź tworzy się we WSZYSTKICH forkach: jmv, jmvplots,
jamovi-i18n i jmvplots-i18n — na commitach, na które wskazują wskaźniki submodułów.
Gałęzi w forkach nie usuwa się, dopóki wskazuje na nie którykolwiek wydany tag.

## Katalogi robocze: jedna linia = jeden katalog

Nigdy nie przełączaj linii przez `git checkout` w jednym katalogu: zostają artefakty
builda (`packaging/build`, `node_modules`, `engine/jamovi-engine.exe`) i submoduły
z poprzedniej linii. Każda linia ma własny worktree, nazwany od KANAŁU (nie od wersji),
więc nazwy katalogów się nie zmieniają:

```
jamovi-upwr-latest/    main          (główny klon, w nim jest .git)
jamovi-upwr-stable/    release/1.0   (worktree; gałąź zmienia się przy zmianie stable)
```

Jednorazowo na nowej maszynie (Linux/WSL, macOS, Windows — ścieżki analogiczne):

```
git clone --recurse-submodules https://github.com/TheMacSquared/jamovi-upwr.git jamovi-upwr-latest
cd jamovi-upwr-latest
git worktree add ../jamovi-upwr-stable release/1.0
cd ../jamovi-upwr-stable
git submodule update --init --recursive
```

Przemianowanie dawnego układu (`jamovi-upwr` + `jamovi-upwr-1.0`), przy zatrzymanym
Dockerze i bez trwającego buildu, z katalogu nadrzędnego. Stary worktree stable się
USUWA i zakłada od nowa — `git worktree move`/`remove` odmawiają przy submodułach
(„working trees containing submodules cannot be moved or removed”), a po przemianowaniu
głównego klonu pliki `.git` submodułów w worktree mają zaszytą starą ścieżkę względną
(`../../jamovi-upwr/.git/worktrees/...`), której `worktree repair` nie poprawia.
Najpierw sprawdź, że w stable nie ma nic do zachowania (`git status`, `git stash list`):

```
mv jamovi-upwr jamovi-upwr-latest
rm -rf jamovi-upwr-1.0                       # Windows: Remove-Item -Recurse -Force
git -C jamovi-upwr-latest worktree prune -v
git -C jamovi-upwr-latest worktree add ../jamovi-upwr-stable release/1.0
git -C jamovi-upwr-stable submodule update --init --recursive
git -C jamovi-upwr-latest worktree list      # dwie pozycje: latest [main], stable [release/1.0]
```

Inne dawne worktree (np. `jamovi-upwr-28.11`) usuwa się tak samo: `rm -rf` + `prune`.

Tylko Windows: worktree `jamovi-upwr-legacy` (`legacy/win81`) zostaje na miejscu.
Po przemianowaniu głównego klonu: `git -C jamovi-upwr-latest worktree repair
../jamovi-upwr-legacy`, a submoduły trzeba poprawić ręcznie: w pliku `.git`
każdego submodułu ścieżkę `../../jamovi-upwr/.git/worktrees/...` zamienić na
`../../jamovi-upwr-latest/.git/worktrees/...` (sam `worktree repair` tego nie robi).

Potem w `jamovi-upwr-latest` usuń `packaging/build` i `node_modules` (klient, electron,
jamovi-compiler, i18n) — cache CMake, venv i linki w `.bin` mają zaszyte stare ścieżki;
następny build odtworzy je od zera (stable jest świeży). `node_modules` z builda Dockera
może należeć do roota: `docker run --rm -v "$PWD/client:/c" alpine rm -rf /c/node_modules`.
Pamięć Claude Code jest przypisana do ścieżki
(`~/.claude/projects/<ścieżka z myślnikami>/`): przenieś ten katalog pod nazwę wynikającą
z nowej ścieżki, inaczej agent zacznie bez pamięci. Jeśli nowy katalog już istnieje
(otwarto sesję w nowej ścieżce), przenieś do niego zawartość starego, łącznie z `memory/`.

### Zmiana linii stable (np. release/1.0 → release/1.1)

Jedyny moment, gdy katalog zmienia gałąź w miejscu — rzadki i jednokierunkowy:

```
cd jamovi-upwr-stable
git status                      # musi być czysto
git fetch origin && git switch release/1.1
git submodule update --init --recursive
rm -rf packaging/build client/node_modules electron/node_modules \
       jamovi-compiler/node_modules i18n/node_modules
```

Poprawka do zamrożonej linii robi się w tymczasowym worktree
(`git worktree add ../jamovi-upwr-1.0 release/1.0`), usuwanym po wydaniu
(`git worktree remove`).

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
6. Docker build, potem build natywny w katalogu tej linii. Windows: jedno polecenie
   `pwsh -ExecutionPolicy Bypass -File packaging\scripts\windows\release.ps1 -Docker`
   (Docker, build.ps1, NSIS, release-check.sh, `RELEASE-INFO-<wersja>.txt`).
   macOS: skrypty `packaging/scripts/macos/` po kolei. Build nie powinien zostawiać
   zmian w `git status` (npm ci nie zapisuje lockfile'a, pliki przepisane przez jmc
   są przywracane) — jeśli zostawia, to błąd skryptu, nie rzecz do commitowania.
7. Tag `jupwr-<wersja>`, push gałęzi i tagu, GitHub Release.
