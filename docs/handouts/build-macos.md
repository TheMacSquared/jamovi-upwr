# Build natywny macOS — procedura

Powtarzalna procedura budowania paczki jUPWR na macOS arm64: `jUPWR.app`, `.dmg`
oraz pliki `.jmo` modułów opcjonalnych.

**Kiedy:** przy każdym wydaniu jUPWR. `.jmo` jest kompilowany tym samym `jmc`/`jmvcore`
co aplikacja i zawiera pakiet R zbudowany pod platformę hosta, więc **wymaga przebudowy
nawet gdy kod modułu się nie zmienił** — patrz macierz zgodności w
[`packaging/MODULES.md`](../../packaging/MODULES.md).

**Czas:** ~2–3 h ze świeżego klonu.
**Zakres:** *jak* zbudować i zweryfikować na macOS. Kolejność czynności wydania
(bump wersji, CHANGELOG, Docker) jest w [`MODULES.md` → Procedura wydania](../../packaging/MODULES.md).
Szczegóły techniczne faz: [`packaging/10-build-macos.md`](../../packaging/10-build-macos.md).
Odpowiednik dla Windows: [`build-windows.md`](build-windows.md).

---

## 1. Klon

Buduj ze **świeżego klonu** — build dotyka user-lib R, scratcha i payloadu, a resztki
po poprzednich przebiegach dają wynik, którego nie da się odtworzyć.

```bash
cd ~/praca
mv jamovi-upwr jamovi-upwr-OLD 2>/dev/null

git clone --recurse-submodules https://github.com/TheMacSquared/jamovi-upwr.git
cd jamovi-upwr
git submodule update --init --recursive --force   # gdy klon niekompletny
```

### Kontrola klonu — przed budowaniem

```bash
git log --oneline -1
git status --short          # ma byc PUSTO
git submodule status        # cztery wpisy, zaden bez '-' ani '+' na poczatku
```

`-` = submoduł niezainicjalizowany, `+` = wskazuje inny commit niż superprojekt.
Oba znaczą, że klon jest niekompletny — **nie buduj**.

Odczytaj wersje, które będą budowane:

```bash
grep JUPWR_VERSION client/common/jupwr.ts
cat version
grep '^version:' jRISK/jamovi/0000.yaml jSpace/jamovi/0000.yaml
```

> **Wersja modułu mieszka w `jamovi/0000.yaml`, nie w `DESCRIPTION`.** `jmc` czyta ją
> wyłącznie stamtąd ([`index.js:299-306`](../../jamovi-compiler/index.js#L299-L306)).

---

## 2. Build

Skrypty uruchamiaj **po kolei**, z katalogu repo. Każdy kończy się `set -euo pipefail`,
więc pierwszy błąd przerywa fazę.

```bash
bash packaging/scripts/macos/00-prereqs.sh       # Homebrew + weryfikacja toolchainu
bash packaging/scripts/macos/10-client.sh        # vite
bash packaging/scripts/macos/20-modules.sh       # jmvcore + moduly wbudowane (jmc)
bash packaging/scripts/macos/25-i18n.sh          # tlumaczenia
bash packaging/scripts/macos/30-server.sh        # Python 3.12 + jamovi.core/server
bash packaging/scripts/macos/40-engine.sh        # silnik C++
bash packaging/scripts/macos/50-assemble-app.sh  # montaz jUPWR.app (wariant DEV)
bash packaging/scripts/macos/55-relocate.sh      # RELOKOWALNOSC + ponowny podpis
bash packaging/scripts/macos/60-package-dmg.sh   # .dmg
bash packaging/scripts/macos/70-jmo-jrisk.sh     # .jmo jRISK
bash packaging/scripts/macos/71-jmo-jspace.sh    # .jmo jSpace
```

Artefakty: `packaging/build/dist/` — `jUPWR.app`, `jUPWR-<wersja>-arm64.dmg`,
`jRISK_<wersja>-macos-arm64.jmo`, `jSpace_<wersja>-macos-arm64.jmo`.

**Kroku 55 nie pomijaj.** Po `50` aplikacja działa tylko na maszynie deweloperskiej —
linkuje R i biblioteki Homebrew po ścieżkach absolutnych. `55` przenosi je do bundla
i **podpisuje ponownie** (`install_name_tool` unieważnia podpis). Bez tego paczka wymaga
obchodzenia Gatekeepera na komputerze studenta.

**jSpace buduje się BEZ `--skip-deps`** — `jmc` dociąga `sf`/`terra`/`asteRisk` z CRAN
i pakuje je do `.jmo`. Pierwszy przebieg trwa długo i wymaga internetu; `.jmo` jest przez
to dużo większy niż jRISK i to jest oczekiwane.

---

## 3. Weryfikacja `.jmo`

Powstanie pliku niczego nie dowodzi — sprawdź, co jest **w środku**:

```bash
cd packaging/build/dist
unzip -p jRISK_0.3.2-macos-arm64.jmo jRISK/jamovi.yaml | grep -E '^version:|^rVersion:|^build-time:'
```

Oczekiwane:

```
version: 0.3.2              ← wersja MODULU, nie jamovi (28.x)
rVersion: 4.6.0-arm64
build-time: '<dzisiejsza data>'   ← nie z poprzedniego wydania
```

To samo dla jSpace.

**`version: 28.x` oznacza regresję** — skrypt czytałby wersję z `DESCRIPTION` zamiast
z `0000.yaml`. **Stary `build-time`** oznacza, że `.jmo` nie został przebudowany i w paczce
jest artefakt z poprzedniego wydania (tak zdarzyło się na Windows przed `5617104d`).

---

## 4. Testy regresji

Pełną listę przechodź po zmianie wersji jamovi, R lub silnika. Przy rutynowym wydaniu
wystarczą **krytyczne**.

**Krytyczne — blokują wydanie:**

- [ ] aplikacja startuje z `/Applications` (nie tylko z `dist/`), okno się renderuje
- [ ] **uruchomienie na czystym Macu bez Homebrew i bez R** — sedno kroku `55`
- [ ] brak ostrzeżenia Gatekeepera („uszkodzona / niezidentyfikowany deweloper")
- [ ] otwieranie `.omv` — przez menu i przez dwuklik w Finderze
- [ ] zapis i „Zapisz jako" — poprawne rozszerzenie, plik otwiera się ponownie
- [ ] analizy liczą się i renderują (descriptives + dowolny wykres)
- [ ] kopiowanie tabeli wyników → wklej do Pages/Worda
- [ ] kopiowanie wykresu — trafia do schowka jako grafika
- [ ] **sideload `.jmo`** — w *Moduły* widnieje **wersja modułu**, nie 28.x
- [ ] analizy sideloadowanych modułów działają (jRISK: 5, jSpace: 5)

**Ważne:**

- [ ] eksport do PDF, CSV, LaTeX, obrazu
- [ ] jSpace: analiza używająca `sf`/`terra` — moduł ma być samowystarczalny
- [ ] Biblioteka — zbiory jDane otwierają się
- [ ] zmiana języka na polski i z powrotem
- [ ] zakładka **Wykresy** — 7 kategorii, liczba wykresów zgodna z README
- [ ] `.dmg` montuje się i instaluje przeciągnięciem

### Miejsce, które psuje się najczęściej

**Relokowalność.** Objawy: działa u Ciebie, nie działa u studenta („nie można otworzyć",
brak R, crash przy starcie silnika). Diagnostyka na zbudowanej paczce:

```bash
otool -L packaging/build/dist/jUPWR.app/Contents/MacOS/jamovi-engine | grep -E 'homebrew|/opt/|/usr/local'
codesign --verify --deep --strict packaging/build/dist/jUPWR.app
```

Pierwsze polecenie ma nic nie zwrócić (żadnych ścieżek Homebrew), drugie ma przejść bez
błędu. Jeśli `codesign` zgłasza naruszenie — krok `55` przeniósł biblioteki, ale nie
podpisał ich ponownie (naprawione w `de7b8509`).

---

## 5. Zamknięcie

W [`packaging/MODULES.md`](../../packaging/MODULES.md), w sekcjach *jRISK* i *jSpace*,
uzupełnij kolumnę `.jmo` macOS dla bieżącego wydania: `✅ <data>`.

```bash
bash packaging/scripts/release-check.sh    # bez ostrzezen
git add packaging/MODULES.md
git commit -m "MODULES.md: .jmo macOS zbudowane dla <wersja>"
git push
```

### Uwaga: build brudzi drzewo źródłowe

`jmc` przepisuje źródłowe `<moduł>/jamovi/0000.yaml`
([`index.js:576-593`](../../jamovi-compiler/index.js#L576-L593)): `--patch-version` podmienia
`version` na wersję aplikacji, a sama serializacja YAML zmienia cudzysłowy i zawijanie.
Po `20-modules.sh` i krokach `70`/`71` `git status` pokaże ~10 zmienionych plików.

**To są artefakty builda — nie commituj ich.** Przywróć przed commitem:

```bash
git checkout -- '*/jamovi/0000.yaml'
git status --short    # tylko pliki, ktore swiadomie edytowales
```

Nie commituj też `*.h.R`, `jamovi.yaml`, `jamovi-full.yaml`, `*.src.js` ani `.jmo`.

> Windows rozwiązuje to automatycznie — `build.ps1` robi snapshot i odtwarza `0000.yaml`
> po kompilacji (`1f7b84de`). W `20-modules.sh` tego jeszcze nie ma; jeśli zaczniesz sesję
> macOS, to dobry kandydat na przeniesienie tej samej logiki.

---

## Gdy coś pójdzie nie tak

| Objaw | Przyczyna | Co zrobić |
|---|---|---|
| `00-prereqs.sh` zgłasza BRAK | brakuje pakietu Homebrew | log wskaże który; `brew install <pakiet>` |
| app działa u Ciebie, nie u studenta | pominięty krok `55` | uruchom `55-relocate.sh`, sprawdź `otool -L` |
| Gatekeeper: „uszkodzona" | podpis unieważniony przez `install_name_tool` | `55` podpisuje ponownie — sprawdź `codesign --verify` |
| `version: 28.x` w `.jmo` | build ze starego checkoutu | krok 1 od nowa |
| stary `build-time` w `.jmo` | `.jmo` nie został przebudowany | usuń plik z `dist/` i powtórz `70`/`71` |
| jSpace: błąd przy `sf`/`terra` | pierwszy build ciągnie zależności z CRAN | potrzebny internet; drugi raz szybciej (cache w `jSpace/build/`) |
| moduł niezgodny po sideloadzie | `rVersion` ≠ wersja R aplikacji | porównaj `rVersion` z `.jmo` z wersją R w bundlu |
| analiza działa w Dockerze, nie w paczce | brak pakietu R w user-lib | Docker bierze zależności z `jamovi-deps`; build natywny ma własną listę (`20-modules.sh`) |
| `create-dmg` zwraca błąd | AppleScript nie ułoży okna headless | `.dmg` zwykle i tak powstaje — skrypt to sprawdza; zweryfikuj plik |

---

## Definicja ukończenia

- [ ] `.jmo` obu modułów opcjonalnych mają **wersję modułu** i **dzisiejszy `build-time`**
- [ ] oba sideloadują się i pokazują poprawną wersję w *Moduły*
- [ ] `.app` uruchamia się na czystym Macu bez Homebrew i bez R
- [ ] `codesign --verify --deep --strict` przechodzi
- [ ] testy krytyczne przechodzą
- [ ] macierz w `MODULES.md` ma wiersz bieżącego wydania z datą
- [ ] `release-check.sh` bez ostrzeżeń
- [ ] commit bez artefaktów builda

**Poza zakresem:** `.jmo` i instalator dla Windows (osobna maszyna:
[`build-windows.md`](build-windows.md)), notaryzacja u Apple.
