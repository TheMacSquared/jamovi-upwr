# Build natywny Windows — procedura

Powtarzalna procedura budowania paczki jUPWR na Windows x64: instalator NSIS,
portable `.zip` oraz pliki `.jmo` modułów opcjonalnych.

**Kiedy:** przy każdym wydaniu jUPWR. `.jmo` jest kompilowany tym samym `jmc`/`jmvcore`
co aplikacja, więc **wymaga przebudowy nawet gdy kod modułu się nie zmienił** —
patrz macierz zgodności w [`packaging/MODULES.md`](../../packaging/MODULES.md).

**Czas:** ~2–3 h ze świeżego klonu (krócej przy ciepłym cache).
**Zakres:** ten dokument opisuje *jak* zbudować i zweryfikować na Windows.
Kolejność czynności wydania (bump wersji, CHANGELOG, Docker) jest w
[`MODULES.md` → Procedura wydania](../../packaging/MODULES.md).

---

## 1. Klon

Buduj ze **świeżego klonu**, nie z katalogu roboczego — build dotyka wielu ścieżek
(user-lib R, scratch, payload), a resztki po poprzednich przebiegach potrafią dać
wynik, którego nie da się odtworzyć.

```powershell
cd D:\praca
Rename-Item jamovi-upwr jamovi-upwr-OLD -EA SilentlyContinue

git clone --recurse-submodules https://github.com/TheMacSquared/jamovi-upwr.git
cd jamovi-upwr
```

Gdy `--recurse-submodules` nie dociągnie wszystkiego:

```powershell
git submodule update --init --recursive --force
```

### Kontrola klonu — przed budowaniem

```powershell
git log --oneline -1
git status --short          # ma byc PUSTO
git submodule status        # cztery wpisy, zaden bez '-' ani '+' na poczatku
```

`-` = submoduł niezainicjalizowany, `+` = wskazuje inny commit niż superprojekt.
Oba znaczą, że klon jest niekompletny — **nie buduj**, powtórz `submodule update`.

Odczytaj wersje, które za chwilę będą budowane (przydadzą się w krokach 3–5):

```powershell
Select-String -Path "client\common\jupwr.ts"  -Pattern "JUPWR_VERSION"
Get-Content version
Select-String -Path "jRISK\jamovi\0000.yaml"  -Pattern "^version:"
Select-String -Path "jSpace\jamovi\0000.yaml" -Pattern "^version:"
Select-String -Path "packaging\scripts\windows\build.ps1" -Pattern 'ElectronVer='
```

> **Wersja modułu mieszka w `jamovi/0000.yaml`, nie w `DESCRIPTION`.** `jmc` czyta ją
> wyłącznie stamtąd ([`index.js:299-306`](../../jamovi-compiler/index.js#L299-L306)).
> `release-check.sh` pilnuje zgodności obu plików.

Stary katalog zostaw do czasu przejścia testów. Gdyby zostały tam niezacommitowane
zmiany: `cd ..\jamovi-upwr-OLD; git status --short; git stash list`.

---

## 2. Build

```powershell
cd D:\praca\jamovi-upwr
powershell -ExecutionPolicy Bypass -File packaging\scripts\windows\build.ps1
```

Prereqs sprawdzane na starcie: VS2022 Build Tools (VCTools), RTools45, R 4.6.0,
Boost 1.84, Node 20+, cmake, protoc.

Skrypt jest idempotentny tam, gdzie to tanie. `.jmo` powstają w krokach **4e** (jRISK)
i **4f** (jSpace); jSpace budowany jest **bez** `--skip-deps` — `jmc` dociąga `sf`/`terra`/
`asteRisk` z przypiętego snapshotu CRAN i pakuje je do `.jmo`, żeby moduł był
samowystarczalny. Pierwszy build tego kroku trwa długo i wymaga internetu.

Artefakty: `packaging\build\dist\`.

---

## 3. Weryfikacja `.jmo`

Powstanie pliku niczego nie dowodzi — sprawdź, co jest **w środku**. Podstaw wersje
odczytane w kroku 1:

```powershell
cd packaging\build\dist
$v = "0.3.2"   # wersja jRISK z jRISK\jamovi\0000.yaml
Expand-Archive -Path "jRISK_$v-win64.jmo" -DestinationPath tmp-jrisk -Force
Select-String -Path "tmp-jrisk\jRISK\jamovi.yaml" -Pattern "^version:|^rVersion:"
```

Oczekiwane:

```
version: 0.3.2          ← wersja MODUŁU, nie jamovi (28.x)
rVersion: 4.6.0-x64
```

To samo dla jSpace. Po sprawdzeniu skasuj katalogi `tmp-*`.

**`version: 28.x` oznacza regresję** — skrypty czytałyby wtedy wersję z `DESCRIPTION`
zamiast z `0000.yaml` (błąd naprawiony w `21000ae0`). Sprawdź, czy build nie poszedł
ze starego checkoutu.

---

## 4. Weryfikacja Electrona

Aplikacja → **Ctrl+Shift+I** → konsola:

```js
process.versions.electron    // ma zgadzac sie z $ElectronVer z build.ps1
```

Skrypt cache'uje pobrany ZIP (nazwa zawiera wersję, więc bump wymusza pobranie).
Jeśli widzisz starą wersję — skasuj `packaging\build\scratch\electron*.zip` i przebuduj.

> **Nie podbijaj Electrona do 44+ bez przepisania schowka.** Od 44 moduł `clipboard`
> działa po W3C (metody zwracają Promise, `readHTML`/`writeHTML` usunięte), a
> [`main.js:511-523`](../../electron/app/main.js#L511-L523) używa go synchronicznie —
> kopiowanie tabel i wklejanie z Excela przestałoby działać. Linia 43 to najstarsza
> wspierana bez zmian w kodzie.

---

## 5. Testy regresji

Pełną listę przechodź po zmianie Electrona, wersji jamovi lub silnika.
Przy rutynowym wydaniu wystarczą pozycje **krytyczne**.

**Krytyczne — blokują wydanie:**

- [ ] aplikacja startuje, okno się renderuje (nie biały ekran)
- [ ] otwieranie `.omv` — przez menu i przez dwuklik w Eksploratorze
- [ ] zapis i „Zapisz jako" — dialog, poprawne rozszerzenie, plik otwiera się ponownie
- [ ] analizy liczą się i renderują (descriptives + dowolny wykres)
- [ ] **kopiowanie tabeli wyników → wklej do Worda** *(schowek — patrz uwaga wyżej)*
- [ ] **kopiowanie wykresu** — trafia do schowka jako grafika
- [ ] **wklejanie z Excela do arkusza** *(używa `readHTML`)*
- [ ] **brak monitu o dostęp do lokalizacji przy starcie** — patrz niżej
- [ ] **sideload `.jmo`** — w *Moduły* widnieje **wersja modułu**, nie 28.x
- [ ] analizy sideloadowanych modułów działają (jRISK: 5, jSpace: 5)

**Ważne:**

- [ ] eksport do PDF, CSV, LaTeX, obrazu
- [ ] jSpace: analiza używająca `sf`/`terra` — moduł ma być samowystarczalny
- [ ] menu aplikacji pełne i klikalne
- [ ] zoom Ctrl +/− oraz `Ctrl+Shift+±`
- [ ] Biblioteka — zbiory jDane otwierają się
- [ ] zmiana języka na polski i z powrotem
- [ ] zakładka **Wykresy** — 7 kategorii, liczba wykresów zgodna z README
- [ ] instalator NSIS instaluje się do `%ProgramFiles%` i uruchamia

### Miejsce, które psuje się najczęściej

[`main.js:218-248`](../../electron/app/main.js#L218-L248) włącza `NetworkServiceSandbox`,
żeby uniknąć systemowego monitu o lokalizację (Windows gatuje odczyt SSID za tym
uprawnieniem). To kod upstreamu ściśle związany z zachowaniem Chromium — **przy każdej
zmianie wersji Electrona sprawdź to w pierwszej kolejności**.

Objawy: białe okno przy starcie albo pytanie o dostęp do lokalizacji.
Diagnostyka: zakomentuj `appendSwitch('enable-features', 'NetworkServiceSandbox')`.

Sandbox wymaga, żeby aplikacja leżała tam, gdzie token AppContainer ma prawo odczytu.
Build **NSIS** (`%ProgramFiles%`) i MSIX spełniają ten warunek; **portable `.zip`
rozpakowany do Pobranych/Pulpitu — nie** („Sandbox cannot access executable", pętla
crashów). Testuj build NSIS.

---

## 6. Zamknięcie

W [`packaging/MODULES.md`](../../packaging/MODULES.md), w sekcjach *jRISK* i *jSpace*,
dodaj wiersz dla bieżącego wydania: `| <jUPWR> | <jamovi> | <wersja modułu> | ⬜ | ✅ <data> |`.
Kolumna *jamovi* to zawartość pliku `version` (dwa pierwsze człony, np. `28.2`).

```powershell
bash packaging\scripts\release-check.sh    # bez brakow dla win64
git add packaging\MODULES.md CHANGELOG.md
git commit -m "MODULES.md: .jmo win64 zbudowane dla <wersja>"
git push
```

`git status` ma pokazywać **tylko** pliki, które świadomie edytowałeś — żadnych
`*.h.R`, `jamovi.yaml`, `*.src.js` ani `.jmo` (te są w `.gitignore`).

---

## Gdy coś pójdzie nie tak

| Objaw | Przyczyna | Co zrobić |
|---|---|---|
| `.jmo` nie powstaje | brak prereq | log wskaże który; lista na starcie skryptu |
| jSpace: błąd przy `sf`/`terra` | pierwszy build ciągnie zależności z CRAN | potrzebny internet; `jSpace\build\R*-win64` cache'owany, drugi raz szybciej |
| `version: 28.x` w `.jmo` | build ze starego checkoutu | krok 1 od nowa |
| stara wersja Electrona w konsoli | ZIP w cache | skasuj `packaging\build\scratch\electron*.zip` |
| białe okno przy starcie | sandbox usługi sieciowej | patrz sekcja o `NetworkServiceSandbox` |
| „Sandbox cannot access executable" | portable poza `%ProgramFiles%` | testuj build NSIS |
| kopiowanie/wklejanie nie działa | Electron 44+ zmienił API schowka | sprawdź `process.versions.electron` |
| moduł niezgodny po sideloadzie | `rVersion` ≠ wersja R aplikacji | porównaj `rVersion` z `.jmo` z `JAMOVI_R_VERSION` w `env.conf` |
| analiza działa w Dockerze, nie w paczce | brak pakietu R w user-lib | Docker bierze zależności z `jamovi-deps`; build natywny ma własną listę (`build.ps1` krok 4b) |

---

## Definicja ukończenia

- [ ] `.jmo` obu modułów opcjonalnych mają w metadanych **wersję modułu**, nie 28.x
- [ ] oba sideloadują się i pokazują poprawną wersję w *Moduły*
- [ ] `process.versions.electron` zgodne z `build.ps1`
- [ ] testy krytyczne przechodzą
- [ ] macierz w `MODULES.md` ma wiersz bieżącego wydania z datą
- [ ] `release-check.sh` bez braków dla win64
- [ ] commit bez artefaktów builda

**Poza zakresem:** `.jmo` dla macOS (osobna maszyna: `70-jmo-jrisk.sh`, `71-jmo-jspace.sh`),
podpis i notaryzacja instalatorów.
