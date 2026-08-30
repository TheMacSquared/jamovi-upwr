# Sesja Windows — przebudowa `.jmo` + weryfikacja Electron 43

**Maszyna:** Windows x64. **Czas:** ~2–3 h (build + testy).
**Zadania A i C z audytu 30.08.2026 w jednym przebiegu** — `build.ps1` robi obie rzeczy
za jednym razem, więc nie ma sensu dzielić tego na dwie sesje.

---

## Co i po co

### A. Przebudowa `.jmo` (jRISK, jSpace)

`jmc` czyta wersję modułu **wyłącznie** z `jamovi/0000.yaml`
([`jamovi-compiler/index.js:299-306`](../../jamovi-compiler/index.js#L299-L306)) i do
`DESCRIPTION` nie zagląda. Skrypty pakujące czytały ją tymczasem z `DESCRIPTION`, więc
moduły opcjonalne dostawały poprawną nazwę pliku i **złą wersję w środku**:

| | nazwa pliku | wersja w metadanych |
|---|---|---|
| jRISK | `jRISK_0.3.2-win64.jmo` | **28.1.0.0** ❌ |
| jSpace | `jSpace_0.2.0-win64.jmo` | **28.1.0.0** ❌ |

Dla studenta: w *Moduły* widać „28.1.0.0" zamiast wersji modułu, a sideload nowszej wersji
nie jest rozpoznawany jako aktualizacja. Źródła są naprawione (commit `21000ae0`) — brakuje
przebudowania artefaktów.

> Dodatkowy powód, żeby przebudować **teraz**: po merge'u upstreamu aplikacja to już
> **jamovi 28.2.0.0**, a obecne `.jmo` zbudowano pod 28.1. `.jmo` jest kompilowany tym samym
> `jmc`/`jmvcore` co aplikacja, więc przy każdym wydaniu i tak wymaga przebudowy.

### C. Weryfikacja Electron 43.4.1

Wersja podbita z `32.3.3` (poza wsparciem od marca 2025) na **43.4.1** — commit `f604575c`,
**zmiana już wypchnięta, tu tylko build i testy**. Electron uruchamia się dopiero
w paczce natywnej, więc na WSL nie dało się tego zweryfikować.

Dlaczego akurat 43: wspierane linie to 42–46, ale w **44** moduł clipboard przeszedł na W3C
(metody zwracają Promise, `readHTML`/`writeHTML` usunięte), a
[`main.js:511-523`](../../electron/app/main.js#L511-L523) używa go synchronicznie — na 44+
kopiowanie tabel i wklejanie z Excela przestałoby działać. 43 to najstarsza wspierana linia
(EOL 2027-01) niewymagająca zmian w kodzie.

---

## 1. Świeży klon — **nie używaj istniejącego katalogu**

Podejrzenie uszkodzenia lokalnego checkoutu. Klonujemy od zera, obok starego:

```powershell
cd D:\praca
Rename-Item jamovi-upwr jamovi-upwr-OLD -EA SilentlyContinue

git clone --recurse-submodules https://github.com/TheMacSquared/jamovi-upwr.git
cd jamovi-upwr
```

Jeśli `--recurse-submodules` się nie dociągnie w całości:

```powershell
git submodule update --init --recursive --force
```

### Weryfikacja klonu — zanim cokolwiek zbudujesz

```powershell
git log --oneline -1
git status --short          # ma być PUSTO
git submodule status        # cztery wpisy, zaden bez '-' ani '+' na poczatku
```

`-` na początku = submoduł niezainicjalizowany, `+` = wskazuje na inny commit niż superprojekt.
Oba oznaczają, że klon jest niekompletny — **nie buduj**, powtórz `submodule update`.

Sprawdź, że masz poprawki z audytu:

```powershell
Select-String -Path "jRISK\jamovi\0000.yaml"  -Pattern "^version:"    # 0.3.2
Select-String -Path "jSpace\jamovi\0000.yaml" -Pattern "^version:"    # 0.2.0
Select-String -Path "plots\jamovi\0000.yaml"  -Pattern "^version:"    # 2.9.0
Select-String -Path "packaging\scripts\windows\build.ps1" -Pattern "ElectronVer"   # 43.4.1
Get-Content version                                                    # 28.2.0.0
```

**Każda z tych wartości musi się zgadzać.** Jeśli którakolwiek nie — klon jest nieaktualny
albo niepełny; `git pull` i powtórz.

### Stary katalog

Zostaw `jamovi-upwr-OLD` do czasu, aż build i testy przejdą — dopiero potem skasuj.
Jeśli miałeś tam niezacommitowane zmiany, teraz jest moment, żeby je wyłuskać:

```powershell
cd D:\praca\jamovi-upwr-OLD; git status --short; git stash list
```

---

## 2. Build

```powershell
cd D:\praca\jamovi-upwr
powershell -ExecutionPolicy Bypass -File packaging\scripts\windows\build.ps1
```

Prereqs sprawdzane na starcie: VS2022 Build Tools (VCTools), RTools45, R 4.6.0,
Boost 1.84, Node 20+, cmake, protoc. `.jmo` powstają w krokach **4e** (jRISK) i **4f** (jSpace).

Build jest idempotentny tam, gdzie to tanie — ale w świeżym klonie wszystko leci od zera,
łącznie z zależnościami R i Electronem (~143 MB).

---

## 3. Weryfikacja `.jmo` — **sedno zadania A**

Sam fakt powstania pliku niczego nie dowodzi. Sprawdź, co jest **w środku**:

```powershell
cd packaging\build\dist
Expand-Archive -Path jRISK_0.3.2-win64.jmo -DestinationPath tmp-jrisk -Force
Select-String -Path "tmp-jrisk\jRISK\jamovi.yaml" -Pattern "^version:|^rVersion:"
```

Oczekiwane:

```
version: 0.3.2          ← NIE 28.1.0.0 ani 28.2.0.0
rVersion: 4.6.0-x64
```

To samo dla `jSpace_0.2.0-win64.jmo` (`version: 0.2.0`). Po sprawdzeniu skasuj `tmp-*`.

Jeśli nadal widzisz `28.1.0.0` → build poszedł ze starego checkoutu, wróć do kroku 1.

---

## 4. Weryfikacja Electrona — **sedno zadania C**

Najpierw potwierdź, że build wziął nową wersję (skrypt cache'uje ZIP-a):

Uruchom aplikację → **Ctrl+Shift+I** → konsola:

```js
process.versions.electron    // ma dać "43.4.1"
```

Jeśli widzisz `32.3.3` → skasuj `packaging\build\scratch\electron*.zip` i przebuduj.
(Nazwa ZIP-a zawiera teraz wersję, więc nie powinno się zdarzyć — ale sprawdź.)

### Testy regresji

Skok o 11 wersji głównych to ~9 wydań Chromium. Używane API się nie zmieniło,
ale zachowanie powłoki mogło.

**Krytyczne — blokują wydanie:**

- [ ] aplikacja startuje, okno się renderuje (nie biały ekran)
- [ ] otwieranie `.omv` — przez menu i przez dwuklik w Eksploratorze
- [ ] zapis i „Zapisz jako" — dialog, poprawne rozszerzenie, plik otwiera się ponownie
- [ ] analizy liczą się i renderują (descriptives + dowolny wykres)
- [ ] **kopiowanie tabeli wyników → wklej do Worda** *(główne ryzyko: clipboard)*
- [ ] **kopiowanie wykresu** — trafia do schowka jako grafika
- [ ] **wklejanie z Excela do arkusza** *(używa `readHTML`)*
- [ ] **brak monitu o dostęp do lokalizacji przy starcie** — patrz niżej

**Ważne:**

- [ ] eksport do PDF (`printToPDF`), CSV, LaTeX, obrazu
- [ ] **sideload `.jmo`** (Moduły → Sideload) — łączy zadania A i C:
      wersja w *Moduły* ma być **0.3.2** / **0.2.0**, nie 28.x
- [ ] analizy sideloadowanych modułów działają (jRISK: 5, jSpace: 5)
- [ ] jSpace: analiza używająca `sf`/`terra` — moduł ma być samowystarczalny
- [ ] menu aplikacji pełne i klikalne
- [ ] zoom Ctrl +/− oraz `Ctrl+Shift+±`
- [ ] Biblioteka — zbiory jDane otwierają się
- [ ] zmiana języka na polski i z powrotem
- [ ] zakładka **Wykresy** — 7 kategorii, 26 wykresów (po scatr 2.9.0)
- [ ] instalator NSIS instaluje się do `%ProgramFiles%` i uruchamia

### Najbardziej prawdopodobne miejsce regresji

[`main.js:218-248`](../../electron/app/main.js#L218-L248) włącza `NetworkServiceSandbox`,
żeby uniknąć systemowego monitu o lokalizację (Windows gatuje odczyt SSID za tym
uprawnieniem). To kod upstreamu ściśle związany z zachowaniem Chromium — przy 9 wydaniach
różnicy to najbardziej realne ryzyko.

**Objawy awarii:** białe okno przy starcie albo pytanie o dostęp do lokalizacji.
**Obejście diagnostyczne:** zakomentuj `appendSwitch('enable-features', 'NetworkServiceSandbox')`
i sprawdź, czy pomaga. Jeśli tak — zgłoś, przemyślimy rozwiązanie (to kod upstreamu).

Uwaga: sandbox wymaga, żeby aplikacja leżała tam, gdzie token AppContainer ma prawo odczytu.
Build **NSIS** (`%ProgramFiles%`) i MSIX spełniają ten warunek, **portable .zip rozpakowany
do Pobranych/Pulpitu — nie** (objaw: „Sandbox cannot access executable", pętla crashów).
Testuj build NSIS.

---

## 5. Zamknięcie

W [`packaging/MODULES.md`](../../packaging/MODULES.md), w sekcjach *jRISK* i *jSpace*,
wiersz `0.8.6`, kolumna `.jmo Windows` → `✅ <data>`. Zwróć uwagę na kolumnę **jamovi**:
wiersze mówią `28.1`, a repo jest już na **28.2.0.0** — popraw przy okazji.

```powershell
bash packaging\scripts\release-check.sh    # bez braków dla win64
git add packaging\MODULES.md
git commit -m "MODULES.md: .jmo win64 jRISK/jSpace przebudowane pod jamovi 28.2; Electron 43 zweryfikowany"
git push
```

W [`CHANGELOG.md`](../../CHANGELOG.md) dopisz notkę o Electronie 43.4.1 z informacją,
że zweryfikowano go na Windows.

`git status` ma pokazywać **tylko** te pliki — żadnych `*.h.R`, `jamovi.yaml`, `*.src.js`
ani `.jmo` (te są w `.gitignore`).

### Przy okazji: drobiazg do poprawienia

`MODULES.md:17` podaje **jboot 0.2.2**, a faktyczna wersja to **0.3.0**.
`release-check.sh` tego nie łapie, bo porównuje z MODULES.md tylko moduły *opcjonalne*.
Jednolinijkowa poprawka — warto w tym samym commicie.

---

## Gdy coś pójdzie nie tak

| Objaw | Przyczyna | Co zrobić |
|---|---|---|
| `.jmo` nie powstaje | brak prereq | log wskaże który; lista na starcie skryptu |
| jSpace: błąd przy `sf`/`terra` | pierwszy build ciągnie zależności z CRAN | potrzebny internet; `jSpace\build\R*-win64` jest cache'owany, drugi raz szybciej |
| wersja w `.jmo` to 28.x | build ze starego checkoutu | krok 1 od nowa |
| `process.versions.electron` = 32.3.3 | stary ZIP w cache | skasuj `packaging\build\scratch\electron*.zip` |
| białe okno przy starcie | sandbox usługi sieciowej | patrz sekcja o `NetworkServiceSandbox` |
| „Sandbox cannot access executable" | portable poza `%ProgramFiles%` | testuj build NSIS |
| kopiowanie/wklejanie nie działa | zmiana clipboardu | **nie powinno na 43** — sprawdź `process.versions.electron`, czy to nie 44+ |
| moduł niezgodny po sideloadzie | `rVersion` ≠ wersja R aplikacji | porównaj `rVersion` z `.jmo` z `JAMOVI_R_VERSION` w `env.conf` |

**Wycofanie Electrona:** przywróć `32.3.3` w
[`build.ps1:51`](../../packaging/scripts/windows/build.ps1#L51) i
[`50-assemble-app.sh:10`](../../packaging/scripts/macos/50-assemble-app.sh#L10), przebuduj.
Zmiana jest w pełni odwracalna — logika aplikacji nietknięta.

---

## Definicja ukończenia

**Zadanie A:**
- [ ] `jRISK_0.3.2-win64.jmo` — w metadanych `version: 0.3.2`
- [ ] `jSpace_0.2.0-win64.jmo` — w metadanych `version: 0.2.0`
- [ ] oba sideloadują się i pokazują poprawną wersję w *Moduły*
- [ ] macierz w `MODULES.md` zaktualizowana (data + kolumna jamovi 28.2)

**Zadanie C:**
- [ ] `process.versions.electron` = `43.4.1`
- [ ] wszystkie testy krytyczne przechodzą
- [ ] brak monitu o lokalizację, brak białego okna
- [ ] wpis w `CHANGELOG.md`

**Wspólne:**
- [ ] `release-check.sh` bez braków dla win64
- [ ] commit zawiera wyłącznie `MODULES.md` i `CHANGELOG.md`

**Poza zakresem:** `.jmo` dla macOS (osobna maszyna: `70-jmo-jrisk.sh`, `71-jmo-jspace.sh`),
przejście na Electron 44+ (wymaga przepisania clipboardu na W3C), podpis i notaryzacja.
