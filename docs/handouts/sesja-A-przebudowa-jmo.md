# Sesja A — przebudowa `.jmo` (jRISK, jSpace) na Windows

**Maszyna:** Windows (checkout `D:\praca\jamovi-upwr`) — `.jmo` zawiera pakiet R
skompilowany pod platformę hosta, więc tego nie da się zrobić na WSL/Linuksie.
**Czas:** ~30–60 min (jSpace dłużej: bez `--skip-deps`, dociąga sf/terra/asteRisk).
**Punkt wyjścia:** `origin/main` = `d9882cd6` lub nowszy.

---

## Po co to

Audyt z 30.08.2026 wykrył, że `jmc` czyta wersję modułu **wyłącznie** z
`jamovi/0000.yaml` ([`jamovi-compiler/index.js:285-290`](../../jamovi-compiler/index.js#L285-L290))
i do `DESCRIPTION` w ogóle nie zagląda. Skrypty pakujące czytały ją tymczasem
z `DESCRIPTION`, więc moduły opcjonalne dostawały:

| | nazwa pliku | wersja w metadanych |
|---|---|---|
| jRISK | `jRISK_0.3.2-win64.jmo` | **28.1.0.0** ❌ |
| jSpace | `jSpace_0.2.0-win64.jmo` | **28.1.0.0** ❌ |

Skutek dla studenta: w *Moduły* widać „28.1.0.0" zamiast wersji modułu, a sideload
nowszej wersji nie jest rozpoznawany jako aktualizacja.

Źródła są już naprawione (commit `21000ae0`), ale **artefakty `.jmo` nadal mają starą
wersję w środku**. Ta sesja to domyka. Nazwy plików się nie zmieniają (`0.3.2` / `0.2.0`),
więc macierz w `MODULES.md` pozostaje aktualna — zmienia się tylko zawartość.

---

## Krok po kroku

### 1. Synchronizacja

```powershell
cd D:\praca\jamovi-upwr
git fetch origin
git checkout main
git pull
git submodule update --init --recursive
```

> Klon Windows jest tylko do fetch/checkout/build — nie commituj tu zmian.

Sprawdź, że masz poprawki wersji:

```powershell
Select-String -Path "jRISK\jamovi\0000.yaml"  -Pattern "^version:"   # ma być 0.3.2
Select-String -Path "jSpace\jamovi\0000.yaml" -Pattern "^version:"   # ma być 0.2.0
```

Jeśli któryś pokazuje `28.1.0.0` — pull się nie zaciągnął, **nie buduj dalej**.

### 2. Build

```powershell
powershell -ExecutionPolicy Bypass -File packaging\scripts\windows\build.ps1
```

Skrypt robi całość; `.jmo` powstają w krokach **4e** (jRISK) i **4f** (jSpace).
Prereqs (sprawdzane na starcie): VS2022 Build Tools, RTools45, Boost 1.84, NSIS, protoc.

Jeśli chcesz same `.jmo` bez pełnego instalatora — przerwij po kroku 4f;
kroki 5+ (i18n, engine, NSIS) nie są potrzebne do sideloadu.

### 3. Weryfikacja — **to jest sedno tej sesji**

Sam fakt powstania pliku nic nie dowodzi. Sprawdź wersję **w środku**:

```powershell
cd packaging\build\dist
Expand-Archive -Path jRISK_0.3.2-win64.jmo -DestinationPath tmp-jrisk -Force
Select-String -Path "tmp-jrisk\jRISK\jamovi.yaml" -Pattern "^version:|^rVersion:"
```

Oczekiwane:

```
version: 0.3.2          ← NIE 28.1.0.0
rVersion: 4.6.0-x64
```

To samo dla jSpace (`version: 0.2.0`). Po sprawdzeniu skasuj katalogi `tmp-*`.

Jeśli nadal widzisz `28.1.0.0` — build poszedł ze starego checkoutu, wróć do kroku 1.

### 4. Test w aplikacji

Zainstaluj `.jmo` przez **Moduły → Sideload** i sprawdź, że:
- na liście modułów widnieje **0.3.2** / **0.2.0**, nie 28.1.0.0,
- analizy modułu pojawiają się na wstążce (jRISK: 5, jSpace: 5),
- jSpace: otwórz analizę używającą `sf`/`terra` — moduł ma być samowystarczalny.

### 5. Aktualizacja macierzy i commit

W [`packaging/MODULES.md`](../../packaging/MODULES.md), w sekcjach *jRISK* i *jSpace*,
wiersz `0.8.6` → kolumna `.jmo Windows` = `✅ <data>`.

```powershell
bash packaging\scripts\release-check.sh   # ma nie zgłaszać braków dla win64
git add packaging\MODULES.md
git commit -m "MODULES.md: .jmo win64 jRISK/jSpace przebudowane (wersje w metadanych)"
git push
```

`git status` ma pokazywać **tylko** `MODULES.md` — żadnych `*.h.R`, `jamovi.yaml`,
`*.src.js` ani `.jmo` (te są w `.gitignore`).

---

## Przy okazji: drobna rozbieżność do poprawienia

`MODULES.md:17` podaje **jboot 0.2.2**, a po audycie faktyczna wersja to **0.3.0**.
`release-check.sh` tego nie wyłapuje, bo porównuje z MODULES.md tylko moduły
*opcjonalne*. Jednolinijkowa poprawka — warto zrobić w tym samym commicie.

---

## Gdy coś pójdzie nie tak

| Objaw | Przyczyna | Co zrobić |
|---|---|---|
| `.jmo nie powstal` | brak prereq | log wskaże który; sprawdź listę na starcie skryptu |
| jSpace: błąd przy sf/terra | pierwszy build ciągnie zależności z CRAN | potrzebny internet; `jSpace\build\R*-win64` jest cache'owany — drugi raz pójdzie szybciej |
| wersja w `.jmo` dalej 28.1.0.0 | build ze starego checkoutu | krok 1 od nowa |
| moduł niezgodny po sideloadzie | `rVersion` ≠ wersja R aplikacji | porównaj `rVersion` z `.jmo` z `JAMOVI_R_VERSION` w `env.conf` aplikacji |

---

## Definicja ukończenia

- [ ] `jRISK_0.3.2-win64.jmo` — w metadanych `version: 0.3.2`
- [ ] `jSpace_0.2.0-win64.jmo` — w metadanych `version: 0.2.0`
- [ ] oba sideloadują się i pokazują poprawną wersję w *Moduły*
- [ ] macierz w `MODULES.md` zaktualizowana, `release-check.sh` bez braków dla win64
- [ ] commit zawiera wyłącznie `MODULES.md`

**Poza zakresem:** `.jmo` dla macOS (osobna maszyna, skrypty `70-jmo-jrisk.sh` /
`71-jmo-jspace.sh`) — macierz nadal pokaże dla nich ⬜.
