# Sesja C — weryfikacja Electron 43.4.1 (build natywny)

**Maszyna:** Windows (`D:\praca\jamovi-upwr`) i/lub macOS — po jednej sesji na platformę.
**Czas:** ~1–2 h (build + testy regresji).
**Punkt wyjścia:** `origin/main` z commitem podbicia Electrona.

> Zmiana w kodzie jest **już zrobiona i wypchnięta**: wersja podbita z 32.3.3 na 43.4.1
> w obu skryptach. Ta sesja to **wyłącznie build i testy regresji** — Electron uruchamia
> się dopiero w zbudowanej paczce, więc na WSL nie dało się tego zweryfikować.

---

## Dlaczego 43, a nie najnowszy

Electron 32.3.3 wyszedł ze wsparcia w marcu 2025 — Chromium bez łatek bezpieczeństwa
przez ponad rok. Wspierane linie to dziś **42–46** (41 wypadł 25.08.2026).

Wybrano **43**, bo to najstarsza linia z aktualnym wsparciem (EOL styczeń 2027),
która **nie wymaga zmian w kodzie**. Granica biegnie przy 44:

| wersja | clipboard | werdykt |
|---|---|---|
| ≤ 43 | API synchroniczne, `readHTML()` istnieje | ✅ nasz kod działa bez zmian |
| ≥ 44 | W3C: metody zwracają Promise, `readHTML`/`writeHTML` **usunięte** | ❌ wymaga przepisania |

Nasz [`electron/app/main.js:519-523`](../../electron/app/main.js#L519-L523) używa clipboardu
synchronicznie w 4 miejscach. Na v44+ kopiowanie i wklejanie **przestałoby działać** —
a to realna funkcja dydaktyczna (przenoszenie tabel i wykresów do Worda).
Przepisanie clipboardu pod v44+ to osobne, świadome zadanie.

## Co dokładnie zmieniono

| plik | zmiana |
|---|---|
| [`50-assemble-app.sh:10`](../../packaging/scripts/macos/50-assemble-app.sh#L10) | `ELECTRON_VERSION` → `43.4.1` |
| [`50-assemble-app.sh:16`](../../packaging/scripts/macos/50-assemble-app.sh#L16) | katalog cache zawiera wersję |
| [`build.ps1:51`](../../packaging/scripts/windows/build.ps1#L51) | `$ElectronVer` → `43.4.1` |
| [`build.ps1:309`](../../packaging/scripts/windows/build.ps1#L309) | nazwa ZIP-a zawiera wersję |
| [`main.js:644`](../../electron/app/main.js#L644) | usunięty `enableRemoteModule` (martwy od Electrona 14) |

Poprawka cache jest istotna: oba skrypty sprawdzały tylko, **czy** plik istnieje — nie jaką
ma wersję. Bez niej build po bumpie rozpakowałby zcache'owanego Electrona 32 i „przeszedłby"
bez żadnego efektu.

---

## Krok po kroku

### 1. Synchronizacja i build

**Windows:**
```powershell
cd D:\praca\jamovi-upwr
git fetch origin; git checkout main; git pull
git submodule update --init --recursive
powershell -ExecutionPolicy Bypass -File packaging\scripts\windows\build.ps1
```

**macOS:**
```bash
cd ~/praca/jamovi-upwr && git pull && git submodule update --init --recursive
cd packaging/scripts/macos
for s in 00-prereqs 10-client 20-modules 25-i18n 30-server 40-engine 50-assemble-app 55-relocate 60-package-dmg; do
  bash $s.sh || break
done
```

### 2. Potwierdź, że to faktycznie 43

Zanim zaczniesz testy — sprawdź, że build nie użył starego cache:

**Windows:** kliknij prawym na `jUPWR.exe` → Właściwości → Szczegóły (wersja pliku ~43.4.1)
**macOS:** `defaults read "$PWD/dist/jUPWR.app/Contents/Info.plist" DTSDKBuild 2>/dev/null; grep -A1 ElectronVersion dist/jUPWR.app/Contents/Info.plist`

Albo w działającej aplikacji: **Ctrl/Cmd+Shift+I** → konsola → `process.versions.electron`
(powinno dać `43.4.1`, wcześniej `32.3.3`).

Jeśli widzisz 32.3.3 — build wziął stary ZIP; skasuj `packaging/build/scratch/electron*.zip`
(Windows) lub `packaging/build/electron-*` (macOS) i powtórz.

### 3. Testy regresji — **sedno tej sesji**

Skok o 11 wersji głównych to ~9 wydań Chromium. API, których używamy, nie zmieniło się,
ale zachowanie powłoki mogło. Przejdź listę:

#### Krytyczne (blokują wydanie)

- [ ] **Aplikacja startuje** i pokazuje okno (nie biały ekran)
- [ ] **Otwieranie pliku** `.omv` — przez menu i przez dwuklik w systemie
- [ ] **Zapis i „Zapisz jako"** — dialog, poprawne rozszerzenie, plik da się otworzyć ponownie
- [ ] **Analizy liczą się i renderują** — descriptives + jakiś wykres
- [ ] **Kopiowanie do schowka** — tabela wyników → wklej do Worda/TextEdit *(główne ryzyko: clipboard)*
- [ ] **Kopiowanie wykresu** — obraz trafia do schowka jako grafika
- [ ] **Wklejanie do arkusza** — skopiuj z Excela → wklej do jamovi *(używa `readHTML`)*

#### Ważne

- [ ] **Eksport do PDF** — `printToPDF` (wywoływany z pustymi opcjami, zmiana z v21 nas nie dotyczy)
- [ ] **Eksport** CSV / LaTeX / obrazu
- [ ] **Sideload modułu** `.jmo` (Moduły → Sideload)
- [ ] **Menu aplikacji** — pełne i klikalne
- [ ] **Zoom** Ctrl/Cmd +/− oraz skróty `Ctrl+Shift+±`
- [ ] **Biblioteka** — zbiory danych jDane otwierają się
- [ ] **Zmiana języka** na polski i z powrotem

#### Specyficzne dla platformy

- [ ] **Windows: brak monitu o lokalizację** przy starcie — [`main.js:218-248`](../../electron/app/main.js#L218-L248)
      włącza `NetworkServiceSandbox`, żeby go uniknąć. To kod upstreamu związany z zachowaniem
      Chromium; przy 9 wersjach różnicy **to najbardziej prawdopodobne miejsce regresji**.
      Objaw awarii: białe okno przy starcie albo pytanie o dostęp do lokalizacji.
- [ ] **Windows: build NSIS** instaluje się do `%ProgramFiles%` i uruchamia
- [ ] **macOS: aplikacja startuje** mimo braku podpisu (Gatekeeper — prawym → Otwórz)
- [ ] **macOS: `defaultPath` w dialogach** — v43 zmienił domyślny katalog na Pobrane, gdy
      `defaultPath` nie jest podany. Klient go podaje ([`backstage.ts:113`](../../client/main/backstage.ts#L113)),
      ale sprawdź, czy „Otwórz" startuje w Dokumentach, a nie w Pobranych.

### 4. Zamknięcie

Jeśli wszystko przechodzi:

```bash
git commit -m "Electron 43.4.1: zweryfikowane na <Windows|macOS>"   # jeśli coś poprawiałeś
```

W [`CHANGELOG.md`](../../CHANGELOG.md) dopisz do bieżącego wydania notkę o podbiciu Electrona
z informacją, na której platformie zweryfikowano.

---

## Gdy coś pójdzie nie tak

| Objaw | Prawdopodobna przyczyna | Co zrobić |
|---|---|---|
| białe okno przy starcie (Windows) | sandbox usługi sieciowej / monit o lokalizację | tymczasowo zakomentuj `appendSwitch('enable-features', 'NetworkServiceSandbox')` i sprawdź, czy pomaga |
| „Sandbox cannot access executable", pętla crashów | build portable rozpakowany poza `%ProgramFiles%` | to znany warunek z komentarza w `main.js` — testuj build NSIS, nie portable |
| kopiowanie/wklejanie nie działa | zmiana clipboardu | **nie powinno na 43** — jeśli jednak, sprawdź `process.versions.electron`, czy to na pewno nie 44+ |
| dialogi otwierają Pobrane | zmiana `defaultPath` w v43 | sprawdź, czy klient przekazuje `defaultPath` |
| wersja nadal 32.3.3 | stary cache ZIP-a | patrz krok 2 |

**Wycofanie:** przywróć `32.3.3` w dwóch miejscach z sekcji „Co dokładnie zmieniono"
i przebuduj. Zmiana jest w całości odwracalna — nie dotknęliśmy logiki aplikacji.

---

## Definicja ukończenia

- [ ] `process.versions.electron` = `43.4.1` w działającej aplikacji
- [ ] wszystkie testy krytyczne przechodzą
- [ ] testy ważne przechodzą albo mają zapisane odstępstwo
- [ ] Windows: brak monitu o lokalizację, brak białego okna
- [ ] wpis w `CHANGELOG.md` z nazwą zweryfikowanej platformy

**Poza zakresem:** przejście na Electron 44+ (wymaga przepisania clipboardu na W3C —
Promise i `ClipboardItem`), podpisywanie i notaryzacja instalatorów.
