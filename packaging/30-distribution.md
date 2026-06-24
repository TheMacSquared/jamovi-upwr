# Dystrybucja jUPWR studentom

Jak wydać gotowe installki i jak studenci mają je zainstalować — z uwzględnieniem, że
**aplikacja nie jest podpisana** (świadoma decyzja na ten etap).

## Kanał dystrybucji

Rekomendacja: **GitHub Releases** w repo forka (tag = wersja, np. `v2.7.35.0`), pliki:
- `jUPWR-<wersja>-arm64.dmg` (macOS Apple Silicon)
- `jUPWR-<wersja>-x64-setup.exe` (Windows installer)
- `jUPWR-<wersja>-x64-portable.zip` (Windows bez instalacji)

Alternatywa dla uczelni: dysk sieciowy / Teams / strona kursu. Pliki są duże
(macOS ~0.5–1 GB po wbudowaniu R, Windows podobnie) — unikać załączników mailowych.

---

## macOS — obejście Gatekeeper (brak podpisu/notaryzacji)

Niepodpisana i nienotaryzowana aplikacja zostanie zablokowana komunikatem typu
*„nie można otworzyć, bo pochodzi od niezidentyfikowanego dewelopera"* lub
*„aplikacja jest uszkodzona"* (efekt atrybutu kwarantanny po pobraniu z internetu).

### Instrukcja dla studenta (macOS)
1. Otwórz pobrany `jUPWR-...arm64.dmg`, przeciągnij **jUPWR** do folderu **Programy** (Applications).
2. **Pierwsze uruchomienie:** kliknij ikonę jUPWR **prawym przyciskiem** (lub Ctrl+klik) → **Otwórz**
   → w oknie ostrzeżenia ponownie **Otwórz**. (Zwykłe dwukliknięcie za pierwszym razem nie pozwoli.)
3. Jeśli pojawia się *„jUPWR jest uszkodzony i nie można go otworzyć"* — to atrybut kwarantanny.
   Otwórz **Terminal** i wykonaj:
   ```bash
   xattr -dr com.apple.quarantine /Applications/jUPWR.app
   ```
   po czym uruchom aplikację normalnie.

> Po podpisaniu i notaryzacji (Apple Developer ID, 99 USD/rok) te kroki znikną — patrz „Plan podpisywania".

---

## Windows — obejście SmartScreen (brak podpisu)

Niepodpisany `.exe` wywoła **Microsoft Defender SmartScreen**: *„System Windows ochronił Twój
komputer"*.

### Instrukcja dla studenta (Windows)
1. Uruchom `jUPWR-...setup.exe`.
2. W oknie SmartScreen kliknij **Więcej informacji** → **Uruchom mimo to**.
3. Przejdź przez instalator (instaluje do `%LocalAppData%\Programs\jUPWR` lub `Program Files`).
4. Wariant portable: rozpakuj `.zip` i uruchom `jUPWR.exe` (uwaga: rozpakuj do stałej lokalizacji,
   nie z wnętrza archiwum).

> Po zakupie certyfikatu code-signing (OV/EV) ostrzeżenie zniknie (EV od razu, OV po zbudowaniu reputacji).

---

## Plan podpisywania (gdy zdecydujecie)

| Platforma | Co potrzebne | Efekt |
|---|---|---|
| macOS | Apple Developer ID Application cert + `codesign --options runtime` + `notarytool` (notaryzacja) + `stapler` | brak ostrzeżeń, dwuklik działa |
| Windows | Certyfikat code-signing (OV lub EV) + `signtool` na `.exe` | brak/mniej SmartScreen |

Pipeline jest przygotowany pod późniejsze dołożenie podpisu (krok po `50-assemble-app.sh`/NSIS).

---

## Aktualizacje

Brak auto-updatera (Squirrel/electron-updater nie skonfigurowany). Model: nowe wydanie = nowy plik
w Releases, studenci pobierają ręcznie. Wersja widoczna w `version` i w tytule okna. Rozważ dodanie
auto-update w przyszłości, jeśli częstotliwość wydań wzrośnie.

## Checklist wydania

- [ ] Zbuduj installki (macOS: `scripts/macos/*`, Windows: `scripts/windows/*`).
- [ ] Przetestuj na **czystej** maszynie (bez Homebrew/R/Pythona) — krytyczne dla wykrycia braków relokowalności.
- [ ] Sprawdź: uruchomienie, otwarcie danych, Descriptives (boxMean/histFacet/V), jeden moduł (jCI).
- [ ] Wgraj do GitHub Releases z notką o obejściu Gatekeeper/SmartScreen.
- [ ] Zlinkuj instrukcję instalacji na stronie kursu.
