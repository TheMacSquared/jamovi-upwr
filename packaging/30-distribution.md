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
3. Przejdź przez instalator. Na zwykłym koncie instaluje się „tylko dla mnie" do
   `%LocalAppData%\Programs\jUPWR` (bez uprawnień administratora); na koncie z prawami
   administratora pojawia się wybór „dla wszystkich użytkowników" (`C:\Program Files\jUPWR`)
   albo „tylko dla mnie".
4. Wariant portable: rozpakuj `.zip` i uruchom `bin\jUPWR.exe` (uwaga: rozpakuj do stałej lokalizacji,
   nie z wnętrza archiwum).

> Po zakupie certyfikatu code-signing (OV/EV) ostrzeżenie zniknie (EV od razu, OV po zbudowaniu reputacji).

### Pracownia komputerowa (instrukcja dla administratora)

Paczka jest samowystarczalna: własny R, Python, Electron, moduły i czcionki (rejestrowane
w locie). Nie wymaga R, jamovi, VC++ Redistributable ani .NET. Instalator tylko kopiuje pliki,
tworzy skróty i dopisuje wpis w „Odinstaluj"; nie rejestruje rozszerzenia `.omv`. Wszystkie
ścieżki w `bin\env.conf` są względne, więc katalog można położyć gdziekolwiek.

Poza katalogiem programu jUPWR pisze tylko do `%AppData%\jamovi` (ustawienia, moduły
doinstalowane przez użytkownika) i `%Temp%` — zwykłe uprawnienia każdego konta wystarczą.
Katalog `%AppData%\jamovi` jest wspólny ze standardowym jamovi; to nieszkodliwe (moduły
zbudowane pod inną wersję R drugi program oznacza jako niekompatybilne i nie ładuje).
Oba programy mogą być zainstalowane i uruchomione równolegle: osobne katalogi, skróty,
wpisy deinstalacji, port serwera wybierany automatycznie.

**Instalacja dla wszystkich kont (zalecana w pracowni):** uruchomić instalator jako
administrator i wybrać „Dla wszystkich użytkowników tego komputera" — program trafi do
`C:\Program Files\jUPWR`, skróty do wspólnego menu Start i pulpitu publicznego, wpis
deinstalacji do HKLM. Działa niezależnie od tego, czy studenci logują się na wspólne
konto lokalne, konta domenowe czy profil czyszczony po wylogowaniu (Deep Freeze i podobne
trzeba odmrozić na czas instalacji).

**Wdrożenie skryptem (GPO / Intune / SCCM / skrypt logowania):**

```bat
jUPWR-<wersja>-x64-setup.exe /S /AllUsers
jUPWR-<wersja>-x64-setup.exe /S /AllUsers /D=C:\jUPWR
```

| Przełącznik | Znaczenie |
|---|---|
| `/S` | instalacja cicha; wykryta poprzednia wersja jest odinstalowywana bez pytania |
| `/AllUsers` | tryb dla wszystkich (wymaga uprawnień administratora; bez nich instalator kończy się komunikatem) |
| `/CurrentUser` | tryb „tylko dla mnie" (także na koncie administratora) |
| `/D=C:\katalog` | katalog docelowy; musi być **ostatnim** argumentem i **bez cudzysłowów**, nawet gdy zawiera spacje |

Deinstalacja cicha: `"C:\Program Files\jUPWR\uninstall.exe" /S` (klucz `QuietUninstallString`
w rejestrze). Deinstalator sam rozpoznaje tryb, w którym program zainstalowano (wartość
`InstallMode` we wpisie deinstalacji). Instalator uruchomiony ponownie na tej samej maszynie
domyślnie proponuje ten sam tryb co poprzednio; instalując „dla wszystkich" usuwa też
własną instalację „tylko dla mnie", żeby nie zostały dwie kopie.

Instalator nie jest podpisany — jeśli polityka uczelni blokuje niepodpisane `.exe`
(SmartScreen/AppLocker), wariant portable rozpakowany do `C:\Program Files\jUPWR`
ze skrótem w `C:\Users\Public\Desktop` daje ten sam efekt bez instalatora.

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
- [ ] Sprawdź: uruchomienie, otwarcie danych, Descriptives (boxMean/histFacet/V), jeden moduł (jCI), sideload jRISK z .jmo i menu Ryzyko (modele czasu życia, niezawodność systemów) i nowe rozkłady (gamma, Weibulla, ujemny dwumianowy).
- [ ] Wgraj do GitHub Releases z notką o obejściu Gatekeeper/SmartScreen.
- [ ] Zlinkuj instrukcję instalacji na stronie kursu.
