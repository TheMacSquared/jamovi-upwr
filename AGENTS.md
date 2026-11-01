# AGENTS.md — jamovi fork (jUPWR)

## Projekt
Zmodyfikowana dystrybucja jamovi na potrzeby dydaktyki statystyki
na Uniwersytecie Przyrodniczym we Wrocławiu.

## Repozytorium
Fork https://github.com/TheMacSquared/jamovi-upwr z submodułami (jmv, plots, i18n, readstat).
Budowanie przez Docker: `docker compose --profile main build && docker compose --profile main up`
Aplikacja dostępna pod http://127.0.0.1:41337 po zbudowaniu.

## Architektura — co gdzie jest

### Moduł analityczny (jmv/)
Każda analiza to trójka/czwórka plików:
- `jmv/jamovi/<analiza>.a.yaml` — definicja opcji (parametry, checkboxy, listy)
- `jmv/jamovi/<analiza>.u.yaml` — układ panelu UI (layout checkboxów, sekcji)
- `jmv/jamovi/<analiza>.r.yaml` — definicja tabel wynikowych (kolumny, typy, formatowanie)
- `jmv/R/<analiza>.b.R` — logika R + wykresy (ggplot2)

Konwencja nazw opcji: camelCase (np. boxMean, histFacet).
Wykresy generowane przez ggplot2.

### Aplikacja (jamovi/)
- `client/` — interfejs Electron (HTML/JS/CSS), budowany przez vite
- `electron/` — punkt wejścia Electron
- `server/` — serwer Python/tornado
- `engine/` — silnik C++ (NIE MODYFIKOWAĆ)
- `platform/` — ikony, pliki OS, env.conf
- `i18n/` — tłumaczenia
- `docker/` — Dockerfile'e i skrypty budowania

### Proces budowania (Docker)
Dockerfile: `docker/jamovi-Dockerfile` — multi-stage build:
1. `node-bin` — pobiera Node.js
2. `server` — buduje serwer Python
3. `client` — buduje frontend (vite build)
4. `engine` — kompiluje silnik C++
5. `jmvcore` → `compiler` → `jmv` — buduje moduły R przez `jmc --install`
6. `i18n` — kompiluje tłumaczenia
7. `jamovi` (final) — składa wszystko w `/usr/lib/jamovi/`

Moduły R instalowane do: `/usr/lib/jamovi/modules/`
Kompilator modułów: `jamovi-compiler/` (`jmc`)

## Release/build workflow — ważne dla agentów

Cel: uniknąć rozjazdów między Dockerem, natywnym buildem Windows i buildem macOS.
Docker jest referencyjnym, powtarzalnym buildem aplikacji. Buildy natywne służą
do pakowania na konkretny system i nie powinny być źródłem zmian w kodzie.

### Źródło prawdy
- Zmiany commituj tylko w plikach źródłowych i skryptach builda:
  - moduły: `.a.yaml`, `.u.yaml`, `.r.yaml`, `.b.R`, `DESCRIPTION`
  - klient: `client/`
  - Electron/packaging: `electron/`, `packaging/`
  - Docker: `docker-compose.yaml`, `docker/`
- Nie commituj artefaktów wygenerowanych przez build lub `jmc`, chyba że użytkownik
  wyraźnie prosi o aktualizację takich plików:
  - `*.h.R`
  - wygenerowane `0000.yaml`, `jamovi.yaml`, `jamovi-full.yaml`
  - wygenerowane `*.src.js`, `*.js` w katalogach modułów, jeśli powstały tylko przez build
  - `engine/jamovi.pb.cc` i `engine/jamovi.pb.h`, chyba że świadomie aktualizujemy
    protobuf dla całego projektu
- Po każdym buildzie uruchom `git status` i usuń z planowanego commita artefakty
  wygenerowane automatycznie.

### Wersje
- Wersja dystrybucji jUPWR ma być spójna we wszystkich miejscach:
  - `client/common/jupwr.ts`
  - `docker-compose.yaml` (`image: jupwr/jupwr:<wersja>`)
  - `packaging/scripts/windows/jUPWR.nsi`
  - skrypty macOS w `packaging/scripts/macos/`, jeśli zawierają nazwę/wersję artefaktu
- Wersje modułów bumpuj tylko dla modułów, które faktycznie się zmieniły:
  - `DESCRIPTION`
  - module metadata, np. `jamovi/0000.yaml`, jeśli jest źródłowym plikiem wersji modułu
- Nie podbijaj wersji modułu tylko dlatego, że został przebudowany.

### Kolejność sprawdzania
1. Najpierw zbuduj i sprawdź Docker:
   `docker compose --profile main build`
   `docker compose --profile main up`
2. Dopiero po poprawnym Dockerze buduj paczkę natywną dla aktualnej maszyny:
   - Windows: `packaging/scripts/windows/build.ps1`
   - macOS: skrypty w `packaging/scripts/macos/`
3. Jeśli pracujesz na Windows, nie próbuj finalnie weryfikować paczki macOS.
   Sprawdź tylko skrypty i wersje, a build macOS zostaw do wykonania na macOS.
4. Jeśli pracujesz na macOS, analogicznie nie próbuj finalnie weryfikować instalatora
   Windows. Sprawdź skrypty i wersje, a build Windows wykonuj na Windows.

### Protobuf i engine
- Nie edytuj `engine/` bez wyraźnego polecenia.
- Docker powinien regenerować `engine/jamovi.pb.cc` i `engine/jamovi.pb.h` wewnątrz
  kontenera tym samym `protoc`, którego używa do kompilacji. Dzięki temu Docker nie
  zależy od wersji `protoc` użytej lokalnie na Windows/macOS.
- Jeżeli build Dockera psuje się na protobuf, najpierw sprawdź zgodność generowania
  w `docker/jamovi-Dockerfile`, a nie commituj lokalnie wygenerowanych plików `engine`.

### Checklist przed commitem/pushem
- `git status` pokazuje tylko ręczne, zamierzone zmiany.
- Przy zmianie wersji jUPWR: wpis w `CHANGELOG.md`, aktualizacja `packaging/MODULES.md`
  (rejestr modułów + macierz zgodności modułów opcjonalnych) i przebudowa `.jmo` każdego
  modułu opcjonalnego; `packaging/scripts/release-check.sh` bez ostrzeżeń.
- Docker build działa albo w finalnej odpowiedzi jasno opisano, dlaczego nie był uruchomiony.
- Wersja jUPWR jest taka sama w kliencie, Dockerze i skryptach packagingu.
- Artefakty z builda natywnego lub `jmc` nie są dodane do commita.
- Dla buildów systemowych raportuj dokładnie, na jakiej maszynie były sprawdzane:
  Docker / Windows native / macOS.

## Zasady pracy

### Bezpieczeństwo
- NIGDY nie modyfikuj `engine/` ani `server/` bez wyraźnego polecenia
- NIGDY nie usuwaj istniejących opcji — tylko dodawaj nowe
- Przed każdą modyfikacją `.b.R` przeczytaj cały plik i zrozum kontekst
- Zachowuj istniejące domyślne zachowanie (default: false dla nowych opcji)

### Styl kodu R (w plikach .b.R)
- Używaj self$options$nazwaOpcji do odczytu opcji
- Wykresy: dodawaj warstwy do istniejącego obiektu `plot` przez `+`
- Nie twórz nowych zmiennych globalnych
- Komentarze w języku angielskim (konwencja projektu)

### Styl YAML (pliki .a.yaml i .u.yaml)
- Zachowuj istniejącą indentację (2 spacje)
- Nowe opcje dodawaj na końcu odpowiedniej sekcji
- Sprawdź czy nazwa opcji nie koliduje z istniejącymi

### Dane do testów ręcznych
- Nowe dane testowe dodawaj do `packaging/tests/manual/` i dołączaj do obrazu
  Dockera, żeby użytkownik mógł je otworzyć bez kopiowania plików z hosta.
- Umieszczaj je obok istniejących testów w `/root/Documents/jupwr-test-audyt/`
  (`Otwórz → Ten komputer → Dokumenty → jupwr-test-audyt`), nie w Bibliotece
  ani w modułach dydaktycznych. Użytkownik nie chce danych testowych w wydaniach.
- Dockerfile automatycznie kopiuje ten katalog źródłowy do folderu audytu oraz
  do `/usr/lib/jamovi/test-data/` w obrazie. Nie zmieniaj list modułów wydania.
- Compose montuje Dokumenty hosta na `/root/Documents` i zasłania kopię obrazu.
  Po buildzie i uruchomieniu kopiuj nowe zestawy również do działającego kontenera,
  np. `docker compose cp packaging/tests/manual/upstream-28.17.csv jamovi:/root/Documents/jupwr-test-audyt/`.
  Zachowaj pozostałe pliki audytu i nie nadpisuj zmodyfikowanych kopii użytkownika.
- W planie testów podawaj ścieżkę w interfejsie. Po buildzie sprawdź obecność
  zestawu w folderze audytu i jego otwarcie przez działający serwer.

### Workflow modyfikacji
1. Przeczytaj odpowiedni plik .a.yaml — zrozum istniejące opcje
2. Przeczytaj odpowiedni plik .u.yaml — zrozum układ UI
3. Przeczytaj odpowiedni plik .r.yaml — zrozum strukturę tabel
4. Przeczytaj odpowiedni plik .b.R — znajdź sekcję wykresu/logiki
5. Wprowadź zmiany w kolejności: .a.yaml → .u.yaml → .r.yaml → .b.R
6. Przebuduj najpierw Dockerem: `docker compose --profile main build && docker compose --profile main up`
7. Jeśli celem jest instalator/paczka systemowa, po poprawnym Dockerze uruchom build
   właściwy dla aktualnej maszyny: Windows albo macOS.

## Stan dystrybucji (kolejka pierwotnych zadań zrealizowana)

Wszystkie zadania pierwotnej kolejki (1–7) są zrealizowane — stan na sierpień 2026:
- descriptives: boxMean, współczynnik zmienności V (%); facetowanie histogramów
  zapewnia wbudowany mechanizm „Podziel według" upstreamu (osobna opcja
  histFacet okazała się zbędna); CI dla średniej usunięte (przeniesione do jCI)
- moduły preinstalowane: jmv, plots (scatr), jperm, jCI, jboot,
  jdistrACTION 1.3.x (11 rozkładów, w tym gamma, Weibulla i ujemny dwumianowy
  z wyborem konwencji zmiennej losowej); jRISK (ryzyko i niezawodność:
  modele czasu życia z cenzorowaniem, systemy, FTA, zdarzenia/warunkowanie,
  schemat Bernoulliego + zbiory danych Bananpol w bibliotece) jest modułem
  OPCJONALNYM: nie preinstalowany, budowany jako .jmo (macOS: 70-jmo-jrisk.sh,
  Windows: build.ps1 krok 4e), instalowany przez Moduły → Sideload
- branding jUPWR: nazwa, ikony (icns/ico/iconset), wersja dystrybucji
  w client/common/jupwr.ts (patrz sekcja Wersje)

Nowe zadania dopisuj poniżej jako "### Zadanie N: ..." z plikiem docelowym
i oczekiwanym zachowaniem.

### Zadanie 8: Niezawodność obsługi analiz po merge 28.17
- Pliki: `server/jamovi/server/{clientconnection,engine,enginemanager,scheduler,pool,session,server}.py`,
  `server/jamovi/server/analyses/`, `server/jamovi/tests/test_analysis_lifecycle.py`,
  `docker-compose.yaml`, `docker/jamovi-Dockerfile`.
- Żądania WebSocket i eksporty kończą się wynikiem, błędem albo anulowaniem;
  stara analiza nie nadpisuje statusu swojego nowszego żądania.
- Start, restart i zamknięcie czekają na zadania silników; awaria i timeout
  pozwalają odbudować proces R, a zamknięcie nie pozostawia procesów i wątków.
- Testy automatyczne obejmują także prawdziwy proces C++/R w Dockerze oraz
  nieudane odtwarzanie stanu i usunięcie analizy podczas eksportu.

### Zadanie 9: Instalacja modułów przez przeciąganie
- Pliki: `client/main/store/pagesideload.ts`,
  `client/main/store/tests/pagesideload.test.ts`.
- Upuszczenie pliku w panelu instalacji modułów w Electronie uruchamia ten sam
  instalator co wybór pliku strzałką i zatrzymuje otwieranie go jako danych.
- Kilka plików jest instalowanych kolejno; błąd pozostawia panel otwarty,
  a udana instalacja zamyka go i pokazuje powiadomienie.

### Zadanie 10: Bezpieczna instalacja i aktualizacja modułów
- Pliki: `server/jamovi/server/modules/modules.py`,
  `server/jamovi/tests/test_module_installation.py`.
- Rozpakowanie i walidacja struktury archiwum, metadanych, obecności deklarowanego
  kodu R/danych i zgodności R/jamovi poprzedzają podmianę katalogu. Instalacje i odinstalowania są serializowane.
- Poprzednia wersja jest zachowywana do odświeżenia rejestru; błąd lub anulowanie
  przywraca jej pliki i metadane. Gdy system uniemożliwi przywrócenie lub usunięcie
  kopii zapasowej, kopia pozostaje w ukrytym katalogu `.install-*/previous`.

### Zadanie 11: Kolejki i kontrola obciążenia
- Pliki: `server/jamovi/server/{clientconnection,engine,session}.py`,
  `server/jamovi/server/utils/{websocketqueue,conf}.py`,
  `server/jamovi/tests/test_backpressure.py`, `packaging/BACKEND.md`.
- Jedna ograniczona kolejka wysyłania na połączenie, scalanie postępu oraz
  bezpieczne rozłączenie przeciążonego klienta. Liczba R konfigurowalna;
  anulowanie zwalnia zajęty proces i umożliwia następne obliczenia.

### Zadanie 12: Zapis i odzyskiwanie dokumentów
- Pliki: `server/jamovi/server/formatio/{__init__,omv}.py`,
  `server/jamovi/server/{instance,session}.py`, `docker-compose.yaml`,
  `server/jamovi/tests/test_document_recovery.py`, `packaging/BACKEND.md`.
- Unikalny plik tymczasowy, synchronizacja i atomowa podmiana dokumentu;
  anulowanie nie zwalnia danych, dopóki wątek zapisujący ich używa.
- Kopie zmienionych i nowych dokumentów co 120 sekund oraz przy kończeniu
  sesji, oddzielne od zwykłego zapisu, dostępne przez `{{Recovery}}`.
- Wolumen `jupwr-recovery` przechowuje kopie Dockera; nie umieszczaj w nim danych
  testowych ani nie usuwaj kopii użytkownika podczas testów. Usługa `test`
  korzysta z osobnego katalogu `/tmp/jupwr-test-recovery`.

### Zadanie 13: Potwierdzenie usunięcia wszystkich wyników
- Pliki: `client/main/resultspanel.ts`, `client/main/utils/confirmdeleteall.ts`,
  `client/main/utils/tests/confirmdeleteall.test.ts`, `i18n/src/pl.po`.
- „Wszystko → Usuń” ostrzega o nieodwracalnym usunięciu analiz, wyników,
  uwag i kolumn wynikowych; anulowanie pozostawia także bieżące zaznaczenie.
- Electron: domyślny przycisk i Escape anulują; w przeglądarce natywne
  potwierdzenie. Usuwanie pojedynczej analizy zachowuje dotychczasowe zachowanie.
