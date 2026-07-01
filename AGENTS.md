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

### Workflow modyfikacji
1. Przeczytaj odpowiedni plik .a.yaml — zrozum istniejące opcje
2. Przeczytaj odpowiedni plik .u.yaml — zrozum układ UI
3. Przeczytaj odpowiedni plik .r.yaml — zrozum strukturę tabel
4. Przeczytaj odpowiedni plik .b.R — znajdź sekcję wykresu/logiki
5. Wprowadź zmiany w kolejności: .a.yaml → .u.yaml → .r.yaml → .b.R
6. Przebuduj najpierw Dockerem: `docker compose --profile main build && docker compose --profile main up`
7. Jeśli celem jest instalator/paczka systemowa, po poprawnym Dockerze uruchom build
   właściwy dla aktualnej maszyny: Windows albo macOS.

## Kolejka zadań

### Zadanie 1: Boxplot — średnia jako kwadrat
Plik: descriptives. Nowa opcja: boxMean (Bool, default: false).
stat_summary(fun=mean, geom="point", shape=15, size=3)

### Zadanie 2: Histogram — facetowanie grup
Plik: descriptives. Nowa opcja: histFacet (Bool, default: false).
facet_wrap(~ grupująca, ncol=1, scales="free_y")

### Zadanie 3: Współczynnik zmienności (V)
Plik: descriptives (.a.yaml, .u.yaml, .r.yaml, .b.R). Nowa opcja: v (Bool, default: false).
Etykieta: "V (%)" (nie "CV" — unikamy kolizji z cross-validation).
sd / mean * 100. Obsłużyć mean=0.

### Zadanie 4: Preinstalowane moduły (distrACTION, jboot, jperm, jCI)
### Zadanie 5: Nowy moduł jperm (testy permutacyjne)
### Zadanie 6: Nowy moduł jCI (przedziały ufności)
Dedykowany moduł z pełną obsługą przedziałów ufności (CI) — dydaktycznie
spójne miejsce zamiast rozproszenia po eksploracji i testach.
Przy okazji: usunąć CI dla średniej z descriptives (eksploracja).
Szczegóły do ustalenia.
### Zadanie 7: Branding
