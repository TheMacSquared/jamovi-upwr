# CLAUDE.md — jamovi fork (jUPWR)

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
6. Przebuduj: `docker compose --profile main build && docker compose --profile main up`

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
