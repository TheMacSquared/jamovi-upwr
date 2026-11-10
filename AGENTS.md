# AGENTS.md — jamovi fork (jUPWR)

## Projekt
Zmodyfikowana dystrybucja jamovi na potrzeby dydaktyki statystyki
na Uniwersytecie Przyrodniczym we Wrocławiu.

## Repozytorium
Fork https://github.com/TheMacSquared/jamovi-upwr z submodułami (jmv, plots, i18n, readstat).
Budowanie przez Docker: `docker compose --profile main build && docker compose --profile main up`
Po uruchomieniu otwórz http://localhost:41337, zgodnie z ustawieniem
`JAMOVI_HOST_A` w `docker-compose.yaml`.

## Kanały wydań
- Ten katalog (`jamovi-upwr-latest`) obsługuje `main`, kanał latest.
  Bieżące wersje odczytuj z `client/common/jupwr.ts` i pliku `version`.
- Kanał stable (`release/1.0`, katalog `jamovi-upwr-stable`) przyjmuje
  wyłącznie poprawki krytyczne. Przenoś je przez `git cherry-pick -x`.
  `release-check.sh` wykrywa poprawki ze stable, których brakuje na main.
- Submoduły jmv, plots i i18n śledzą na latest gałęzie `main`; readstat
  używa `sync-28`. `legacy/win81` jest osobną, zamrożoną linią.
- Zasady awansu do stable, układ katalogów i checklista wydań są w
  [packaging/CHANNELS.md](packaging/CHANNELS.md).

## Architektura — co gdzie jest

### Moduł analityczny (jmv/)
Każda analiza to trójka/czwórka plików:
- `jmv/jamovi/<analiza>.a.yaml` — definicja opcji (parametry, checkboxy, listy)
- `jmv/jamovi/<analiza>.u.yaml` — układ panelu UI (layout checkboxów, sekcji)
- `jmv/jamovi/<analiza>.r.yaml` — definicja tabel wynikowych (kolumny, typy, formatowanie)
- `jmv/R/<analiza>.b.R` — logika R + wykresy (ggplot2)

Konwencja nazw opcji: camelCase (np. boxMean, histFacet).
Wykresy generowane przez ggplot2.

### Aplikacja (katalog główny repozytorium)
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
- Zachowuj istniejące opcje analiz. Jeśli jUPWR zastępuje analizę jmv,
  ukryj ją we wstążce przez `JUPWR_HIDDEN_ANALYSES`, aby starsze pliki
  `.omv` nadal mogły ją uruchomić. W modułach jUPWR pokaż opcje kursu
  podstawowego od razu, a pozostałe grupuj w sekcji „Zaawansowane”.
- Dla tabel z `rows: 0` uzupełnianych przez `addRow()` w `.run` uwzględnij
  w `clearWith` wszystkie opcje analizy, także `metody` i opcje wykresów.
  Przywrócenie niewyczyszczonej tabeli może prowadzić do powielenia wierszy.
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

## Stan dystrybucji

Aktualne wersje, skład wydania i macierz zgodności opisuje
[rejestr modułów](packaging/MODULES.md). `jCI` obejmuje bootstrap z dawnego
`jboot`. `jRISK` i `jSpace` są opcjonalnymi modułami `.jmo`; `jSpace`
buduj bez `--skip-deps`, aby paczka zawierała zależności przestrzenne.
`jRol`, `jPomiar` i `jWody` to zamrożone moduły wewnętrzne, poza wydaniem.
Buduj je na wyraźne żądanie. W Windows służy do tego `-InternalModules`,
a na macOS osobne skrypty 72–74. Dla `jPomiar` i `jWody` dostępne są też
cele budowania pod Linux opisane w rejestrze modułów. Metody, dane i ćwiczenia
opisują [jPomiar/README.md](jPomiar/README.md) oraz [jWody/README.md](jWody/README.md).
Testy kompilacji i analiz uruchamiają `packaging/scripts/test-jpomiar.sh`
oraz `packaging/scripts/test-jwody.sh`.

W `jmv::descriptives` zachowaj `boxMean` i współczynnik zmienności V (%).
Facetowanie histogramów obsługuje „Podziel według”, a przedziały ufności dla
średniej są w `jCI`.

### Wstążka i wygląd interfejsu

- zakładka „Wykresy" podzielona na kategorie dydaktyczne wg zastosowań
  (wzór: r-graph-gallery.com). Mechanizm: `menuGroup` w `plots/jamovi/*.a.yaml`
  → jedno menu na wstążce (client/main/ribbon/plotstab.ts, stała kolejność);
  tłumaczenia nazw grup w `plots/jamovi/i18n/pl.po`; ikony kategorii
  `client/assets/analysis-plots-<kategoria>.svg` + `plotstab.css`.
  Kategorie (klucz EN → nazwa PL) i wykresy (nazwy analiz w scatr):
  Distribution→Rozkład ilościowe (jmvhist, discbar, dens, qq, raincloud;
  ridge UKRYTY 2026-10-26 jak parcoord — `scatr::ridge`
  w `JUPWR_HIDDEN_ANALYSES`, tę samą rolę pełni dens z `densFacet`),
  Composition→Rozkład jakościowe (stackbar, waffle, treemap,
  mosaic) — decyzja 2026-10-26: kryterium kategorii rozkładu to TYP
  zmiennej głównej (grupująca to dodatek), obie grupy obok siebie na
  początku wstążki, nazwy łamane na dwie linie (`plotstab.css`: label
  `width: min-content`, margines górny przycisku = dodatkowa linia, żeby
  ikony były w jednym rzędzie); discbar = zmienna dyskretna (całkowite): słupek/prążek
  na każdą wartość, zera dla nieobecnych, liczność/udział (udział w grupie),
  grupy obok siebie albo panele, wartości niecałkowite → błąd z odesłaniem
  do histogramu. W jmvbar tryb „Kategorialne” UKRYTY (`options` ModeSelectora
  w `jmvbar.u.yaml`, domyślny tryb `continuous`; Content zostaje dla starych
  .omv) — liczności jednej zmiennej jakościowej rysuje stackbar.
  Comparison→Porównania (jmvbox, violin, stripmean), Ranking→Ranking
  (jmvbar, lollipop, wordcloud, circbar), Profiles→Profile (radar,
  parcoord, meanprofile, dumbbell, slope; decyzja 2026-10-22: profile na
  wielu zmiennych to nie ranking; meanprofile = profil średnich z SE/CI,
  dumbbell i slope = dwa pomiary w formacie szerokim, przed–po; parcoord
  UKRYTY w menu (`scatr::parcoord` w `JUPWR_HIDDEN_ANALYSES`, filtrowane też
  w plotstab.ts) — nieczytelny w kursie podstawowym, kod zostaje dla starych
  .omv; radar ma przełącznik `facet` = osobny radar dla każdej grupy),
  Correlation→Zależności (scat, bubble, heatmap, corrgram, hexbin),
  Evolution→Trendy (jmvline, area) (mozaika przeniesiona z Zależności
  2026-10-22 do Composition — zmienne jakościowe, ta sama informacja co
  stackbar), Other→Inne (pareto; tu też lądują wykresy
  z modułów zewnętrznych z menuGroup '.'/'More'/'Other plots').
  Wykresy o własnej geometrii w czystym R (bez zależności): treemap
  (squarify), wordcloud (spirala + AABB), radar (kartezjańska geometria,
  nie coord_polar), raincloud (half-violin z gęstości).
  Zarezerwowane nazwy przyszłych kategorii — tworzyć dopiero razem z pierwszym
  wykresem i dopisać do groupOrder w plotstab.ts: Map→Mapy, Flow→Przepływy.
  Plan dydaktyczny: bez wykresu kołowego (świadoma decyzja — oduczamy);
  boxplot celowo tylko w Porównaniach (porównania między grupami),
  a rozkładową rolę boxplota pełni raincloud w Rozkładach.
- branding jUPWR: nazwa, ikony (icns/ico/iconset), wersja dystrybucji
  w `client/common/jupwr.ts`; kolory interfejsu jako
  tokeny `--jupwr-*` w client/common/jupwr-colors.css (bordo, powierzchnie,
  ciepłe szarości karty opcji, linie, tekst), kolory wykresów osobno
  w jmvcore/R/themes.R
- dwa wyglądy interfejsu (od 2026-11-01, linia latest): ustawienie „Interface”
  w szufladzie ustawień — Classic (jamovi bez zmian) albo Light (A).
  Mechanizm: client/common/uistyle.ts (atrybut html[data-ui-style] w oknie
  i w ramkach opcji/wyników), style A w client/{main,analysisui}/jupwr-ui-a.css,
  nagłówki kart i edytor w karcie danych w client/main/uicards.ts.
  ZASADA: wygląd Classic ma zostać identyczny z jamovi (sprawdzać zrzutami);
  wyjątki: etykiety menu (JUPWR_MENU_LABELS) i podpowiedzi pisania w wynikach
  (client/resultsview/jupwr-annotation.css). Stan i ograniczenia:
  packaging/tests/manual/handoff-redesign-A-C.md, sekcja 7


## Kolejka zadań i historia decyzji

Pełne opisy zadań (decyzje, świadome pominięcia, pułapki, stan wdrożenia) są
w [docs/DECYZJE.md](docs/DECYZJE.md). Ten plik zawiera tylko zasady bieżące
i spis poniżej. Przed pracą nad modułem przeczytaj w `docs/DECYZJE.md` zadania
wskazane dla niego w spisie. Nowe zadania i zmiany statusu wpisuj w
`docs/DECYZJE.md` (pliki docelowe, oczekiwane zachowanie) i aktualizuj spis.

### Zasady przekrojowe wynikające z decyzji

- „Opis zastosowanych metod” (`metody`, zadanie 19) mówi „co” i „jak”, nigdy
  „dlaczego” i nigdy wyniki liczbowe. Noty pod tabelami: jedno zdanie, tylko
  gdy bez niego tabeli nie da się odczytać.
- Podział obowiązków: jTestyT testuje (bez przedziałów), jCI estymuje.
  Warianty wizualne wykresów tylko w scatr, wykresy w modułach analiz proste.
- `ggtheme` niesie dyskretne skale `fill`/`colour` palety: własną skalę
  kolorów dodawaj PO `+ ggtheme`; `.plot` ma sygnaturę
  `function(image, ggtheme, theme, ...)`.
- `.plot` zaczyna od `if (is.null(state)) return(FALSE)`; po błędzie w `.run`
  jamovi i tak woła `.plot` z pustym stanem.
- jmvcore przesłania `format()` (ignoruje `big.mark`): w `.b.R` używaj
  `formatC` albo `base::format`. Wrapper analizy `anova()` zasłania
  `stats::anova`: w jANOVA zawsze `stats::anova(...)`.
- W `.r.yaml` widoczność tylko od pojedynczej opcji; wyrażenia złożone
  (`(a && b)`) ustawiaj w `.b.R`.
- Klucze ukrywanych analiz to `ns::name` z pola `name:` w `.a.yaml`
  (camelCase, np. `jmv::anovaOneW`), nie nazwy plików.
- Wykres z punktem na obserwację (rozrzut, Q-Q, punkty na pudełkach) zostaje
  rastrowy; pozostałe mogą mieć `mode: vector` (zadanie 21).
- Cytowania pakietów: wpis w `packaging/refs.yaml` i kopia klucza
  w `jamovi/00refs.yaml` modułu; `refs:` tylko na elemencie, który z pakietu
  korzysta (zadanie 20).
- `plots/` to submoduł: zmiany osobnym commitem w submodule i bump wskaźnika.

### Spis zadań dydaktycznych

| Nr | Temat | Moduły | Status |
|---|---|---|---|
| mapa 1.0.0 | Kurs podstawowy: jperm, jCI (+jboot), jEksplor, jRegr, podział jCI ↔ jTestyT | jperm, jCI, jEksplor, jRegr, jTestyT | zrobione |
| 8 | Doświadczalnictwo rolnicze | jRol (wewnętrzny) | zrobione |
| 9 | Rozszerzenia statystyk opisowych | jEksplor | zrobione |
| 10 | Testy nieparametryczne | jANOVA, jmv, `jNiepar` | OTWARTE: część poza ANOVA |
| 11 | Szeregi czasowe | `jSzereg` (nowy) | OTWARTE: nierozpoczęte |
| 12 | Moc testu i liczebność próby | `jMoc` (nowy) | OTWARTE: nierozpoczęte |
| 13 | Wstęp do uczenia maszynowego | `jML` (nowy) | OTWARTE: nierozpoczęte |
| 14 | ANOVA jUPWR | jANOVA | zrobione |
| 15 | Testy t jUPWR | jTestyT | zrobione |
| 16 | Przegląd not pod tabelami | jANOVA, jTestyT, jCzest, jRol | zrobione poza jRol |
| 17 | Częstości jUPWR | jCzest, jEksplor | zrobione |
| 18 | Tryb szeroki w wykresach | scatr | OTWARTE: etap 3 (`jmvbar`), etap 4 (`dens`) |
| 19 | Opis zastosowanych metod | jmvcore, wszystkie moduły kursu | zrobione |
| 20 | Bibliografia pakietów i zbiorów | wszystkie, klient | zrobione (bez literatury metod) |
| 21 | Elementy wyników 28.4+ (Text, Svg, vector, File) | jmvcore, jEksplor, scatr | OTWARTE: vector w jEksplor i scatr zrobione; reszta do decyzji |
| 22 | Audyt 2026-10-14 | serwer, klient | OTWARTE: typ IF, zgłoszenia do upstreamu, testy składni scatr |
| 23 | Motyw i paleta na wykresach | jCzest, jperm, jCI | zrobione |
| 24 | Audyt zmiennej przekształconej | klient, serwer | OTWARTE: punkty 4, 7, 10 i drobniejsze |
| 25 | Częstości naturalne | jRISK | zrobione |
| 26 | RD i RR w zdarzeniach | jRISK | zrobione |
| 27 | Audyt kodu 2026-11-05 | wszystkie | OTWARTE: lista pozostałych punktów |
| 28 | Dane empiryczne obok rozkładu | distrACTION | zrobione |
| 29 | Stabilizacja przed 1.4.1 | wiele | zrobione |
| 30 | Audyt zmiennej przekształconej 2026-11-10 | klient, serwer | zrobione (2 poprawki; 2 punkty świadomie bez zmian; lista słabszych otwarta) |

### Spis zadań technicznych

Wszystkie zrobione; opisy określają zachowanie, którego nie wolno zepsuć.

| Nr | Temat | Obszar |
|---|---|---|
| 8 | Niezawodność obsługi analiz | `server/jamovi/server/` (silniki, kolejki) |
| 9 | Instalacja modułów przez przeciąganie | `client/main/store/pagesideload.ts` |
| 10 | Bezpieczna instalacja i aktualizacja modułów | `server/jamovi/server/modules/modules.py` |
| 11 | Kolejki i kontrola obciążenia | serwer, `packaging/BACKEND.md` |
| 12 | Zapis i odzyskiwanie dokumentów | `server/jamovi/server/formatio/`, wolumen `jupwr-recovery` |
| 13 | Potwierdzenie usunięcia wszystkich wyników | `client/main/resultspanel.ts` |
| 14 | Interwał kopii odzyskiwania w preferencjach | klient, `session.py` |
| 15 | Odpowiedzi JSON Lines dzielone przez sieć | `client/main/utils/jsonlines.ts` |
| 16 | Ochrona danych przy nieudanym imporcie | `server/jamovi/server/{instance,datasetmodel}.py` |
