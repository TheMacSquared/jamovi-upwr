# CLAUDE.md — jamovi fork (jUPWR)

## Projekt
Zmodyfikowana dystrybucja jamovi na potrzeby dydaktyki statystyki
na Uniwersytecie Przyrodniczym we Wrocławiu.

## Repozytorium
Fork https://github.com/TheMacSquared/jamovi-upwr z submodułami (jmv, plots, i18n, readstat).
Budowanie przez Docker: `docker compose --profile main build && docker compose --profile main up`
Aplikacja dostępna pod http://localhost:41337 po zbudowaniu (nie `127.0.0.1` — serwer
przyjmuje tylko origin `localhost`, `JAMOVI_HOST_A` w `docker-compose.yaml`).

## Linia wydań — TA GAŁĄŹ: `main` (kanał latest, jUPWR 1.1.x, jamovi 28.13)
- Tu trafiają nowe funkcje i nowy upstream. Stable to `release/1.0` (1.0.x,
  jamovi 28.2) — tylko poprawki, w katalogu `jamovi-upwr-stable` (ten katalog to `jamovi-upwr-latest`).
- Poprawki robimy tylko tu (decyzja 2026-10-18: latest z czasem zastąpi stable).
  Na `release/1.0` przenosimy (`git cherry-pick -x`) wyłącznie błędy KRYTYCZNE
  (utrata/zepsucie danych, awaria aplikacji, błędne wyniki na zajęciach);
  poprawki ze stable przychodzą tu jak dotąd (`release-check.sh` wypisuje nieprzeniesione).
- Submoduły jmv, plots, i18n śledzą tu gałąź `main` w swoich repozytoriach.
- `legacy/win81` to osobna, zamrożona linia (Windows 8.1).
- Pełne zasady, układ katalogów i checklista wydania: `packaging/CHANNELS.md`.

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
- Opcji analiz jmv nie usuwaj z kodu: gdy jUPWR je zastępuje, ukrywaj je w kliencie
  (`JUPWR_HIDDEN_ANALYSES`). W modułach jUPWR opcje wolno usuwać i upraszczać —
  priorytetem jest dydaktyczna prostota panelu (rdzeń widoczny od razu, reszta
  w sekcji „Zaawansowane")
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

## Stan dystrybucji (kolejka pierwotnych zadań zrealizowana)

Wszystkie zadania pierwotnej kolejki (1–7) są zrealizowane — stan na sierpień 2026:
- descriptives: boxMean, współczynnik zmienności V (%); facetowanie histogramów
  zapewnia wbudowany mechanizm „Podziel według" upstreamu (osobna opcja
  histFacet okazała się zbędna); CI dla średniej usunięte (przeniesione do jCI)
- moduły preinstalowane: jmv, plots (scatr), jperm, jCI (od 0.2.0 z bootstrapem, dawny jboot wchłonięty),
  jdistrACTION 1.3.x (11 rozkładów, w tym gamma, Weibulla i ujemny dwumianowy
  z wyborem konwencji zmiennej losowej); jRISK (ryzyko i niezawodność:
  modele czasu życia z cenzorowaniem, systemy, FTA, zdarzenia/warunkowanie,
  schemat Bernoulliego + zbiory danych Bananpol w bibliotece) jest modułem
  OPCJONALNYM: nie preinstalowany, budowany jako .jmo (macOS: 70-jmo-jrisk.sh,
  Windows: build.ps1 krok 4e), instalowany przez Moduły → Sideload
- MODUŁY WEWNĘTRZNE (decyzja 2026-10-18): jRol, jPomiar, jWody to moduły testowe
  użytkownika, nigdzie niepublikowane — ZAMROŻONE i POZA WYDANIEM. Nie budować ich
  .jmo przy wydaniu (`release-check.sh` pomija je — lista `INTERNAL`; Windows
  `build.ps1` tylko z `-InternalModules`; macOS 72–74 tylko ręcznie), NIE naprawiać
  w nich niczego przy okazji zmian w innych modułach (nawet znanych błędów, np.
  mapa jWody: gradient przed `ggtheme`), nie bumpować. Opisy niżej są historyczne.
- jRol 0.1.3 (doświadczalnictwo rolnicze) — WEWNĘTRZNY
  (macOS: 72-jmo-jrol.sh, Windows: build.ps1 -InternalModules krok 4g)
- jSpace 0.2.0 (statystyka danych kosmicznych: orbity TLE/SGP4 przez asteRisk,
  mapy punktowe/kartogramy i statystyki regionalne przez sf, statystyki rastrów
  przez terra, klasyfikacja k-NN/drzewo; 4 zbiory danych w bibliotece) jest
  modułem OPCJONALNYM jak jRISK, ale budowanym BEZ --skip-deps (jmc bundluje
  sf/terra/asteRisk do .jmo; macOS: 71-jmo-jspace.sh, Windows: build.ps1
  krok 4f); szczegóły w packaging/MODULES.md
- jPomiar 0.3.0 — WEWNĘTRZNY (pomiary i niepewność, pilotaż międzykierunkowy; menu „Pomiary":
  powtarzane pomiary z u_A, budżet niepewności typ A / wzorcowanie U/k /
  rozdzielczość d/√12, propagacja sumy/różnicy/iloczynu/ilorazu z korelacją,
  kowariancja i elipsy 2D: standardowa, rozrzutu χ², ufności średniej T²
  Hotellinga; 2 zbiory syntetyczne) jest modułem OPCJONALNYM jak jRISK
  (macOS: 73-jmo-jpomiar.sh, Windows: build.ps1 krok 4h, Linux: osobny target
  Dockerfile `jpomiar-artifact` przez packaging/scripts/build-jpomiar-docker.sh).
  Silnik w czystym R: `jPomiar/R/utils.R`, `R/ellipse.R`; pełny test (kompilacja
  jmc + testthat): `packaging/scripts/test-jpomiar.sh`. Każda analiza ma „Opis
  zastosowanych metod" (zadanie 19); objaśnienia założeń i ćwiczenia są
  w `jPomiar/README.md`, nie w wynikach. PUŁAPKA: tabele budowane przez
  `addRow()` w `.run` (rows: 0) muszą mieć w `clearWith` KAŻDĄ opcję analizy
  (także `metody` i wykresy) — tabela niewyczyszczona wraca ze stanu,
  a `addRow` dopisuje wiersze drugi raz.
- jWody 0.1.1 — WEWNĘTRZNY (hydrologia; menu „Hydrologia": kontrola szeregu, reżim
  i sezonowość, przepływy charakterystyczne, trendy MK/Sen/Pettitt, niżówki;
  daty jako tekst `RRRR-MM-DD`; zbiory w bibliotece) jest modułem OPCJONALNYM
  jak jRISK (macOS: 74-jmo-jwody.sh, Windows: build.ps1 krok 4i, Linux:
  packaging/scripts/build-jwody-docker.sh); testy: `packaging/scripts/test-jwody.sh`,
  metody i walidacja w `jWody/README.md`
- zakładka „Wykresy" podzielona na kategorie dydaktyczne wg zastosowań
  (wzór: r-graph-gallery.com). Mechanizm: `menuGroup` w `plots/jamovi/*.a.yaml`
  → jedno menu na wstążce (client/main/ribbon/plotstab.ts, stała kolejność);
  tłumaczenia nazw grup w `plots/jamovi/i18n/pl.po`; ikony kategorii
  `client/assets/analysis-plots-<kategoria>.svg` + `plotstab.css`.
  Kategorie (klucz EN → nazwa PL) i wykresy (nazwy analiz w scatr):
  Distribution→Rozkład (jmvhist, dens, qq, raincloud, ridge),
  Comparison→Porównania (jmvbox, violin, stripmean), Ranking→Ranking
  (jmvbar, lollipop, wordcloud, circbar), Profiles→Profile (radar,
  parcoord, meanprofile, dumbbell, slope; decyzja 2026-10-22: profile na
  wielu zmiennych to nie ranking; meanprofile = profil średnich z SE/CI,
  dumbbell i slope = dwa pomiary w formacie szerokim, przed–po; parcoord
  UKRYTY w menu (`scatr::parcoord` w `JUPWR_HIDDEN_ANALYSES`, filtrowane też
  w plotstab.ts) — nieczytelny w kursie podstawowym, kod zostaje dla starych
  .omv; radar ma przełącznik `facet` = osobny radar dla każdej grupy),
  Correlation→Zależności (scat, bubble, heatmap, corrgram, hexbin),
  Evolution→Trendy (jmvline, area), Composition→Części całości
  (stackbar, waffle, treemap, mosaic; mozaika przeniesiona z Zależności
  2026-10-22 — zmienne jakościowe, ta sama informacja co stackbar),
  Other→Inne (pareto; tu też lądują wykresy
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
  w client/common/jupwr.ts (patrz sekcja Wersje)
- jANOVA 0.3.0 (wbudowany) zastępuje w menu ANOVA analizy jmv (jednoczynnikowa,
  ANOVA, ANCOVA, powtórzonych pomiarów) — lista ukrytych w client/common/jupwr.ts

Nowe zadania dopisuj poniżej jako "### Zadanie N: ..." z plikiem docelowym
i oczekiwanym zachowaniem.

### Mapa drogowa jUPWR 1.0.0 (decyzja 2026-09-04, wariant „kurs podstawowy")
Definicja 1.0.0: każde menu kursu podstawowego ma analizy jUPWR (rdzeń →
„Zaawansowane") i KAŻDA analiza jUPWR ma „Opis zastosowanych metod"
(zadanie 19). Analizy poza kursem zostają jmv, widoczne (precedens: MANCOVA,
logLinear), ewentualnie na końcu podmenu przez `JUPWR_MENU_LAST`.
Kolejność prac:
1. Opisy metod w istniejących modułach: jTestyT → jANOVA (razem
   z zadaniem 16: przycinanie not to ta sama robota).
2. jCI, jboot, jperm (własne moduły, starsze): najpierw przegląd panelu
   wg obecnego schematu „krytyczne → opcjonalne → do usunięcia", potem opisy.
   Decyzje z przeglądu (2026-09-04):
   - jperm 0.2.0 ZROBIONE: konwencje jTestyT (`vars`/`pairs`/`group`,
     `hypothesis` different/greater/less jako ComboBox, `testValue`, `plot`),
     wiele zmiennych naraz, rdzeń = Hipoteza/Wykres/Opis metod, Zaawansowane =
     liczba permutacji, ziarno, test dokładny; opis metod; podpisy w menu.
     Permutacje zostają osobnym menu (dydaktycznie oddzielone od klasycznych).
   - jCI ZOSTAJE osobnym modułem i ma nieść ciężar przedziałów ufności
     (dydaktycznie CI przed testami): rozbudować CI w jCI, a w testach
     (jTestyT) CI tylko w prostej postaci albo wcale — podział obowiązków
     jako osobne zadanie. Błędy do naprawy w jCI: CI Spearmana wzorem
     Fishera dla Pearsona (i wstęga OLS na wykresie), wykres różnicy
     proporcji bez przedziału, „Newcombe" z podwójnym z; „Error bar plot"
     po angielsku; brak menuSubtitle.
   - jCI 0.2.0 ZROBIONE = jCI + jboot SCALONE (decyzja 2026-09-04): jboot
     usunięty z drzewa i skryptów builda (Dockerfile, build.ps1,
     20-modules.sh, MODULES.md, groupOrder klienta). W każdej analizie jCI
     lista „Metoda przedziału": klasyczna (t / Welch / Wilson /
     Clopper-Pearson / Wald / Newcombe / Fisher z / t dla lm) albo
     bootstrap percentylowy / BCa (pakiet boot; „Zaawansowane": B, ziarno,
     histogram replikacji). Nowe analizy: `ciregression` (wyraz wolny
     i nachylenie, pasmo klasyczne z predict() albo bootstrapowe)
     i `cibootstrap` „Jak działa bootstrap" (podgrupa Dydaktyka, próby krok
     po kroku + zbieżność). Średnia jednej próby ma listę miary
     (średnia/mediana/ucięta; mediana i ucięta wymuszają bootstrap
     percentylowy z notą). Naprawione: CI Spearmana (Bonett-Wright),
     Newcombe bez podwójnego z (zwalidowany z Newcombe 1998), wykres
     różnicy proporcji rysuje przedział (Gardner-Altman na skali udziału).
     Silnik: `jCI/R/utils.R` (`bootCI` — jedno losowanie daje wszystkie
     statystyki, np. oba współczynniki regresji; BCa z fallbackiem do
     percentyli, gdy boot.ci zwraca NULL). Świadomie bez: showClassical
     (testy są w jTestyT), metody „basic", CI per poziom w proporcji
     (jCzest zgodność). Stare .omv z jboot i jCI 0.1.0 nie otwierają tych
     analiz.
3. Eksploracja: moduł `jEksplor` zamiast rozszerzania jmv descriptives
   z zadania 9 — zakres z zadania 9 przechodzi do niego; jmv::descriptives
   ukryte. STAN: `ilosciowe` ZROBIONE (0.1.0; decyzje 2026-09-04: kwartyle
   w rdzeniu, „Podziel według" jak w jmv — wiele zmiennych, kombinacje
   poziomów; łodyga-liście pominięte; bez CI średniej i bez wag). Rdzeń:
   N, braki, średnia, mediana, Q1/Q3, SD, min, max, V; układ tabeli
   wiersze/kolumny (kolumny budowane w .init z opcji, `clearWith` na
   wszystkich statystykach); Zaawansowane: położenie (dominanta, suma,
   geometryczna, harmoniczna, ucięta, winsoryzowana), rozproszenie
   (wariancja, rozstęp, IQR, odch. przeciętne, MAD bez stałej, ćwiartkowe,
   V_Q, typowy obszar), kształt (G1/G2 z SE jak SPSS/jmv, Pearsona,
   kwartylowa), kwantyle i punkty podziału, wartości skrajne, koncentracja
   (Gini, Lorenz), wykresy (histogram+gęstość, pudełkowy+skrzypce+punkty+
   średnia, ECDF); Założenia: Shapiro, Lilliefors i Anderson-Darling
   w czystym R (zwalidowane z nortest, p wg Dallala-Wilkinsona/Stephensa),
   Q-Q. `szereg` ZROBIONE: klasy wg Sturgesa / liczby / szerokości
   (lewostronnie domknięte, opcjonalny początek), tabela z środkami,
   licznościami, częstościami i skumulowanymi, miary interpolowane
   z szeregu (średnia, mediana, dominanta, wariancja, SD) obok wartości
   z danych, histogram klas i ogiwa. `jakosciowe` ZROBIONE: własna
   implementacja (nie port jmv qualitative): tabela liczności z % i
   skumulowanymi albo tabela krzyżowa z jedną zmienną grupującą (% w
   kolumnie/wierszu/ogółem), podsumowanie (N, braki, kategorie, dominanta
   z udziałem), słupkowy i mozaikowy. jmv::descriptives i jmv::qualitative
   ukryte w kliencie (kod jmv nietknięty). ETAP 3 ZAMKNIĘTY.
4. Regresja: moduł `jRegr` tylko z korelacją, regresją liniową (prostą
   i wieloraką) i logistyczną dwumianową; wielomianowa i porządkowa
   zostają jmv (widoczne, później — po 1.0.0). Decyzje 2026-09-04:
   ukryć jmv::linReg całkowicie (bez bloków, średnich brzegowych, wag,
   interakcji), jmv::simpleCorr i jmv::corrMatrix też; jmv::corrPart,
   logRegMulti, logRegOrd widoczne na końcu menu (JUPWR_MENU_LAST).
   Predyktory jakościowe z WYBOREM poziomu odniesienia (deklaratywnie jak
   `refLevels` w jmv linReg: Array items (factors) + ListBox z
   LevelSelector). Korelacja: przy DOKŁADNIE 2 zmiennych tabela jednego
   wiersza (r, CI, p, N) i rozrzut, przy >2 macierz — bez macierzy 2×2
   z polami redundantnymi. Kolejność: korelacja → liniowa → logistyczna.
   STAN: jRegr 0.1.0 ZROBIONE — `korelacja` (para: jeden wiersz z CI
   i rozrzut; ≥3: dolny trójkąt macierzy jako komórki tekstowe r/p/N
   z gwiazdkami, macierz rozrzutów jako panele par), `liniowa` (lm; R²,
   R² skoryg., F, RMSE; b, SE, t, p, CI; β; ANOVA typu II przez car;
   AIC/BIC; Shapiro reszt, Q-Q, reszty vs dopasowane, Durbin-Watson
   z p przez car (ziarno 1), VIF liczony ręcznie na kolumnach macierzy
   modelu, Cook; rozrzut z prostą tylko przy jednym predyktorze
   ilościowym; zapis przewidywanych i reszt), `logistyczna` (glm logit;
   wybór kategorii „zdarzenie" opcją Level, domyślnie DRUGI poziom;
   dewiancja, test LR, R² McFaddena i Nagelkerkego; b, SE, z, p, OR z CI;
   tabela klasyfikacji i trafność/czułość/swoistość/AUC przy progu;
   ROC w czystym R; krzywa logistyczna przy jednym predyktorze; VIF,
   Cook; zapis prawdopodobieństw). Poziomy odniesienia: opcja Array
   `refLevels` z `items: (factors)` + ListBox z LevelSelector, JS
   `jamovi/js/<analiza>.js` ustawia `variable` selektora (wzór jmv).
   Ukryte: jmv::simpleCorr, corrMatrix, linReg, logRegBin. Kolejność menu
   Regresja (2026-09-04): Korelacja → Korelacja cząstkowa → Regresja liniowa,
   belka „Regresja logistyczna" (jRegr logistyczna ma `menuSubgroup: Logistic
   Regression` — klucz NIEtłumaczony jak w jmv, tytuł tłumaczy i18n jmv) →
   dwupoziomowa → Porządkowa → Wielomianowa. Mechanizm: `JUPWR_MENU_ORDER`
   w `client/common/jupwr.ts` (stabilne sortowanie w podmenu; nienotowane =
   500, `JUPWR_MENU_LAST` = 1000). ETAP 4 ZAMKNIĘTY — mapa drogowa
   1.0.0 w części „moduły" wykonana; do wydania 1.0.0 zostaje bump wersji
   jUPWR, CHANGELOG, MODULES.md, .jmo modułów opcjonalnych i buildy
   natywne.
5. Podział obowiązków jCI ↔ jTestyT (decyzja 2026-09-04, wariant A —
   ścisły): jTestyT = tylko testowanie (statystyka, df, p, różnica i d
   Cohena jako punkty, bez przedziałów, bez opcji poziomu ufności, wykres
   tylko pudełkowy); jCI = estymacja (przedziały klasyczne i bootstrap,
   Gardner-Altman, d Cohena z przedziałem niecentralnego t w różnicy
   średnich i parach, wybór Student/Welch). Opis metod jTestyT odsyła do
   jCI. Wersje: jTestyT 0.4.0, jCI 0.3.0. ZROBIONE: w jTestyT usunięte
   `ciWidth`, `plotType`, kolumny CI różnicy i d, `estimationPlot`
   i `dInterval`; wykres = `boxPlotTests` (pudełka + punkty + średnia,
   linia odniesienia dla wartości testowej / zera; w parach trzecie
   pudełko różnic). W jCI: `ciTwoMeansStudent` (metoda `student`
   w różnicy średnich), `dInterval` przeniesiony z jTestyT, opcja
   `effSize` (d Cohena z przedziałem niecentralnego t) w różnicy średnich
   i parach. Stare .omv z jTestyT ≤ 0.3.0 tracą kolumny CI.
WYDANIE 1.0.0 (2026-09-04): numer nadany zamiast 0.9.5.7 (ten sam dzień, bez
dystrybucji natywnej); wszystkie moduły mapy drogowej gotowe; do zrobienia na
maszynach docelowych: build Windows (build.ps1) i macOS (skrypty packaging),
przebudowa .jmo modułów opcjonalnych oznaczonych w MODULES.md „do zbudowania".
Poza zakresem 1.0.0: opisy metod dla wykresów scatr (świadomie nie),
regresja wielomianowa/porządkowa, jRol oraz moduły opcjonalne (jRISK,
jSpace) — te dostają opisy później, w miarę potrzeby.
Zasada (decyzja 2026-09-04, „by design"): opis metod mówi „co" i „jak"
(np. „poziomy w kolejności alfabetycznej — odwrotna kolejność odwraca OR"),
NIGDY „dlaczego" (np. dlaczego ART zamiast rang na surowych danych). Opis
porządkuje to, co zrobiono; nie jest materiałem dydaktycznym — wyniki mają
nie być przeładowane treścią. Objaśnienia „dlaczego" wypadają z aplikacji
razem z przycinaniem not (zadanie 16).

### Zadanie 8: jRol — doświadczalnictwo rolnicze (moduł OPCJONALNY .jmo) — ZREALIZOWANE 0.1.0
Katalog `jRol/` (wzór: jCI; pierwotnie jDosw, przemianowany i zmieniony na opcjonalny,
bo to moduł jednego kursu; budowa: macOS 72-jmo-jrol.sh, Windows build.ps1 krok 4g), wspólny silnik w `R/utils.R`. Menu „Doświadczalnictwo":
układ całkowicie losowy (crd), losowanych bloków (rcbd), kwadrat łaciński (latin),
split-plot (splitplot) — każdy z tabelą ANOVA (SS sekwencyjne, w split-plot błąd (a)
z `blok:A` i błąd (b) resztowy), porównaniami wielokrotnymi (Tukey HSD domyślnie,
NIR/LSD, Scheffé, Dunnett przez mvtnorm, Holm; świadomie BEZ Duncana i SNK),
grupami jednorodnymi CLD (insert-absorb; konwencja R: „a" = najniższa średnia),
różnicą graniczną per para (jedna NIR w nocie, gdy liczebności równe), wykresem
średnich z literami (SE/CI/SD), interakcji, reszt, testami Levene'a/Bartletta
i Shapiro reszt. W split-plot porównania A przy różnych B używają błędu łączonego
z df Satterthwaite'a (klasyczne wzory). Analiza `plan`: randomizacja układu +
mapa pola. Zależności: base R + mvtnorm + car (w modules/base/R). Zbiory
w bibliotece jRol (`jRol/data`, `data-raw/datasets.R`): oats (Yates, split-plot),
pszenica_latin (syntetyczny kwadrat łaciński) — przeniesione z jDane.
Testy: `jRol/tests/testthat` (silnik vs aov/TukeyHSD/multcomp + integracyjne).

### Zadanie 9: Eksploracja — rozszerzenia statystyk opisowych (wariant B) — ZREALIZOWANE w jEksplor (mapa drogowa, etap 3; bez łodygi-liści)
Pliki `jmv/jamovi/descriptives.{a,u,r}.yaml`, `jmv/R/descriptives.b.R`: nowe
checkboxy: średnia geometryczna/harmoniczna/ucięta/winsoryzowana; odchylenie
przeciętne, MAD, odchylenie ćwiartkowe, V_Q, typowy obszar zmienności; skośność
Pearsona i kwartylowa; sekcja „Koncentracja": Gini + krzywa Lorenza; normalność:
Lilliefors, Anderson-Darling; wykresy: ECDF, łodyga-liście. Osobna nowa analiza
w menu Eksploracja: „Szereg rozdzielczy" (klasy wg Sturgesa/liczby/szerokości,
tabela liczności i częstości skumulowanych, średnia/dominanta/mediana
interpolowane z szeregu). Nie przywracać CI dla średniej (jest w jCI).

### Zadanie 10: testy nieparametryczne (wariant C — hybryda) — CZĘŚĆ ANOVA ZREALIZOWANA w jANOVA
Część ANOVA (Kruskal-Wallis z ε² i Dunnem, Jonckheere-Terpstra, test mediany
Mooda, Friedman z W Kendalla, Nemenyi/Conover, test Page'a oraz ART — aligned
rank transform dla układów czynnikowych, własna implementacja zwalidowana
z ARTool) jest w jANOVA jako przełączniki w panelach „ANOVA" i „ANOVA
powtórzonych pomiarów"; jmv::anovanp i jmv::anovarmnp ukryte w kliencie.
Pozostaje: checkboxy w jmv: test znaków (ttestps, ttestones), K-S dwóch prób
i test mediany Mooda (ttestis); testy bez naturalnego miejsca w małym module
wbudowanym `jNiepar/` (menu „Nieparametryczne"): Q Cochrana, test serii,
K-S/Lillieforsa jednej próby, Fligner-Killeen i Ansari-Bradley. Czyste R.

### Zadanie 11: jSzereg — szeregi czasowe (moduł wbudowany)
Nowy katalog `jSzereg/`. Menu „Szeregi czasowe": indeksy dynamiki (łańcuchowe,
jednopodstawowe, średnie tempo), średnie ruchome, dekompozycja addytywna/
multiplikatywna i wskaźniki sezonowości, wygładzanie wykładnicze (Holt-Winters),
trend liniowy/wielomianowy z prognozą. Base R (`decompose`, `HoltWinters`).

### Zadanie 12: jMoc — moc testu i liczebność próby (moduł wbudowany)
Nowy katalog `jMoc/` (wzór: jpower z biblioteki jamovi, ale po polsku, jak fork
distrACTION). Menu „Moc testu": test t (jedna/dwie/pary), proporcje, korelacja,
ANOVA jednoczynnikowa, χ²; tryby: moc / n / wykrywalny efekt; wykresy krzywych
mocy. Czyste R (`power.t.test` i własne) lub `pwr` bundlowany.

### Zadanie 13: jML — wstęp do uczenia maszynowego (moduł wbudowany)
Nowy katalog `jML/`. Menu „Uczenie maszynowe": bez nadzoru (k-średnich z łokciem
i silhouette, hierarchiczne z dendrogramem, zapis klastra do zmiennej), z nadzorem
(k-NN, drzewo decyzyjne z rysunkiem własną geometrią, naiwny Bayes, regresja
logistyczna jako klasyfikator z ROC/AUC, bagging drzew), dydaktyka (przeuczenie
na żywo: błąd trening vs test w funkcji złożoności; walidacja krzyżowa; kompromis
obciążenie-wariancja). Wspólne: podział trening/test z ziarnem, macierz pomyłek,
metryki (dokładność, precyzja, czułość, F1, kappa; RMSE, MAE, R²). Zależności:
class, rpart, cluster, MASS, nnet (recommended, w R jamovi). Analiza klasyfikacji
w jSpace zostaje bez zmian (kontekst kosmiczny). Biplot PCA jako opcja w jmv pca.

### Zadanie 14: jANOVA — ANOVA jUPWR zamiast ANOVY jmv (wariant D) — ZREALIZOWANE 0.3.0
Katalog `jANOVA/` (wbudowany), menuGroup ANOVA (pozycje lądują w menu ANOVA obok
testów nieparametrycznych jmv). Analizy: `anova` (zależna, czynniki, czynniki
blokujące addytywne, kowarianty; SS typu I/II/III przez car::Anova z contr.sum;
Welch dla jednego czynnika; η², η²p, ω²; post-hoc na średnich brzegowych emmeans:
Tukey/Dunnett vs pierwszy poziom/Brak z literami CLD i różnicą graniczną
(od 0.5.0; przy Welchu i jednym czynniku Games-Howell i Dunnett z osobnymi
wariancjami, lista podmieniana w `js/anova.js`, a R decyduje sam przez
`phEffectiveMethod`; przy Welchu i ≥ 2 czynnikach średnie bez liter z notą;
`nonpar` przy jednym czynniku ukrywa porównania parametryczne i wykres
średnich); kontrasty przez emmeans::contrast; Levene/Bartlett/Shapiro,
Q-Q, reszty do arkusza) i `anovarm` (format DŁUGI: zależna, jednostka, czynniki
wewnątrz- i międzyobiektowe, kowarianty; afex::aov_ez z include_aov; Mauchly,
poprawki GG/HF; emmeans model="univariate", poziomy wewnątrzobiektowe mapowane
z make.names). Silnik wspólny w `R/utils.R` (wywodzi się z jRol).
Panel „streamline" (decyzja 2026-09-02): rdzeń = zmienne, „Nierówne wariancje
(Welch)", „Nieparametrycznie (rangi)", „Wielkość efektu (η²p)", metoda porównań,
α, tabela par, wykresy; „Założenia" i „Zaawansowane" (bloki, kowarianty, typ SS,
interakcje, η²/ω², komórki interakcji, d Cohena, kontrasty, opisowe, słupki
błędów, reszty do arkusza) zwinięte. Jeden przełącznik `nonpar` sam wybiera:
1 czynnik → Kruskal-Wallis z ε² i Dunnem-Holmem (litery); ≥ 2 czynniki → ART
(Wobbrock 2011: inkluzja-ekskluzja średnich komórkowych, rangi, ANOVA typu III
lub afex na rangach, zgodne z ARTool) z porównaniami efektów głównych na
wyrównanych rangach; w RM 1 czynnik wewnątrz → Friedman z W Kendalla i Nemenyim.
`welch` = Welch (1 czynnik, = oneway.test) lub Welch-James (Johansen 1980:
kontrasty Kroneckera, Σ = diag(s²/n), F = T/(q+2A−6A/(q+2)), df2 = q(q+2)/(3A);
walidacja z welchADF, na CRAN brak wydania dla R 4.6 — instalować z archiwum).
Usunięte świadomie: test mediany, Jonckheere, Page, Bonferroni, Conover, HC3;
w 0.5.0 NIR, Holm i Scheffé z post-hoc (NIR zostaje w jRol).
UI warunkowe: `jamovi/js/anova.js`, `anovarm.js` (jus 3.0: handlery
`<kontrolka>_changed`, `view_updated`, `ui.x.setPropertyValue('enable', …)`).
PUŁAPKA: wrapper analizy `anova()` zasłania `stats::anova` — w kodzie modułu
zawsze `stats::anova(...)`. Ukrywanie analiz jmv: `JUPWR_HIDDEN_ANALYSES`
w `client/common/jupwr.ts` (jmv::anovaOneW, anova, ancova, anovaRM, …) filtrowane
w `client/main/ribbon/analysetab.ts` (a `JUPWR_MENU_LAST` przesuwa np. jmv::mancova
na koniec podmenu); UWAGA: klucze to `ns::name` z pola `name:`
w .a.yaml jmv, czyli camelCase (anovaOneW, anovaRM, anovaNP, anovaRMNP, ttestIS,
ttestPS, ttestOneS), nie nazwy plików. Kod jmv nietknięty, stare pliki .omv
otwierają się. MANCOVA jmv zostaje widoczna (jANOVA jej nie obsługuje).

### Zadanie 15: jTestyT — testy t jUPWR zamiast testów t jmv — ZREALIZOWANE 0.2.0
Katalog `jTestyT/` (wbudowany, menuGroup T-Tests): `ttestone`, `ttesttwo`,
`ttestpaired` z PŁASKIM panelem w stylu jmv (decyzja: testy t są zbyt proste
na podział rdzeń/zaawansowane; każdy test to osobny checkbox, żeby dało się
odznaczyć Studenta i zostawić Wilcoxona): „Testy" (t Studenta, t Welcha [dwie
grupy], Mann-Whitney / Wilcoxon), „Hipoteza" (wartość
testowa + lista), „Dodatkowe statystyki" (poziom ufności, opisowe), „Wykres",
„Sprawdzenie założeń" (Shapiro, Q-Q, Levene). Zawsze: różnica z CI i d Cohena
z CI (niecentralny t, `dInterval`). Wykresy: grup i różnicy (Gardner-Altman,
oś różnicy po prawej zakotwiczona w średniej drugiej grupy), par i różnicy,
średniej z H₀. Usunięte świadomie: czynnik Bayesa (osobny moduł w planach),
permutacje i bootstrap (są w jperm/jCI), test znaków, K-S. Ukryte w kliencie:
jmv::ttestis, ttestps, ttestones.

### Zadanie 16: przegląd podpisów (not) pod tabelami w jANOVA, jTestyT, jRol — DO ZROBIENIA (osobna sesja)
Noty pod tabelami (setNote) rozrosły się: po kilka zdań o metodzie, konwencji liter,
różnicy granicznej, poziomie CI. Przejrzeć gruntownie: każda nota ma mieć jedno
zdanie z tym, co niezbędne do odczytania tabeli; objaśnienia metod przenieść do
dokumentacji/handoutu. Sprawdzić też długość podtytułów wykresów (obcinane przy
długich nazwach zmiennych/poziomów).

### Zadanie 17: jCzest — częstości jUPWR zamiast analiz częstości jmv (wzór jANOVA) — ZREALIZOWANE (0.2.3; wszystkie 6 punktów „do dodania”, bez warstw i Mantela-Haenszela)
Katalog `jCzest/` (wbudowany, menuGroup „Częstości"). Diagnoza kategorii jmv
(2026-09-03): problem odwrotny niż w ANOVIE — panel nie jest przeładowany, tylko
PUSTY. `contTables` ma 40 opcji, wszystkie w zwiniętych sekcjach; po wrzuceniu
zmiennych widać samą tabelę liczności i χ². Domyślnie wyłączone jest wszystko,
czego trzeba do interpretacji: procenty wierszami, liczebności oczekiwane,
V Craméra. Sprawdzone w kodzie: jedyny `Notice` w `jmv/R/conttables.b.R:38`
dotyczy ważenia danych — **nie ma żadnej kontroli założenia E ≥ 5**; tabela `nom`
ma dla V Craméra wyłącznie kolumnę `Value` (bez CI); tabela `chiSq` nie ma
kolumny z wielkością efektu.

Trzy analizy zamiast pięciu:
- `tabela` (← `contTables`) — RDZEŃ: wiersze, kolumny, liczności, χ²,
  **V Craméra domyślnie ON**, procenty jako radio (brak/wierszami/kolumnami),
  α, wykres (słupkowy/mozaikowy). „Założenia": liczebności oczekiwane
  + **automatyczne ostrzeżenie, gdy E < 5 w > 20 % komórek, z sugestią Fishera**.
  „Zaawansowane": iloraz wiarygodności, poprawka ciągłości, z dla różnicy
  proporcji, miary 2×2 (OR, RR, różnica proporcji + CI, `compare`, `hypothesis`),
  miary porządkowe (gamma, tau-b, Mantel-Haenszel), reszty standaryzowane
  („które komórki decydują"), warstwy.
- `zgodnosc` (← `propTestN` + `propTest2` SCALONE) — jeden panel, metoda dobierana
  automatycznie jak przełącznik `nonpar` w jANOVIE: 2 kategorie → test dwumianowy,
  N kategorii → χ² zgodności. Dziś to dwie pozycje w menu, a dla studenta jedno
  pytanie.
- `zalezne` (← `contTablesPaired` + Q COCHRANA) — McNemar dla 2 pomiarów,
  Q Cochrana dla k pomiarów. Daje Q Cochrana naturalny dom (zadanie 10 wymienia
  go jako test „bez naturalnego miejsca", planowany do `jNiepar` — stamtąd usunąć).

Usunięte świadomie: `contCoef` (zdominowany przez V Craméra, zależy od wymiaru
tabeli), `logOdds` (szczegół techniczny, OR wystarczy), `resU` (reszty surowe,
nieporównywalne między komórkami), `resA`/`hlresA` (reszty dewiancji z Poisson GLM),
oraz w `propTest2` czynnik Bayesa z priorami i wykresami posterior (`bf`, `priorA`,
`priorB`, `ciBayes`, `ciBayesWidth`, `postPlots` — 6 z 13 opcji; ta sama decyzja
co w jTestyT: Bayes to osobny moduł w planach).

Do dodania (nie ma tego w jmv):
1. ostrzeżenie o E < 5 z sugestią Fishera — najważniejsze, dziś wybór testu
   jest niewspierany,
2. porównania wielokrotne dla tabel r×c (które pary kategorii się różnią,
   poprawka Holma) — dziś tylko reszty, które trzeba umieć czytać,
3. V Craméra z przedziałem ufności (bootstrap),
4. test trendu Cochrana-Armitage'a dla uporządkowanych kategorii
   (Mantel-Haenszel to nie to samo),
5. w Cohena jako wielkość efektu testu zgodności,
6. wykres mozaikowy wprost w analizie (dziś tylko w zakładce Wykresy jako
   `scatr::mosaic`, więc nie łączy się z tabelą).

`logLinear` ZOSTAJE jako jmv, widoczne — jedyne narzędzie do tabel 3+-wymiarowych,
poziomem poza kursem podstawowym; casus MANCOVY (nie zastępujemy, ewentualnie
`JUPWR_MENU_LAST`). CI dla pojedynczej proporcji świadomie NIE dublujemy — jest
w jCI (`ciproportion`).

Kolejność prac: `tabela` najpierw (≈80 % użycia kategorii; ostrzeżenie o E < 5
daje najwięcej wartości przy najmniejszym nakładzie), potem `zgodnosc`, `zalezne`.
Ukrywanie jmv w `JUPWR_HIDDEN_ANALYSES`: `jmv::contTables`, `jmv::contTablesPaired`,
`jmv::propTest2`, `jmv::propTestN` (UWAGA: klucze to `ns::name` z pola `name:`
w .a.yaml jmv, czyli camelCase). Grupa „Częstości" w `groupOrder`
(`client/main/ribbon/analysetab.ts`) w miejscu dzisiejszego `Frequencies: 70`.

### Zadanie 18: wykresy — tryb „szeroki" (kilka zmiennych) w scatr — CZĘŚCIOWO (2026-10-22: kategoria Profile)
Diagnoza (2026-09-04): z 26 wykresów w `plots/` tylko trzy przyjmują kilka zmiennych
naraz (`parcoord`, `radar`, `corrgram` — pole `vars`); reszta wymaga jednej zmiennej
plus grupującej, czyli formatu DŁUGIEGO. Danych z powtórzonymi pomiarami w formacie
SZEROKIM (kolumny `przed`, `po`, `po_pol_roku` — jak w zbiorze jDane `szkolenie`)
nie da się więc niczym narysować. `mode` (ModeSelector) mają dziś tylko `jmvbar`
(Kategorialne/Ciągła/Liczebności) i `jmvline` (Punkty indywidualne/Zagregowane),
ale żaden tryb nie dotyczy formatu danych.

Mechanizm do powielenia: opcja `mode` typu List w `.a.yaml` + `type: ModeSelector`
z `Content` na tryb w `.u.yaml` (wzór: `plots/jamovi/jmvbar.u.yaml`).

Priorytet 1 (tam, gdzie brak najbardziej boli):
- `jmvbar` — tryb „Kilka zmiennych": każda kolumna to jeden pomiar, słupki
  liczności albo udziału wybranej kategorii,
- `jmvbox`, `violin`, `raincloud` — rozkłady kilku pomiarów obok siebie
  (przed-po na zmiennych ciągłych),
- `jmvline` — pomiary na osi X, profil średnich.

Priorytet 2: `stripmean`, `ridge`, `dens` — te same argumenty, rzadsze użycie.

PUŁAPKA: dla zmiennych BINARNYCH tryb szeroki wymaga dodatkowego pytania, która
kategoria jest „zdarzeniem" (inaczej wykres nie wie, czy rysować udział „tak", czy
„nie"). Dla zmiennych ciągłych problem nie występuje. W jCzest `zalezne`
ROZWIĄZANE 2026-10-18 (jCzest 0.2.4): opcja Level `event` bez `variable`,
zmienną selektora ustawia `jamovi/js/zalezne.js` na pierwszy pomiar,
domyślnie druga kategoria (`defaultLevelIndex: 1`, w R `.eventLevel`) —
wzór do powtórzenia w trybie szerokim scatr.

Bez sensu: `scat`, `hexbin`, `bubble`, `heatmap` (z natury x–y), `mosaic`,
`stackbar`, `waffle`, `treemap`, `pareto`, `wordcloud` (liczności kategorii),
`qq` (z definicji jedna zmienna). Już działają w wide i warto to tylko
udokumentować: `parcoord` (najlepszy wykres przed-po: każda linia to jednostka),
`radar`, `corrgram`.

UWAGA: `plots/` to submoduł (fork scatr) — zmiany idą osobnym commitem w submodule
i bumpem wskaźnika w superprojekcie.
STAN 2026-10-22: format szeroki obsługują nowe wykresy kategorii Profile —
`meanprofile` (średnie kilku kolumn na osi X z SE/CI; pokrywa punkt
`jmvline` — profil średnich), `dumbbell` i `slope` (dwa pomiary przed–po).
`parcoord` jest UKRYTY w menu (nieczytelny), więc jego rola „najlepszego
wykresu przed–po” przeszła na `slope`.
ETAP 1–2 ZROBIONE 2026-10-22 (scatr 2.9.9): `violin`, `jmvbox`, `raincloud`,
`stripmean` mają opcję `mode` (long = jedna zmienna, domyślnie / wide =
kilka zmiennych) i pole `vars`; `violin` i `jmvbox` także `wideGroup`
(kolor w obrębie kolumny). Wspólne `wideToLong()` i `insertWideCountsNotice()`
w `plots/R/utils.R`: braki usuwane osobno w każdej kolumnie (decyzja),
„Wyklucz przypadki” (`naOmit`) = tylko wiersze kompletne we wszystkich
kolumnach; nota z liczbą obserwacji, gdy kolumny różnią się licznością.
W `violin`/`jmvbox` `.groupVars()` podstawia kolumny pod group1, a `wideGroup`
pod group2. Składnia `jmvbox`: `tidyr::pivot_longer` w `generateDataPrepCode`.
Bez linii łączących tę samą osobę (decyzja; tę rolę pełni `slope`). PUŁAPKA:
wrappery scatr rozwiązują argumenty przez NSE — `vars = cols` szuka kolumny
„cols”; w testach podawać wektor wprost. Testy: `tests/testthat/testwide.R`.
Zostaje: etap 3 — `jmvbar` dla kilku kolumn jakościowych (wybór kategorii
„zdarzenie”, wzór jCzest `zalezne`), etap 4 — `ridge`, `dens`.

### Zadanie 19: „Opis zastosowanych metod" — dynamiczne podsumowanie per analiza (ZREALIZOWANE: jCzest 0.2.0, jTestyT 0.3.0, jANOVA 0.4.0)
Diagnoza (2026-09-04): noty pod tabelami i teksty UI w przebudowanych modułach
(jANOVA, jTestyT, jCzest) rozrosły się. Zamiast tego: na końcu wyników KAŻDEJ
analizy element `metody` (`type: Html`, `visible: (metody)`) z checkboxem
`metody` („Opis zastosowanych metod", default false — w eksploracji ogląda się
więcej analiz, niż się używa; włącza się w tych, które idą do raportu).
Treść: CO policzono, z JAKIMI parametrami, JAK przedstawiono (kierunek OR,
poziom „wystąpiło", mianownik procentów, orientacja tabeli, progi Cohena,
warunek stosowalności i czy spełniony) — NIGDY wyniki liczbowe. Sekcje w stałej
kolejności: Dane, Model, Testy, Wielkość efektu, Post-hoc, Wykres (nieznane
przed Wykres). Noty pod tabelami zostają tylko tam, gdzie bez nich tabeli nie da
się odczytać (błąd, brak wiersza, N); orientację tabel rozwiązywać strukturalnie
(title/superTitle), nie notą.
Mechanizm (decyzja 2026-09-04: w forku jmvcore, jak motywy w `themes.R`):
`jmvcore/R/metody.R` — `jmvcore::metodyNew()` (`add(sekcja, fmt, ...)` jak
sprintf z htmlEscape argumentów tekstowych, `addIf`, `render(element)`) i
`jmvcore::metodyCyt()`; eksporty dopisane ręcznie do NAMESPACE (roxygen
6.1.1 nie jest przeganiany); testy `jmvcore/tests/testthat/test-metody.R`.
Odrzucone: globalny przełącznik w kliencie (bibliografia jamovi jest JEDNYM
blokiem na końcu dokumentu — `client/main/references.ts`) i osobny pakiet
w modules/base (nowy krok w trzech pipeline'ach builda dla jednego pliku).
Wzorce z wdrożeń: wspólne fragmenty opisu per moduł w `R/utils.R`
(`metodyWspolne` w jTestyT, `metodyAnovaWspolne` w jANOVA); opcje, których
analiza nie ma, przekazywać argumentem, bo `o$brak` rzuca błąd; w .b.R
akumulator nazywać `md`, gdy `m` jest zajęte przez macierze/średnie;
UI: osobna sekcja „Opis metod" w rdzeniu panelu. Noty przycięte do jednego
zdania (zadanie 16 zrobione dla jCzest, jTestyT, jANOVA; zostaje jRol).
jANOVA testować w obrazie Dockera (`docker run --entrypoint
/opt/R/4.6.0/bin/Rscript -v $PWD/jANOVA:/mod:ro jupwr/jupwr:<ver> …`),
bo lokalnie brak afex.
Etykiet opcji z `.a.yaml` nie da się odczytać w R (kompilator wycina `title`),
więc nazwy metod trzeba pisać w `.b.R`. Kolejne moduły po akceptacji pilotażu:
jTestyT, jANOVA, jRol, jCI.

### Zadanie 20: bibliografia — cytowania pakietów R używanych przez analizy (PO 1.0.0) — ZROBIONE 2026-10-22 (pakiety bez literatury metod + zbiory danych)
Dziś bibliografia ma trzy stałe pozycje (jUPWR, jamovi, R — `client/main/
references.ts`). Dodać cytowania pakietów, na których faktycznie opierają się
analizy jUPWR: mechanizm upstreamu to `refs:` przy elemencie wyniku w `.r.yaml`
wskazujące klucz z `jamovi/00refs.yaml` modułu (wzór: `jmv/jamovi/00refs.yaml`
i `refs: car` w `jmv/jamovi/ttestis.r.yaml`); jmvcore rozwija klucze przy
serializacji (`analysis.R`), klient numeruje i deduplikuje. Kandydaci: car
(Levene, Durbin-Watson, Anova II — jTestyT, jANOVA, jRegr), emmeans i afex
(jANOVA), mvtnorm (jRol, Dunnett), boot (jCI), ggplot2 (wykresy wszędzie),
nortest jako źródło przybliżeń p (jEksplor, bez zależności), ARTool/Wobbrock
2011 i Johansen 1980 jako literatura metod (jANOVA), Newcombe 1998 i
Bonett-Wright 2000 (jCI). Cytować tylko wtedy, gdy dana tabela/wykres
naprawdę z pakietu korzysta (refs na elemencie, nie na całej analizie).
STAN 2026-10-22 (decyzje: pakiety bez literatury metod — wariant A3;
cytowanie warunkowe; polski APA; zbiory danych — wariant B2):
- Wspólne wpisy pakietów w `packaging/refs.yaml` (źródło prawdy: pakiet jako
  oprogramowanie, autorzy z rolą „aut” z DESCRIPTION, rok wydania CRAN,
  wersja z migawki CRAN 2026-05-11); każdy moduł ma KOPIĘ potrzebnych kluczy
  w `jamovi/00refs.yaml` (11 modułów + scatr) — przy bumpie pakietu zmieniać
  w obu miejscach. jmvcore pomija bez komunikatu klucz nieobecny w 00refs.
- Warunkowość: refs zbiera się tylko z WIDOCZNYCH elementów, więc statyczne
  `refs:` na elemencie, którego widoczność = użycie pakietu; `setRefs()`
  w `.b.R` tam, gdzie użycie zależy od opcji w zawsze widocznej tabeli
  (typ SS w jANOVA, metoda przedziału w jCI, metoda klasyfikacji w jSpace,
  separacja w jRegr). ggplot2 na każdym Image. Pakietów pomocniczych
  (jmvcore, R6, dplyr, rlang, scales) nie cytujemy.
- Zbiory z biblioteki: pole `documentation.citations` (format jak 00refs)
  obok tekstowego `references`; klient (`client/main/datasetdocs.ts`,
  wspólne z oknem „O zbiorze”) rozpoznaje zbiór po ścieżce
  `{{Examples}}/<moduł>/<plik>` i dopisuje pozycje po jUPWR/jamovi/R.
  Eksport/schowek (`resultspanel._hydrateAll`) bierze tę samą listę,
  duplikaty usuwa po treści, nie po kluczu.
- Ograniczenia: zbiór zapisany jako własny .omv traci cytowanie (poza jDane,
  rozpoznawanym po tytule); pozycje jmv (angielskie) i nasze (polskie) tego
  samego pakietu pojawiają się dwa razy; literatura metod (Wobbrock,
  Johansen, Newcombe, Bonett-Wright) — nie dodana.


### Zadanie 21: nowe elementy wyników jamovi 28.4+ (Text, Svg, vector, File) — PLAN, DECYZJE OTWARTE
Stan 2026-10-12: tylko plan, nic nie wdrożone; decyzje niepodjęte (wszystkie opcje
poniżej). Dotyczy wyłącznie linii latest — gałąź `main` (jUPWR 1.1.x, baza jamovi
28.11). Moduły używające tych elementów wymagają jamovi ≥ 28.4 — `.jmo` z nimi nie
zainstaluje się w 1.0.x (odnotować w macierzy `packaging/MODULES.md`).
Rekomendacja ogólna (do decyzji): 1.1.0 wydać jako SAM merge 28.11 (eksport
.docx/.odt/LaTeX już działa), nowe elementy dopiero w 1.1.x/1.2.0 — wtedy regresja
jest jednoznacznie przypisana do merge'a albo do nowej funkcji.

Elementy (źródła: `jmvcore/R/text.R`, `svg.R`, `image.R`, `analysis.R`;
`jamovi-compiler/schemas/*.yaml`; `client/resultsview/text.ts`, `svg.ts`):
- `Text` (`.r.yaml`): markdown renderowany przez klienta, `setContent()` przyjmuje
  tekst lub dowolny obiekt (capture.output); linki otwierane zewnętrznie; dobra
  postać tekstowa w eksporcie/kopiowaniu.
- `Svg` (`.r.yaml`): znaczniki SVG + skrypty/style modułu wykonywane w panelu
  wyników; klient zapisuje narysowany SVG do .omv (path), silnik R nie umie go
  zapisać (`saveAs` odrzuca).
- `Image` z `mode: vector`: ggplot renderowany przez `grDevices::svg` (cairo)
  zamiast PNG przez ragg; bez zmian w kodzie wykresu.
- `File` (`.a.yaml`, `extensions`, `multiple`, `hidden`): plik wybrany w panelu,
  zapisywany WEWNĄTRZ .omv; w R domyślnie NULL (gałąź w `header.template`).

Skala (moduły wbudowane): 50 wykresów Image (jANOVA 7, jTestyT 6, jCI 15, jCzest 3,
jEksplor 9, jRegr 7, jperm 3) + scatr 26; `metody` (Html) w 27 analizach
wbudowanych + 4 w jPomiar.

Etap 1 — wykresy wektorowe.
1a. PUŁAPKA fontów: cairo szuka fontów przez fontconfig, a motywy jUPWR rejestrują
    fonty tylko przez `systemfonts::add_fonts()` (`jmvcore/R/themes.R`, fonty w
    `/usr/lib/jamovi/fonts/`) — widzi je ragg, nie cairo. Opcje:
    A) instalacja fontów jUPWR systemowo (fontconfig) w Dockerze, macOS, Windows —
       cairo zamienia glify na krzywe, SVG wygląda identycznie wszędzie (także
       w Wordzie bez naszych fontów); zostajemy na ścieżce kodu upstreamu; wada:
       trzy pipeline'y, niezweryfikowane jak cairo w R na Windows szuka fontów.
       (rekomendowane; najpierw kilkunastominutowy test w Dockerze)
    B) w forku jmvcore renderować SVG przez `svglite` (widzi systemfonts) — jedna
       zmiana w `analysis.R`; wady: tekst w SVG jako tekst z nazwą fontu (inna
       czcionka bez fontu u odbiorcy), nowy pakiet, rozjazd z upstreamem przy
       każdym merge'u.
    C) akceptować czcionkę zastępczą — zero pracy, wykresy różnią się od PNG
       i motywów wykładowych.
    TEST 2026-10-18 (Docker, obraz 1.1.1): bez zmian `fc-match "IBM Plex Sans"`
    → DejaVu Sans, `svg()` rysuje DejaVu. Po skopiowaniu
    `/usr/lib/jamovi/fonts/*.ttf` do `/usr/local/share/fonts/jupwr/` + `fc-cache -f`
    cairo rysuje IBM Plex Sans/Mono z polskimi znakami, glify jako krzywe
    (0 elementów `<text>` w SVG). Wariant A w Dockerze WDROŻONY
    (`docker/jamovi-Dockerfile`, RUN po `COPY … /usr/lib/jamovi/fonts/`).
    macOS (2026-10-18): w jUPWR.app `svg()` NIE DZIAŁA — „failed to load cairo
    DLL”: `grDevices/libs/cairo.so` z R CRAN linkuje `/opt/X11/lib/lib{Xrender,
    SM,ICE,X11,Xext}` (XQuartz), a `55-relocate.sh` sprawdza tylko ścieżki
    Homebrew/R.framework. Upstream jamovi.app pakuje libX11, libXrender, libSM,
    libICE, libXext, libxcb, libXau do `R.framework/.../Resources/lib`
    i przepina ścieżki. Test z tymi bibliotekami (DYLD_FALLBACK_LIBRARY_PATH)
    + `FONTCONFIG_FILE` z `<include>` configu R i `<dir>` z fontami jUPWR:
    cairo rysuje IBM Plex Sans/Mono jak w Dockerze. WDROŻONE 2026-10-18:
    (1) `00-prereqs.sh` instaluje libx11/libxrender/libxext/libsm/libice
    z Homebrew, `55-relocate.sh` wbudowuje je (z libxcb/libXau/libXdmcp) TYLKO
    dla `grDevices/libs/cairo.so` i kontroluje, że cairo.so nie wskazuje
    `/opt/X11` (R_X11.so, R_de.so, tcltk, rgl zostają z XQuartz — jamovi ich
    nie ładuje); (2) jmvcore 28.3.2 `jupwrFontconfig()` (themes.R): na macOS,
    gdy brak `FONTCONFIG_FILE`, zapisuje w tempdir config z `<include>` configu R
    i `<dir>` fontów jUPWR; wołane z `jupwrRegisterFonts()` i w gałęzi vector
    w `analysis.R` przed `svg()` (cairo czyta fontconfig leniwie, przy pierwszym
    tekście). Sprawdzone w zbudowanej jUPWR.app bez XQuartz: `svg()` + motyw
    jUPWR = IBM Plex. Nie sprawdzone: Image mode: vector w silniku (brak
    jeszcze takiego wykresu — pilot jEksplor) i Windows (do testu natywnie).
1b. Pilot na jEksplor (9 wykresów): wygląd w aplikacji, eksport .docx/.odt,
    rozmiar .omv, wydajność przy N ~ 10 000 (ECDF, punkty na boxplocie).
1c. Zakres:
    A) wszystkie 50 + scatr z wyjątkami (hexbin, scat, bubble przy dużym N
       rastrowe) — spójność, ryzyko ciężkich SVG.
    B) tylko wykresy z małą liczbą elementów (średnie z CI, słupki, krzywe,
       diagnostyczne) — bezpieczne, ocena per wykres. (rekomendowane na start,
       potem rozszerzenie do A po pilocie)
    C) globalny przełącznik „Wykresy wektorowe" w ustawieniach — wybór
       użytkownika; upstream ma `mode` statycznie w `.r.yaml`, więc zmiany
       w kliencie, serwerze i jmvcore. (nierekomendowane)

Etap 2 — „Opis zastosowanych metod" jako Text:
    A) zostawić Html — eksport .docx wygląda dobrze (sprawdzone 2026-10-12),
       zero ryzyka dla zapisanych .omv. (rekomendowane)
    B) `metodyNew()` generuje markdown, typ `Text` w 31 `.r.yaml` — prostszy kod
       (bez ręcznego HTML/escapowania), kopiowanie jako markdown; wady: bump
       wszystkich modułów, sprawdzić otwieranie starych .omv z elementem Html;
       pilot na jednej analizie (np. jTestyT `ttestone`) + test starego .omv.

Etap 3 — opcja File:
    A) nie teraz, czekać na potrzebę z zajęć. (rekomendowane)
    B) pilot jSpace: własny plik TLE / GeoJSON granic / raster.
    C) pilot jPomiar: plik z danymi wzorcowania.
    Przy pilocie sprawdzić: limit rozmiaru (upstream ma limit wielkości żądania),
    zapis .omv z dużym plikiem, jak R dostaje ścieżkę.

Etap 4 — interaktywne Svg:
    A) odłożyć po 1.1.0, najwyżej jeden pilot. (rekomendowane)
    B) pilot `jCI::cibootstrap` (animacja kolejnych prób bootstrapowych).
    C) pilot jdistrACTION (zaznaczanie obszaru pod gęstością).
    D) zrezygnować.
    Koszt: stały kod JavaScript w module, tłumaczenia, testy w przeglądarce,
    zapis/odtworzenie z .omv, eksport .docx.

Kolejność (proponowana): 1.1.0 = sam merge → test fontów w Dockerze → pilot
vector na jEksplor → rozszerzenie → ewentualnie File i Svg. Każdy etap osobnym
commitem z bumpem zmienionych modułów. Testy per etap: analizy w obrazie Dockera
porównane z poprzednią wersją (klasy `*Options`/`*Class`, w obrazie brak
wrapperów; urządzenie `png(type = "cairo")`, bo domyślny pdf daje „invalid font
type"), eksport .docx/.odt ręcznie, otwarcie starego .omv, `release-check.sh`.

### Zadanie 22: audyt 2026-10-14 (Codex) — pozostałe punkty i zgłoszenia do upstreamu
Naprawione w ramach 1.1.1 i 1.0.8 (bez bumpu — nieopublikowane; na
`release/1.0` cherry-pick bez importu .omv, który dotyczy tylko 28.11):
import z .omv/.omt (`formatio/__init__.py` `read_dataset`,
`omv.read(..., dataset=)`), cudzysłowy w builderze reguł (`displayValue`
z nazwami kolumn), krotki kategorii w `_rule_levels`, ostrzeżenie dla starych
przekształceń (`setUnrepresentable`, `lastRepresentable`). Testy:
`server/jamovi/tests/test_jupwr_fixes.py`, `client/main/vareditor/tests/
conditionsbuilder.test.ts`.
Pozostaje:
- ~~(P2) `recodedvarwidget._applyFormula` i zmiana poziomu pomiaru nadpisują
  przekształcenie współdzielone przez inne kolumny~~ — ZROBIONE 2026-10-18
  (`isTransformShared`, kopia jako własne przekształcenie zmiennej; tylko latest).
- ~~(P2) builder zapisuje wyrażenie (`2 + 3`) w „użyj” i w wartości warunku
  jako tekst~~ — ZROBIONE 2026-10-18 (`roundTrips` w `formulaToChain`; tylko latest).
- ~~(marginalne) `transform.is_rules` szuka `$source` także w tekstach
  w cudzysłowie~~ — ZROBIONE 2026-10-18 (`_QUOTED` w `transform.py`).
- IF z kategorią kolumny nominalnej z kodami i liczbą całkowitą daje typ
  całkowity i kody zamiast etykiet (wnioskowanie typu IF, nie `_rule_levels`).
  Ocena 2026-10-18: zmiana wnioskowania typu w `compute/nodes.py` dotyka
  wszystkich formuł — nie jako drobna poprawka, osobno z testami.
- Zgłosić do jamovi (upstream, PÓŹNIEJ): `read_dataset` bez .omv/.omt
  i `import_from` zerujący wiersze przed odczytem źródła (błąd czytania
  dowolnego pliku nadal czyści dane); `Analysis.set_svg` używa
  `self.dataset.instance_path` (jest w `Project`; padają upstreamowe testy
  SVG w `test_analyses.py`); `client/main/utils/jsonlines.ts` nie buforuje
  niedokończonej linii między fragmentami i dekoduje UTF-8 bez `stream: true`.
  Padające testy klienta upstreamu (28.13, także u nich; nie poprawiamy
  lokalnie): `resultsview/test/svg.test.ts` — cykl importów `element.ts` →
  `create.ts` (`AnalysisStatus` używany w runtime) → `table.ts` → `element.ts`;
  `common/tests/formatio.test.ts` (eksport HTML z SVG dubluje `<rect>`);
  `common/tests/htmlelementcreator.test.ts` (marked 18 sam dekoduje encje
  liczbowe `&#8212;` przed ponownym kodowaniem w `richMarkdown`).
- Testy składni scatr (`expect_plot_equivalent`, 14 testów: jmvbar, jmvhist
  i in.) padają na rozjeździe kolorów (`colour`) między kodem składni
  (`getSyntaxThemeColors`) a wykresem renderowanym z motywem jUPWR — stan
  sprzed 2026-10-22, nie regresja; do zdiagnozowania osobno.


### Zadanie 23: jCzest — wykresy bez motywu i palety aplikacji — ZROBIONE 2026-10-18 (jCzest 0.2.4)
Zgłoszone 2026-10-18 (test 28.13): wykresy w analizach jCzest (tabela kontyngencji,
także zgodność i zależne) mają domyślne kolory ggplot zamiast palety ustawionej
w aplikacji. Przyczyna: `.plot = function(image, ...)` w `jCzest/R/tabela.b.R`,
`zgodnosc.b.R`, `zalezne.b.R` ignoruje przekazywane `ggtheme` i `theme`, kończy
na `ggplot2::theme_minimal()` i nie dodaje skali palety (`zalezne` ma na sztywno
`fill = "grey60"`). Naprawa wg wzoru jEksplor (`R/utils.R`, `jakosciowe.b.R`):
sygnatura `function(image, ggtheme, theme, ...)`, `+ ggtheme` zamiast
`theme_minimal()`, kolory z `theme$fill`/`theme$color`, a dla zmiennej grupującej
`fill` skala z palety aplikacji (jak w pozostałych modułach: `theme$palette` /
`jmvcore::colorPalette`). Bump jCzest (patch), sprawdzić motyw jasny/ciemny
i palety UPWr/CVD.
Przegląd wszystkich modułów (2026-10-18) przy okazji: jperm 0.2.5 (obszar
odrzucenia w drugim kolorze palety), jCI 0.3.5 (kafelki: `scale_fill_manual`
stało PRZED `ggtheme` i było nadpisywane). jWody ma ten sam błąd (gradient przed
`ggtheme` → „Continuous value supplied to a discrete scale”, mapy się nie rysują),
ale jest modułem wewnętrznym — świadomie NIE naprawiany. PUŁAPKA: `ggtheme` niesie dyskretne skale `fill`/`colour` palety —
każdą własną skalę kolorów (manual, gradient) dodawać PO `+ ggtheme`.
Celowo bez zmian: szare linie odniesienia, `theme_void()` w diagramach jRISK;
kolory na sztywno w jPomiar i jWody (moduły wewnętrzne).

### Zadanie 24: audyt zmiennej przekształconej (2026-10-18) — 1.1.2: 1–3, 5, 6, 8, 9, 11, 12 ZROBIONE
Trzy przeglądy (builder `conditionsbuilder.ts`, widget `recodedvarwidget.ts`,
serwer `transform.py`/`compute/nodes.py`/`.omv`), kluczowe potwierdzone skryptami
w obrazie Dockera. Ciche błędne wyniki / utrata reguł:
1. ~~przecinek dziesiętny („125,5" zapisywane jako tekst)~~ — ZROBIONE 2026-10-18:
   ŚCIŚLE wg `decSymbol` z ustawień (decyzja dydaktyczna: zapis wynika
   z ustawień — inaczej niż arkusz, który przyjmuje oba): `formatValue`
   zamienia przecinek tylko przy ustawionym przecinku, `wrongDecimal`
   wykrywa ułamek z drugim symbolem → pole `.invalid` i komunikat słowny
   w `$errorMessage` widgetu (`validationMessage`, `onValidity`),
   `_commit` nic nie zapisuje (nie cichy tekst). `VariableModel.settings`
   ustawiane w `VariableEditor.init`, cache `setFormula` z symbolem.
   Spójność w całej aplikacji (arkusz, schowek) świadomie NIE — kod upstreamu.
~~2. `== NA`~~, ~~3. `"M" or "K"`~~, ~~5. kilka zmiennych~~, ~~6. filtry~~,
~~8. poziomy porządkowe~~, ~~9. cudzysłów/ukośnik~~ — ZROBIONE 2026-10-18:
2 — `parseClause` pokazuje „NA”; 3 — ścisły `isQuoted` (bez cudzysłowu
w środku) + `badQuotes` → „Nieprawidłowy cudzysłów” (jak symbol dziesiętny:
`chainProblem`/`valueProblem`, `_commit` nic nie zapisuje), a `M or K` bez
cudzysłowów (słowo or/and w wartości) → „Użyj osobnego warunku z or / and”; 5 — `_isMixed`
w widgecie (kilka id, `transform` null): builder zablokowany z komunikatem;
6 — `_rule_levels(row_count, filt, …)` z tym samym filtrem co komórki;
8 — `_rule_constants` zbiera wartości łańcucha IF w kolejności reguł, także
liczby; 9 — `Parser.escape` (compute/parser.py) bierze tekst w cudzysłowie
DOSŁOWNIE (escapuje `\` i `"` przed `ast.parse`; wcześniej `\b` = escape
Pythona, a `'a"b'` łamało składnię), klient cytuje tekst z `"` apostrofami,
`_QUOTED` w transform.py bez escape'ów. Pozostają (świadomie po 1.1.2):
4. tekst równy nazwie kolumny zapisuje się jako odwołanie do kolumny; model
   `Clause` nie pamięta, czy wartość jest tekstem czy kolumną (średnie).
7. zmiana nazwy kolumny użytej w regule → „kolumna nie istnieje" (stare
   przekształcenie z `$source` to przeżywało; nic nie przepisuje nazw w formułach).
Widoczne na co dzień: 10. automatyczne nazwy „wiek - T1"/„? - T1"
(`datasetcontroller._calc_column_name` + `_ensureParent`); 11. wartość „else"
z nawiasem albo samo „else" bez reguł → nie otwiera się jako reguły
(ZROBIONE 2026-10-18: bez testu nawiasów, `roundTrips` wystarcza);
12. kody z zerem na początku („01") i `1e5` → błąd składni (ZROBIONE
2026-10-18: `isNumeric` = to, co serwer czyta jako liczbę; „01”, „1e5”,
„1.” → tekst); 13. wydajność kwadratowa (20 reguł × 10 tys.
wierszy = 3,3 s na edycję komórki).
Drobniejsze: podwójne tworzenie przekształcenia przy szybkiej edycji
i podwójne przerysowanie buildera; osierocone przekształcenia w pliku;
Ctrl+Z pomija edycje reguł (semantyka upstreamu); stary plik bez kolumny
źródłowej po zmianie typu liczy się z przypadkowej kolumny (`_ensureParent`
tylko dla reguł); kolumny `NA`/`and`/`or`; możliwa pętla parentów;
`_updateChannelList`/`_updateTransformColour` bez osłon; `_QUOTED` a backtick
z apostrofem; `lastFormula` przenosi niedokończone wiersze między zmiennymi.
Sprawdzone i poprawne: zapis/odczyt .omv, odwołania cykliczne, zależności,
kolejność poziomów tekstowych, nawiasy i pierwszeństwo and/or, nazwy ze
spacjami/polskimi literami.


### Zadanie 25: jRISK — częstości naturalne w „Zdarzenia i warunkowanie" — DO ZROBIENIA
Zgłoszone 2026-10-21 (lista 2, zad. 2 z analizy ryzyka): tabela „Częstości
naturalne (na 1000 przypadków)" pokazuje 16 incydentów, a P(B) = 0,0165 daje 16,5.
Przyczyna (`jRISK/R/eventtables.b.R`, blok `showNaturalFreq`): każda komórka
i każda suma osobno przez `round()`. R zaokrągla połówki do parzystej
(4,5 → 4, 16,5 → 16, ale 895,5 → 896), a kolumny `type: number` dostają cyfry
znaczące („12.00", „88.0"), więc liczby zaokrąglone wyglądają na dokładne.
Sumy brzegowe zaokrąglane niezależnie od komórek mogą się rozjechać (12 + 4 ≠ 17).
Praktyka dziedzinowa (komunikowanie ryzyka, Gigerenzer): częstości naturalne
są liczbami CAŁKOWITYMI; podstawę (100/1000/10 000/…) dobiera się tak, by
wyszły dokładnie, a zaokrągla się tylko w ostateczności.
Naprawa (do decyzji szczegółów):
- automatyczny dobór najmniejszej podstawy 10^k (≥ 1000?), przy której
  wszystkie komórki są całkowite (w zadaniu N = 20 000 → na 10 000: 120, 45,
  165, 880, 8955, 9835), ewentualnie opcja wyboru podstawy; tytuł tabeli
  ustawiany z kodu,
- gdy całkowite się nie da (limit podstawy): zaokrąglać tylko komórki łączne
  (połówki w górę, nie `round()`), sumy jako sumy zaokrąglonych komórek,
  nota „wartości zaokrąglone"; wartości < 1 jako „< 1",
- kolumny `type: integer` (bez „12.00"),
- spójnie z drzewem częstości (`treePlot`), jeśli pokazuje skalę na 1000.
Bump jRISK 0.4.2 → 0.4.3, przebudowa `.jmo` (moduł opcjonalny), MODULES.md.
Cytowanie (PÓŹNIEJ, zweryfikować w bazie literatury przed wpisaniem): Gigerenzer
i Hoffrage (1995, Psychological Review — częstości naturalne), Hoffrage i in.
(2000, Science — komunikowanie informacji statystycznej), fact boxes Harding
Center — do opisu metod / `00refs.yaml` jRISK (por. zadanie 20).

### Zadanie 26: jRISK — RD i RR w „Zdarzenia i warunkowanie" — DO ZROBIENIA
Zgłoszone 2026-10-21 (lista 2, zad. 7c: buty antypoślizgowe a poślizgnięcia,
RD i RR „w butach względem bez butów"). `eventtables` liczy P(B | A), ale nie
P(B | Aᶜ), RD ani RR — student liczy je ręcznie albo w jCzest. W jCzest
(`twoByTwo`, `tabela.b.R`) kierunek zależy od KOLEJNOŚCI poziomów (zdarzenie =
pierwsza kolumna, po imporcie alfabetycznie „nie" przed „tak"): domyślnie
wychodzi RR = 0,80 dla BRAKU poślizgnięcia zamiast 0,31; RD = −0,19 zgadza się
tylko przypadkiem (RD zdarzenia przeciwnego przy zamienionych grupach). Wynik
wzorcowy dopiero po przestawieniu „tak" na pierwsze miejsce w obu zmiennych.
W jRISK kierunek jest jednoznaczny, bo A (`levelA`, ekspozycja) i B (`levelB`,
zdarzenie) to wybrane poziomy — przy tej opcji trzeba to wprost wykorzystać.
Do zrobienia (`jRISK/jamovi/eventtables.{a,u,r}.yaml`, `R/eventtables.b.R`):
- nowa opcja (np. `showRiskMeasures`, default false) i tabela „Porównanie
  ryzyk": P(B | A), P(B | Aᶜ), RD = P(B | A) − P(B | Aᶜ), RR = P(B | A) / P(B | Aᶜ)
  z kolumnami Wzór/Wartość jak w `probTable` (konwencja modułu); ewentualnie
  OR i NNT/NNH = 1/|RD| (do decyzji — zad. 7d pyta o „zapobiegnięte
  poślizgnięcia na osobę", czyli wprost RD);
- przypadki brzegowe: P(B | Aᶜ) = 0 → RR nieokreślone (nota), puste A lub Aᶜ;
- przedziały ufności raczej NIE (estymacja jest w jCzest/jCI; jRISK to
  rachunek prawdopodobieństwa) — do potwierdzenia;
- „Opis zastosowanych metod" jeśli jRISK go dostanie; w nocie/tytule
  jednoznacznie „A względem Aᶜ, zdarzenie B";
- nazwy w panelu (zgłoszone 2026-10-21): „Zdarzenie A"/„Zdarzenie B" →
  „Ekspozycja"/„Zdarzenie" (`eventtables.a.yaml` `title` varA/levelA/varB/levelB,
  `.u.yaml` `label` 12 i 19). PUŁAPKA: ta sama analiza służy metrykom detektora
  (A = stan rzeczywisty, B = alarm, nota w `detectorTable`) i rachunkowi zdarzeń
  (P(A ∩ B), P(A ∪ B)), gdzie „ekspozycja" nie pasuje — rozważyć etykiety
  z symbolem, np. „A — ekspozycja / warunek" i „B — zdarzenie", a symbole A, B
  zostawić we wzorach i nagłówkach tabel (`labA`, kolumna „B: …"); nota
  detektora do uzgodnienia z nowymi nazwami;
- warstwy („Podziel według", zad. 7b — rozbicie na miejsca pracy) poza
  zakresem, dalej filtr.
Bump jRISK patch (razem z zadaniem 25 → 0.4.3), `.jmo`, MODULES.md; na liście
zadań (`analiza-ryzyka/listy`, zad. 2.7 i `jrisk-pokrycie-zadan.md`) zmienić
wskazanie z jCzest na `eventtables`.
