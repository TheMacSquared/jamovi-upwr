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
- moduły preinstalowane: jmv, plots (scatr), jperm, jCI, jboot,
  jdistrACTION 1.3.x (11 rozkładów, w tym gamma, Weibulla i ujemny dwumianowy
  z wyborem konwencji zmiennej losowej); jRISK (ryzyko i niezawodność:
  modele czasu życia z cenzorowaniem, systemy, FTA, zdarzenia/warunkowanie,
  schemat Bernoulliego + zbiory danych Bananpol w bibliotece) jest modułem
  OPCJONALNYM: nie preinstalowany, budowany jako .jmo (macOS: 70-jmo-jrisk.sh,
  Windows: build.ps1 krok 4e), instalowany przez Moduły → Sideload
- jRol 0.1.0 (doświadczalnictwo rolnicze) jest modułem OPCJONALNYM jak jRISK
  (macOS: 72-jmo-jrol.sh, Windows: build.ps1 krok 4g)
- jSpace 0.2.0 (statystyka danych kosmicznych: orbity TLE/SGP4 przez asteRisk,
  mapy punktowe/kartogramy i statystyki regionalne przez sf, statystyki rastrów
  przez terra, klasyfikacja k-NN/drzewo; 4 zbiory danych w bibliotece) jest
  modułem OPCJONALNYM jak jRISK, ale budowanym BEZ --skip-deps (jmc bundluje
  sf/terra/asteRisk do .jmo; macOS: 71-jmo-jspace.sh, Windows: build.ps1
  krok 4f); szczegóły w packaging/MODULES.md
- zakładka „Wykresy" podzielona na kategorie dydaktyczne wg zastosowań
  (wzór: r-graph-gallery.com). Mechanizm: `menuGroup` w `plots/jamovi/*.a.yaml`
  → jedno menu na wstążce (client/main/ribbon/plotstab.ts, stała kolejność);
  tłumaczenia nazw grup w `plots/jamovi/i18n/pl.po`; ikony kategorii
  `client/assets/analysis-plots-<kategoria>.svg` + `plotstab.css`.
  Kategorie (klucz EN → nazwa PL) i wykresy (nazwy analiz w scatr):
  Distribution→Rozkład (jmvhist, dens, qq, raincloud, ridge),
  Comparison→Porównania (jmvbox, violin, stripmean), Ranking→Ranking
  (jmvbar, lollipop, radar, parcoord, wordcloud, circbar),
  Correlation→Zależności (scat, bubble, heatmap, corrgram, mosaic, hexbin),
  Evolution→Trendy (jmvline, area), Composition→Części całości
  (stackbar, waffle, treemap), Other→Inne (pareto; tu też lądują wykresy
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
- jANOVA 0.2.0 (wbudowany) zastępuje w menu ANOVA analizy jmv (jednoczynnikowa,
  ANOVA, ANCOVA, powtórzonych pomiarów) — lista ukrytych w client/common/jupwr.ts

Nowe zadania dopisuj poniżej jako "### Zadanie N: ..." z plikiem docelowym
i oczekiwanym zachowaniem.

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
mapa pola. Zależności: base R + mvtnorm + car (w modules/base/R). Zbiory w jDane
0.5.0: oats (Yates, split-plot), pszenica_latin (syntetyczny kwadrat łaciński).
Testy: `jRol/tests/testthat` (silnik vs aov/TukeyHSD/multcomp + integracyjne).

### Zadanie 9: Eksploracja — rozszerzenia statystyk opisowych (wariant B)
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

### Zadanie 14: jANOVA — ANOVA jUPWR zamiast ANOVY jmv (wariant D) — ZREALIZOWANE 0.2.0
Katalog `jANOVA/` (wbudowany), menuGroup ANOVA (pozycje lądują w menu ANOVA obok
testów nieparametrycznych jmv). Analizy: `anova` (zależna, czynniki, czynniki
blokujące addytywne, kowarianty; SS typu I/II/III przez car::Anova z contr.sum;
Welch dla jednego czynnika; η², η²p, ω²; post-hoc na średnich brzegowych emmeans:
Tukey/NIR/Scheffé/Dunnett vs pierwszy poziom/Holm/Bonferroni z literami CLD
i różnicą graniczną; kontrasty przez emmeans::contrast; Levene/Bartlett/Shapiro,
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
Usunięte świadomie: test mediany, Jonckheere, Page, Bonferroni, Conover, HC3.
UI warunkowe: `jamovi/js/anova.js`, `anovarm.js` (jus 3.0: handlery
`<kontrolka>_changed`, `view_updated`, `ui.x.setPropertyValue('enable', …)`).
PUŁAPKA: wrapper analizy `anova()` zasłania `stats::anova` — w kodzie modułu
zawsze `stats::anova(...)`. Ukrywanie analiz jmv: `JUPWR_HIDDEN_ANALYSES`
w `client/common/jupwr.ts` (jmv::anovaonew, anova, ancova, anovarm) filtrowane
w `client/main/ribbon/analysetab.ts` (także jmv::anovanp, anovarmnp); kod jmv
nietknięty, stare pliki .omv otwierają się. MANCOVA jmv pozostaje widoczna.

### Zadanie 15: jTestyT — testy t jUPWR zamiast testów t jmv — ZREALIZOWANE 0.1.0
Katalog `jTestyT/` (wbudowany, menuGroup T-Tests): `ttestone`, `ttesttwo`,
`ttestpaired` z PŁASKIM panelem w stylu jmv (decyzja: testy t są zbyt proste
na podział rdzeń/zaawansowane): „Testy" (Welch [dwie grupy], „Nieparametrycznie
(rangi)" = Mann-Whitney/Wilcoxon, permutacje, test znaków / K-S), „Hipoteza"
(lista), „Dodatkowe statystyki" (poziom ufności, opisowe, bootstrap), „Wykres",
„Sprawdzenie założeń" (Shapiro, Q-Q, Levene). Bez czynnika Bayesa (osobny moduł
w planach). Zawsze: różnica
z CI i d Cohena z CI (niecentralny t, `dInterval`). Wykresy: estymacyjny
Gardnera-Altmana (oś różnicy przy średniej drugiej grupy, jak w jCI), linie par,
punkty z H₀. Ukryte w kliencie: jmv::ttestis, ttestps, ttestones. Zadanie 10:
test znaków i K-S są tu, nie w jmv.
