# Wydania jUPWR

Numeracja jUPWR jest niezależna od wersji jamovi (plik `version`). Rejestr modułów i macierz
zgodności modułów opcjonalnych: [`packaging/MODULES.md`](packaging/MODULES.md).

## 0.9.5.7 — 2026-09-04
- „Opis zastosowanych metod": każda analiza jUPWR ma na końcu wyników
  opcjonalny (domyślnie wyłączony) blok z tym, co policzono, z jakimi
  parametrami i jak przedstawiono (kierunek różnicy, poziom odniesienia,
  mianownik procentów, metoda przedziału, schemat losowania). Bez wyników
  liczbowych i bez dydaktyki — porządkuje raport. Mechanizm wspólny w forku
  jmvcore (`metodyNew`). Noty pod tabelami skrócone do jednego zdania.
- Nowy moduł jEksplor (menu Eksploracja) zamiast opisowych jmv: „Zmienne
  ilościowe" (rdzeń: N, braki, średnia, mediana, kwartyle, SD, min, max, V;
  zaawansowane: miary pozycyjne, skośność Pearsona i kwartylowa, percentyle,
  wartości skrajne, Gini z krzywą Lorenza, ECDF; założenia: Shapiro-Wilk,
  Lilliefors, Anderson-Darling, Q-Q), „Zmienne jakościowe" (tabele liczności,
  także w grupach, z podsumowaniem) i nowy „Szereg rozdzielczy" (klasy,
  częstości skumulowane, miary interpolowane z szeregu, ogiwa). Ukryte:
  `jmv::descriptives`, `jmv::qualitative`.
- Nowy moduł jRegr (menu Regresja) w zakresie kursu podstawowego: „Korelacja"
  (para = jeden wiersz z przedziałem i rozrzutem, kilka zmiennych = macierz
  bez pól redundantnych), „Regresja liniowa" (predyktory ilościowe
  i jakościowe z wyborem poziomu odniesienia, dopasowanie, współczynniki
  z przedziałami, β, ANOVA modelu, Shapiro reszt, Q-Q, reszty vs dopasowane,
  Durbin-Watson, VIF, Cook), „Regresja logistyczna" (wybór kategorii
  „zdarzenie", test LR, R² McFaddena i Nagelkerkego, OR z przedziałami,
  tabela klasyfikacji, czułość, swoistość, AUC, ROC). Ukryte:
  `jmv::simpleCorr`, `corrMatrix`, `linReg`, `logRegBin`; korelacja
  cząstkowa oraz logistyczne wielomianowa i porządkowa na końcu menu.
- jCI 0.3.0 wchłonął jboot (moduł jboot usunięty): w każdej analizie
  przedziałów lista „Metoda przedziału" — klasyczna albo bootstrap
  percentylowy / BCa (B, ziarno, histogram replikacji w Zaawansowane);
  nowe analizy „Regresja liniowa" (przedziały współczynników z pasmem)
  i „Jak działa bootstrap" (podgrupa Dydaktyka); średnia jednej próby także
  dla mediany i średniej uciętej; d Cohena z przedziałem niecentralnego t
  w różnicy średnich i parach; metoda t-Studenta obok Welcha. Naprawione:
  przedział Spearmana (Bonett-Wright), Newcombe (bez podwójnego z), wykres
  różnicy proporcji rysuje przedział.
- Podział obowiązków: jTestyT 0.4.0 tylko testuje (statystyka, p, różnica
  i d Cohena jako punkty, wykres pudełkowy); przedziały ufności i wykres
  estymacyjny są w jCI, opis metod odsyła. jTestyT i jANOVA 0.4.0 mają opis
  metod.
- jperm 0.2.0: konwencje jTestyT (wiele zmiennych, ta sama lista hipotez,
  wartość testowa), panel rdzeń/zaawansowane (liczba permutacji, ziarno,
  test dokładny), opis metod, podpisy w menu.
- jCzest 0.2.0: opis metod, sekcje paneli nazwane wg tabeli kontyngencji.
- jDane 0.7.1: usunięto trzy zbiory dydaktyczne o statystykach niezwiązanych
  z programem zajęć (postacie Star Wars, Datasaurus, kwartet Anscombe'a);
  opisy zbiorów wskazują nowe pozycje menu (jEksplor, jRegr, Testy t).
- Zgodność wstecz: pliki .omv z analizami jboot oraz z jperm/jCI/jTestyT
  sprzed tych wersji nie otwierają tych analiz (zmienione opcje).

## 0.9.5.6 — 2026-09-04
- Analizy jCzest trafiają do istniejącego menu „Częstości" zamiast tworzyć drugie
  o tej samej nazwie. Moduł używa teraz `menuGroup: Frequencies` — tak jak jANOVA
  używa `ANOVA`, a jTestyT `T-Tests` — więc dzieli menu i ikonę z `logLinear`,
  który jest w nim wyświetlany na końcu.
- jSpace 0.3.0: skrócone opisy zbiorów w Bibliotece (ze 161–206 do 114–120 znaków,
  czyli tyle co w jDane) — długie streszczenia rozlewały się na karcie zbioru.

## 0.9.5.5 — 2026-09-04
- Analizy częstości z jmv są ukryte w menu — zastępuje je jCzest. Ukryte:
  `contTables`, `contTablesPaired`, `propTest2`, `propTestN`; ich kod zostaje
  w dystrybucji, więc zapisane pliki `.omv` nadal się otwierają. `logLinear`
  pozostaje widoczny jako jedyne narzędzie do tabel wielowymiarowych
  (ten sam wyjątek co MANCOVA).
- Kolejność w menu „Częstości" ustawiona dydaktycznie: Jedna zmienna →
  Tabela kontyngencji → Próby zależne.
- Próby zależne: wielkość efektu (OR par niezgodnych z przedziałem) przeniesiona
  do głównej tabeli testu zamiast osobnej tabeli z jednym wierszem; przy trzech
  i więcej pomiarach OR jest podawany parami w porównaniach post-hoc, bo jest
  miarą pary, a nie zestawu pomiarów. Tabela par przeniesiona do rdzenia panelu
  i włączona domyślnie, a jej brzegi (liczność z udziałem, np. „42 (35.0%)")
  niosą rozkłady obu pomiarów — osobna tabela udziałów powtarzała te same liczby.
- Próby zależne: nowy wykres udziałów w kolejnych pomiarach. Słupki połączone
  linią, bo to te same jednostki mierzone kilka razy — to pierwszy wykres
  w dystrybucji dla danych z powtórzonymi pomiarami w formacie szerokim
  (zob. CLAUDE.md, zadanie 18).
- Moduły bez zmian: jmv 2.8.4, scatr 2.9.0, jdistrACTION 1.3.2, jCI 0.1.0,
  jperm 0.1.0, jboot 0.3.0, jDane 0.7.0, jANOVA 0.3.0, jTestyT 0.2.0,
  jCzest 0.1.0; opcjonalne: jRISK 0.3.3, jSpace 0.2.2, jRol 0.1.0.

## 0.9.5 — 2026-09-03
- Nowy moduł jCzest 0.1.0 (wbudowany, menu „Częstości"): analizy tabel liczności
  z panelem pod dydaktyczną prostotę. Diagnoza kategorii w jmv była odwrotna niż
  w ANOVIE — panel nie był przeładowany, tylko pusty: po wrzuceniu zmiennych
  widać było samą tabelę i χ², a wszystko potrzebne do interpretacji (procenty,
  liczebności oczekiwane, V Craméra) było domyślnie wyłączone.
  „Tabela kontyngencji": χ², V Craméra i procenty wierszami widoczne od razu,
  dokładny test Fishera obok χ², a w „Zaawansowane" iloraz wiarygodności,
  poprawka ciągłości, miary 2×2 (OR, RR, różnica proporcji), miary porządkowe,
  test trendu Cochrana-Armitage'a, reszty standaryzowane i porównania par
  wierszy z korektą Holma; wykres słupkowy i mozaikowy.
  „Jedna zmienna" scala propTestN i propTest2: 2 kategorie → dokładny test
  dwumianowy, 3+ → χ² zgodności; własne proporcje oczekiwane wpisuje się
  w liście z nazwami kategorii obok, z podglądem wynikowego udziału.
  „Próby zależne": 2 pomiary → McNemar, 3+ → Q Cochrana z porównaniami par
  (to daje Q Cochrana miejsce w dystrybucji).
  Czego nie ma w jmv, a jest tutaj: automatyczna kontrola warunku stosowalności
  (E ≥ 5 dla χ², liczba par niezgodnych dla McNemara) z ostrzeżeniem nad
  wynikami i wskazaniem testu dokładnego, wielkości efektu (V Craméra z CI
  bootstrapowym, w Cohena, OR par niezgodnych) oraz noty mówiące, który poziom
  jest odniesieniem — poziomy są sortowane alfabetycznie, więc bez tego kierunek
  ilorazu szans bywa odwrotny do intuicji.
  Analizy jmv pozostają widoczne obok: jCzest nie ma jeszcze warstw,
  Mantel-Haenszela, z dla różnicy proporcji ani kierunku hipotezy przy 2×2.
- jDane 0.7.0: siedem „lekkich" zbiorów z 0.6.0 (anscombe, datasaurus, starwars,
  movies, fastfood, autobusy, TeachingRatings) oraz nowy zbiór syntetyczny
  „szkolenie" — 120 gospodarstw i trzy powtórzone pomiary binarne do testów dla
  prób zależnych; dobrany tak, by porównania par dawały dwa wyniki istotne
  i jeden nieistotny.
- Moduły: jmv 2.8.4, scatr 2.9.0, jdistrACTION 1.3.2, jCI 0.1.0, jperm 0.1.0,
  jboot 0.3.0, jDane 0.7.0, jANOVA 0.3.0, jTestyT 0.2.0, jCzest 0.1.0;
  opcjonalne: jRISK 0.3.3, jSpace 0.2.2, jRol 0.1.0.

## 0.9.4 — 2026-09-02
- Nowa filozofia ANOVY: moduł jANOVA 0.3.0 (wbudowany) zastępuje w menu
  ANOVA analizy jmv (jednoczynnikowa, ANOVA, ANCOVA, powtórzonych pomiarów),
  które klient ukrywa listą `JUPWR_HIDDEN_ANALYSES`; ich kod zostaje, więc
  zapisane pliki nadal się otwierają, a merge upstreamu nie koliduje.
  „ANOVA": czynniki, czynniki blokujące (addytywne), kowarianty, SS typu
  I/II/III, Welch dla jednego czynnika, η²/η²p/ω², porównania wielokrotne
  na średnich brzegowych (Tukey, NIR, Scheffé, Dunnett, Holm, Bonferroni)
  z grupami jednorodnymi (litery), różnicą graniczną i d Cohena, kontrasty,
  Levene/Bartlett/Shapiro, Q-Q, reszty do arkusza, wykresy średnich z literami
  i interakcji. „ANOVA powtórzonych pomiarów" pracuje na danych w formacie
  długim (afex): jednostka, czynniki wewnątrz- i międzyobiektowe, kowarianty,
  test Mauchly'ego, poprawki GG/HF, η²G/η²p; obejmuje układy split-plot.
  Panel zaprojektowany pod dydaktyczną prostotę: na pierwszym ekranie tylko
  zmienne, „Nierówne wariancje (Welch)", „Nieparametrycznie (rangi)",
  wielkość efektu, metoda porównań, α, tabela par i wykresy; bloki,
  kowarianty, typ SS, kontrasty, komórki interakcji i reszta w zwiniętej
  sekcji „Zaawansowane". Przełącznik nieparametryczny sam dobiera metodę:
  Kruskal-Wallis z ε² i Dunnem (litery) przy jednym czynniku, ART (aligned
  rank transform, Wobbrock i in. 2011, własna implementacja zgodna z ARTool)
  z porównaniami efektów głównych przy kilku czynnikach, Friedman z W Kendalla
  i Nemenyim w powtórzonych pomiarach. Przełącznik Welcha daje test Welcha
  przy jednym czynniku i test Welcha-Jamesa (Johansen) przy kilku, zgodny
  z welchADF. Ukryte w menu także Kruskal-Wallis i Friedman z jmv; MANCOVA
  z jmv zostaje widoczna na końcu menu (mechanizm `JUPWR_MENU_LAST`
  w kliencie), bo jANOVA jej nie obsługuje.
- Nowy moduł jTestyT 0.2.0 (wbudowany) zastępuje w menu testy t z jmv (ukryte
  w kliencie): jedna próba, dwie grupy, próby sparowane z płaskim panelem jak
  w jmv — każdy test to osobny checkbox (t Studenta, t Welcha, Mann-Whitney /
  Wilcoxon), więc odznaczenie Studenta usuwa jego wiersz z tabeli;
  hipoteza alternatywna i wartość testowa w jednej sekcji, poziom ufności,
  opisowe, sprawdzenie założeń (Shapiro, Q-Q, Levene). Zawsze różnica
  i d Cohena z przedziałami ufności (niecentralny t). Wykresy: grup i różnicy
  (Gardner-Altman z osią różnicy po prawej), par i różnicy z liniami
  łączącymi pomiary, średniej z przedziałem i linią H₀; opcjonalnie pudełkowy
  z punktami. Bez czynnika Bayesa, permutacji i bootstrapu (osobne moduły).
- jRol 0.1.0 (doświadczalnictwo rolnicze) jako nowy moduł OPCJONALNY `.jmo`:
  układ całkowicie losowy, losowanych bloków, kwadrat łaciński i split-plot
  z tabelą ANOVA o właściwych błędach, porównania wielokrotne z literami
  i NIR, wykresy średnich z literami, generator planu doświadczenia z mapą
  pola; w bibliotece zbiory owies Yatesa (split-plot) i syntetyczny kwadrat
  łaciński z pszenicą z pełną dokumentacją. Budowa: macOS `72-jmo-jrol.sh`,
  Windows `build.ps1` krok 4g.
- jDane 0.5.0: panel dokumentacji „O zbiorze" dla wszystkich 12 zbiorów
  (streszczenie, szczegóły z podpowiedzią analizy, zmienne, źródło,
  bibliografia, pochodzenie, zmiany); krótsze opisy w bibliotece; przycisk
  „O zbiorze" z ikoną SVG w stylu paska stanu.
- CLAUDE.md: kolejka zadań 8–13 (Eksploracja, testy nieparametryczne,
  jSzereg, jMoc, jML) i zadanie 14 (jANOVA).
- Dokumentacja zbiorów („O zbiorze", jak w jDane) dla modułów opcjonalnych:
  jSpace 0.2.2 (4 zbiory: streszczenie, szczegóły z podpowiedzią analizy,
  zmienne, źródło, bibliografia, pochodzenie, zmiany) i jRISK 0.3.3 (3 zbiory
  Bananpol; do tego tagi i krótkie jednowierszowe opisy w Bibliotece).
- jSpace: krótkie jednowierszowe opisy zbiorów w Bibliotece (długie
  nachodziły na sąsiednie pozycje). Plik `.jmo` dla macOS ładuje się teraz w silniku: binaria CRAN
  asteRisk i classInt (a przez niego sf) odwoływały się do libR/libgfortran
  przez `@executable_path/../Frameworks/R.framework/...` i do libtbb przez
  LC_RPATH systemowego R — wrapper `R` maskował to zmienną
  DYLD_FALLBACK_LIBRARY_PATH, silnik nie. `jmo_relocate_macho` w lib.sh
  przepisuje odwołania na `@rpath` (jak 55-relocate.sh dla aplikacji)
  i pakuje libtbb obok asteRisk.so.
- Build macOS: 20-modules.sh nie usuwa już z payloadu jANOVA i jTestyT
  (lista `EXPECTED` nie znała nowych modułów wbudowanych).
- Moduły: jmv 2.8.4, scatr 2.9.0, jdistrACTION 1.3.2, jCI 0.1.0, jperm 0.1.0,
  jboot 0.3.0, jDane 0.5.0, jANOVA 0.3.0, jTestyT 0.2.0; opcjonalne: jRISK 0.3.3,
  jSpace 0.2.2, jRol 0.1.0.

## 0.9.2.2 — 2026-08-31
- jCI i jperm dostają polskie znaki w interfejsie. Oba moduły miały teksty
  zapisane bez znaków diakrytycznych („CI dla sredniej (jedna proba)",
  „Zmienna grupujaca", „Za malo obserwacji") — inaczej niż jboot czy
  jdistrACTION. Poprawione tytuły analiz, etykiety opcji i paneli, nagłówki
  tabel, opisy w dokumentacji R i komunikaty błędów (37 plików).
- Menu jRISK trafia na koniec wstążki „Analizy". Wcześniej wchodziło między
  Rozkłady a Przedziały ufności, bo `groupOrder` w kliencie miał wpis dla
  grupy „Ryzyko" z czasów, gdy jRISK był modułem wbudowanym. Po sideloadzie
  moduł zachowuje się teraz jak jSpace i inne moduły doinstalowane.
- Moduły bez zmian: jmv 2.8.4, scatr 2.9.0, jdistrACTION 1.3.2, jCI 0.1.0,
  jperm 0.1.0, jboot 0.3.0, jDane 0.4.0; opcjonalne: jRISK 0.3.2, jSpace 0.2.0.

## 0.9.2.1 — 2026-08-31
- Krótkie etykiety w menu wracają do jboot, jCI i jperm. Były wpisane tylko
  w `jamovi/0000.yaml`, który `jmc` regeneruje przy każdym buildzie z plików
  `.a.yaml` — brak `menuTitle` w źródle powodował, że do wydań trafiał pełny
  tytuł analizy („Bootstrapowy CI dla mediany" zamiast „CI dla mediany").
  Etykiety przeniesione do `.a.yaml` (18 analiz), więc przeżywają regenerację.
- Build natywny Windows przerywa się, gdy `jmc` zwróci błąd. Wcześniej błąd
  ginął w `| Out-Null`, a jedyny strażnik (`Test-Path`) przechodził na starym
  `.jmo` z poprzedniego wydania — build kończył się zielono z nieaktualnym
  modułem opcjonalnym w paczce.
- Build nie zostawia przepisanych plików źródłowych: `0000.yaml` modułów jest
  odtwarzany po kompilacji, a `<moduł>/temp/` trafił do `.gitignore`.
- macOS: odczyt wersji jUPWR obsługuje numery czteroczłonowe (wcześniej
  `0.9.2.1` skracało się do `0.9.2` w nazwach artefaktów).
- Moduły bez zmian: jmv 2.8.4, scatr 2.9.0, jdistrACTION 1.3.2, jCI 0.1.0,
  jperm 0.1.0, jboot 0.3.0, jDane 0.4.0; opcjonalne: jRISK 0.3.2, jSpace 0.2.0.

## 0.9.2 — 2026-08-30
- Zmienna przekształcona dostaje builder reguł „jeżeli … użyj …" — ten sam,
  który wcześniej był trybem zmiennej obliczonej. Warunki mogą odwoływać się
  do dowolnych zmiennych (nie tylko do źródłowej), łączyć się przez and/or
  i grupować nawiasami. Zmienna obliczona wraca do samej formuły, więc reguły
  mają jedno miejsce zamiast dwóch.
- W panelu przekształconej znika wybór zmiennej źródłowej (nie wpływał na
  wynik) i wybór transformacji z listy; zostaje typ pomiaru, bez opcji ID.
  Lista transformacji i jej edytor działają jak dotąd.
- Builder warunków: nawiasy do grupowania członów i zmiana kolejności wiązania
  (domyślnie „and" wiąże silniej niż „or"), plus testy jednostkowe serializacji
  i parsowania formuł. Rozróżnienie w nazwach: „reguła" to para jeżeli…użyj,
  „warunek" to jej człon.
- Panel informacji o zbiorze danych w jDane 0.4.0: opis, szczegóły i objaśnienia
  zmiennych czytane z metadanych modułu, dostępne po wczytaniu zbioru
  z biblioteki (na razie dla zbioru CASchools).
- jdistrACTION 1.3.2: zacieniowany obszar p pod krzywą gęstości między granicami
  kwantyla w siedmiu rozkładach ciągłych — kwantyl widać jako pole, nie tylko
  jako linie przerywane.
- macOS: relokowane biblioteki uruchomieniowe są ponownie podpisywane po zmianie
  ścieżek (`install_name_tool` unieważnia podpis), więc paczka nie wymaga
  obchodzenia Gatekeepera.
- Moduły: jmv 2.8.4, scatr 2.9.0, jdistrACTION 1.3.2, jCI 0.1.0, jperm 0.1.0,
  jboot 0.3.0, jDane 0.4.0; opcjonalne: jRISK 0.3.2, jSpace 0.2.0.

## 0.9.1 — 2026-08-30
- Utwardzony profil deweloperski Dockera: Vite działa bez trybu uprzywilejowanego,
  jako nieuprzywilejowany użytkownik, z systemem plików tylko do odczytu,
  bez capabilities i bez możliwości uzyskania dodatkowych uprawnień.
- Dodane testy jednostkowe modułów jCI, jboot i jperm oraz smoke test analiz
  z tych modułów w kompletnej aplikacji.

## 0.9.0 — 2026-08-30
- Merge upstreamu jamovi 28.2 (z 28.1): zaktualizowane tłumaczenia wbudowane,
  poprawki fokusa list rozwijanych, nowe ikony modułów.
- Electron podbity z 32.3.3 (bez wsparcia od marca 2025) do 43.4.1 (EOL 2027-01);
  build i testy regresji wykonane na Windows x64. Linia 43 wybrana świadomie:
  44+ zmienia API schowka na W3C (Promise), na czym opiera się kopiowanie tabel
  i wklejanie z Excela.
- scatr 2.9.0: merge upstreamu (składnia tidyverse, `linewidth`, poprawki osi).
- Wersje modułów w `.jmo` naprawione: `jamovi/0000.yaml` jest jedynym źródłem
  prawdy — sideload jRISK/jSpace pokazuje teraz wersję modułu, nie 28.x.
- Moduły: jmv 2.8.4, scatr 2.9.0, jdistrACTION 1.3.1, jCI 0.1.0, jperm 0.1.0,
  jboot 0.3.0, jDane 0.3.0; opcjonalne: jRISK 0.3.2, jSpace 0.2.0.

## 0.8.6 — 2026-08-26
- Nowy wykres mozaikowy w kategorii Zależności (scatr 2.8.5): dwie zmienne
  jakościowe, szerokości kolumn ∝ liczebnościom brzegowym X, podział kolumn
  wg P(Y|X), opcjonalne wagi (liczności) i etykiety procentowe w polach;
  geometria własna (geom_rect), pełne opcje tytułów/osi/legendy.

## 0.8.5 — 2026-08-26
- Zakładka „Wykresy" przeorganizowana w kategorie dydaktyczne wg zastosowań
  (wzór: r-graph-gallery.com): Rozkład, Porównania, Ranking, Zależności, Trendy,
  Części całości, Inne — stała kolejność menu, ikony kategorii; wykresy
  z doinstalowanych modułów trafiają automatycznie do „Inne".
- 17 nowych wykresów w module plots (scatr 2.8.4), wszystkie z pełnym zestawem
  opcji tytułów/osi/legendy i czystym ggplot2 (zero nowych zależności R):
  gęstości, Q-Q (facety), raincloud, skrzypcowy, punktowy grup (średnia±CI),
  lizakowy, radarowy, współrzędne równoległe, chmura słów, kolisty słupkowy,
  bąbelkowy, mapa ciepła, korelogram, warstwowy, skumulowany słupkowy
  (100%/liczności, etykiety % wewnątrz lub obok słupka), waflowy (procenty
  w legendzie/na wykresie, panele wg drugiej zmiennej), mapa drzewa
  (zagnieżdżanie wg drugiej zmiennej); własne implementacje squarified treemap,
  spirali chmury słów i geometrii radaru.
- Ridgeline: pełne opcje legendy (jak histogram) + czytelny pasek gradientu.
- Wykres słupkowy przeniesiony do kategorii Ranking; świadomie brak wykresu
  kołowego (decyzja dydaktyczna — zamiast niego skumulowany 100%, waffle, treemap).

## 0.8.0 — 2026-08-24
- Motywy wykresów przeprojektowane: „Default" realizuje konwencje APA 7 (czysta czerń,
  jeden rozmiar pisma, ticki na zewnątrz) i jest pozycją „APA" w menu; nowe motywy
  Grid, Presentation oraz jUPWR — jasny / ciemny (styl skryptu wykładowego);
  Black & white dostał własny wygląd (ramka, ticki, siatka). Usunięte z menu:
  I ♥ SPSS, Hadley (funkcje pozostają w jmvcore dla zgodności).
- Palety uporządkowane: tylko zestawy predefiniowane — jmv, Okabe–Ito (bezpieczna
  dla daltonistów), jUPWR — jasny / ciemny, Skala szarości. Wybór motywu jUPWR/B&W
  ustawia pasującą paletę.
- Skale ciągłe (heatmapy, macierz korelacji) respektują wybraną paletę — nowe
  `jmvcore::gradientPalette()`, `theme$gradient` / `theme$divergent`; kafelki
  macierzy korelacji używają rampy rozbieżnej palety.
- Fonty dystrybucji (OFL 1.1): Source Serif 4, Atkinson Hyperlegible, JetBrains Mono
  w `platform/fonts/` + rejestracja przez systemfonts we wszystkich buildach.
- Moduły: jmv 2.8.4 (macierz korelacji), jmvcore 2.7.41; pozostałe bez zmian.
  Opcjonalnie jRISK 0.3.2 — bez zmian, `.jmo` z 0.7.8 pozostaje zgodny.

## 0.7.8 — 2026-08-23
- jRISK przestaje być preinstalowany — moduł opcjonalny dystrybuowany jako `.jmo`
  (skrypty `70-jmo-jrisk.sh` / `build.ps1` krok 4e).
- Build macOS: `20-modules.sh` usuwa z przyrostowego `stage/` moduły spoza listy wbudowanych.
- Moduły: jmv 2.8.3, scatr 2.8.3, distrACTION 1.3.1, jCI 0.1.0, jperm 0.1.0, jboot 0.2.2;
  opcjonalnie jRISK 0.3.2 (`.jmo` macOS ✅, Windows ⬜).

## 0.7.7 — 2026-08-22
- Zmienne wyliczane: ustrukturyzowany tryb „Warunki" (Conditions).

## 0.7.6 / 0.7.6.1 — 2026-08-22
- Wszystkie wykresy modułów używają motywu i palety aplikacji (`jmvcore::colorPalette`);
  zawijanie długich etykiet (`jmvcore::wrapLabels`).

## 0.7.5.1 / 0.7.5.2 — 2026-08-22
- GGally w `modules/base/R`; jmv 2.8.1 — przeprojektowany wykres macierzy korelacji.

## 0.7.5 — 2026-08-11
- jRISK 0.3: ważność Birnbauma w miejsce sprawdzenia koherentności, MTTF/mediana w trybie
  „Dane", auto-wybór poziomu zdarzenia, zbiór „Bananpol — linia dojrzewania".

## 0.7.0 – 0.7.2 — 2026-08-11
- jRISK 0.2: tryb „Dane" w modelach czasu życia (Kaplan–Meier, MLE z cenzorowaniem),
  zdarzenia i warunkowanie, schemat Bernoulliego, FTA; zbiory Bananpol (urządzenia, wypadki).
- Patch jmc: opcje `Level` dostają `= NULL` w sygnaturach wrapperów R.

## 0.6.0 — 2026-08-11
- Nowy moduł jRISK 0.1 (modele czasu życia, niezawodność systemów).
- distrACTION 1.3.0: gamma, Weibull, ujemny dwumianowy, konwencje zmiennej losowej.

## 0.5.5 — 2026-08-01
- Merge upstream jamovi 28.1; jmc: naprawa kolizji nazw opcji w generowanych wrapperach.

## 0.5.4 — 2026-08-01
- Merge upstream jamovi 2.7.38 (poprawka zamykania silnika).

## 0.5.3 — 2026-07-02
- Merge upstream jamovi 2.7.37.

## 0.5.0 — 2026-06-23
- Wprowadzenie wersjonowania jUPWR z jednym źródłem prawdy (`client/common/jupwr.ts`).
