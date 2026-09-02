# Wydania jUPWR

Numeracja jUPWR jest niezależna od wersji jamovi (plik `version`). Rejestr modułów i macierz
zgodności modułów opcjonalnych: [`packaging/MODULES.md`](packaging/MODULES.md).

## 0.9.3 — 2026-09-02
- Nowa filozofia ANOVY: moduł jANOVA 0.2.0 (wbudowany) zastępuje w menu
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
  z jmv zostaje widoczna, bo jANOVA jej nie obsługuje.
- Nowy moduł jTestyT 0.1.0 (wbudowany) zastępuje w menu testy t z jmv (ukryte
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
- Moduły: jmv 2.8.4, scatr 2.9.0, jdistrACTION 1.3.2, jCI 0.1.0, jperm 0.1.0,
  jboot 0.3.0, jDane 0.5.0, jANOVA 0.2.0, jTestyT 0.1.0; opcjonalne: jRISK 0.3.2,
  jSpace 0.2.0, jRol 0.1.0.

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
