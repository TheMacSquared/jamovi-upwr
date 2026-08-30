# Wydania jUPWR

Numeracja jUPWR jest niezależna od wersji jamovi (plik `version`). Rejestr modułów i macierz
zgodności modułów opcjonalnych: [`packaging/MODULES.md`](packaging/MODULES.md).

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
