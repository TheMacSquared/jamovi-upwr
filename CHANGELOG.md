# Wydania jUPWR

Numeracja jUPWR jest niezależna od wersji jamovi (plik `version`). Rejestr modułów i macierz
zgodności modułów opcjonalnych: [`packaging/MODULES.md`](packaging/MODULES.md).

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
