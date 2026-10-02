# jRISK 0.4.0 — plan testów ręcznych (interfejs jamovi)

Obliczenia sprawdzają testy automatyczne (`tests/testthat`). Ten plan dotyczy tego, czego
one nie widzą: panelu, aktywności kontrolek, domyślnych poziomów, komunikatów, wykresów
i regresji.

- Środowisko: kontener `jamovi` z obrazu `jupwr/jupwr:1.0.5` + sideload jRISK 0.4.0,
  http://127.0.0.1:41337
- Zbiory: ☰ → Otwórz → Biblioteka danych → jRISK („Ćwiczenia — …”, „Bananpol — …”).
  Surowe dane: Bananpol (urządzenia, wypadki, alarmy, linia); gotowe przykłady ćwiczeniowe
  tylko dwa: układ hamowania i dwie bariery. Pozostałe warianty buduje się ręcznie, tak jak studenci (zmiana wartości
  albo nowy zbiór), więc testy niżej sprawdzają także ten tryb pracy.
- Analizy: menu **Ryzyko**
- Data testu: ____________  Tester: ____________

Legenda: `[x]` OK, `[!]` problem (opisz w „Uwagi”), `[-]` pominięty.

## A. Zdarzenia i warunkowanie — liczności

- [x] **A1. Kontrole rampy (zad. 1.2)** — wykonany na usuniętym już zbiorze `kontrole_rampy`.
- [ ] **A1b. Kontrole rampy, zbiór ręczny.** Nowy zbiór 4 wiersze: brak_oznakowania
  (tak, tak, nie, nie), mokra_posadzka (tak, nie, tak, nie), licznosc (6, 22, 11, 61).
  A = brak_oznakowania, B = mokra_posadzka, Liczność = licznosc. Zanotuj domyślnie
  wybrany poziom (tak/nie): ______; ustaw „tak”.
  Oczekiwane: suma 100, P(A ∪ B) = 0,39, nota „Liczności z kolumny «licznosc»”.
- [ ] **A2. Czujnik awarii (zad. 3.1), zbiór ręczny.** awaria (tak, tak, nie, nie), alarm
  (tak, nie, tak, nie), licznosc (95, 5, 495, 9405). A = awaria (tak), B = alarm (tak),
  Liczność = licznosc; włącz metryki detektora, częstości naturalne, drzewo częstości.
  Oczekiwane: PPV ≈ 0,161, czułość 0,95, swoistość ≈ 0,95; drzewo 95 / 5 / 495 / 9405.
- [ ] **A3. Częstość bazowa 0,1% (zad. 3.2).** W zbiorze z A2 zmień liczności na
  95, 5, 4995, 94905. Oczekiwane: wyniki przeliczają się same, PPV ≈ 0,019.
- [ ] **A4. Test przesiewowy (zad. 3.4).** W tym samym zbiorze liczności 90, 10, 198, 9702.
  Oczekiwane: PPV = 0,3125. Przywróć 95, 5, 495, 9405.
- [ ] **A4b. Dziennik alarmów (zad. 3.6).** „Bananpol — alarmy przegrzania”, filtr
  czujnik = stary. A = przegrzanie (tak), B = alarm (tak), bez Liczności, metryki detektora.
  Oczekiwane: TP 27, FN 2, FP 174, TN 2797; PPV ≈ 0,134. Dodaj do filtru sekcja = A:
  PPV ≈ 0,045; sekcja = C: PPV ≈ 0,190. Czy filtrowanie jest wygodne w praktyce? ______
- [ ] **A5. Wagi jamovi.** Na zbiorze z A2 usuń pole Liczność, włącz Dane → Wagi = licznosc.
  Oczekiwane: wyniki jak w A2, nota „Dane ważone zmienną «licznosc»”.
- [ ] **A5b.** Wyłącz wagi. Oczekiwane: powrót do 4 obserwacji.
- [ ] **A6. Liczność ujemna.** Wpisz −5 w jednej komórce. Oczekiwane: czytelny błąd zamiast
  wyniku. Cofnij zmianę.
- [ ] **A7. Regresja: Bananpol — wypadki** (bez Liczności). A = poslizgniecie (tak),
  B = buty_antyposlizgowe (tak). Oczekiwane: 0,15; 0,65; 0,055; 0,745 (zad. 1.4).

## B. Schemat Bernoulliego

- [ ] **B1. Zero wad (zad. 4.5).** Nowy zbiór: `wada` = nie, tak; `n` = 100, 0. Wynik = wada,
  poziom „tak”, Liczność = n, zaznacz „Jednostronna górna granica p (95%)”.
  Oczekiwane: górna granica ≈ 0,0295, nota z 1 − 0,05^(1/n), wykres częstości
  skumulowanej ukryty, nota wyjaśnia dlaczego.
- [ ] **B2.** Odznacz górną granicę. Oczekiwane: kolumna znika.
- [ ] **B2b. Przeoczenia przegrzań (zad. 4.6).** Alarmy, filtr przegrzanie = tak i czujnik = nowy;
  wynik = alarm, poziom „nie”, górna granica. Oczekiwane: n = 42, 0 sukcesów, granica ≈ 0,069,
  wykres serii widoczny (surowe wiersze). Czujnik = stary: n = 29, 2, granica ≈ 0,202.
- [ ] **B3. Regresja: Bananpol — wypadki**, wynik = poslizgniecie, bez Liczności.
  Oczekiwane: wykres serii jak dotąd, nota o kolejności wierszy.

## C. Niezawodność systemów

- [ ] **C1. Układ hamowania (zad. 9.1, 9.3).** Tryb danych: niezawodność = niezawodnosc,
  etykieta = element, podsystem = podsystem; w podsystemie równolegle, między szeregowo.
  Włącz ścieżki/przekroje, tabelę stanów, koherentność.
  Oczekiwane: R = 0,9405; przekroje {C}, {A, B}; 8 stanów; koherentność tak / tak / tak.
- [ ] **C2. Wspólna przyczyna, tryb danych (zad. 9.4).** Odfiltruj C (element ≠ "C"),
  zaznacz „Wspólna przyczyna”, q = 0,01, włącz Birnbauma.
  Oczekiwane: R = 0,9801; w „Dane wejściowe” dopisek „+ wspólna przyczyna CCF (q = 0.01)”;
  przekrój {CCF}; CCF pierwszy w rankingu Birnbauma; blok „CCF 0,99” na końcu schematu;
  nota założeń wspomina CCF.
- [ ] **C3. Wspólna przyczyna, tryb ręczny (zad. 9.c).** Równoległa, n = 2, r = 0,9, CCF
  q = 0,01. Oczekiwane: R = 0,9801.
- [ ] **C3b.** n = 3. Oczekiwane: R = 0,98901.
- [ ] **C3c.** Mostek + CCF. Oczekiwane: schemat się nie rozjeżdża.
- [ ] **C4.** Pole q nieaktywne, gdy „Wspólna przyczyna” odznaczona.
- [ ] **C5. k-z-n, tryb danych (zad. 9.5).** Nowy zbiór: 3 wiersze r = 0,9, jedna grupa;
  w podsystemie „k-z-n”, k = 2. Oczekiwane: R = 0,972, 3 ścieżki minimalne.
- [ ] **C6. Aktywność pola k.** Aktywne tylko przy: tryb ręczny + struktura k-z-n albo tryb
  danych + bramka k-z-n; poza tym wyszarzone.
  Czy osobny blok „k-z-n” w panelu jest czytelny? ______
- [ ] **C7.** k = 4 na zbiorze z C5. Oczekiwane: błąd z nazwą podsystemu z za małą liczbą
  elementów.
- [ ] **C8.** Bananpol — linia, w podsystemie k-z-n (k = 1), między podsystemami równolegle.
  Oczekiwane: schemat ukryty.
- [ ] **C9. Misja termiczna (zad. 11.1), zbiór ręczny.** W „Modelach czasu życia”: wykładniczy,
  λ = 1/1500, t = 1000 → R ≈ 0,5134. Nowy zbiór: element (P, C, FAN1, FAN2), podsystem
  (P, C, wentylatory, wentylatory), niezawodnosc (0,98; 0,95; 0,5134; 0,5134); tryb danych.
  Oczekiwane: R ≈ 0,7106.
- [ ] **C9b.** Ten sam zbiór z PUSTYMI komórkami wentylatorów: analiza po cichu liczy tylko
  P i C. Czy potrzebna nota? ______
- [ ] **C10. Bananpol — linia (dane 2026-10).** Oczekiwane: R = 0,9551, Birnbaum STER ≈ 0,975,
  potem nawilżacze ≈ 0,076, wentylatory ≈ 0,070.

## D. Drzewo błędów (FTA)

- [ ] **D1. Drzewo zasilania (zad. 10.3), zbiór ręczny.** zdarzenie (C, A, B), p (0,01; 0,05;
  0,05), galaz (C, AB, AB). Gałąź = galaz, wewnątrz AND, na górze OR.
  Oczekiwane: 0,012475; przekroje {C}, {A, B}; diagram „P = 0.0125”.
- [ ] **D2. Łańcuch barier (zad. 10.1, 10.a), zbiór ręczny.** zdarzenie (I, D0, S0, C),
  p (0,005; 0,05; 0,08; 0,01), galaz (inicjacja, bariera, bariera, bariera).
  Wewnątrz OR, na górze AND.
  Oczekiwane: 0,0006737.
- [ ] **D2b.** Odfiltruj C. Oczekiwane: 0,00063; diagram bez notacji „6e-04”.
- [ ] **D3. Powtórzony liść (zad. 10.2), zbiór ręczny.** zdarzenie (C, C), p (0,05; 0,05),
  galaz (G1, G2), bez checkboxa. Oczekiwane: błąd z podpowiedzią
  o opcji.
- [ ] **D3b.** Zaznacz „Powtórzona etykieta = to samo zdarzenie”. Oczekiwane: 0,05 przy AND
  i przy OR na górze.
- [ ] **D3c.** Etykiety C1, C2, checkbox odznaczony. Oczekiwane (błąd dydaktyczny):
  0,0025 (AND) i 0,0975 (OR).
- [ ] **D4. Dwie bariery (zad. 10.a).** Zbiór „Ćwiczenia — dwie bariery…”. Wewnątrz AND, na górze OR, checkbox zaznaczony.
  Oczekiwane: P(TOP) ≈ 0,0000698; przekroje {I, C}, {I, B1, B2}; ranking 4 zdarzeń,
  I na górze; nota wymienia powtórzone I.
- [ ] **D5.** W D4 zmień p drugiego I na 0,006. Oczekiwane: błąd z nazwą zdarzenia.
- [ ] **D6. Bananpol — linia jako FTA (dane 2026-10).** Oczekiwane: 0,04489 = 1 − 0,95511.

## F. Modele czasu życia — grupy

- [ ] **F1. Bez grupy (zad. 8.2).** „Bananpol — urządzenia”, tryb danych, czas = czas_pracy,
  status = awaria (1). Oczekiwane: 130 awarii, 20 cenzurowanych; Weibull β ≈ 1,51, η ≈ 23,8;
  AIC: wykładniczy 1078,1, gamma 1058,3, Weibull 1054,6; brak kolumny „Grupa”.
- [ ] **F2. Grupa = urzadzenie, t = 6 (zad. 8.6).** Oczekiwane: kolumna „Grupa” scalona w pionie;
  R(6) Weibulla: agregat ≈ 0,995, wentylator ≈ 0,732, nawilżacz ≈ 0,921 (te same co w
  „Bananpol — linia”); nota AIC wymienia model dla każdej grupy (wentylator: Gamma).
- [ ] **F3. Wykres KM z grupami.** Kolor na grupę, KM ciągłe, model przerywany; czy legenda jest
  zrozumiała (pokazuje tylko kreskę przerywaną)? ______
- [ ] **F4.** Pole „Grupa” nieaktywne w trybie parametrycznym.

## E. Ogólne

- [ ] **E1.** Zapisz .omv z kilkoma analizami z A–D, zamknij, otwórz. Oczekiwane: opcje
  i wyniki odtworzone.
- [ ] **E2.** Otwórz .omv zapisany z jRISK 0.3.4 (jeśli jest). Oczekiwane: otwiera się, nowe
  opcje mają wartości domyślne.
- [ ] **E3.** „O zbiorze” dla 2–3 nowych zbiorów: polskie znaki, opisy zgodne z menu.
- [ ] **E4.** Ciemny motyw: czytelność bloku CCF i diagramu FTA.

## Uwagi

| Test | Co widać | Zrzut ekranu |
|---|---|---|
| | | |
