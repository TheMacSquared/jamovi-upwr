# Testy ręczne po scaleniu jamovi 28.17

Cel: sprawdzić zachowanie interfejsu i dokumentów po merge `3786706e`, przede
wszystkim poprawki undo/redo, postępu operacji i zgodność funkcji jUPWR.
Wersja bazowa serwera: **28.17.0.0**. Numer dystrybucji pozostaje **1.2.4** do
przygotowania kolejnego wydania; sam numer jUPWR nie identyfikuje tego builda.

Plan przygotowany 2026-11-01. **Wszystkie poniższe testy ręczne są do wykonania.**
Przejście testów automatycznych i technicznego testu HTTP/WebSocket nie oznacza
zaliczenia tej checklisty przez użytkownika.

## Organizacja

- **P0: przed zaakceptowaniem merge — około 30–40 minut.** T01–T08, po kolei.
- **P1: regresje jUPWR i interfejsu — około 35–45 minut.** T09–T16.
- **P2: przed wydaniem — czas zależny od środowiska.** T17–T20.
- Pierwsza sesja: Docker, <http://127.0.0.1:41337>, jedna przeglądarka.
- Następnie P0 na drugiej przeglądarce, jeśli ma być wspierana, oraz P0/P1
  osobno na faktycznie zbudowanym Windows native i macOS. Docker nie potwierdza
  działania instalatora, schowka ani okien plikowych aplikacji Electron.
- Statusy: `NIE WYKONANO`, `OK`, `BŁĄD`, `NIE DOTYCZY` (z powodem).
- Pracuj na kopiach dokumentów. Przed każdym testem z edycją danych otwórz
  ponownie plik wzorcowy, chyba że instrukcja mówi o kontynuacji.
- Przy błędzie zapisz: numer testu, kroki, oczekiwany i rzeczywisty wynik,
  przeglądarkę/system, zrzut ekranu i kopię pliku, jeśli pomaga go odtworzyć.
  W Dockerze dołącz odpowiedni fragment `docker compose --profile main logs jamovi`.

## Dane i wyniki wzorcowe

W Dockerze wybierz **Otwórz → Ten komputer → Dokumenty → jupwr-test-audyt
→ upstream-28.17.csv**. Zestaw jest częścią obrazu i jest też kopiowany do
podmontowanego folderu audytu. Nie trafia do Biblioteki danych dydaktycznych.
Plik źródłowy: [upstream-28.17.csv](upstream-28.17.csv), UTF-8, separator średnik.
Jest sześć obserwacji; polskie litery, spacje i nawiasy w nazwach są celowe.

Po imporcie ustaw/sprawdź typy:

| Zmienna | Typ danych i poziom pomiaru |
|---|---|
| `id` | liczba całkowita; identyfikator albo zmienna ciągła |
| `grupa`, `ocena` | tekst; nominalna |
| `wynik (cm)`, `pomiar po` | liczbowa; ciągła |
| `data`, `start`, `koniec` | tekst; identyfikator |

Zapisz kopię jako `wzorzec-28.17.omv`. Przeglądarka może pobrać plik zamiast
pokazać natywne okno zapisu — potem otwieraj pobraną kopię.

Wyniki bez filtrów i wag dla `wynik (cm)`:

| Miara | Wynik |
|---|---:|
| N / braki | 6 / 0 |
| Średnia / mediana | 35 / 35 |
| Minimum / maksimum | 10 / 60 |
| Odchylenie standardowe próby | 18,70829 |
| V (%) | 53,45225 |
| Średnia w A / B | 20 / 50 |
| Klasyczny 95% przedział t dla średniej | [15,36686; 54,63314] |

Akceptuj różnice wynikające z wyświetlanego zaokrąglenia. Te liczby dotyczą
wyłącznie niezmienionego wzorca, bez filtrowania, wag i braków danych.

## P0 — testy krytyczne

### T01. Start, import i polskie znaki — 3 min

- [ ] Uruchom aplikację, otwórz CSV, sprawdź sześć wierszy i osiem kolumn.
- [ ] Przy nowym profilu sprawdź polski interfejs także w przeglądarce
  preferującej angielski. Jawna zmiana języka na angielski ma obowiązywać po
  restarcie; po sprawdzeniu przywróć polski i ponownie uruchom aplikację.
- [ ] Sprawdź `średnia`, nazwy `wynik (cm)` i `pomiar po`, typy z tabeli wyżej.
- [ ] Otwórz analizę i wróć do danych; przesuń granicę między arkuszem a wynikami.

**Oczekiwane:** dane nie są sklejone w jedną kolumnę, polskie znaki są poprawne,
panele działają i nie ma pustego ekranu ani ciągłego ładowania.

### T02. Statystyki, wykresy i odświeżanie — 4 min

- [ ] W Eksploracja → Zmienne ilościowe wybierz `wynik (cm)`; włącz braki,
  minimum, maksimum i V. Porównaj z wartościami wzorcowymi.
- [ ] Włącz histogram i wykres pudełkowy; wyłącz/włącz zaznaczenie średniej.
- [ ] Dodaj `grupa` do podziału, sprawdź średnie 20 i 50; usuń podział.
- [ ] Zmień ostatnie 60 na 600. Sprawdź średnią 125 i aktualizację wykresów.
  Cofnij edycję — wraca średnia 35.

**Oczekiwane:** tabela i wykresy opisują tę samą, aktualną wersję danych.
Średnia na wykresie pojawia się zgodnie z przełącznikiem.

### T03. Undo/redo przy ukrytych wierszach — 6 min

- [ ] Na wzorcu ustaw filtr `id > 3` i ukryj odfiltrowane wiersze.
  Widoczne identyfikatory to 4, 5, 6.
- [ ] W pierwszym widocznym wierszu zmień `wynik (cm)` z 40 na 400.
  Cofnij: wraca 40. Ponów: wraca 400.
- [ ] W dwóch następnych widocznych wierszach wklej pionowo 500 i 600.
  Cofnij i ponów wklejenie.
- [ ] Pokaż wszystkie wiersze bez usuwania filtra. Sprawdź, że `id` 1, 2, 3
  nadal mają wartości 10, 20, 30, a zmiany dotyczą tylko `id` 4, 5, 6.
- [ ] Cofaj, aż wycofasz edycje; następnie ponów je. Obserwuj identyfikatory.
  Sama zmiana widoczności wierszy również może być krokiem historii.

**Oczekiwane:** edycje, cofnięcia i ponowienia zawsze dotyczą tych samych
obserwacji. Żadna wartość nie trafia do innego wiersza po zmianie widoku.

### T04. Undo po usunięciu kolumny wynikowej — 5 min

- [ ] Na wzorcu uruchom Regresja → Regresja liniowa: zależna `pomiar po`,
  predyktor ilościowy `wynik (cm)`.
- [ ] Włącz zapis reszt do arkusza. Potwierdź utworzenie kolumny.
- [ ] Zmień wartości w kolumnie `wynik (cm)` dla `id` 1 na 11, a dla `id` 2 na 22.
- [ ] Wyłącz zapis reszt do arkusza; potwierdź zniknięcie kolumny wynikowej.
- [ ] Cofnij obie edycje danych, a następnie ponów je.

**Oczekiwane:** cofanie przechodzi przez historię mimo nieistniejącej już kolumny
reszt. Wracają 10 i 20, a po ponowieniu 11 i 22. Historia nie zapętla się ani
nie blokuje. Samo wyłączenie opcji analizy nie musi być krokiem undo danych.

### T05. Zapis i ponowne otwarcie — 4 min

- [ ] Na wzorcu dodaj analizę ilościową z histogramem, analizę jakościową
  oraz filtr `id > 3`. Zapisz pod nową nazwą `.omv`.
- [ ] Zamknij dokument i otwórz zapisaną kopię. Sprawdź sześć wierszy danych,
  filtr, wybrane zmienne, opcje analiz i wyniki dla trzech aktywnych obserwacji.
- [ ] Wyłącz filtr; średnia wraca do 35. Zmień jedną wartość i zapisz ponownie.
  Jeszcze raz otwórz plik i potwierdź zmianę.

**Oczekiwane:** dokument zachowuje dane i ustawienia, a analizy nadal reagują
na edycje. Nie wymagaj zachowania historii undo po ponownym otwarciu pliku.

### T06. Dokument ze starszego wydania — 4 min

- [ ] Otwórz kopię istniejącego `.omv` zapisanego w jUPWR opartym na 28.13;
  najlepiej z analizą, wykresem i zmienną przekształconą.
- [ ] Porównaj liczby i ustawienia z oryginałem lub wcześniejszym eksportem.
- [ ] Zmień opcję analizy i zapisz jako nowy plik; ponownie go otwórz.

**Oczekiwane:** zachowane są dane, opcje i znaczenie wyników. Ponowne obliczenie
analiz przy otwieraniu jest dopuszczalne. Różnicę liczbową wyjaśnij przed akceptacją.
Jeśli nie ma starego pliku, oznacz test jako niewykonany.

### T07. Szybkie zmiany i końcowy wynik — 3 min

- [ ] Dodaj podział według `grupa`, usuń go i kilka razy przełącz histogram.
- [ ] Szybko zmień tę samą komórkę kolejno na 100, 200 i z powrotem na 10.
- [ ] Poczekaj na zakończenie obliczeń. Ostatni stan: bez podziału, histogram
  włączony, dane wzorcowe.

**Oczekiwane:** średnia wynosi 35, histogram jest widoczny, nie wracają stare
wyniki i nie pozostaje wskaźnik trwającej operacji.

### T08. Schowek i eksport — 4 min

- [ ] Skopiuj tabelę wyników do edytora tekstu oraz arkusza kalkulacyjnego.
- [ ] Skopiuj wykres i wyeksportuj go do dostępnego formatu, np. PNG.
- [ ] Wyeksportuj raport do formatu dostępnego w danym buildzie, np. HTML/PDF.

**Oczekiwane:** liczby, etykiety i polskie znaki są zachowane, wykres nie jest
pusty ani ucięty. W przeglądarce uwzględnij uprawnienia schowka; zanotuj format
i program docelowy. Niedostępny format oznacz z powodem, nie jako zaliczony.

## P1 — regresje funkcji jUPWR i interfejsu

### T09. Braki danych i filtr — 3 min

- [ ] Na wzorcu usuń wartość 60 z `wynik (cm)`.
- [ ] Sprawdź N=5, braki=1, średnią=30. Następnie włącz filtr `id > 3`:
  aktywne wartości 40 i 50 dają N=2, braki=1, średnią=45.
- [ ] Cofnij zmiany; sprawdź powrót do wartości wzorcowych.

**Oczekiwane:** brak nie jest zerem, filtr wpływa na statystyki i wykresy.

### T10. Reguły przekształcenia i leniwe IF — 5 min

- [ ] Dodaj zmienną obliczaną `IF(id > 0, id * 2, LN(-1))`.
  Wyniki: 2, 4, 6, 8, 10, 12.
- [ ] Przez interfejs reguł utwórz zmienną jakościową zależną od `id`:
  poniżej 3 → „niska”, poniżej 5 → „średnia”, pozostałe → „wysoka”.
- [ ] Sprawdź kolejność kategorii, polskie etykiety oraz aktualizację po
  zmianie `id`. Zapisz i ponownie otwórz dokument.

**Oczekiwane:** niewybrana gałąź `LN(-1)` nie psuje wyniku; reguły i kategorie
działają po edycji i ponownym otwarciu dokumentu.

### T11. Daty i czas — 3 min

- [ ] Dodaj `TIMEDIFF(start, koniec, unit='min')`.
  Oczekiwane: 30, 20, 10, 20, 30, 40.
- [ ] Dodaj `DAY(DATEVALUE(data))`: oczekiwane 1, 2, 3, 4, 5, 6.
- [ ] Zastąp jedną datę tekstem `brak`; wynik ma być brakujący, bez awarii
  pozostałych wierszy. Cofnij zmianę.

**Oczekiwane:** polski zapis daty oraz przejście przez północ są obsługiwane.

### T12. Nazwy ze spacjami i nawiasami — 4 min

- [ ] Uruchom korelację dla `wynik (cm)` i `pomiar po`: r≈0,9784483.
- [ ] Dodaj trzecią zmienną liczbową `id` i włącz dostępny wykres korelacji.
- [ ] Uruchom regresję z T04: wyraz wolny≈1,466667, nachylenie≈0,977143.

**Oczekiwane:** analizy i wykresy działają dla nazw ze spacjami/nawiasami;
w tabelach nie pojawiają się techniczne apostrofy wokół nazw zmiennych.

### T13. Podział analiz jakościowych i ilościowych — 3 min

- [ ] W Eksploracja sprawdź osobne analizy Zmienne ilościowe i Zmienne jakościowe.
- [ ] Dla `ocena` w jakościowych sprawdź po dwie obserwacje w każdej kategorii
  i udziały około 33,33%; dodaj podział według `grupa`.
- [ ] Sprawdź sumy i procenty zgodnie z nagłówkiem wybranego mianownika.

**Oczekiwane:** tabele częstości pozostają w analizie jakościowej. Merge nie
przywrócił zdublowanego panelu częstości w analizie ilościowej jUPWR.

### T14. Wykresy, palety i separator dziesiętny — 5 min

- [ ] Zmień motyw i paletę na dwa różne zestawy. Sprawdź histogram, pudełkowy
  z zaznaczeniem średniej oraz wykres regresji.
- [ ] Zmień separator dziesiętny w ustawieniach; oceń tabelę, legendę i osie
  zawierające ułamki. W razie potrzeby dodaj pomocniczą zmienną `id / 10`.
- [ ] Zapisz i otwórz dokument; sprawdź czytelność i zachowanie ustawień.

**Oczekiwane:** aktualizacja nie pozostawia mieszaniny starego i nowego stylu,
etykiety nie nachodzą na siebie, liczby nie zmieniają znaczenia. Nowy typ opcji
`Color` sprawdzaj dodatkowo dopiero w module, który faktycznie udostępnia go w UI;
sam merge infrastruktury nie gwarantuje obecności selektora w każdej analizie.

### T15. Przedziały i odtwarzalność losowania — 5 min

- [ ] W jCI wybierz klasyczny 95% przedział dla średniej `wynik (cm)`:
  około [15,36686; 54,63314].
- [ ] Włącz metodę bootstrapową, 1000 powtórzeń i ziarno 123. Zanotuj wynik;
  utwórz drugą analizę z identycznymi ustawieniami.
- [ ] Uruchom test permutacyjny jednej próby z ziarnem 123 i ponów w nowej
  analizie przy identycznej liczbie permutacji i trybie losowania.

**Oczekiwane:** identyczne ustawienia i ziarno dają te same wyniki w ramach
tego builda. Klasycznego przedziału nie porównuj z bootstrapowym jako równych.

### T16. Arkusz i obsługa błędów — 5 min

- [ ] Zmieniaj szerokość kolumn, przeciągaj podział paneli, przerwij przeciąganie
  wyjściem kursora z okna i wróć. Zaznacz i wklej prostokąt komórek; cofnij.
- [ ] Otwórz uszkodzoną kopię pliku: zapisz zwykły tekst jako `uszkodzony.omv`
  i spróbuj go otworzyć w nowym dokumencie.
- [ ] Po komunikacie błędu ponownie otwórz prawidłowy wzorzec i policz średnią.

**Oczekiwane:** brak zawieszenia/przewijania w pętli; błąd jest czytelny,
operacja kończy się i można dalej pracować.

## P2 — przed wydaniem, zależnie od środowiska

### T17. Uśpienie i ponowne połączenie

- [ ] Otwórz dokument, dodaj analizę i zmień komórkę bez zapisywania.
- [ ] Uśpij komputer na 1–2 minuty, wybudź i poczekaj na powrót połączenia.
- [ ] Sprawdź edycję i wyniki, zmień kolejną komórkę, zapisz dokument.

**Oczekiwane:** sesja wraca bez utraty danych, jeśli backend nadal działał.
Zanotuj, czy w czasie uśpienia działał Docker Desktop. Pełny restart Dockera
lub serwera jest innym scenariuszem i nie zakłada odzyskania niezapisanej sesji.

### T18. Biblioteka, proxy i certyfikaty uczelniane

- [ ] Otwórz bibliotekę modułów; sprawdź odczyt listy i opisów.
- [ ] Jeśli dostępne są obie sieci, powtórz w zwykłej sieci i uczelnianej
  z faktycznie skonfigurowanym proxy/systemowymi certyfikatami.
- [ ] Zanotuj system, sieć i wynik. Nie wyłączaj weryfikacji TLS na potrzeby testu.

**Oczekiwane:** lista ładuje się lub pojawia się czytelny błąd. Test zaliczony
w domu nie potwierdza działania przez uczelniane proxy.

### T19. Moduł opcjonalny i instalacja z pliku

- [ ] Na kopii środowiska zainstaluj jRISK przez Sideload, używając `.jmo`
  zbudowanego dla tej platformy i bieżącego runtime/kompilatora.
- [ ] W aplikacji Electron sprawdź oba sposoby instalacji: wybór pliku strzałką
  oraz przeciągnięcie `.jmo` na strzałkę i na puste miejsce panelu. Plik nie
  powinien trafić do otwierania danych ani zgłosić „Unrecognised file format”.
- [ ] Uruchom jedną analizę modułu, zapisz dokument i otwórz go ponownie.
- [ ] Powtórz po restarcie aplikacji. jSpace sprawdź analogicznie, jeśli jest
  przewidziany dla odbiorców wydania.

**Oczekiwane:** moduł ładuje się i wykonuje analizę. Stare artefakty opisane
w macierzy dla jamovi 28.13 nie są automatycznie potwierdzone dla 28.17.
Brak nowego `.jmo` oznacza test niewykonany, nie błąd instalatora aplikacji.

### T20. Duży plik i pakiety natywne

- [ ] Otwórz reprezentatywny plik z zajęć, np. co najmniej 10 tys. wierszy.
  Zmierz import, przeliczenie filtra i zapis; porównaj z poprzednim buildem
  na tej samej maszynie i tym samym pliku.
- [ ] Na Windows/macOS uruchom zbudowaną paczkę na maszynie bez developerskiego
  R/Pythona. Wykonaj P0, sprawdź okna plików, schowek i ponowny start.

**Oczekiwane:** operacje kończą się, interfejs odzyskuje responsywność,
aplikacja natywna używa własnego runtime. Zanotuj czasy; nie zakładaj wspólnego
limitu sekund dla różnych komputerów. Wyraźny regres względem poprzedniego
builda wymaga wyjaśnienia.

## Protokół i decyzja

Skopiuj tabelę dla każdej kombinacji builda i środowiska:

| Pole | Wartość |
|---|---|
| Data i tester | |
| Commit / wersja serwera / tag obrazu | |
| System / architektura / przeglądarka lub Electron | |
| Testy P0: T01–T08 | |
| Testy P1: T09–T16 | |
| Testy P2: T17–T20 | |
| Numery błędów i załączniki | |
| Niewykonane testy i powody | |
| Decyzja: akceptacja / blokada | |

Merge można zaakceptować użytkowo po zaliczeniu P0 i wyjaśnieniu błędów P1.
Błędne komórki po undo/redo, utrata danych przy zapisie i niezgodne wyniki
liczbowe blokują akceptację. Przed wydaniem paczki systemowej zalicz odpowiednie
testy na tym systemie; wyniku Dockera nie wpisuj jako Windows native/macOS.
