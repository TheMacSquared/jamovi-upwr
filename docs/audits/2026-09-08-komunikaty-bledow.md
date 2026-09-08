# Błędy uniemożliwiające obliczenie — aktualny zakres prac

Data ustaleń: 2026-09-08. Audytowany checkout: `5b8018ca`. Podstawa: scalony audyt Codex i Claude’a oraz weryfikacja w kodzie i lokalnym R. Poniżej zakres zawężony zgodnie z decyzją użytkownika; nie jest to polecenie wdrożenia poprawek w tej sesji.

## Trzy kategorie i decyzja użytkownika

| Kategoria | Znaczenie | Obecny zakres |
|---|---|---|
| **1. Nie da się policzyć** | Zlecone obliczenie nie daje wyniku głównego: wejście jest niewystarczające/niedopuszczalne dla wykonania operacji albo obliczenia kończą się błędem. | **Wyłącznie ta kategoria — do realizacji.** |
| **2. Da się policzyć tylko część** | Wynik główny powstaje, lecz brakuje pojedynczej miary, diagnostyki, części wierszy lub dodatkowego wykresu. | **Odłożona.** |
| **3. Da się policzyć, ale wybór może być metodologicznie słaby lub wymaga objaśnienia** | Wynik istnieje; uwaga dotyczy zasadności wyboru, założeń, interpretacji albo czytelności. | **Odłożona.** |

Kategorie 2 i 3, pełną dokumentację źródłową oraz indeks analiz przeniesiono do [ustaleń odłożonych](2026-09-08-komunikaty-odlozone.md). Nie należy włączać ich do bieżącego planu napraw ani czytać jako listy obowiązkowych ostrzeżeń.

**Cel: użytkownik ma wiedzieć „nie da się policzyć, bo…”. Aplikacja nie ma walidować za studenta zasadności modelu ani podpowiadać lepszej metody.** Nie dodajemy ostrzeżeń o setkach kategorii, małej próbie dla Walda, naruszeniu założeń, podejrzanym doborze zmiennych czy legalnych wynikach granicznych.

## Granica zakresu

- Klasyfikujemy konkretny przebieg analizy, nie całą funkcję pomocniczą. Stała zmienna w pojedynczej korelacji bez wyniku to kategoria 1; jedna niewyznaczalna para w działającej macierzy to kategoria 2.
- Brak wyniku w samodzielnej analizie wykresowej to kategoria 1; brak dodatkowego wykresu przy poprawnym teście to kategoria 2.
- Nieukończony wybór wymaganych zmiennych pozostaje zwykłym stanem początkowym. Nie zamieniać go w błąd.
- Istniejące warunki zatrzymujące analizę mają dawać czytelną przyczynę. Ich progi opisujemy jako wymagania bieżącej implementacji; ten etap nie służy ustanawianiu nowych reguł metodologicznych.
- Nie zmieniamy milcząco zlecenia, żeby uzyskać wynik. Jeżeli wskazana grupa/kontrola nie istnieje lub wejście zawiera niedopuszczalne liczności, zgłaszamy, że obliczenia na wskazanym wejściu nie mogą zostać wykonane. Nie mylić nieistniejącego poziomu z istniejącą kategorią o zerowej liczności. Nie dodawać przy tej okazji nowych konwencji kodowania ani zaleceń wyboru grupy.
- Nie wprowadzać globalnego zakazu SD=0, n=1, p=1, OR=Inf czy x poza nośnikiem. Sprawdzać, czy właśnie żądana operacja faktycznie nie daje wyniku. Wynik liczbowy słaby metodologicznie nie jest błędem obliczeń.

## Potwierdzona przyczyna wspólna

`jTestyT/R/ttesttwo.b.R:17–19` wykrywa liczbę grup różną od 2, zapisuje `setNote()` i wraca bez wierszy. `client/resultsview/table.ts:712–720` kończy renderowanie pustego body przed stopką (`:808`). Komunikat istnieje w R, ale nie jest widoczny. Upstreamowy `jmv/R/ttestis.b.R:44` zgłasza ten problem przez `reject()`.

**Pierwsza naprawa:** renderować i czyścić przypisy również przy pustej tabeli; sprawdzić przejście poprawny wynik → błąd → poprawny wynik. Widoczność błędu blokującego nie może zależeć od włączenia opcjonalnej tabeli lub „Zastosowanych metod”. Poprawa wspólnego renderera może przy okazji ujawnić istniejące noty kategorii 2; nie oznacza to zgody na dopisywanie nowych.

## Lista przypadków kategorii 1

K = mechanizm potwierdzony w kodzie; R = dodatkowa próba czystej funkcji w R. Żadne oznaczenie nie oznacza pełnej reprodukcji GUI. W gałęziach wielozmiennych poniższy zakres dotyczy braku wyniku głównego; lokalne braki przy pozostałych poprawnych wynikach pozostają kategorią 2.

| Moduł / źródło | Kiedy brak wyniku | Oczekiwane zachowanie |
|---|---|---|
| jTestyT — `R/ttesttwo.b.R:17,31`; `ttestone.b.R:25`; `ttestpaired.b.R:32` (K) | Liczba grup ≠2; po usunięciu braków brak grupy, obserwacji lub kompletnych par wymaganych przez implementację. | Widoczny komunikat z nazwą zmiennej, liczbą grup/obserwacji/par i wymaganym minimum. |
| jTestyT — `R/utils.R`, `oneSampleT`, `twoSampleT` (R dla stałej jednej próby) | Test główny rzuca wyjątek, np. dane/różnice są numerycznie stałe. | „Nie można obliczyć testu t dla «X»: …”. Nie kasować poprawnych wyników innych zmiennych, jeśli powstały. |
| jperm — `R/permtesttwo.b.R:18,28`; `permtestone.b.R:22`; `permtestpaired.b.R:30` (K) | Nieprawidłowa liczba grup lub brak danych wystarczających do uruchomienia bieżącego kodu. | Uwidocznić istniejącą przyczynę. Nie kopiować progu n≥2 na grupę z testu t. |
| jCI — `R/cibootstrap.b.R:11`, `cipairedmeans.b.R:12`, `cicorrelation.b.R:12`, `ciregression.b.R:12–14`, `cionemean.b.R:30,51`, `citwomeans.b.R:16`, `cidiffprop.b.R:18`, `ciproportion.b.R:11–13` (K) | Analiza kończy się przed wynikiem z powodu braku danych/par lub niewykonalnego modelu. | Pokazać powód istniejącego zatrzymania. Brak wyłącznie CI, d lub SE przy istniejącej estymacie należy do kategorii 2. |
| jCI — `R/utils.R:305–309,317` (K) | Dwie identyczne/nieistniejące grupy, brak grup; jawnie wybrana nieistniejąca kategoria zdarzenia. | Nie liczyć zastępczego zlecenia; wskazać niedostępny wybór. |
| jANOVA — `R/anova.b.R:83–97`; `anovarm.b.R:84–107` (K) | Cichy powrót po filtracji, za mało poziomów, niekompletny układ wymagany przez implementację, brak df błędu lub nieudane dopasowanie. | Zgłosić konkretny warunek uniemożliwiający wykonanie, bez oceny adekwatności modelu i rekomendacji innej analizy. |
| jRegr — `R/liniowa.b.R:12–15`; `logistyczna.b.R:12–17`; `utils.R:15–18,68` (K/R dla stałej korelacji) | Model zatrzymany przed wynikiem; odpowiedź nie ma wymaganych dwóch kategorii; pojedyncza korelacja niewyznaczalna; jawny poziom zdarzenia/odniesienia nie istnieje. | Wyjaśnić brak wyniku lub niemożność wykonania wskazanego porównania. Braki dodatkowych diagnostyk odłożyć. |
| jEksplor — `R/szereg.b.R:11–15`; `jakosciowe.b.R:74` (K) | Brak obserwacji lub nie można utworzyć żądanego szeregu klas. | Uwidocznić obecny komunikat z parametrem/zmienną; nie ostrzegać teraz o liczbie kategorii ani sensowności typu zmiennej. |
| jCzest — `R/tabela.b.R:68`; `zgodnosc.b.R:23–36`; `zalezne.b.R:68,135,142`; `utils.R:9–18` (K/R dla odrzucania ujemnych counts) | Układ nieobsługiwany przez żądany test, brak danych, błędne proporcje oczekiwane; niedopuszczalne liczności wejściowe. | Wyjaśnić brak możliwości obliczenia; nie usuwać ujemnych counts, by po cichu policzyć inny zbiór. Całkowitość wymaga sprawdzenia względem konkretnej metody, nie wszystkich wag. |
| jRol — `R/utils.R:374–399,221,317` (K) | Cichy powrót, zatrzymanie istniejącej walidacji układu, brak df, wyjątek modelu lub niedostępna wskazana kontrola. | Widoczny powód; nie rozszerzać teraz kontroli metodologicznej układów doświadczeń. |
| jSpace — `R/satgroups.b.R:19`; `geomap.b.R:23`; `tleorbit.b.R:40,74,102`; `rasterstats.b.R:23–50` (K) | Brak danych/grup/współrzędnych, niewczytany raster/TLE, brak komórek rastra z wartościami lub nieudana propagacja uniemożliwiająca wynik. | Komunikat niezależny od widoczności tabel dodatkowych. Brak statystyk przy działającej mapie to kategoria 2. |
| jRISK — istniejące `setError` w pięciu analizach; `lifetime.b.R:120–122`, `bernoulli.b.R:18–30`, `eventtables.b.R:18–23` (K) | Istniejące błędy wejścia; jawnie wybrany poziom nie istnieje, więc nie można wykonać wskazanego obliczenia. | Zachować działające błędy; nie zastępować nieistniejącego wyboru zerową liczbą zdarzeń. Ostrzeżenia o binaryzacji odłożone. |
| jdistrACTION — `jamovi/binomialdistribution.a.yaml:64–73`, `normaldistribution.a.yaml:66`, `fdistribution.a.yaml:51–64` (K) | Parametry spoza dziedziny obliczeń: np. ujemne SD, niedopuszczalne n/p rozkładu dwumianowego, niedodatnie df F. | Wskazać parametr uniemożliwiający obliczenie. Nie dodawać błędu dla poprawnie określonego prawdopodobieństwa przy x1<0. |
| plots — `raincloud.b.R:51–53`, `stackbar.b.R:55`, `treemap.b.R:97`, `wordcloud.b.R:58,70–72`, `waffle.b.R:87,156`, `mosaic.b.R:57`, `radar.b.R:68`, `parcoord.b.R:82` (K) | Samodzielny wykres nie powstaje: puste dane, brak dodatniej sumy, niewystarczające wejście lub brak rozmieszczonych słów/kafelków. | Wyjaśnić przyczynę pustego wyniku. Nie dodawać ostrzeżeń o stylistyce, czytelności ani wyborze zmiennych. |

jmv ma już wiele poprawnych mechanizmów `reject` i przypisów; nie planujemy wymiany ich wszystkich. jDane nie ma analiz, a jboot nie ma bieżących źródeł `.b.R`. Wątpliwe scenariusze z szerokiego audytu nie są automatycznie listą napraw: do obecnego zakresu trafiają dopiero po potwierdzeniu, że uniemożliwiają wynik główny.

## Sposób komunikowania i techniczne ograniczenia

1. Błąd całego zlecenia: istniejący `reject`, błąd elementu lub programowy Notice, zależnie od zakresu awarii. Nie budować teraz nowego systemu ostrzeżeń metodologicznych.
2. Znany warunek: komunikat z danych i kontekstu, np. „Zmienna «grupa» ma 3 poziomy; test wymaga dokładnie 2”. Nie diagnozować przyczyny na podstawie angielskiej treści wyjątku.
3. Nieprzewidziany wyjątek: „Nie udało się obliczyć [nazwa operacji]”, z zachowaniem oryginalnych szczegółów diagnostycznych; nie zgadywać, że użytkownik źle wybrał model.
4. W obecnym zakresie przejrzeć surowe wyjątki **dopasowania głównego**: `jANOVA/R/anova.b.R:92`, `anovarm.b.R:107`, `jRol/R/utils.R:391–392`. Pozostałe 7 miejsc dotyczące dodatkowych testów/porównań są zachowane w pliku odłożonym.
5. `type: Notice` nie jest obecnie dopuszczony przez schemat YAML kompilatora, mimo obsługi w generatorze. Preferować działające mechanizmy; nie rozszerzać kompilatora bez potrzeby wynikającej z tej naprawy.
6. Sam brak `tryCatch` nie dowodzi niewidocznego błędu: framework obsługuje wyjątki, także podczas renderowania. Sprawdzić faktyczny efekt; nie otaczać mechanicznie wszystkich obliczeń przechwytywaniem.

## Odbiór wyłącznie kategorii 1

- Po kompletnym skonfigurowaniu niewykonalnej analizy widoczny jest powód braku wyniku, także przy pustej tabeli i wyłączonych tabelach dodatkowych.
- Przejście poprawny wynik → niewykonalne wejście → poprawny wynik usuwa stare wyniki/komunikaty i przywraca aktualne.
- Poprawnie wykonalne obliczenia zachowują dotychczasowe wyniki i domyślne zachowanie; nie pojawiają się nowe ostrzeżenia kategorii 2 ani 3.
- Nie podmieniamy jawnego wyboru i nie odrzucamy niedopuszczalnych danych w celu uzyskania innego wyniku. Nie zmieniamy zwykłej obsługi braków danych.
- Sprawdzić reprezentatywne przypadki: >2 grupy w t, brak grupy po usunięciu braków, brak kompletnych par, wyjątek testu głównego, brak df modelu, błędny parametr rozkładu, puste wejście wykresu.
- Po implementacji: odpowiednie testy R/renderera, Docker build i GUI. Obecna zmiana dotyczy tylko dokumentacji; build nie był uruchamiany.

Ograniczenia dowodowe audytu: przegląd źródeł i punktowe reprodukcje w lokalnym R, bez pełnego wykonania klas jmvcore i GUI; podczas audytu brak lokalnego pakietu jmvcore i dostępu do socketu Dockera. Nie sprawdzano Windows native ani macOS.
