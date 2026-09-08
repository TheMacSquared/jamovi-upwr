# Komunikaty analiz — ustalenia odłożone i materiał referencyjny

**Poza obecnym zakresem prac. Nie wdrażać na podstawie tego pliku.**

Aktualny zakres określa [raport błędów uniemożliwiających obliczenie](2026-09-08-komunikaty-bledow.md). Użytkownik wybrał wyłącznie kategorię 1. Ten plik przechowuje kategorie 2 i 3 oraz pełne szczegóły wcześniejszego audytu, aby nie obciążać bieżącego kontekstu i nie utracić ustaleń. Nie trzeba go czytać podczas realizacji kategorii 1.

## Kategoria 2 — da się policzyć tylko część (odłożona)

Wynik główny powstaje, ale brakuje dodatkowej miary, diagnostyki, wiersza lub wykresu. Docelowo lokalne wyjaśnienie bez kasowania poprawnych wyników; obecnie nie rozszerzamy o to prac.

| Obszar | Zachowane ustalenia i źródła |
|---|---|
| Testy t / permutacje | Pojedyncza pominięta zmienna lub para przy poprawnych pozostałych wynikach (`*.b.R`, warunki `next`); niewyznaczalny Shapiro, Levene, test rangowy (`jTestyT/R/utils.R`). |
| jCI | Brak CI lub d przy istniejącej estymacie, `dInterval`, niewystarczające repliki, SE przy B=1 (`utils.R`, `cibootstrap.b.R`); brak pasma/histogramu dodatkowego. |
| jANOVA / jRol | Kontrasty, ART/post-hoc, średnie, sferyczność, Levene/Bartlett, normalność i wykresy interakcji przy poprawnym wyniku głównym. Dokładne miejsca `conditionMessage` zachowano niżej. |
| jRegr | Niewyznaczalne komórki macierzy, CI korelacji przy n=3, diagnostyki ANOVA/VIF/Durbin–Watson i zapisane kolumny przy działającym modelu. |
| jEksplor | Poszczególne miary opisowe, testy normalności, Gini/Lorenz, częściowo odrzucone percentyle. |
| jCzest | CI V Craméra, CI OR, dodatkowe miary i porównania niedostępne dla rozmiaru tabeli; brak części wyników McNemara. |
| jSpace / plots / jmv | Brak dodatkowego wykresu lub części komórek/diagnostyk; stare elementy po zmianie opcji wymagające reprodukcji. Samodzielna analiza wykresowa bez żadnego wyniku należy natomiast do kategorii 1. |

## Kategoria 3 — wynik powstaje, ale wymaga oceny lub objaśnienia (odłożona)

Nie dodawać teraz ostrzeżeń dydaktycznych, oceny zasadności modelu, rekomendacji metody ani nowych progów „dobrej próby”. Student odpowiada za wybór i interpretację metody.

| Obszar | Zachowane ustalenia |
|---|---|
| jEksplor | Setki poziomów w analizie jakościowej, podejrzenie użycia identyfikatora, czytelność mozaiki, umowna szerokość klas przy stałych danych, skumulowane częstości nominalnych. |
| jCI | Wald przy małej próbie i udziałach krańcowych, w tym potwierdzone n₁=n₂=1 → szerokość CI=0; sugestia Newcombe’a, interpretacja zdegenerowanych CI i bootstrapu. |
| jdistrACTION | x1<0 w χ²/F jako informacja o nośniku, legalne prawdopodobieństwo 0, kwantyle nieskończone; spójność objaśnień między rozkładami. |
| Modele i testy | Założenia statystyczne, liczba zdarzeń na predyktor, małe df, adekwatność modelu i kodowania, układy doświadczeń, remisy, poprawne p=1 i OR=Inf. Nie tworzyć automatycznych zakazów. |
| Znaczenie wyników | Binaryzacja „zdarzenie kontra reszta”, poziomy kategorii Q Cochrana, dwuwymiarowy model na wykresie granic jSpace wobec pełnego modelu w tabeli. |
| Interfejs / wydajność | Limity liczby kategorii, paneli, klas, replik i poletek; etykiety, tłumaczenia, postęp obliczeń. Pomiary i decyzje projektowe odłożone. |

Ciche odrzucanie danych lub podmienianie jawnie wybranego poziomu nie jest poradą metodologiczną. Granicę tych przypadków określa główny raport: kategoria 1 tylko wtedy, gdy nie można wykonać zleconego obliczenia na wskazanym wejściu; samo objaśnianie istniejącej konwencji pozostaje tutaj.

## Materiał referencyjny — pełny poprzedni audyt

Poniższa treść zachowuje szczegóły, lokalizacje, reprodukcje, korekty fałszywych alarmów i indeks 107 analiz. **Historyczne P1/P2/P3 oznaczają priorytety dawnego szerokiego audytu, nie trzy aktualne kategorie. Zalecenia „dodać”, „sprawdzić”, „naprawić” i dawna kolejność prac nie stanowią obecnego zlecenia.** O zakresie rozstrzyga wyłącznie główny raport. Fragmenty kategorii 1 zachowano tu jako dokumentację źródłową, nie drugą listę zadań.

## Archiwalny audyt scalony

Data: 2026-09-08. Repozytorium: `5b8018ca`. Wersja scalona: audyt Codex, szczegółowy przegląd Claude’a oraz weryfikacja jego ustaleń w kodzie i R. Ten dokument zastępuje osobny raport szczegółowy i indeks. Audyt nie zmienia działania aplikacji.

## Najważniejszy wynik

Przykład z testem t jest potwierdzony w kodzie. `jTestyT/R/ttesttwo.b.R:17` sprawdza liczbę grup i dodaje poprawny komunikat przez `tt$setNote()`, po czym kończy analizę. Tabela ma `rows: 0` (`jTestyT/jamovi/ttesttwo.r.yaml:10`). W `client/resultsview/table.ts:712` renderer pustej tabeli wykonuje `return` przed pętlą renderującą przypisy (`:808`). **Komunikat istnieje w wynikach R, lecz nie dociera na ekran.**

To defekt wspólny dla modułów, a nie brak pojedynczego warunku. Upstreamowy `jmv/R/ttestis.b.R:44` używa `jmvcore::reject()` dla niewłaściwej liczby grup; nowy jTestyT zastąpił tę ścieżkę przypisem. Jest to konkretna różnica wyjaśniająca regresję po zastąpieniu analiz. Audyt nie ustala, który build pierwszy ujawnił problem, ani czy zainstalowana u użytkownika paczka odpowiada temu checkoutowi.

## Zakres i pewność

Objęto przeglądem wszystkie katalogi modułów z rejestru `packaging/MODULES.md`: jmv, plots/scatr, jdistrACTION, jCI, jperm, jDane, jCzest, jEksplor, jRegr, jANOVA, jTestyT, jRISK, jSpace, jRol. Sprawdzono też pozostałość jboot, wspólne funkcje R, definicje wyników, walidację opcji oraz ścieżkę renderowania przypisów. Indeks w załączniku A wylicza wszystkie 107 źródłowych analiz i miejsca sygnalizowania problemów; sam wynik wyszukiwania nie jest dowodem defektu.

- **K** — mechanizm potwierdzony w kodzie; nie oznacza reprodukcji w GUI.
- **R** — dodatkowo odtworzone przez wywołanie czystej funkcji w lokalnym R.
- **W** — scenariusz wymagający dalszej weryfikacji integracyjnej; nie deklarujemy potwierdzonego błędu GUI.
- Priorytet nie jest nakazem zatrzymania całej analizy: należy odróżniać błąd wejścia, niewyznaczalny pojedynczy wynik i poprawny wynik wymagający objaśnienia.
- P1 — blokuje diagnozę błędnych danych lub pozwala milcząco zmienić dane; P2 — brak wyjaśnienia częściowego wyniku; P3 — poprawa precyzji komunikatu.

To audyt statyczny całego zakresu i punktowe próby R, **nie pełne przetestowanie każdej kombinacji opcji w GUI**. Brak lokalnego jmvcore uniemożliwia wykonanie pełnych klas analiz. Dostęp do socketu Dockera zakończył się `permission denied`. Build nie był uruchamiany: zamówionym wynikiem jest raport, a nie poprawki ani nowe paczki. Nie sprawdzano Windows native ani macOS.

## 1. Wspólna infrastruktura — P1/K

Naprawy wymaga `client/resultsview/table.ts:705–720`: renderowanie i czyszczenie stopki musi następować również dla pustego body. Samo poprawienie renderera nie sprawi jednak, że błąd analizy stanie się wyraźny: błędy blokujące powinny trafiać do widocznego komunikatu błędu, niezależnego od checkboxów tabel dodatkowych. `setNote` pozostaje właściwe dla objaśnień istniejącego wyniku. Nie należy mechanicznie zamieniać wszystkich przypisów na błędy całej analizy.

Przy przejściu wynik poprawny → błędny trzeba sprawdzić także usunięcie poprzedniej stopki i poprzednich wykresów. Wczesny `return` omija przebudowę stopki; faktyczne pozostawanie starej treści zależy od cyklu aktualizacji widoku (W).

### Dobór mechanizmu diagnostycznego

| Mechanizm | Zastosowanie i ograniczenie |
|---|---|
| `jmvcore::reject()` | Błąd uniemożliwiający całą analizę. Nie stosować do jednej wadliwej zmiennej, gdy pozostałe wyniki dają się policzyć. |
| Programowe `Notice$new()` + `results$insert()` | Widoczny komunikat ERROR/WARNING/INFO przy wynikach. Wzorzec istnieje w jCzest; zadbać o aktualizowanie i usuwanie nieaktualnych komunikatów. |
| `element$setError()` | Błąd konkretnego elementu; stosowany m.in. w jRISK i jdistrACTION. |
| `addFootnote()` / `setNote()` | Przyczyna braku pojedynczej wartości lub objaśnienie tabeli. Przy pustym body konieczna naprawa renderera. |
| Ograniczenia `.a.yaml` | Sprawdzanie parametrów przed obliczeniami. `suggested` jest podpowiedzią, a dopuszczenie danych liczbowych nie dowodzi ich sensowności statystycznej. |

**Nie wdrażać obecnie `type: Notice` w YAML bez zmiany kompilatora.** `compiler.js:290` rozpoznaje Notice przy generowaniu R, ale `schemas/resultsschema.yaml:38` nie dopuszcza go na liście typów, a walidacja wykonywana jest wcześniej (`compiler.js:131`). Potrzebna jest weryfikacja pełnej ścieżki kompilacji albo użycie istniejącego mechanizmu programowego.

Niezłapany wyjątek R nie oznacza automatycznie pustego wyniku: framework ustawia błąd analizy, również dla wyjątku renderowania obrazu (`jmvcore/R/analysis.R:519`). To co innego niż cichy `return(FALSE)`. Język `conditionMessage()` zależy od lokalizacji R. Komunikat użytkowy powinien opisywać przyczynę i sposób poprawy, a szczegóły oryginalnego wyjątku pozostać dostępne diagnostycznie. Nie opierać całej obsługi na słowniku angielskich tekstów błędów.

### Surowe wyjątki w komunikatach — lista do poprawy (P2/K)

Łącznie **10 miejsc** (9 w jANOVA i 1 w jRol) przekazuje `conditionMessage()` do noty. Poniższa lista jest zadaniem implementacyjnym, niezależnym od naprawy widoczności pustych tabel:

| Miejsce | Kontekst i komunikat zastępczy dla nieprzewidzianego wyjątku |
|---|---|
| `jANOVA/R/anova.b.R:92` | „Nie udało się dopasować modelu ANOVA dla wybranych zmiennych.” |
| `jANOVA/R/anova.b.R:141` | „Nie udało się obliczyć testu Welcha–Jamesa dla tego układu.” |
| `jANOVA/R/anova.b.R:188` | „Nie udało się obliczyć analizy ART dla tego układu.” |
| `jANOVA/R/anova.b.R:202` | „Nie udało się obliczyć porównań ART dla czynnika «…».” |
| `jANOVA/R/anova.b.R:223` | „Nie udało się obliczyć średnich i porównań dla składnika «…».” |
| `jANOVA/R/anovarm.b.R:107` | „Nie udało się dopasować modelu powtarzanych pomiarów.” |
| `jANOVA/R/anovarm.b.R:182` | „Nie udało się obliczyć analizy ART dla powtarzanych pomiarów.” |
| `jANOVA/R/anovarm.b.R:198` | „Nie udało się obliczyć porównań ART dla składnika «…».” |
| `jANOVA/R/anovarm.b.R:218` | „Nie udało się obliczyć średnich i porównań dla składnika «…».” |
| `jRol/R/utils.R:391–392` | „Nie udało się dopasować modelu dla układu «…».” |

Zamiast rozpoznawania angielskiego tekstu wyjątku zastosować trzy warstwy:

1. Przed wywołaniem modelu sprawdzać znane warunki na danych/modelu: brak kompletnych obserwacji, liczba poziomów, puste komórki, ranga macierzy modelu i wymagane stopnie swobody — tylko gdy dany warunek uniemożliwia wybraną metodę. Zwracać własny kod przyczyny i parametry (zmienna, składnik, n), z których powstaje konkretny polski komunikat.
2. W helperach przekazywać rozpoznane problemy jako własną klasę warunku lub wynik z kodem, np. `insufficientResidualDf`, zamiast tłumaczyć `conditionMessage`. Pokazać przyczynę w odpowiednim elemencie i zachować pozostałe możliwe wyniki.
3. Dla nieprzewidzianego wyjątku użyć komunikatu kontekstowego z tabeli, bez zgadywania przyczyny. Zachować oryginalny tekst, klasę i wywołanie w szczegółach diagnostycznych/logu; wskazać użytkownikowi możliwość zgłoszenia błędu ze szczegółami. Nie ukrywać błędów programu pod wymyśloną diagnozą danych.

Test odbioru: dla każdej pozycji błąd znanej walidacji daje własny komunikat z nazwą zmiennej/składnika; nieznany wyjątek daje komunikat kontekstowy i zachowane szczegóły, niezależnie od języka R oraz liczby wierszy tabeli.

## 2. Testy t — jTestyT

| Priorytet/status | Miejsce | Warunek i obecny rezultat | Potrzebny komunikat/zachowanie |
|---|---|---|---|
| P1/K | `R/ttesttwo.b.R:17` | 0, 1 lub >2 obserwowane grupy: niewidoczny przypis do pustej tabeli | „Zmienna «G» musi mieć dokładnie 2 grupy; znaleziono k: … . Odfiltruj pozostałe grupy.” |
| P1/K | `R/ttesttwo.b.R:31` | Po usunięciu braków dla konkretnej zmiennej pozostaje jedna grupa albo n<2 w grupie; `next`, bez wiersza | Podać zmienną, obie grupy i ich n po usunięciu braków. Gdy inne zmienne dają wiersze, przypis może się pokazać, ale pominięta zmienna nadal powinna mieć wyraźny status. |
| P1/K | `R/ttestone.b.R:25`, `R/ttestpaired.b.R:32` | n<2 lub <2 kompletnych par: identyczna utrata komunikatu, jeśli brak jakiegokolwiek wiersza | „Dla «X» pozostało n obserwacji/kompletnych par; wymagane co najmniej 2.” |
| P1/R | `R/utils.R:20`, `:52`, wywołania w trzech `.b.R` | Stałe dane/różnice: `t.test` rzuca wyjątek; brak lokalnego zabezpieczenia dla pojedynczej zmiennej i testu | „Nie można obliczyć testu t dla «X»: zerowa (lub numerycznie zbyt mała) zmienność.” Zachować możliwe wyniki pozostałych zmiennych/testów. |
| P2/R | `R/utils.R:76` (`shapiroRow`) | n<3 lub n>5000 daje NA bez przyczyny; stałe x rzuca wyjątek | Osobno: „Shapiro–Wilk wymaga 3–5000 obserwacji; n=…”, „Brak zmienności”. Awaria diagnostyki nie powinna przerywać testu głównego. |
| P2/K | `R/utils.R:27`, `:60` | Ostrzeżenia Wilcoxona tłumione; niewyznaczalność dla danych zdegenerowanych nie jest opisana lokalnie | Wykrywać brak niezerowych różnic/brak zmienności rang; komunikat przy konkretnym teście. Nie zgłaszać zwykłych remisów jako automatycznego zakazu testu. |
| P2/W | Wszystkie trzy analizy | Filtry używają `!is.na`, a nie kontroli skończoności; Inf może dotrzeć do testów i wykresów | „Zmienna «X» zawiera wartości nieskończone (n=…).” Sprawdzić przejście takiej wartości przez dataset aplikacji. |

## 3. Permutacje — jperm

| Priorytet/status | Miejsce | Luka i komunikat |
|---|---|---|
| P1/K | `R/permtesttwo.b.R:18` | Dokładnie ten sam niewidoczny przypis o liczbie grup co w jTestyT. Podać faktyczną liczbę i nazwy grup. |
| P1/K | `R/permtesttwo.b.R:28`, `permtestone.b.R:22`, `permtestpaired.b.R:30` | Pominięte zmienne/pary bez wiersza; przy całkiem pustej tabeli znika informacja o małej próbie. Wymagania n powinny odpowiadać testowi permutacyjnemu, nie być kopiowane z t. |
| P2/W | `R/utils.R`, filtry w `.b.R` | Nieskończone dane mogą dać niefinitywną statystykę lub p bez lokalnego objaśnienia. Dodać kontrolę danych i wyniku; rozkład permutacyjny skupiony w punkcie sam w sobie nie musi oznaczać błędu. |

## 4. Przedziały ufności — jCI

| Priorytet/status | Miejsce | Luka i komunikat |
|---|---|---|
| P1/K | `R/cibootstrap.b.R:11`, `cipairedmeans.b.R:12`, `cicorrelation.b.R:12`, `ciregression.b.R:12` | Za mała próba/pary: przypis przed utworzeniem wierszy. Pokazać n i wymagane minimum (2 lub 4 według analizy). |
| P1/K | `R/cionemean.b.R:30,51`, `citwomeans.b.R:16`, `cidiffprop.b.R:18`, `ciproportion.b.R:13` | Brak danych lub zbyt mała grupa: analogicznie ukryty komunikat pustej tabeli. |
| P1/K | `R/utils.R:305–309` (`pickTwoLevels`) | Dwie identyczne grupy, grupa nieistniejąca, <2 dostępne poziomy: błąd jako przypis do pustej tabeli. |
| P2/K | `R/ciproportion.b.R:11`, `cidiffprop.b.R:11` | `pickLevel` zwraca NULL → cichy powrót. Rozróżnić brak kategorii w danych i nieukończony wybór; przy kompletnym wejściu: „Brak kategorii zdarzenia z obserwacjami”. |
| P2/R | `R/utils.R:78` (`ciTwoMeansWelch`), `citwomeans.b.R` | Obie grupy stałe: df i granice NaN. „Nie można wyznaczyć przedziału Welcha: wariancje obu grup są zerowe.” |
| P2/R | `R/utils.R:125` (`ciCorrelation`), `cicorrelation.b.R` | Stała zmienna: korelacja i CI niewyznaczalne, bez dedykowanego wyjaśnienia w klasie. „Korelacja wymaga zmienności obu zmiennych; «X» jest stała.” Odtworzono również bezpośrednio `ciCorrelation`: NA i ostrzeżenie R o zerowym odchyleniu standardowym. |
| P2/K | `R/utils.R:17–43` i wywołania bootstrapu poza `ciregression` | Helper zapisuje nValid/nFailed/sufficient, lecz informacja o replikach nie jest raportowana jednolicie. „Poprawnych replik k/B; nie wyznaczono CI” lub ostrzeżenie o odrzuconych replikach. Nie mylić tego z fallbackiem BCa, który już jest opisany przez `ciNote`. |
| P2/K | `R/utils.R:331–336` | CI d Cohena: niefinitywne t lub nieudane szukanie granicy zwraca NA. Podać, że nie udało się wyznaczyć CI wielkości efektu, pozostawiając możliwy wynik główny. |

Istniejące dobre zachowania: informacja o automatycznym wyborze pierwszych dwóch spośród wielu grup; komunikat fallbacku BCa → percentylowy; szczegółowa obsługa odrzuconych replik regresji. Nie są to braki walidacji. Błędy regresji dodane przed pierwszym wierszem nadal podlegają defektowi renderera.

Dodatkowe ustalenia:

| Priorytet/status | Miejsce | Luka i komunikat |
|---|---|---|
| P1/K | `jCI/R/utils.R:317` (`pickLevel`) | Jawnie podany poziom zwracany bez sprawdzenia obecności. Nie jest zastępowany pierwszym. W proporcjach może to dać zero zdarzeń; odróżnić nieistniejącą kategorię od istniejącej kategorii z zerową licznością. „Wybrana kategoria «…» nie istnieje; wybierz zdarzenie ponownie.” |
| P2/R | `jCI/R/cidiffprop.b.R:18`, `jCI/R/utils.R` (`ciDiffProportion`) | Warunek n≥1 dopuszcza pojedynczą obserwację w grupie. Dla n₁=n₂=1 oba udziały są krańcowe, więc Wald zawsze ma szerokość 0; odtworzono x₁=1, x₂=0 → [1;1]. Sama jedna grupa z n=1 nie gwarantuje zerowej szerokości całego CI. Dodać przy wyniku ostrzeżenie: „Przy tak małych grupach lub udziałach 0/1 przybliżenie Walda jest zawodne; zerowa szerokość nie oznacza braku niepewności. Rozważ przedział Newcombe’a”. Ogólna uwaga w opcjonalnym opisie metod już istnieje. Nie zastępować tego mechanicznym minimum n=2 dla wszystkich metod. |
| P2/K | `jCI/R/cibootstrap.b.R:35–41` | Dozwolone B=1 daje niewyznaczalne SE. Ogólna nota o ilustracyjnym charakterze już istnieje; uzupełnić ją o „SE wymaga co najmniej 2 replik”. |
| P2/W | `jCI/R/cipairedmeans.b.R`, `utils.R` | Stałe różnice mogą dać niewyznaczalne d, choć CI średniej różnic ma zerową szerokość. Opisać osobno d i przedział; nie uznawać całej analizy za błędną. |

## 5. ANOVA — jANOVA

| Priorytet/status | Miejsce | Luka i komunikat |
|---|---|---|
| P1/K | `R/anova.b.R:83`, `anovarm.b.R:84` | Po complete cases n<3 / n<4 → zwykły `return()`, bez jakiegokolwiek komunikatu. „Po usunięciu braków pozostało n wierszy; za mało danych do analizy.” |
| P1/K | `R/anova.b.R:85–97` | <2 poziomy, błąd dopasowania, brak df błędu: istniejące przypisy giną na pustej tabeli. |
| P1/K | `R/anovarm.b.R:87–107` | <2 poziomy, niepełny układ jednostka×pomiar, zmienny czynnik międzyobiektowy wewnątrz jednostki, błąd dopasowania: te same niewidoczne przypisy. Podać wadliwą jednostkę/czynnik i liczbę brakujących komórek. |
| P2/K | `R/anova.b.R:243–246`, `anovarm.b.R:236–239` | Błąd kontrastu przechwycony do NULL i `next`. „Nie udało się obliczyć kontrastów czynnika «…»: …”. |
| P2/K | `R/anova.b.R:280`, `anovarm.b.R:270` | Nieudane `termMeans` → NULL bez przyczyny braku wykresu interakcji. Komunikat przy wykresie. |
| P2/K | `R/anovarm.b.R:134–136` | Każdy wyjątek testu sferyczności zamieniony w sugestię, że potrzeba ≥3 poziomów. Rozdzielić brak zastosowania testu od awarii obliczeń/degeneracji danych. |
| P2/K | `R/anova.b.R:138–141,188,202,223`, `anovarm.b.R:158,182,198,218` | Niedostępny Welch/nieparametryczne, nieudany ART lub post-hoc: przypisy giną, gdy dana tabela dodatkowa nie ma wierszy. Wyświetlić przyczynę w tej sekcji. |
| P2/W | Normalność reszt, Levene/Bartlett, idealne dopasowanie | Sprawdzić zerową wariancję reszt i grup, Inf, model osobliwy. Obecne komunikaty o liczebności nie wyjaśniają wszystkich takich przypadków. |

## 6. Regresja i korelacja — jRegr

| Priorytet/status | Miejsce | Luka i komunikat |
|---|---|---|
| P1/K | `R/liniowa.b.R:12–14`, `logistyczna.b.R:12–17` | Za mało poziomów predyktora, za mała próba, odpowiedź inna niż dwukategorialna/brak jednej kategorii: niewidoczne przypisy przed wierszami. |
| P2/R | `R/utils.R:15–18`, `korelacja.b.R:47` | Stała zmienna daje NA; przypis mówi wyłącznie o n<3. Dla stałej zmiennej n≥3 nie ma wyjaśnienia. |
| P2/K | `R/korelacja.b.R:49–67` | Macierz: brak diagnostyki konkretnych par z małym n/stałą zmienną. Lista par i przyczyn albo przypisy do komórek. Dla n=3 wyjaśnić brak CI. |
| P2/K | `R/liniowa.b.R:57–74`, `logistyczna.b.R:81` | Nieudana tabela ANOVA, brak Shapiro lub VIF opisane jedynie przypisem pustej tabeli diagnostycznej. |
| P2/K | `R/utils.R:104–107` | Wyjątek testu Durbina–Watsona zamieniony na NA p bez wyjaśnienia. Podać przyczynę, nie ukrywać poprawnie obliczonej statystyki. |
| P2/W | `R/liniowa.b.R`, `utils.R:86–90` | Stała odpowiedź, idealne dopasowanie, Inf: wymagana integracyjna kontrola wyniku, β, reszt i diagnostyk, z komunikatem zależnym od problemu. |

Logistyczna ma już osobne komunikaty separacji, osobliwości i braku zbieżności oraz raportuje przechwycone ostrzeżenia GLM. Nie należy zgłaszać ich jako brakujących. Treść surowych ostrzeżeń zależy od lokalizacji R; warto dopisać objaśnienie użytkowe.

| Priorytet/status | Miejsce | Dodatkowa luka i komunikat |
|---|---|---|
| P1/K | `jRegr/R/logistyczna.b.R:15` | Nieistniejące wybrane zdarzenie zostaje zastąpione drugim poziomem. „Wybrany poziom zdarzenia «…» nie jest dostępny po filtracji.” Nie zmieniać bez informacji znaczenia OR. |
| P1/K | `jRegr/R/utils.R:68` | Nieistniejący poziom odniesienia jest ignorowany. Sprawdzić wybór względem danych użytych w modelu i wyjaśnić brak poziomu zamiast milczącego powrotu do domyślnego. |

## 7. Eksploracja — jEksplor

| Priorytet/status | Miejsce | Luka i komunikat |
|---|---|---|
| P1/K | `R/szereg.b.R:11–15` | n<2, początek klasy >min, niedodatnia szerokość: przypisy pustej tabeli niewidoczne. |
| P2/K | `R/jakosciowe.b.R:74` | Brak obserwacji: przypis bez wiersza. „Brak obserwacji «X» w grupie «G» po odfiltrowaniu braków.” |
| P2/R | `R/utils.R:76–101`, `ilosciowe.b.R:161` | Shapiro: n poza 3–5000, Lilliefors: n<5, AD: n<8, wszystkie: zerowe SD → NA. Dodać konkretną przyczynę do wyniku każdego testu. |
| P2/K | `R/utils.R:48–68` | Geometryczna/harmoniczna dla x≤0, V przy średniej 0, V kwartylowy przy medianie 0, asymetria przy zerowym mianowniku, małe n dla momentów: NA bez objaśnienia obok wyniku. Podać ograniczenie danej miary, nie blokować całej tabeli. |
| P2/R | `R/utils.R:36–43`, `ilosciowe.b.R:157` | Gini dla sumy 0/n<2 → NA; ogólna nota „G tylko dla wartości nieujemnych” nie wyjaśnia tych przypadków. Lorenz wymaga osobnej informacji przy sumie 0. |

Nie traktować samego n=0 w tabeli opisowej ani braku dominanty przy wszystkich różnych wartościach jako awarii całej analizy. Są to wyniki wymagające czytelnego objaśnienia.

| Priorytet/status | Miejsce | Dodatkowa luka i komunikat |
|---|---|---|
| P2/K | `jEksplor/R/jakosciowe.b.R:71–73` i dalsza pętla tabeli/wykresy | Setki obserwowanych kategorii zmiennej nominalnej tworzą tabelę i mozaikę bez ostrzeżenia o możliwym wyborze identyfikatora lub zmiennej ilościowej. Dodać widoczny komunikat: „Zmienna «X» ma k kategorii w n obserwacjach. Sprawdź, czy nie wybrano identyfikatora lub zmiennej ilościowej; tabela i mozaika mogą być nieczytelne”. Liczyć poziomy po filtracji; próg ostrzegania ustalić jako regułę interfejsu, nie warunek poprawności analizy. Nie scalać kategorii ani nie blokować obliczeń automatycznie. To osobna luka dydaktyczna, niezależna od limitów wydajności plots. |
| P2/K | `jEksplor/R/utils.R:118–122` | `parsePercentiles` usuwa niesparsowane wartości i liczby spoza (0,100). „Pominięto nieprawidłowe percentyle: …; podaj liczby z przedziału (0,100).” |
| P2/K | `jEksplor/R/utils.R:193–202` | Dodatnia, bardzo mała szerokość może wygenerować ogromną liczbę klas; brak ograniczenia k w tej gałęzi. Sprawdzić k przed alokacją, podać liczbę klas i poprosić o większą szerokość. Próg ustalić po pomiarach. Dla stałych danych objaśnić umowną szerokość 1. |

## 8. Częstości — jCzest

| Priorytet/status | Miejsce | Luka i komunikat |
|---|---|---|
| P1/K | `R/tabela.b.R:68`, `zgodnosc.b.R:23–36`, `zalezne.b.R:68,135,142` | Nieodpowiednia liczba kategorii, brak obserwacji, nieprawidłowe proporcje oczekiwane, nieodpowiedni układ do McNemara/Q: niewidoczne przypisy do pustych tabel. |
| P1/R | `R/utils.R:9–12` (`buildTable`) | Ujemne liczności są po cichu wykluczane. Odtworzono: liczność −1 znika z tabeli. „Kolumna liczności zawiera ujemne wartości (wiersze …); popraw dane.” Nie zmieniać milcząco zbioru wejściowego. |
| P1/K | `R/utils.R:9–18` | Brak walidacji całkowitości i skończoności counts w helperze. Ułamkowe i Inf przechodzą do tabeli. Komunikat zależny od tego, czy wspieramy wagi, czy wyłącznie liczności; dla testów dokładnych wymagane całkowite liczności. |
| P2/K | `R/utils.R:70–86` (`cramersVCI`) | <50 poprawnych replik → NA granic bez raportu o odrzuconych replikach. |
| P2/K | `R/tabela.b.R:254,297,327` | Miary tylko dla 2×2, trend tylko 2×k/k×2, porównania przy ≥3 wierszach: przypis w pustej tabeli znika. |
| P3/K | `R/tabela.b.R:216` | Nieudany Fisher opisany jako „za duża” tabela; nie każdy błąd Fishera musi wynikać z rozmiaru. Zachować przyczynę awarii. |

Są już komunikaty naruszenia warunków przybliżenia chi-kwadrat, niewykonalności dokładnego wielomianowego i braku niezgodnych par. Nie należy dopisywać ich ponownie; trzeba sprawdzić, czy w danej gałęzi mają widoczną tabelę/sekcję.

| Priorytet/status | Miejsce | Dodatkowa luka i komunikat |
|---|---|---|
| P1/K | `jCzest/R/zalezne.b.R:36–48` | Q Cochrana koduje pierwszy poziom każdej zmiennej jako 1 bez uzgodnienia znaczenia zdarzenia. Udostępnić/sprawdzić spójne kodowanie i pokazać „Zdarzenie w każdym pomiarze: …”. Różne etykiety nie muszą oznaczać błędu, lecz nie wolno ukrywać mapowania. |
| P2/K | `jCzest/R/utils.R:101–118` | Zerowe komórki mogą dawać OR=0/Inf i niewyznaczalne granice przybliżonego CI. Objaśnić wynik graniczny i niewyznaczalność CI; samo Inf nie jest błędem danych. |

## 9. Doświadczalnictwo — jRol

Dotyczy wspólnego silnika CRD, RCBD, latin i splitplot (`R/utils.R:355`), a nie czterech niezależnych implementacji.

| Priorytet/status | Miejsce | Luka i komunikat |
|---|---|---|
| P1/K | `R/utils.R:374` | n<3 po complete cases: cichy powrót. Podać n i wymóg danych. |
| P1/K | `R/utils.R:376–399` | <2 poziomy A, nieprawidłowe wymiary kwadratu łacińskiego, błąd modelu, brak df: przypisy niewidoczne przed wierszami. |
| P2/K | `R/utils.R:501–510` | Levene/Bartlett przechwycone do NULL bez osobnej przyczyny; ogólna nota o porównywaniu komórek nie jest diagnostyką awarii. |
| P2/K | `R/utils.R:520` | Brak Shapiro opisany przypisem pustej tabeli. |
| P2/W | `R/utils.R:379–386` | Kontrola kwadratu obejmuje liczbę poziomów i k², lecz nie pełny warunek wystąpienia każdego obiektu raz w każdym wierszu/kolumnie. Potrzebny scenariusz z poprawnymi wymiarami i błędnym rozmieszczeniem. |
| P3/W | `R/plan.b.R`, `planDesign` | Generator planów wymaga oddzielnego testu niepełnych/powtórzonych etykiet. Nie znaleziono podstaw do przypisania mu defektu pustej tabeli błędu. |

| Priorytet/status | Miejsce | Dodatkowa luka i komunikat |
|---|---|---|
| P1/K | `jRol/R/utils.R:221,317` | Nieistniejąca kontrola Dunnetta zastępowana pierwszym poziomem. Odróżnić wybór domyślny od utraty jawnie wybranej kontroli; wskazać brakujący poziom. |

## 10. Moduły opcjonalne — jSpace i jRISK

| Priorytet/status | Miejsce | Luka i komunikat |
|---|---|---|
| P1/K | `jSpace/R/satgroups.b.R:19` | <2 grupy lub <3 obserwacje: przypis pustej tabeli desc. |
| P1/K | `jSpace/R/geomap.b.R:23`, `jamovi/geomap.r.yaml:11` | Walidacja współrzędnych raportowana do regiony, tabeli dynamicznej i opcjonalnie ukrytej. Błąd mapy ma być widoczny niezależnie od „pokaż regiony”. |
| P1/K | `jSpace/R/tleorbit.b.R:40,74,102` | Błąd TLE/propagacji → przypisy pustych tabel; statystyki dodatkowo mają warunkową widoczność. Pokazać błąd niezależnie od „pokaż statystyki”. |
| P2/K | `jSpace/R/rasterstats.b.R:49` | Raster wyłącznie NA: cichy powrót. „Raster nie zawiera komórek z wartościami.” |
| P3/K | `jSpace/R/rasterstats.b.R:23–42` | <4 współrzędne lub błąd terra zastępowany wspólną poradą o regularnej siatce. Rozdzielić liczebność, nieregularną siatkę i błąd odczytu. Tabela staty ma 1 wiersz, więc ten przypis nie podlega defektowi pustego body. |
| P2/W | `jSpace/R/satclassify.b.R:29–34` | Losowy podział może usunąć rzadką klasę z treningu/testu. Potrzebne ostrzeżenie o klasie i części danych oraz obsługa niewykonalnego modelu. Początkowy komunikat n<20/<2 klas jest w tabeli z 2 wierszami: nie zaliczać do znikających przypisów. |
| P2/W | `jSpace/R/satgroups.b.R:36–60` | n=liczba grup, stałe dane, Inf: możliwe niewyznaczalne F/H; brak dedykowanej kontroli przed testami. |

**jRISK:** w pięciu analizach wykryto poprawne użycie `setError` dla blokujących problemów: brak kompletnych danych, ujemne czasy, awarie w t=0, wyłącznie cenzurowanie, p/reliability poza [0,1], błędne k/n i ograniczenia układu. `lifetime.b.R:176–184` wpisuje brak zbieżności w wiersz modelu; osobliwy Hessian jest objaśniony. Nie potwierdzono tu analogicznej masowej regresji. Pozostają W: nieskończone czasy/probability oraz błędy zależności — wymagają prób integracyjnych. Dalsze ustalenia dotyczące wyboru kategorii i opisu binaryzacji znajdują się w tabeli poniżej; dobra obsługa błędów wejścia nie oznacza certyfikacji wszystkich opcji.

| Priorytet/status | Miejsce | Dodatkowa luka i komunikat |
|---|---|---|
| P1/K | `jRISK/R/lifetime.b.R:120–122`, `bernoulli.b.R:18–30`, `eventtables.b.R:18–23` | Jawnie wybrany nieistniejący poziom nie jest walidowany; porównanie może dać zero zdarzeń. Domyślny wybór następuje tylko przy NULL. Podać brak kategorii; nie mylić go z obserwowanym brakiem awarii/sukcesów. |
| P2/K | Te same analizy jRISK; proporcje jCI | Kodowanie „zdarzenie kontra pozostałe” dla wielu kategorii może być zamierzone. Zapewnić jawny opis kategorii zdarzenia i dopełnienia. Dla statusu cenzury wymaga szczególnie uważnej kontroli znaczenia pozostałych kategorii. |
| P2/K | `jSpace/R/satclassify.b.R:117–146` | Wykres granic ponownie dopasowuje model na dwóch pierwszych predyktorach. Przy większym modelu dopisać: „Granice dla osobnego modelu z predyktorami X i Y; miary tabelaryczne dotyczą pełnego modelu”. |
| P2/R | `jSpace/R/satclassify.b.R:49`, `class::knn` | k większe od liczby obiektów treningowych w lokalnym R daje wynik z ostrzeżeniem, nie wyjątek. Jawnie sprawdzić k względem treningu i wyjaśnić wymóg/korektę. Zachowanie potwierdzić w wersji pakietu z dystrybucji. |

## 11. Rozkłady — jdistrACTION

Przejrzano wszystkie 11 analiz. Komunikaty kolejności x1/x2 używają `setError`; gamma, Weibull i wykładniczy mają dodatkową kontrolę ujemnego x1, ujemny dwumianowy kontroluje całkowitość r. Nie należy zgłaszać tych komunikatów jako brakujących.

| Priorytet/status | Miejsce | Luka i komunikat |
|---|---|---|
| P1/K | `jamovi/binomialdistribution.a.yaml:64–73`, `R/binomialdistribution.b.R:29–31` | n oraz prawdopodobieństwo modelu dp2 bez zakresów i lokalnej walidacji przed obliczeniami. „n musi być nieujemną liczbą całkowitą”; „Prawdopodobieństwo sukcesu musi należeć do [0,1]”. Ograniczenie osobnego parametru p dotyczy kwantyla, nie dp2. |
| P1/K | `jamovi/normaldistribution.a.yaml:66`, `R/normaldistribution.b.R:99` | SD bez dolnej granicy/komunikatu. Ujemne SD jest błędne; zerowe wymaga jawnej decyzji o obsłudze rozkładu zdegenerowanego, zamiast awarii wykresu. |
| P1/K | `jamovi/fdistribution.a.yaml:51–64` | Brak ograniczeń df1, df2 i niecentralności λ. „Stopnie swobody muszą być dodatnie; parametr niecentralności nieujemny.” |
| P2/K | `R/chi2distribution.b.R:277–280`, `R/fdistribution.b.R:299–302` | Brak komunikatu dla x1<0: oba moduły sprawdzają kolejność końców przedziału, lecz nie mają odpowiednika `setError` obecnego w wykładniczym, gamma i Weibullu. Ujednolicić informowanie o nośniku: „Rozkład przyjmuje wartości nieujemne; x1=… leży poza nośnikiem”. Nie dopisywać automatycznie blokady: np. P(X≤−1)=0 jest poprawnie określone, a przedział zaczynający się poniżej 0 może zawierać część nośnika. Jeżeli interfejs ma ograniczać x do nośnika, jawnie ustalić wspólną regułę dla tych pięciu analiz. Brak komunikatu jest potwierdzony; wybór `setError` zamiast informacji wymaga tej decyzji. |
| P2/W | Wszystkie analizy z kwantylami i momentami | p=0/1 może legalnie dać nieskończony kwantyl; nieistniejące momenty t/F wymagają objaśnienia, nie ogólnego błędu. Zweryfikować wykresy i etykiety dla krańców oraz istniejące objaśnienia momentów. |

Dla gamma, Weibulla, wykładniczego, Poissona, geometrycznego i ujemnego dwumianowego istnieją ograniczenia parametrów w YAML. Nie wpisywać ogólnego „brak walidacji dodatniości we wszystkich rozkładach”.

## 12. Wykresy — plots/scatr

Przegląd objął 26 `.b.R`. Typowy `return(FALSE)` dla `image$state == NULL` przed wybraniem zmiennych jest prawidłowym stanem początkowym. Poniżej powroty po otrzymaniu danych, które wymagają informacji:

| Priorytet/status | Miejsce | Luka i komunikat |
|---|---|---|
| P2/K | `R/raincloud.b.R:51–53`, `stackbar.b.R:55`, `treemap.b.R:97`, `wordcloud.b.R:58` | Dane puste po filtracji → brak wykresu bez „Brak obserwacji po usunięciu braków/odfiltrowaniu danych”. |
| P2/K | `R/waffle.b.R:87,156`, `mosaic.b.R:57` | Brak dodatniej sumy/liczności lub kafelków: cichy powrót. „Nie można narysować wykresu: suma liczności wynosi 0.” |
| P2/K | `R/radar.b.R:68`, `parcoord.b.R:82` | Zbyt mało zmiennych lub puste dane: brak informacji o wymaganych ≥3/≥2 zmiennych i kompletnych obserwacjach. |
| P2/K | `R/wordcloud.b.R:70–72` | Nie umieszczono żadnego słowa: cichy powrót. „Nie udało się rozmieścić słów; zmniejsz rozmiar tekstu/liczbę słów.” |
| P2/K | `R/corrgram.b.R:30` | `cor` dla stałych zmiennych/braku wspólnych obserwacji; brak objaśnienia niewyznaczalnych komórek. Podać nazwy zmiennych/par. |
| P2/W | dens, ridge, violin, raincloud; qq; jmvhist; bubble/area/heatmap/lollipop/circbar/pareto/jmvbar/jmvbox/jmvline/scat/hexbin/stripmean | Sprawdzić małe grupy, brak zmienności, niefinitywne dane, niedopuszczalne rozmiary/szerokości. Wyszukanie wywołania ggplot bez `tryCatch` nie dowodzi niewidocznego błędu: renderer obrazów może obsłużyć wyjątek. Wymagane próby GUI, nie deklaracja 26 potwierdzonych defektów. |

## 13. Rdzeń upstream — jmv; dane i pozostałości

Przeskanowano wszystkie `.b.R` rdzenia, w tym ukryte w menu analizy zastąpione przez moduły jUPWR. Upstream częściej stosuje `reject`, istniejące wiersze i przypisy do konkretnych komórek. Przykłady działającej walidacji: liczba grup w t, ujemne/nieskończone liczności w tabelach, nieskończoności i brak wariancji w reliability, walidacja zmiennych ANOVA RM/ANCOVA. Nie ma podstaw do uznania wszystkich starych analiz za dotknięte tą samą regresją.

- **P2/K:** `jmv/R/ttestps.b.R:206–212`: wyjątek Shapiro dla stałych różnic zostaje połknięty; W/p pozostają puste, bez przypisu przyczyny. Istnieją przypisy n<3 i n>5000, więc luka dotyczy innej gałęzi.
- **P2/W:** `simplecorr.b.R:30–40`: ogólne „Nie udało się obliczyć korelacji” nie rozróżnia stałej zmiennej, braku par i błędu numerycznego; przypadek `cor.test` zwracającego NA z ostrzeżeniem może ominąć gałąź błędu. Tabela ma wcześniej zdefiniowany wiersz, więc nie utożsamiać tego z jTestyT.
- Dla pozostałych analiz jmv wynik to przegląd ścieżek diagnostycznych, nie pełna walidacja wszystkich metod i opcji. Indeks pozwala zaplanować sprawdzanie dalszych przypadków; nie znaleziono podstaw do dopisania konkretnych braków bez dalszej reprodukcji.
- **jDane** nie ma analiz R — komunikaty testów statystycznych nie dotyczą tego modułu.
- **jboot** jest pozostałością po migracji do jCI; w checkoutcie brak źródłowych `.b.R`. Nie traktować wygenerowanych `.h.R` jako osobnego aktualnego silnika do naprawy.

## 14. Scenariusze wymagające reprodukcji — W

Poniższe tropy zachowano z przeglądu szczegółowego. Nie są potwierdzonymi błędami blokującymi. Reprodukcja powinna zapisać dane, opcje, wersje pakietów, faktyczny wynik i oczekiwany komunikat.

| Obszar | Scenariusze do sprawdzenia |
|---|---|
| Cykl życia wyników | `jCI/cionemean`: obrazy dodawane w `.run`; `jCzest/tabela` i `zalezne`: `.colsBuilt`/`.pairsBuilt`; zapisane kolumny jRegr. Zmiana zmiennych/opcji, filtrów, ponowne otwarcie analizy. Same prywatne flagi nie dowodzą pozostawania starych wyników. |
| ANOVA i jRol | Puste komórki, powtórzenie zmiennej w rolach, model osobliwy, Welch-James, Friedman, błędy mvtnorm, `rmMseFor`, kolizje etykiet „a b”/„a.b”. Split-plot z jednym blokiem: sprawdzić błąd dopasowania i osobno df obu warstw; nie zakładać z góry, że obliczenia dochodzą do F=NaN. |
| jCzest | Zerowe marginesy i oczekiwane liczności, ułamkowe counts w testach dokładnych, kierunek OR w porównaniach McNemara, hipoteza jednostronna przy ≥3 kategoriach, miary porządkowe dla kategorii bez porządku. |
| Małe próby i degeneracja | Pozostałe scenariusze Walda na krańcach proporcji (przypadek n₁=n₂=1 potwierdzony w sekcji 4), CI dla stałych danych, remisy Spearmana, idealne dopasowanie regresji, puste mianowniki miar klasyfikacji, mało zdarzeń. Nie ustanawiać uniwersalnych zakazów typu EPV<10 czy df<5 bez uzasadnienia dla metody. |
| Wydajność | Enumeracja jperm i dokładnego wielomianowego jCzest, liczba klas jEksplor, plan jRol, wiele poziomów/facetów plots, materializacja rastra i diagramy jRISK. Istniejące progi nie dowodzą zawieszenia: zmierzyć koszt, potem ustalić limity i komunikat. |
| jSpace | Częściowe odrzucanie współrzędnych poza zakresem, treść/suma kontrolna TLE, odczyt wbudowanych plików i zależności, utrata klasy po podziale, brak df satgroups. Heurystyka „poza lądem” nie jest wiarygodnym uniwersalnym testem zamiany lon/lat. |
| plots | Puste/małe grupy w gęstościach, suma 0 w Pareto, ujemne liczności waffle, NaN po standaryzacji QQ, komórki heatmap z samymi NA, faktor na osi x jmvline, brak pakietu hexbin. Sprawdzić konkretny komunikat frameworka i wersję ggplot. |
| Objaśnienia i interfejs | Etykiety bez polskich znaków, rzeczywiste tłumaczenia plots, binarizacja w jRISK/jCI, KM bez wyznaczalnej mediany, brak statusu cenzury, etykiety i ignorowane opcje generatora planów, skumulowane częstości nominalnych, nakładanie list wartości skrajnych. Angielski literał `.()` nie dowodzi braku tłumaczenia. |

## 15. Wnioski odrzucone lub skorygowane po weryfikacji

Nie przenosić poniższych tez do listy napraw jako potwierdzonych awarii:

| Teza | Ustalenie |
|---|---|
| Brak `arrange(x)` psuje jmvline | `geom_line` sortuje po x w grupie. Próba R na x=3,1,2 potwierdziła kolejność 1,2,3. Nie dodawać naprawy „spaghetti” na tej podstawie. |
| `lvAll` i `lv` wskazują inne pary w jTestyT/jperm | Globalnie wymagane są dwie grupy, a zmienna tracąca grupę jest pomijana. Nie wykazano rozbieżności opisywanej pary. |
| Levene nie działa, gdy jedna grupa ma SD=0 | Próba `car::leveneTest` dla grup (1,1,1) i (2,3,4) zwróciła F=4 i p≈0,116. Nadal trzeba obsłużyć rzeczywiste awarie diagnostyki. |
| r=±1 powoduje awarię CI przez `atanh` | `ciCorrelation` zwróciło [1;1] i [−1;−1]. Stała zmienna pozostaje odrębnym potwierdzonym problemem. |
| CI regresji dochodzi do wykresu z `band=NULL` wskutek zbyt małej liczby poprawnych replik | Nie wykazano osiągalności: analiza wymaga ≥50 poprawnych wspólnie estymowalnych par współczynników, helper pasma ≥2. |
| TLE dopuszcza krok=0, r1–r8 nie mają zakresu | YAML już wymusza krok≥0,1 i r1–r8 w [0,1]. |
| `toNumeric` zawsze zamienia nominalne kody na NA | `jmvcore/R/utils.R:1177` zachowuje liczby i wykorzystuje atrybut `values`. Trzeba badać rzeczywisty typ danych, a nie zakładać konwersję. |
| Każdy wynik p=1 testu permutacyjnego oznacza błąd | Dla zerowych różnic wynik jest oczekiwany. Ewentualna informacja o degeneracji, nie blokada testu. |
| Permutacje wymagają n≥2 w każdej grupie jak t | Nie kopiować tego ograniczenia. Próba dokładna z grupami n=1 i n=2 zwróciła skończone p=2/3. |
| Zero poza nośnikiem, nieskończony OR lub zerowa szerokość CI zawsze oznaczają błędną analizę | Rozdzielić poprawny wynik graniczny od niewyznaczalności lub słabości przybliżenia. Podobnie wykres X względem X i drzewo z samym korzeniem mogą być poprawne. |
| Brak `tryCatch` oznacza brak kanału błędu | Framework obsługuje wyjątki. Lokalna obsługa ma chronić pozostałe wyniki i dawać lepszy opis, nie maskować wszystkie wyjątki. |
| k-NN z k>n treningu zawsze rzuca wyjątek | Lokalnie zwrócił wynik i ostrzeżenie; nadal potrzebna jawna diagnostyka parametru. |


## 16. Kolejność prac i scenariusze odbioru

1. Naprawić renderowanie/odświeżanie stopki pustych tabel. Test widoku: rows=0 + note, rows>0 + note, poprawny→błędny→poprawny, usunięcie nieaktualnego przypisu. To test zachowania, którego testy samych obliczeń R nie pokrywają.
2. Wybrać istniejący programowy Notice / błąd elementu / reject zgodnie z zakresem awarii; deklaratywny Notice dopiero po poprawieniu walidacji kompilatora. W nowych modułach przenieść błędy blokujące z przypisów do stałego widocznego kanału diagnostycznego; błędy pojedynczej zmiennej/metody pozostawić lokalne. Sprawdzić widoczność niezależnie od checkboxów tabel i metod.
3. Uzupełnić ciche `return`/`next` i NA o przyczyny z tabel powyżej. Priorytet: jTestyT, jperm, jANOVA, jRegr, jCI, jCzest; równolegle w kolejce napraw liczyć walidację counts i parametrów rozkładów.
4. Ujednolicić rozwiązywanie wybranych poziomów: brak wyboru, nieistniejący poziom i istniejący poziom z zerową licznością to trzy różne sytuacje. Nie podmieniać jawnego wyboru bez informacji.
5. Dodać próby graniczne: 0/1/2/3 grupy; grupa znikająca dopiero po usunięciu braków Y; n=0/1/2 i kompletne pary; stałe x i stałe różnice; wartości Inf; modele bez df; błędne counts; niedostępny post-hoc; nieudane replikacje; puste wykresy; nieprawidłowe dane TLE/raster/układ doświadczenia.
6. Dla każdego problemu sprawdzić osobno pierwsze uruchomienie i zmianę wcześniej poprawnej analizy. W analizie wielozmiennej wadliwa zmienna nie może zabierać wyników poprawnych zmiennych. Jeśli wynik zostaje niewyznaczony, użytkownik musi wiedzieć dlaczego.
7. Po poprawkach: Docker build i sprawdzenie GUI, potem właściwe paczki systemowe. Sam sukces kompilacji modułów nie weryfikuje komunikatów błędów.

Lokalne próby R odtworzyły: wyjątek t dla stałych danych; NA Shapiro przy n=2; wyjątek Shapiro dla stałych danych; NA korelacji stałej zmiennej; NaN df/CI Welcha dla dwóch stałych grup; NA Lillieforsa/AD przy zbyt małej próbie; NA Giniego przy sumie 0; odrzucenie ujemnej liczności. Nie uruchamiano pełnych testów klas ani renderowania w przeglądarce.

## Załącznik A. Indeks 107 analiz

Indeks mechaniczny, nie lista potwierdzonych defektów. Liczby oznaczają linie źródeł w audytowanym checkoutcie. Brak lokalnej diagnostyki nie wyklucza obsługi w helperze lub frameworku.

### jmv — 30 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|
| [ancova](../../jmv/R/ancova.b.R) | 132, 136, 280, 353, 961, 969, 977, 983, 991, 1001, 1007 | 76, 99, 103, 108, 113, 122, 138, 450, 469, 590, 688, 1065 | [YAML](../../jmv/jamovi/ancova.r.yaml) |
| [anova](../../jmv/R/anova.b.R) | — | — | [YAML](../../jmv/jamovi/anova.r.yaml) |
| [anovanp](../../jmv/R/anovanp.b.R) | 256, 262, 268, 274, 279 | — | [YAML](../../jmv/jamovi/anovanp.r.yaml) |
| [anovaonew](../../jmv/R/anovaonew.b.R) | 181, 295 | 280 | [YAML](../../jmv/jamovi/anovaonew.r.yaml) |
| [anovarm](../../jmv/R/anovarm.b.R) | 114, 123, 176, 228, 517, 520, 580, 582, 600, 999, 1007, 1012, 1025, 1030, 1034, 1387, 1402, 1413, 1425 | 100, 101, 110, 678, 690, 795, 843 | [YAML](../../jmv/jamovi/anovarm.r.yaml) |
| [anovarmnp](../../jmv/R/anovarmnp.b.R) | — | — | [YAML](../../jmv/jamovi/anovarmnp.r.yaml) |
| [cfa](../../jmv/R/cfa.b.R) | 148, 191, 224, 855 | 688, 712, 720, 818 | [YAML](../../jmv/jamovi/cfa.r.yaml) |
| [conttables](../../jmv/R/conttables.b.R) | 263, 265, 269, 271, 273, 510, 512, 515, 518, 553, 555, 575, 576, 579, 580, 588, 589, 590, 591, 614 | 326, 328, 329, 346, 352, 359, 363, 375, 790, 791, 810 | [YAML](../../jmv/jamovi/conttables.r.yaml) |
| [conttablespaired](../../jmv/R/conttablespaired.b.R) | 37, 39, 45, 47, 136, 153, 162, 171 | 105, 106, 107 | [YAML](../../jmv/jamovi/conttablespaired.r.yaml) |
| [corrmatrix](../../jmv/R/corrmatrix.b.R) | 86, 89, 92, 95, 98, 101, 105, 159 | 196, 206, 223, 236, 247, 299 | [YAML](../../jmv/jamovi/corrmatrix.r.yaml) |
| [corrpart](../../jmv/R/corrpart.b.R) | 110, 117, 119, 122, 124, 127, 129, 133, 139 | 264, 270 | [YAML](../../jmv/jamovi/corrpart.r.yaml) |
| [descriptives](../../jmv/R/descriptives.b.R) | 364, 422, 464, 525, 809, 843, 862, 909, 1350, 1355 | — | [YAML](../../jmv/jamovi/descriptives.r.yaml) |
| [efa](../../jmv/R/efa.b.R) | — | — | [YAML](../../jmv/jamovi/efa.r.yaml) |
| [empty](../../jmv/R/empty.b.R) | — | — | [YAML](../../jmv/jamovi/empty.r.yaml) |
| [linreg](../../jmv/R/linreg.b.R) | 235, 437, 480, 509, 519, 779, 858, 922, 962, 1034, 1095, 1498 | 406, 419, 1115 | [YAML](../../jmv/jamovi/linreg.r.yaml) |
| [loglinear](../../jmv/R/loglinear.b.R) | 632, 641 | 67, 86, 87, 486 | [YAML](../../jmv/jamovi/loglinear.r.yaml) |
| [logregbin](../../jmv/R/logregbin.b.R) | 251, 359, 476, 583, 709, 724, 821, 899, 979, 1364 | 356, 444, 458 | [YAML](../../jmv/jamovi/logregbin.r.yaml) |
| [logregmulti](../../jmv/R/logregmulti.b.R) | 213, 299, 552, 724, 825, 1024 | 801, 810 | [YAML](../../jmv/jamovi/logregmulti.r.yaml) |
| [logregord](../../jmv/R/logregord.b.R) | 207, 302, 310, 467, 693 | — | [YAML](../../jmv/jamovi/logregord.r.yaml) |
| [mancova](../../jmv/R/mancova.b.R) | 101, 114, 119, 313, 314, 325, 326, 388 | 110, 189 | [YAML](../../jmv/jamovi/mancova.r.yaml) |
| [pca](../../jmv/R/pca.b.R) | 143, 215, 231, 600 | 168, 175, 567 | [YAML](../../jmv/jamovi/pca.r.yaml) |
| [proptest2](../../jmv/R/proptest2.b.R) | 95, 290 | 54, 309 | [YAML](../../jmv/jamovi/proptest2.r.yaml) |
| [proptestn](../../jmv/R/proptestn.b.R) | — | 18, 55 | [YAML](../../jmv/jamovi/proptestn.r.yaml) |
| [qualitative](../../jmv/R/qualitative.b.R) | — | — | [YAML](../../jmv/jamovi/qualitative.r.yaml) |
| [reliability](../../jmv/R/reliability.b.R) | 115, 164, 293, 295, 297 | 67, 75, 83, 102 | [YAML](../../jmv/jamovi/reliability.r.yaml) |
| [simplecorr](../../jmv/R/simplecorr.b.R) | 24, 36, 56 | 30, 31 | [YAML](../../jmv/jamovi/simplecorr.r.yaml) |
| [ttestis](../../jmv/R/ttestis.b.R) | 31, 38, 44, 50, 105, 110, 154, 173, 225, 323, 358, 417, 519, 521, 523 | 100, 129, 184, 255, 265, 344, 398 | [YAML](../../jmv/jamovi/ttestis.r.yaml) |
| [ttestones](../../jmv/R/ttestones.b.R) | 104, 179, 186, 192, 266, 313, 315, 317 | 70, 126, 127, 199, 251 | [YAML](../../jmv/jamovi/ttestones.r.yaml) |
| [ttestps](../../jmv/R/ttestps.b.R) | 134, 163, 190, 200, 203, 268, 353, 358, 363 | 76, 88, 206, 256 | [YAML](../../jmv/jamovi/ttestps.r.yaml) |
| [weights](../../jmv/R/weights.b.R) | — | — | [YAML](../../jmv/jamovi/weights.r.yaml) |

Funkcje pomocnicze (bez plików generowanych): [00jmv.R](../../jmv/R/00jmv.R), [00meta.R](../../jmv/R/00meta.R), [constants.R](../../jmv/R/constants.R), [data.R](../../jmv/R/data.R), [errors.R](../../jmv/R/errors.R), [utils.R](../../jmv/R/utils.R), [utilsanova.R](../../jmv/R/utilsanova.R), [utilsreg.R](../../jmv/R/utilsreg.R)

### plots — 26 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|
| [area](../../plots/R/area.b.R) | — | — | [YAML](../../plots/jamovi/area.r.yaml) |
| [bubble](../../plots/R/bubble.b.R) | — | — | [YAML](../../plots/jamovi/bubble.r.yaml) |
| [circbar](../../plots/R/circbar.b.R) | — | — | [YAML](../../plots/jamovi/circbar.r.yaml) |
| [corrgram](../../plots/R/corrgram.b.R) | — | — | [YAML](../../plots/jamovi/corrgram.r.yaml) |
| [dens](../../plots/R/dens.b.R) | — | — | [YAML](../../plots/jamovi/dens.r.yaml) |
| [heatmap](../../plots/R/heatmap.b.R) | — | — | [YAML](../../plots/jamovi/heatmap.r.yaml) |
| [hexbin](../../plots/R/hexbin.b.R) | — | — | [YAML](../../plots/jamovi/hexbin.r.yaml) |
| [jmvbar](../../plots/R/jmvbar.b.R) | — | — | [YAML](../../plots/jamovi/jmvbar.r.yaml) |
| [jmvbox](../../plots/R/jmvbox.b.R) | — | — | [YAML](../../plots/jamovi/jmvbox.r.yaml) |
| [jmvhist](../../plots/R/jmvhist.b.R) | — | — | [YAML](../../plots/jamovi/jmvhist.r.yaml) |
| [jmvline](../../plots/R/jmvline.b.R) | — | — | [YAML](../../plots/jamovi/jmvline.r.yaml) |
| [lollipop](../../plots/R/lollipop.b.R) | — | — | [YAML](../../plots/jamovi/lollipop.r.yaml) |
| [mosaic](../../plots/R/mosaic.b.R) | — | — | [YAML](../../plots/jamovi/mosaic.r.yaml) |
| [parcoord](../../plots/R/parcoord.b.R) | — | — | [YAML](../../plots/jamovi/parcoord.r.yaml) |
| [pareto](../../plots/R/pareto.b.R) | — | — | [YAML](../../plots/jamovi/pareto.r.yaml) |
| [qq](../../plots/R/qq.b.R) | — | — | [YAML](../../plots/jamovi/qq.r.yaml) |
| [radar](../../plots/R/radar.b.R) | — | — | [YAML](../../plots/jamovi/radar.r.yaml) |
| [raincloud](../../plots/R/raincloud.b.R) | — | — | [YAML](../../plots/jamovi/raincloud.r.yaml) |
| [ridge](../../plots/R/ridge.b.R) | — | — | [YAML](../../plots/jamovi/ridge.r.yaml) |
| [scat](../../plots/R/scat.b.R) | — | — | [YAML](../../plots/jamovi/scat.r.yaml) |
| [stackbar](../../plots/R/stackbar.b.R) | — | — | [YAML](../../plots/jamovi/stackbar.r.yaml) |
| [stripmean](../../plots/R/stripmean.b.R) | — | — | [YAML](../../plots/jamovi/stripmean.r.yaml) |
| [treemap](../../plots/R/treemap.b.R) | — | — | [YAML](../../plots/jamovi/treemap.r.yaml) |
| [violin](../../plots/R/violin.b.R) | — | — | [YAML](../../plots/jamovi/violin.r.yaml) |
| [waffle](../../plots/R/waffle.b.R) | — | — | [YAML](../../plots/jamovi/waffle.r.yaml) |
| [wordcloud](../../plots/R/wordcloud.b.R) | — | — | [YAML](../../plots/jamovi/wordcloud.r.yaml) |

Funkcje pomocnicze (bez plików generowanych): [syntax.R](../../plots/R/syntax.R), [utils.R](../../plots/R/utils.R)

### jdistrACTION — 11 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|
| [binomialdistribution](../../jdistrACTION/R/binomialdistribution.b.R) | 332 | — | [YAML](../../jdistrACTION/jamovi/binomialdistribution.r.yaml) |
| [chi2distribution](../../jdistrACTION/R/chi2distribution.b.R) | 279 | — | [YAML](../../jdistrACTION/jamovi/chi2distribution.r.yaml) |
| [exponentialdistribution](../../jdistrACTION/R/exponentialdistribution.b.R) | 189, 192 | — | [YAML](../../jdistrACTION/jamovi/exponentialdistribution.r.yaml) |
| [fdistribution](../../jdistrACTION/R/fdistribution.b.R) | 301 | — | [YAML](../../jdistrACTION/jamovi/fdistribution.r.yaml) |
| [gammadistribution](../../jdistrACTION/R/gammadistribution.b.R) | 199, 202 | — | [YAML](../../jdistrACTION/jamovi/gammadistribution.r.yaml) |
| [geometricdistribution](../../jdistrACTION/R/geometricdistribution.b.R) | 201 | — | [YAML](../../jdistrACTION/jamovi/geometricdistribution.r.yaml) |
| [negbinomialdistribution](../../jdistrACTION/R/negbinomialdistribution.b.R) | 23, 193 | — | [YAML](../../jdistrACTION/jamovi/negbinomialdistribution.r.yaml) |
| [normaldistribution](../../jdistrACTION/R/normaldistribution.b.R) | 331 | — | [YAML](../../jdistrACTION/jamovi/normaldistribution.r.yaml) |
| [poissondistribution](../../jdistrACTION/R/poissondistribution.b.R) | 173 | — | [YAML](../../jdistrACTION/jamovi/poissondistribution.r.yaml) |
| [tdistribution](../../jdistrACTION/R/tdistribution.b.R) | 371 | — | [YAML](../../jdistrACTION/jamovi/tdistribution.r.yaml) |
| [weibulldistribution](../../jdistrACTION/R/weibulldistribution.b.R) | 198, 201 | — | [YAML](../../jdistrACTION/jamovi/weibulldistribution.r.yaml) |

Funkcje pomocnicze (bez plików generowanych): 

### jCI — 8 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|
| [cibootstrap](../../jCI/R/cibootstrap.b.R) | 11, 41 | — | [YAML](../../jCI/jamovi/cibootstrap.r.yaml) |
| [cicorrelation](../../jCI/R/cicorrelation.b.R) | 12 | — | [YAML](../../jCI/jamovi/cicorrelation.r.yaml) |
| [cidiffprop](../../jCI/R/cidiffprop.b.R) | 18 | — | [YAML](../../jCI/jamovi/cidiffprop.r.yaml) |
| [cionemean](../../jCI/R/cionemean.b.R) | 30, 51 | — | [YAML](../../jCI/jamovi/cionemean.r.yaml) |
| [cipairedmeans](../../jCI/R/cipairedmeans.b.R) | 12 | — | [YAML](../../jCI/jamovi/cipairedmeans.r.yaml) |
| [ciproportion](../../jCI/R/ciproportion.b.R) | 13 | — | [YAML](../../jCI/jamovi/ciproportion.r.yaml) |
| [ciregression](../../jCI/R/ciregression.b.R) | 12, 14, 35, 40, 61 | — | [YAML](../../jCI/jamovi/ciregression.r.yaml) |
| [citwomeans](../../jCI/R/citwomeans.b.R) | 16 | — | [YAML](../../jCI/jamovi/citwomeans.r.yaml) |

Funkcje pomocnicze (bez plików generowanych): [utils.R](../../jCI/R/utils.R)

### jperm — 3 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|
| [permtestone](../../jperm/R/permtestone.b.R) | 22, 30 | — | [YAML](../../jperm/jamovi/permtestone.r.yaml) |
| [permtestpaired](../../jperm/R/permtestpaired.b.R) | 30, 38 | — | [YAML](../../jperm/jamovi/permtestpaired.r.yaml) |
| [permtesttwo](../../jperm/R/permtesttwo.b.R) | 18, 28, 36 | — | [YAML](../../jperm/jamovi/permtesttwo.r.yaml) |

Funkcje pomocnicze (bez plików generowanych): [utils.R](../../jperm/R/utils.R)

### jDane — 0 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|

Funkcje pomocnicze (bez plików generowanych): 

### jCzest — 3 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|
| [tabela](../../jCzest/R/tabela.b.R) | 68, 204, 216, 224, 254, 297, 327 | 44, 214 | [YAML](../../jCzest/jamovi/tabela.r.yaml) |
| [zalezne](../../jCzest/R/zalezne.b.R) | 68, 98, 135, 139, 142, 153, 158, 183 | 29 | [YAML](../../jCzest/jamovi/zalezne.r.yaml) |
| [zgodnosc](../../jCzest/R/zgodnosc.b.R) | 23, 27, 33, 132, 135, 141 | 122 | [YAML](../../jCzest/jamovi/zgodnosc.r.yaml) |

Funkcje pomocnicze (bez plików generowanych): [utils.R](../../jCzest/R/utils.R)

### jEksplor — 3 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|
| [ilosciowe](../../jEksplor/R/ilosciowe.b.R) | 157 | — | [YAML](../../jEksplor/jamovi/ilosciowe.r.yaml) |
| [jakosciowe](../../jEksplor/R/jakosciowe.b.R) | 74 | — | [YAML](../../jEksplor/jamovi/jakosciowe.r.yaml) |
| [szereg](../../jEksplor/R/szereg.b.R) | 11, 13, 15, 41, 43 | — | [YAML](../../jEksplor/jamovi/szereg.r.yaml) |

Funkcje pomocnicze (bez plików generowanych): [utils.R](../../jEksplor/R/utils.R)

### jRegr — 3 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|
| [korelacja](../../jRegr/R/korelacja.b.R) | 47, 48, 67 | — | [YAML](../../jRegr/jamovi/korelacja.r.yaml) |
| [liniowa](../../jRegr/R/liniowa.b.R) | 12, 14, 43, 53, 58, 69, 74 | 57 | [YAML](../../jRegr/jamovi/liniowa.r.yaml) |
| [logistyczna](../../jRegr/R/logistyczna.b.R) | 12, 13, 17, 27, 28, 29, 31, 33, 34, 61, 70, 81 | — | [YAML](../../jRegr/jamovi/logistyczna.r.yaml) |

Funkcje pomocnicze (bez plików generowanych): [utils.R](../../jRegr/R/utils.R)

### jANOVA — 2 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|
| [anova](../../jANOVA/R/anova.b.R) | 42, 45, 48, 61, 63, 85, 92, 96, 132, 138, 141, 163, 178, 188, 202, 204, 223, 263, 271 | 89, 140, 187, 201, 220, 243, 280 | [YAML](../../jANOVA/jamovi/anova.r.yaml) |
| [anovarm](../../jANOVA/R/anovarm.b.R) | 43, 46, 49, 62, 64, 87, 92, 97, 101, 107, 130, 136, 142, 158, 172, 182, 198, 200, 218, 256, 257, 265 | 105, 134, 181, 196, 216, 236, 259, 270 | [YAML](../../jANOVA/jamovi/anovarm.r.yaml) |

Funkcje pomocnicze (bez plików generowanych): [utils.R](../../jANOVA/R/utils.R)

### jTestyT — 3 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|
| [ttestone](../../jTestyT/R/ttestone.b.R) | 25, 34 | — | [YAML](../../jTestyT/jamovi/ttestone.r.yaml) |
| [ttestpaired](../../jTestyT/R/ttestpaired.b.R) | 32, 45 | — | [YAML](../../jTestyT/jamovi/ttestpaired.r.yaml) |
| [ttesttwo](../../jTestyT/R/ttesttwo.b.R) | 18, 31, 45 | — | [YAML](../../jTestyT/jamovi/ttesttwo.r.yaml) |

Funkcje pomocnicze (bez plików generowanych): [utils.R](../../jTestyT/R/utils.R)

### jRISK — 5 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|
| [bernoulli](../../jRISK/R/bernoulli.b.R) | 23, 34, 48 | — | [YAML](../../jRISK/jamovi/bernoulli.r.yaml) |
| [eventtables](../../jRISK/R/eventtables.b.R) | 27, 73, 91 | — | [YAML](../../jRISK/jamovi/eventtables.r.yaml) |
| [fta](../../jRISK/R/fta.b.R) | 28, 32, 37, 57, 75 | — | [YAML](../../jRISK/jamovi/fta.r.yaml) |
| [lifetime](../../jRISK/R/lifetime.b.R) | 46, 131, 135, 139, 147, 151, 200, 206 | — | [YAML](../../jRISK/jamovi/lifetime.r.yaml) |
| [relsystem](../../jRISK/R/relsystem.b.R) | 24, 36, 43, 80, 144, 148, 178, 208 | — | [YAML](../../jRISK/jamovi/relsystem.r.yaml) |

Funkcje pomocnicze (bez plików generowanych): [utils.R](../../jRISK/R/utils.R)

### jSpace — 5 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|
| [geomap](../../jSpace/R/geomap.b.R) | 23, 62, 67, 74, 79, 90 | — | [YAML](../../jSpace/jamovi/geomap.r.yaml) |
| [rasterstats](../../jSpace/R/rasterstats.b.R) | 41, 62 | 27 | [YAML](../../jSpace/jamovi/rasterstats.r.yaml) |
| [satclassify](../../jSpace/R/satclassify.b.R) | 22, 84, 97 | — | [YAML](../../jSpace/jamovi/satclassify.r.yaml) |
| [satgroups](../../jSpace/R/satgroups.b.R) | 19, 67 | — | [YAML](../../jSpace/jamovi/satgroups.r.yaml) |
| [tleorbit](../../jSpace/R/tleorbit.b.R) | 40, 67, 74, 102, 122 | 30, 82 | [YAML](../../jSpace/jamovi/tleorbit.r.yaml) |

Funkcje pomocnicze (bez plików generowanych): [utils.R](../../jSpace/R/utils.R)

### jRol — 5 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|
| [crd](../../jRol/R/crd.b.R) | — | — | [YAML](../../jRol/jamovi/crd.r.yaml) |
| [latin](../../jRol/R/latin.b.R) | — | — | [YAML](../../jRol/jamovi/latin.r.yaml) |
| [plan](../../jRol/R/plan.b.R) | — | — | [YAML](../../jRol/jamovi/plan.r.yaml) |
| [rcbd](../../jRol/R/rcbd.b.R) | — | — | [YAML](../../jRol/jamovi/rcbd.r.yaml) |
| [splitplot](../../jRol/R/splitplot.b.R) | — | — | [YAML](../../jRol/jamovi/splitplot.r.yaml) |

Funkcje pomocnicze (bez plików generowanych): [utils.R](../../jRol/R/utils.R)

### jboot — 0 źródłowych analiz

| Analiza | Diagnostyka lokalna (note/error/reject/stop/footnote) | Przechwytywanie/tłumienie | Definicja wyników |
|---|---|---|---|

Funkcje pomocnicze (bez plików generowanych): 

