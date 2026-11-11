# Wydania jUPWR

Numeracja jUPWR jest niezależna od wersji jamovi (plik `version`). Rejestr modułów i macierz
zgodności modułów opcjonalnych: [`packaging/MODULES.md`](packaging/MODULES.md).

## 1.4.2 — 2026-11-10 (linia latest, jamovi 28.17)

Stan buildów: aplikacja oraz pliki `.jmo` jRISK 0.4.6 i jSpace 0.3.4 zbudowane
na Windows 2026-11-10 i na macOS 2026-11-11 (potwierdzenia użytkownika
z 2026-11-11); obraz Dockera — do zbudowania. Testy automatyczne (R,
klient, serwer) przechodzą w CI; poprawki wyglądu lekkiego sprawdzone
w przeglądarce na kliencie 1.4.1 z wstrzykniętymi stylami, nie na
przebudowanym kliencie.

Zmienione:
- ANOVA powtórzonych pomiarów: jednostka, której brakuje pomiaru w którejś
  kombinacji czynników wewnątrzobiektowych, jest pomijana, a liczba pominiętych
  jednostek jest podana w nocie pod tabelą i w opisie metod. Wcześniej jeden
  brak zatrzymywał całą analizę komunikatem „Dane niekompletne”.
- Korelacja, macierz rozrzutów: każdy panel rysuje obserwacje bez braków
  w swojej parze zmiennych, czyli te same, z których liczony jest współczynnik
  w tytule panelu i w tabeli. Wcześniej panele pokazywały tylko wiersze
  kompletne we wszystkich zmiennych.
- Korelacja: opis metod podaje, że przedział ufności jest dwustronny także
  przy hipotezie jednostronnej (liczby bez zmian).
- Tabela kontyngencji z warstwami: OR, RR i różnica proporcji są liczone tylko
  wtedy, gdy tabela łączna jest 2×2. Warstwa, która stała się 2×2, bo zabrakło
  w niej kategorii, nie dostaje już własnych miar.

Naprawione:
- Duże liczby w opisach i notach nie są już zapisywane w notacji naukowej
  („N = 100 000” zamiast „N = 1e+05”): liczba obserwacji i par w jCzest
  (tabela, zgodność, próby zależne, także komórki tabeli par), liczba
  permutacji w jperm, liczba replikacji bootstrapu w jCI, liczności na
  drzewie częstości w jRISK.
- Eksploracja, test Andersona-Darlinga: statystyka A była zaniżona, gdy
  w danych była obserwacja odległa od średniej o więcej niż około 8 odchyleń
  standardowych (utrata górnego ogona rozkładu normalnego). Wartość p w takich
  danych była i jest bliska zera; dla pozostałych danych wynik bez zmian.
- Przedziały ufności, metoda Wilsona: dolna granica przy 0 sukcesach to
  dokładnie 0, a górna przy samych sukcesach dokładnie 1 (wcześniej resztki
  zaokrągleń rzędu 1e-17). Dotyczy też metody Newcombe'a dla różnicy proporcji.
- Częstości, próby zależne: Q Cochrana liczy się także wtedy, gdy w którymś
  pomiarze wystąpiła tylko jedna z dwóch kategorii (np. same „tak”). Kategorie
  pomiarów są łączone jak w teście McNemara; pomiary muszą mieć te same dwie
  kategorie.

- ANOVA, test Dunnetta (także z osobnymi wariancjami): te same dane dają
  zawsze te same p i różnice graniczne. Wielowymiarowy rozkład t jest liczony
  numerycznie i wcześniej wynik wahał się między uruchomieniami na czwartym
  miejscu po przecinku; teraz obliczenie ma stałe ziarno generatora (1),
  odnotowane w opisie metod. Stan generatora użytkownika jest przywracany.
- Eksploracja, zmienne ilościowe i jakościowe: zmienna bez żadnej obserwacji
  nie przerywa już analizy — tabela pokazuje N = 0, a wykresy tej zmiennej
  pozostają puste.
- Eksploracja: skośność i jej błąd standardowy są liczone już od 3 obserwacji
  (kurtoza nadal od 4); gdy kilka wartości jest równie częstych, nota pod
  tabelą informuje, że jako dominantę podano najmniejszą.
- Szereg rozdzielczy: granice klas, szerokość klasy i etykiety osi bez
  notacji naukowej („[100000; 200000)” zamiast „[1e+05; 2e+05)”).
- Przedziały ufności: gdy wszystkie replikacje bootstrapowe mają tę samą
  wartość, nota mówi o przedziale zerowej szerokości (wcześniej o nieudanej
  metodzie); stałe wartości w obu grupach (różnica średnich) albo stała
  zmienna (korelacja) dają wyjaśnienie zamiast pustych komórek.
- Testy t i testy permutacyjne dla par: para zmiennej z nią samą jest pomijana
  z notą, a ta sama para podana dwa razy liczy się raz (wcześniej błąd wykresu).
- Częstości, tabela kontyngencji: opis metod precyzuje, że zamiana kolejności
  grup daje odwrotności OR i RR, a zamiana kolejności kategorii odwraca OR
  i zmienia kategorię, której dotyczy RR.
- Wykresy: skumulowany słupkowy i Pareto przy zmiennej bez obserwacji oraz
  profil średnich bez wierszy kompletnych pokazują komunikat zamiast błędu;
  wykres hantlowy nie skleja kategorii, których długie nazwy skracają się do
  tego samego tekstu; wykres punktowy grup podpisuje w domyślnym podpisie, co
  pokazują słupki błędu (przedział ufności, błąd standardowy albo odchylenie
  standardowe).
- Rozkłady: χ² z dużym parametrem niecentralności λ pokazuje cały rozkład
  i kilkanaście etykiet osi zamiast kilkuset; w rozkładzie dwumianowym kwantyl
  poza zakresem osi nie jest już rysowany.
- Wygląd lekki, niskie okno (mały ekran albo duże powiększenie, np. 133% na
  rzutniku): po otwarciu zmiennej wyliczonej, przekształconej albo filtrów
  okno nie przesuwa się już w górę, ucinając nagłówki kart bez możliwości
  powrotu. Karta danych mieści się teraz w oknie razem z paskiem stanu
  arkusza, a w bardzo niskim oknie edytor zmiennej przewija się w środku.
  Wygląd klasyczny bez zmian.
- Zmienna przekształcona: reguła z niepełnym warunkiem (wybrana zmienna albo
  „użyj”, ale bez wartości do porównania) nie jest już po cichu pomijana —
  edytor pokazuje „Niepełna reguła: wybierz zmienną i wpisz wartość” i nie
  zapisuje reguł, dopóki nie zostanie uzupełniona. Wcześniej taka reguła była
  widoczna w edytorze, a wszystkie wiersze dostawały wartość „w przeciwnym
  razie”. Porównanie z brakiem danych wpisuje się jako NA.
- Zmienna przekształcona: gdy reguły dają liczby całkowite, poziomy zmiennej
  nominalnej i porządkowej są ułożone rosnąco. Wcześniej powstawały w kolejności
  pierwszego wystąpienia w danych (np. 3, 1, 2), co dla skali porządkowej
  dawało zły porządek kategorii.
- Zmienna przekształcona, reguły: wiersz „w przeciwnym razie użyj” zostaje
  widoczny u dołu listy, gdy reguł jest więcej, niż mieści edytor (wcześniej
  znikał poniżej i nic nie wskazywało, że lista się przewija); w wyglądzie
  lekkim pasek przewijania listy reguł jest wyraźniejszy.
- Wygląd lekki, wąska karta danych: podpis przełącznika „Zachowaj w analizach
  niewykorzystane poziomy” nie nachodzi już na tytuł edytora zmiennej
  przekształconej i wyliczonej — łamie się na dwie linie.
- Opis zastosowanych metod (jmvcore): zdanie z listą wartości (np. kilka
  zmiennych albo pusta zmienna opcjonalna) nie powoduje błędu.

Wersje modułów: jANOVA 0.5.8, jCzest 0.2.9, jCI 0.3.10, jEksplor 0.1.12,
jperm 0.2.10, jRegr 0.1.10, jTestyT 0.4.8, scatr 2.9.15, distrACTION 1.5.2, jmvcore 28.3.5,
jRISK 0.4.6 (`.jmo` do zbudowania).

CI: testy klienta (vitest) i serwera (pytest) oraz pełne testy jANOVA i jRISK.

## 1.4.1 — 2026-11-08 (linia latest, jamovi 28.17)

Stan buildów (2026-11-08): Docker (`jupwr/jupwr:1.4.1`) zbudowany;
Windows i macOS: binarki aplikacji oraz pliki `.jmo` jRISK 0.4.5 i jSpace 0.3.4
zbudowane — potwierdzenie użytkownika. Status testów:
`packaging/tests/manual/release-1.4.1.md`.

Nowe:
- Wykresy wektorowe (SVG) w zakładce „Wykresy” (scatr 2.9.14): słupkowy,
  histogram, słupkowy zmiennej dyskretnej, gęstości, ridgeline, mozaikowy,
  skumulowany słupkowy, kolisty słupkowy, lizakowy, Pareto, mapa ciepła,
  korelogram, hexbin, radarowy, mapa drzewa, waflowy i profil średnich.
  Zachowane opcje, rozmiary i eksport PNG/SVG/PDF. Wykresy z punktami dla
  pojedynczych obserwacji pozostają rastrowe.
- Wygląd „Lekki, przyciemniony” (ustawienie „Interfejs”): wygląd Light (A)
  z ciepłoszarym tłem za kartami i lekko złamaną bielą kart; wyniki i eksport
  bez zmian. Classic i Light (A) bez zmian.
- Palety wykresów Catppuccin Latte (jasna) i Catppuccin Mocha (ciemna), także
  w mapach ciepła i gradientach (licencja MIT, tekst w
  `jmvcore/inst/LICENSE-catppuccin.txt`; jmvcore 28.3.4). Warianty Frappé
  i Macchiato pominięte — różnią się od Mocha prawie tylko tłem.
- Rozkłady: „Dane empiryczne” w ośmiu rozkładach (distrACTION 1.5.0) — model
  zawsze z parametrów panelu, dane rysowane na nim, porównanie wyłącznie
  opisowe: częstość zdarzenia w danych obok P i momenty z danych obok modelu.
  Dyskretne (dwumianowy, Poissona, geometryczny, ujemny dwumianowy): punkty
  częstości na słupkach i tabela liczności obserwowanych i oczekiwanych;
  w dwumianowym także zmienna zero-jedynkowa (jedna próba w wierszu, wybór
  kategorii „sukces”) z rozkładem Bin(n, p̂) obok modelu; w geometrycznym
  i ujemnym dwumianowym dane w konwencji zmiennej wybranej w panelu. Ciągłe
  (normalny, wykładniczy, gamma, Weibulla): histogram gęstości pod krzywą
  (klasy Sturgesa jak w Szeregu rozdzielczym albo liczba podana ręcznie)
  i odchylenie standardowe obok modelu.

Zmienione:
- Rozkład dwumianowy: usunięte p oszacowane z danych, zakres 95% liczności
  i test χ² zgodności z wersji 1.4.0 (przerabiane przed testami; testy są
  w modułach testów). Stare pliki .omv liczą z p podanego w panelu.

Poprawione:
- Wykresy wektorowe: awaryjny PNG, gdy urządzenie SVG/cairo nie może się
  uruchomić; plik ma poprawną zawartość i rozszerzenie.
- Rozkład F: kwantyle p = 0 i p = 1 bez awarii wykresu; pełne zacieniowanie
  przy p = 1; gęstość dla df₁ < 2 bez sztucznego szczytu; zakres uwzględnia λ.
- Weibull: bardzo mały kształt nie przerywa obliczeń prawdopodobieństw i kwantyli;
  przy niereprezentowalnym zakresie wykres pokazuje komunikat. Wariancja bez Inf − Inf.
- Nemenyi: kolumna q pokazuje nieujemną statystykę rozkładu rozstępu
  studentyzowanego zamiast q/√2; wartości p pozostają takie same.
- Rozkłady: komunikat błędu (zły parametr, zła zawartość zmiennej — „Nieprawidłowy
  format zmiennej.”) nie jest już zasłaniany błędem wykresu „colnames…”
  (dwumianowy, ujemny dwumianowy, normalny, F i rozkłady z danymi empirycznymi).
- Wykładniczy, gamma, Weibulla: legenda pola „P (Pole)” zamiast „P (Area)”.

- Windows: zapis dokumentu synchronizowany przez zapisywalny deskryptor
  (`fsync` nie zgłasza już „Bad file descriptor”).
- Lekki interfejs: ustawienia przewijają się przy małym oknie lub dużym powiększeniu.
- distrACTION: tabele ponad 2000 wartości domyślnie grupowane (maks. 202
  przedziały), z notą i wyborem „pokaż wszystkie wartości” pozwalającym wyłączyć
  grupowanie. Dostępny też tryb pytania przed wyświetleniem dużej tabeli.
  Momenty i częstości na wykresie nadal liczone z pojedynczych obserwacji.
  Nieskończone liczności dają czytelny błąd.
- Percentyle respektują „Symbol dziesiętny”: przecinek → „2,5; 97,5”,
  kropka → „2.5; 97.5”. Domyślna lista „25;50;75” działa przy obu ustawieniach;
  dawny separator listy przecinkiem działa przy kropce dziesiętnej. Zmiana
  ustawienia przelicza percentyle i ich podpisy.
- jCI, jTestyT, jperm: wykresy i tabele odświeżają się razem po zmianie opcji,
  także przy losowym ziarnie 0 i przełączaniu widoczności wykresów.
- jCzest: niecałkowite liczności odrzucane przed testami zamiast obcinania;
  nieokreślone przedziały Walda OR/RR przy zerowych komórkach pomijane z notą.
- Heatmapa: komórka z samymi brakami pozostaje brakiem także przy sumowaniu.

Wersje modułów: distrACTION 1.5.1, jmvcore 28.3.4, jEksplor 0.1.11,
jANOVA 0.5.7, jCI 0.3.9, jTestyT 0.4.7, jperm 0.2.9, jCzest 0.2.8, scatr 2.9.14.

## 1.4.0 — 2026-11-05 (linia latest, jamovi 28.17)

Stan buildów (2026-11-05): Docker (`jupwr/jupwr:1.4.0`) zbudowany,
macOS i Windows zbudowane (aplikacja, .jmo jRISK 0.4.5 i jSpace 0.3.4).

Nowe:
- Wykresy wektorowe (SVG) w Eksploracji (jEksplor): histogramy, ECDF, krzywa
  Lorenza, słupkowy, mozaikowy, histogram klas i ogiwa; ostre w eksporcie.
  Wykres pudełkowy i Q-Q zostają rastrowe (punkt na obserwację).
- „Ustaw rozmiar” w menu kontekstowym wykresu: szerokość i wysokość wpisane
  w px, cm, mm albo calach (z jamovi, gałąź rozwojowa); przeciąganie wykresu nie
  przewija już panelu wyników.
- Ekran ładowania z logo UPWr zamiast logo nad wynikami; małe logo i wersje
  jUPWR/jamovi na dole ustawień.
- Kopie odzyskiwania usuwane po zapisie dokumentu i po 30 dniach; zapisany plik
  czytelny dla innych użytkowników (w Dockerze dla użytkownika hosta).
- Zmiana opcji w trakcie liczenia nie restartuje już procesu R, gdy poprzednie
  obliczenie kończy się w ciągu 2 s (`JAMOVI_ANALYSIS_CANCEL_GRACE`).
- Rozkład dwumianowy: zwinięta sekcja „Dane empiryczne” — zmienna z danymi obok
  modelu: częstość zdarzenia w danych obok P, tabela liczności obserwowanych
  i oczekiwanych N·P, momenty z danych, punkty częstości na wykresie; opcjonalnie
  p oszacowane z danych, dokładny zakres 95% liczności i test χ² zgodności
  (distrACTION 1.4.0, pilot — pozostałe rozkłady później).
- Z jamovi: dopasowanie szerokości kolumny podwójnym kliknięciem, spacje na
  początku i końcu wartości obcinane przy imporcie.

Poprawione błędne wyniki (audyt kodu 2026-11-05; także w 1.0.10):
- ANOVA powtórzonych pomiarów z kowariantą: efekty czynników przy średniej
  kowariancie (były przy kowariancie = 0); porównania parami ze stopniami
  swobody kontrastów (były zaniżone).
- Regresja: przedziały ufności współczynników na właściwych wierszach przy
  współliniowości; VIF dla predyktorów jakościowych jako GVIF.
- Zapis reszt i wartości przewidywanych do arkusza (ANOVA, regresja) oraz numery
  wierszy wartości skrajnych przy aktywnym filtrze trafiają we właściwe wiersze.
- Szereg rozdzielczy nie gubi obserwacji i przestrzega przedziałów [a; b).
- Test permutacyjny dokładny liczy obserwowany układ (bywało p = 0).
- Testy t: różnica w wierszu Wilcoxona jednej próby, znak r Manna-Whitneya.
- Częstości: poprawka ciągłości w teście zgodności, kierunek OR w porównaniach
  po Q Cochrana, przedział dla V Craméra z niecentralnego χ².
- Przedziały ufności: BCa przy remisach (proporcje, mediana).
- Rozkład t: średnia nieokreślona dla df ≤ 1; jSpace: punkty na granicy państw.

Poprawione awarie i wydajność:
- ANOVA: polskie komunikaty dla pustych komórek planu, stałej zmiennej
  zależnej i grupy z jedną obserwacją przy Welchu.
- Wykresy: raincloud z brakami w grupie, „wyklucz przypadki” w trybie kilku
  zmiennych, rój przy pustej kolumnie, bubble z wartościami ≤ 0, histogram
  z wartością odstającą, odświeżanie wykresów jEksplor po zmianie ich opcji.
- Szybciej: układ „rój” (do 25×), bootstrap BCa i regresji, ROC, ważność zdarzeń
  w drzewie błędów, duże rozkłady dyskretne; mniej danych zapisywanych w .omv.
- Wygląd Light (A): wstążka bez paska przewijania i uciętych ikon (grupa
  Filtry / Wstaw / Usuń); wygląd Classic bez zmian.

Wersje modułów: jANOVA 0.5.6, jRegr 0.1.9, jEksplor 0.1.10, jperm 0.2.8,
jTestyT 0.4.6, jCzest 0.2.7, jCI 0.3.8, distrACTION 1.4.0, scatr 2.9.13,
jRISK 0.4.5, jSpace 0.3.4.

## 1.3.1 — 2026-11-02 (linia latest, jamovi 28.17)

Stan buildów: Docker (`jupwr/jupwr:1.3.1`) zbudowany 2026-11-02; buildy Windows i macOS
zbudowane tego samego dnia; moduły opcjonalne jRISK 0.4.4 i jSpace 0.3.3 zbudowane jako
`.jmo` na obu maszynach.

- Wygląd interfejsu do wyboru w ustawieniach („Interface”): Klasyczny (jak dotąd)
  albo Lekki (A). Lekki: przełącznik zakładek z tytułem zbioru w pasku, kolorowe
  ikony kategorii, karty danych, opcji i wyników (nagłówki z liczbą wierszy,
  „O zbiorze”, Kopiuj, Eksportuj), edytor zmiennej w karcie danych (także filtry,
  zmienne wyliczone i przekształcone), menu i szuflada ustawień w nowym stylu,
  cienkie paski przewijania, ciepła szarość karty opcji.
- Kolory interfejsu zebrane w jednym pliku (`client/common/jupwr-colors.css`).
- Wyniki: podpowiedź „+ dopisz tekst” pod analizą i wyraźne pola do pisania
  w trybie edycji (oba wyglądy); w wyglądzie Lekkim przycisk „Pisz w wynikach”.
- Menu Regresja: czytelne podpisy „Wielomianowa”, „Porządkowa”, „Korelacja
  cząstkowa”, „Dwie kategorie” (zamiast „N wartości” z angielskiej etykiety).
- distrACTION 1.3.5: przy dużym n oś X rozkładów dwumianowego, geometrycznego,
  Poissona i ujemnego dwumianowego pokazuje około 10 podziałek zamiast wszystkich
  wartości.
- Tłumaczenia polskie etykiet wyglądu (i18n).

## 1.3.0 — 2026-11-01 (linia latest, jamovi 28.17)

Stan buildów: Docker (`jupwr/jupwr:1.3.0`) zbudowany i sprawdzony na Linuxie.
Windows i macOS oraz ich pliki `.jmo` jRISK/jSpace czekają na build i weryfikację
na właściwych maszynach. Wersje modułów opcjonalnych bez zmian: jRISK 0.4.4,
jSpace 0.3.3; wymagają przebudowy dla tej wersji aplikacji.

- Nowa baza jamovi 28.17: poprawki undo/redo przy ukrytych wierszach i usuwanych
  kolumnach wynikowych, postępu i anulowania operacji, proxy, systemowych
  certyfikatów TLS, arkusza i panelu wyników. Typ opcji `Color` i kontrolka
  wyboru koloru w modułach; zaktualizowane tłumaczenia.
- Kopie odzyskiwania obejmują również nowe, niezapisane dokumenty. Są dostępne
  w folderze „Odzyskiwanie” i nie zmieniają nazwy ani oznaczenia niezapisanych
  zmian dokumentu. W Dockerze przetrwają odtworzenie kontenera.
- Preferencje „Dokumenty → Kopie odzyskiwania”: wyłączone, 1, 2, 5, 10 lub
  15 minut; domyślnie 5 minut. Wybór jest zapamiętywany w profilu i działa bez
  restartu. Przy włączonym odzyskiwaniu kopia powstaje też przy kończeniu sesji.
- Atomowy zapis `.omv`: unikalny plik tymczasowy, `fsync` i podmiana dopiero po
  zakończeniu zapisu. Błąd przed podmianą zachowuje poprzedni plik, a anulowanie
  czeka na zakończenie pracy wątku zapisującego.
- „Wszystko → Usuń” wymaga potwierdzenia nieodwracalnego usunięcia analiz,
  wyników, uwag i kolumn wynikowych. W Electronie domyślnym przyciskiem jest
  „Anuluj”; Escape również anuluje.
- Bezpieczniejsza instalacja `.jmo`: rozpakowanie i walidacja przed podmianą,
  przywracanie poprzedniej wersji przy błędzie podmiany lub odświeżenia rejestru.
  Przeciągnięcie pliku do panelu „Transfer danych” uruchamia instalator modułu,
  zamiast próbować otworzyć paczkę jako dane.
- Import przygotowuje wszystkie pliki i sprawdza łączny limit wierszy przed
  zastąpieniem danych. Błąd odczytu dowolnego źródła albo anulowanie przygotowania
  zachowuje stary arkusz, formuły, analizy i historię cofania.
- Ograniczona liczbowo i bajtowo kolejka WebSocket: jeden nadawca, scalanie
  postępu i rozłączenie przeciążonego klienta z możliwością ponownego połączenia.
  Parser JSON Lines obsługuje linie i znaki UTF-8 dzielone między fragmentami
  odpowiedzi; ucięta lub błędna odpowiedź jest zgłaszana jako błąd.
- Konfigurowalna liczba procesów R (`JAMOVI_R_PROCESSES`, domyślnie 4).
  Anulowanie kończy zajęty proces przed następnym obliczeniem. Poprawiona obsługa
  awarii, timeoutów, startu i restartu silników, eksportów oraz zamykania sesji.
  Docker przekazuje sygnał zatrzymania bezpośrednio do serwera i czeka na sprzątanie.
- Polski jest domyślnym językiem nowego profilu także w angielskiej przeglądarce;
  zapisany wybór języka nadal ma pierwszeństwo.
- Zachowany podział analiz jUPWR: tabele częstości pozostają w analizach
  jakościowych, bez przywracania ich do panelu zmiennych ilościowych upstreamu.
- Dane do audytu Dockera są w obrazie jako zwykłe pliki w `Dokumenty →
  jupwr-test-audyt`, poza Biblioteką danych dydaktycznych.

Konfiguracja backendu i odzyskiwania: [`packaging/BACKEND.md`](packaging/BACKEND.md).
Sprawdzenie w Dockerze/Linux: 262 testy backendu, 355 testów klienta, 9 testów
kompilatora tłumaczeń; testy obliczeń R, importu, WebSocket, zapisu/odczytu OMV
oraz odzyskania kopii dokumentu w drugim kontenerze.

## 1.2.4 — 2026-10-30 (linia latest, jamovi 28.13)

- Warstwy (jak w starym jamovi) w dwóch analizach:
  - Eksploracja → Zmienne jakościowe: pole „Warstwy” — osobna tabela
    liczności dla każdego poziomu warstwy i tabela łączna; w podsumowaniu
    wiersz na warstwę; wykresy słupkowy i mozaikowy w panelach wg warstwy.
  - Częstości → Tabela kontyngencji: pole „Warstwy” — tabela liczności
    z kolumną warstwy, testy, V Craméra, miary 2×2 i porządkowe, test trendu,
    reszty i porównania par osobno w każdej warstwie oraz „Ogółem”;
    ostrzeżenie o liczebnościach oczekiwanych sprawdzane w każdej warstwie;
    wykres w panelach.
- Moduły: jEksplor 0.1.7, jCzest 0.2.6.

## 1.2.3 — 2026-10-27 (linia latest, jamovi 28.13)

Stan buildów (2026-10-27): Docker (`jupwr/jupwr:1.2.3`) zbudowany,
macOS i Windows zbudowane (aplikacja, .jmo jRISK i jSpace).

- Nazwy zmiennych ze spacjami, nawiasami lub myślnikami (np. „trolo ew”,
  „wynik (cm)”) działają we wszystkich analizach:
  - Korelacja: przy trzech i więcej zmiennych z wykresem analiza kończyła
    się błędem „arguments imply differing number of rows” (macierz
    rozrzutów).
  - ANOVA powtórzonych pomiarów: model w ogóle się nie dopasowywał.
  - ANOVA/ANCOVA i regresja liniowa/logistyczna: nazwy efektów
    i współczynników pokazywały się w odwróconych apostrofach
    (`` `grupa x` ``); kowarianta o takiej nazwie nie była rozpoznawana jako kowarianta.
- Moduły: jRegr 0.1.6, jANOVA 0.5.3.

## 1.2.2 — 2026-10-26 (linia latest, jamovi 28.13)

Stan buildów (2026-10-26): Docker (`jupwr/jupwr:1.2.2`) zbudowany i uruchomiony,
macOS i Windows zbudowane (aplikacja, .jmo jRISK i jSpace).

- Wykresy, kategorie rozkładu podzielone według typu zmiennej: „Rozkład
  ilościowe” (dawny „Rozkład”) i „Rozkład jakościowe” (dawne „Części
  całości”), obie na początku wstążki, nazwy w dwóch liniach.
- Nowy wykres „Wykres słupkowy (zmienna dyskretna)” w rozkładzie
  ilościowym, dla zmiennych o wartościach całkowitych: słupek albo prążek
  na każdą wartość (wartości nieobecne = zero), liczebności albo proporcje
  (w grupach: udział w obrębie grupy), grupy obok siebie albo w osobnych
  panelach. Wartości niecałkowite → komunikat z odesłaniem do histogramu.
- Wykres słupkowy (Ranking): tryb „Kategorialne” ukryty — liczebności
  jednej zmiennej jakościowej rysuje wykres słupkowy w rozkładzie
  jakościowym; domyślny tryb „Ciągła”. Stare pliki otwierają się bez zmian.
- Wykres grzbietowy (ridgeline) ukryty w menu — to samo daje wykres
  gęstości z rozdzieleniem na grupy; zapisane pliki nadal się otwierają.
- Moduły: scatr 2.9.11.

## 1.2.1 — 2026-10-23 (linia latest, jamovi 28.13)

Stan buildów (2026-10-23): Docker (`jupwr/jupwr:1.2.1`) zbudowany i uruchomiony,
Windows (instalator, portable .zip, .jmo jRISK i jSpace; `release-check.sh` bez
ostrzeżeń) i macOS zbudowane.

- Szereg rozdzielczy (Eksploracja): domyślna szerokość klasy dobierana
  automatycznie do zakresu danych (okrągła liczba: 1, 2, 2,5 lub 5 × 10ⁿ,
  blisko liczby klas z reguły Sturgesa); wcześniej domyślne 1 przy dużym
  zakresie dawało miliony klas i bardzo długie liczenie. Wpisanie szerokości
  dającej ponad 200 klas kończy się komunikatem zamiast liczenia.
- Zmienne ilościowe: współczynnik Giniego liczy się także dla zmiennych
  całkowitych z dużą liczbą obserwacji (wcześniej brak wyniku).
- Wykresy, „Części całości” → wykres słupkowy rysuje się już przy samej
  zmiennej X: w stosie (100%) jako jeden słupek podzielony na kategorie,
  liczebności i proporcje jako słupek na kategorię (proporcje = udział
  w całości). Ze zmienną segmentów bez zmian.
- Moduły: jEksplor 0.1.6, scatr 2.9.10.

## 1.2.0 — 2026-10-22 (linia latest, jamovi 28.13)

- Daty i godziny w zmiennych obliczanych: `TIMEDIFF(start, koniec, unit='min')`
  liczy czas, który upłynął (s, min, h, d; przy samych godzinach koniec
  wcześniejszy niż początek to następny dzień), do tego `TIMEVALUE`, `TIME`,
  `DATETIMEVALUE` oraz `YEAR`, `MONTH`, `DAY`, `WEEKDAY` (poniedziałek = 1),
  `HOUR`, `MINUTE`. Rozpoznawane bez podawania formatu: `8:15`, `08:15:30`,
  `2026-10-22`, `22.10.2026` (dzień przed miesiącem), data z godziną.
  `DATEVALUE` bez formatu przyjmuje też zapis polski; nierozpoznany wpis daje
  brak danych zamiast błędu. Opisy po polsku w pasku formuł („Data / czas”).
- Bibliografia: pakiety R cytowane wtedy, gdy analiza naprawdę ich użyła
  (np. car przy SS typu II/III i teście Levene'a, emmeans przy porównaniach,
  boot przy przedziale bootstrapowym, ggplot2 przy wykresie) — w jANOVA, jCI,
  jCzest, jEksplor, jRegr, jTestyT, jperm, jdistrACTION, jRISK, jSpace
  i w zakładce Wykresy. Zbiór otwarty z biblioteki (jDane, jSpace) dopisuje
  swoje źródła, z pakietem R, z którego pochodzi (np. AER dla CASchools).
  Eksport i kopiowanie mają tę samą bibliografię co widok (wcześniej bez jUPWR).
- Zakładka Wykresy:
  - nowa kategoria „Profile”: radar, profil średnich (średnie kilku zmiennych
    z błędem standardowym lub 95% CI, opcjonalnie standaryzowane), wykres
    hantlowy i wykres nachylenia (dwa pomiary, np. przed i po);
  - radar: opcja „Osobny radar dla każdej grupy”;
  - wykres współrzędnych równoległych ukryty w menu (stare pliki się otwierają);
  - wykres mozaikowy przeniesiony z „Zależności” do „Części całości”;
  - wykres słupkowy w „Rankingu” bez stosu (grupy zawsze obok siebie);
  - wykres słupkowy w „Częściach całości” (dawniej „Skumulowany wykres
    słupkowy”): liczebności obok siebie, proporcje obok siebie albo proporcje
    w stosie (100%); oś proporcji w procentach (też na wykresie mozaikowym);
  - wykresy skrzypcowy, pudełkowy, raincloud i paskowy: tryb „Kilka
    zmiennych” — kolumny w formacie szerokim (np. przed, po, po pół roku)
    obok siebie, w skrzypcowym i pudełkowym także ze zmienną grupującą;
    braki usuwane osobno w każdej kolumnie (z notą o liczbie obserwacji),
    „Wyklucz przypadki” zostawia tylko osoby z kompletem pomiarów;
  - histogram: przedziały domknięte lewostronnie, w trybie automatycznym
    z okrągłą szerokością i granicami na jej wielokrotnościach (np. 0–25,
    25–50); w trybie ręcznym nowa opcja „Początek przedziałów”.
- Zmienne obliczane i przekształcone szybciej przeliczają się po edycji
  komórki (np. 20 reguł na 10 000 wierszy: ok. 0,3 s zamiast 0,9 s):
  IF liczy tylko wybraną gałąź, a kolumna jest liczona jednym przejściem.
  Przy okazji błąd w niewybranej gałęzi IF (np. `IF(x > 0, LN(x), 0)`
  przy x ≤ 0) nie zamienia już wiersza w brak danych.
- Znak UPWr na starcie w prawym górnym rogu panelu wyników, skalowany z jego
  szerokością (wcześniej zachodził na bibliografię w wąskim oknie).
- jRISK, „Zdarzenia i warunkowanie”: częstości naturalne na najmniejszej
  podstawie (1000, 10 000, 100 000), przy której wychodzą liczby całkowite
  bez zaokrąglania (wcześniej zawsze na 1000, z zaokrąglaniem połówek do
  parzystej, np. 16,5 → 16); gdy się nie da — na 1000 z zaokrągleniem
  połówek w górę, sumy z zaokrąglonych komórek i notą. Nowa tabela
  „Porównanie ryzyk”: P(B | A), P(B | Aᶜ), różnica ryzyk (RD) i iloraz
  ryzyk (RR). W panelu A opisane jako „ekspozycja / warunek”, B jako
  „zdarzenie”.
- Wersje modułów: scatr 2.9.9, jANOVA 0.5.2, jCI 0.3.6, jCzest 0.2.5,
  jDane 0.8.2, jEksplor 0.1.5, jRegr 0.1.5, jTestyT 0.4.4, jdistrACTION 1.3.4,
  jperm 0.2.6; moduły opcjonalne jRISK 0.4.4 i jSpace 0.3.3 (`.jmo`
  zbudowane dla Windows i macOS).

## 1.1.2 — 2026-10-18 (linia latest, jamovi 28.13)

- Upstream jamovi 28.13 (z 28.11): stan analiz z pliku .omv nie jest
  przywracany (spreparowany plik mógł uruchomić kod R; analiza przelicza się
  od nowa, a eksport czeka na przeliczenie), przecinek dziesiętny na osiach
  wykresów i odświeżanie wykresów po zmianie symbolu, `RANK()` z brakami
  danych, wklejanie w arkuszu, ścieżka folderów i typ pliku w oknie zapisu,
  komunikat błędu nie zasłania wyników, kopiowanie wykresu jako LaTeX.
- Wykresy jCzest 0.2.4 (tabela kontyngencji, zgodność, zależne) w motywie
  i palecie ustawionych w aplikacji zamiast domyślnych kolorów ggplot.
- jCzest „Próby zależne”: wybór kategorii „wystąpiło” (udziały, wykres, opis
  metod); domyślnie druga kategoria, więc przy „tak/nie” wykres pokazuje
  udział „tak” (wcześniej zawsze pierwsza alfabetycznie, czyli „nie”).
  Statystyki McNemara i Q Cochrana bez zmian; w porównaniach par OR odwraca
  się razem z wybraną kategorią.
- Zmienna przekształcona: reguła z tekstem „$source” w cudzysłowie jest
  rozpoznawana jako reguła (wcześniej traktowana jak stare przekształcenie).
- Reguły zmiennej przekształconej: liczby wpisuje się i wyświetla z symbolem
  dziesiętnym z ustawień (przy przecinku „125,5” = 125.5; wcześniej zapisywane
  jako tekst, więc warunek nie działał i wiersze były puste). Liczba z drugim
  symbolem jest odrzucana — pole na czerwono i komunikat „Nieprawidłowy symbol
  dziesiętny” w wierszu błędów; reguły nie są
  zapisywane, dopóki błąd nie zniknie. Tekst „1,5” — w cudzysłowie.
- Zmienna przekształcona — poprawki z audytu:
  - reguła „== NA” nie znika przy następnej edycji;
  - wartość w rodzaju `"M" or "K"` jest odrzucana („Nieprawidłowy cudzysłów”)
    zamiast po cichu pasować do każdego wiersza, a `M or K` bez cudzysłowów
    — komunikatem „Użyj osobnego warunku z or / and” (wcześniej nie pasowało
    do niczego);
  - tekst z cudzysłowem lub ukośnikiem wstecznym zapisuje się dosłownie
    (wcześniej zniekształcony albo błąd składni);
  - kody z zerem na początku („01”), „1e5”, „1.” są tekstem, nie błędem składni;
  - wartość „w przeciwnym razie” z nawiasem i samo „w przeciwnym razie” bez
    reguł otwierają się jako reguły;
  - zaznaczenie kilku zmiennych o różnych regułach blokuje edytor z komunikatem
    (wcześniej edycja kasowała reguły wszystkich zaznaczonych);
  - funkcje kolumnowe (Z, VMEAN…) w regułach przy aktywnym filtrze: poziomy
    zgodne z wartościami w komórkach;
  - poziomy porządkowe mieszające liczby i tekst w kolejności reguł.
- jperm 0.2.5: obszar odrzucenia i obserwowana statystyka w drugim kolorze
  palety zamiast stałej czerwieni.
- jCI 0.3.5: wykres kafelkowy przedziału proporcji w kolorach palety
  i motywu (wcześniej skala palety nadpisywała kolory, a „porażki” wychodziły
  kolorowe; w ciemnym motywie białe ramki).
- Moduły jRol, jPomiar i jWody są od teraz wewnętrzne: zamrożone, bez `.jmo`
  w wydaniu (Windows: `build.ps1 -InternalModules` buduje je na żądanie).
- Zmienna przekształcona w starym pliku, której przekształcenie jest wspólne
  z innymi kolumnami: edycja reguł albo poziomu pomiaru tworzy kolumnie własne
  przekształcenie zamiast zmieniać przy okazji pozostałe kolumny.
- Reguły zmiennej przekształconej: formuła z wyrażeniem (np. `2 + 3`, `b * 2`)
  albo nazwą nieistniejącej kolumny pokazuje ostrzeżenie „nie da się pokazać jako
  reguły” zamiast reguł, które przy zapisie zamieniłyby wyrażenie w tekst.
- Przygotowanie wykresów wektorowych (SVG przez cairo, jeszcze nieużywane przez
  żaden wykres): jmvcore 28.3.2 kieruje fontconfig na fonty jUPWR (macOS),
  Docker instaluje je w fontconfig, a build macOS wbudowuje biblioteki X11
  z Homebrew — wcześniej `svg()` w jUPWR.app padało bez XQuartz.

## 1.1.1 — 2026-10-14 (gałąź `test/upstream-28.11`)
Poprawki zmiennej przekształconej i tabel wyników, fonty wykresów IBM Plex,
jRISK 0.4.2; te same poprawki zmiennej przekształconej są w 1.0.8 (bez importu
z .omv, który dotyczy tylko 28.11), jRISK 1.0.8 zostaje na 0.4.1.

- Fonty wykresów jak w interaktywnych wykładach: IBM Plex Sans (tekst, tytuły,
  nagłówki paneli) i IBM Plex Mono (etykiety osi, podpisy) zamiast Source Serif 4,
  Atkinson Hyperlegible i JetBrains Mono.
- jRISK 0.4.2: w „Zdarzeniach i warunkowaniu” zdarzenia przeciwne zapisane
  jako Aᶜ/Bᶜ zamiast „nie-A”/„nie-B” (tabele, wzory swoistości i NPV, drzewo
  częstości).
- Poprawka regresji z upstreamu 28.11 w kliencie: tabela, która traci wszystkie
  kolumny (np. Zmienne ilościowe po usunięciu ostatniej zmiennej), czyści też
  wiersze zamiast pokazywać stare wyniki bez nagłówków.
- Zmienna przekształcona nominalna/porządkowa: etykiety tekstowe w kolejności
  reguł niezależnie od (ukrytej) zmiennej źródłowej; wcześniej przy źródle
  ciągłym sortowane po kodach znaków („średnia” za „wysoka”).
- Zmienna przekształcona z regułami liczona bez ukrytej zmiennej źródłowej:
  brak danych w źródle nie daje już braku w wyniku, nie powstaje odwołanie
  cykliczne przez źródło (także przy „Przekształcenie” na pustej kolumnie),
  a poziom pomiaru nie zależy od źródła. Lista poziomów pomiaru bez
  „Automatyczny”: nowa zmienna domyślnie nominalna, stare pliki pokazują typ
  faktycznie nadany kolumnie.
- Import z pliku .omv/.omt znów działa (regresja upstreamu 28.11): wcześniej
  kończył się błędem „Unrecognised file format” i zostawiał pustą tabelę
  (wiersze były usuwane przed odczytem pliku).
- Reguły zmiennej przekształconej: tekst w cudzysłowie równy nazwie kolumny,
  „NA” albo liczbie („3”) zostaje tekstem przy ponownym zapisie reguł
  (wcześniej stawał się odwołaniem do kolumny, brakiem danych lub liczbą).
- Reguły zmiennej przekształconej: kategoria z kolumny nominalnej z kodami
  (np. z SPSS) połączona z liczbą niecałkowitą nie przerywa przeliczania
  (TypeError), a połączona z tekstem daje poziomy w kolejności reguł.
- Stare przekształcenie (z dawnego edytora) pokazuje ostrzeżenie „edycja tutaj
  je zastąpi” zamiast pustego panelu reguł, także po ponownym załadowaniu.
- jRISK 0.4.1 (moduł opcjonalny): czytelne drzewo częstości („Zdarzenia
  i warunkowanie”) i diagram drzewa błędów (FTA) — węzły na tle wykresu
  z ciemnym tekstem (kolor palety tylko na ramce, także w ciemnym motywie),
  większa czcionka; diagram FTA rozszerza się z liczbą zdarzeń elementarnych,
  więc etykiety liści się nie nakładają.

## 1.1.0 — 2026-10-12 (gałąź `test/upstream-28.11`)
Baza jamovi 28.2 → 28.11. Wersja testowa; 1.0.x (gałąź `main`) bez zmian.

Stan buildów (2026-10-12): Docker (`jupwr/jupwr:1.1.0`) zbudowany i sprawdzony
(analizy jUPWR przeliczone i porównane z 1.0.7, eksport .docx sprawdzony),
Windows i macOS zbudowane, aplikacje uruchamiają się.

- Upstream jamovi 28.11: eksport wyników do .docx, .odt i LaTeX, kopiowanie
  tabeli jako obrazu i jako markdown, element wyników `Text` (markdown) i `Svg`,
  opcja `File`, rozwijane opisy w bibliotece danych, autosave, refaktor serwera
  (projekt/instancja), poprawki pamięci w rdzeniu danych.
- jmvcore 28.3.1 (28.3 upstream + `metody`, motywy jUPWR).
- jmv 2.8.5: walidacja danych z upstreamu (regresje, rzetelność, propTestN, Dunn).
- scatr bez zmiany wersji (upstream: wyłącznie tłumaczenia nl, pt_BR, ru).
- Biblioteka danych: układ wpisu z upstreamu (tagi w nagłówku, rozwijany opis),
  pastelowe kolory tagów jUPWR zachowane.
- Narzędzia klienta: TypeScript 7, vite 8, vitest 5, js-yaml 5 (import
  przestrzeni nazw w panelu „O zbiorze”).
- Kompilator: moduły zbudowane nowym `jmc` wymagają jamovi ≥ 28.4, więc nie
  zainstalują się w jUPWR 1.0.x.

## 1.0.8 — 2026-10-13
Poprawki zmiennej przekształconej (reguły „jeżeli … użyj …”) i jRISK 0.4.1.

- Zmienna przekształcona nominalna/porządkowa: etykiety tekstowe w kolejności
  reguł niezależnie od (ukrytej) zmiennej źródłowej; wcześniej przy źródle
  ciągłym sortowane po kodach znaków („średnia” za „wysoka”).
- Zmienna przekształcona z regułami liczona bez ukrytej zmiennej źródłowej:
  brak danych w źródle nie daje już braku w wyniku, nie powstaje odwołanie
  cykliczne przez źródło (także przy „Przekształcenie” na pustej kolumnie),
  a poziom pomiaru nie zależy od źródła. Lista poziomów pomiaru bez
  „Automatyczny”: nowa zmienna domyślnie nominalna, stare pliki pokazują typ
  faktycznie nadany kolumnie.
- jRISK 0.4.1 (moduł opcjonalny): czytelne drzewo częstości („Zdarzenia
  i warunkowanie”) i diagram drzewa błędów (FTA) — węzły na tle wykresu
  z ciemnym tekstem (kolor palety tylko na ramce, także w ciemnym motywie),
  większa czcionka; diagram FTA rozszerza się z liczbą zdarzeń elementarnych,
  więc etykiety liści się nie nakładają.

## 1.0.7 — 2026-10-11
Licencje zbiorów danych w dokumentacji biblioteki, uporządkowany zestaw zbiorów
w jDane, drobne poprawki w jEksplor i uproszczone wykresy pomocnicze.

- jDane 0.8.0 (moduł wbudowany): każdy zbiór ma w panelu „O zbiorze” sekcję
  „Licencja” (nazwa, nota, odnośnik do oficjalnego tekstu i kopia w programie),
  wersję źródła w „Pochodzeniu” i datę zmian względem źródła. Pełne teksty
  GPL-2, GPL-3 i CC0 1.0 w `client/public/assets/licenses/`. Nowe zbiory: ankieta TIPI
  (heplots 1.8.6, GPL ≥ 2) i Music & Mental Health (Kaggle, CC0 1.0).
  Usunięte: PlantGrowth, npk, Irysy, Big5, bugs, Filmy (IMDb). Poprawione
  źródła ToothGrowth, Affairs, Titanic i fastfood (pełny łańcuch pochodzenia).
  Zbiory syntetyczne (autobusy, szkolenie) na CC0 1.0.
  `packaging/scripts/check-dataset-licenses.mjs` (w `release-check.sh`) wymaga
  pola licencji dla każdego zbioru.
  jDane 0.8.1: w TIPI pominięte pytania (kod 0 w surowych danych networktree)
  nie wchodzą już do średnich skal jako zero — 60 wartości w 20 wierszach
  zastąpiono brakiem danych; wyniki skal przeliczone z pozycji.
- jEksplor 0.1.4 (moduł wbudowany): „Zmienne ilościowe” — domyślnie zaznaczone
  tylko N, średnia, mediana, odchylenie standardowe i kwartyle (braki, minimum,
  maksimum i V odznaczone). „Zmienne jakościowe” — w tabeli krzyżowej nagłówek
  grupy dla każdego poziomu zmiennej „Podziel według” z podkolumnami n i %.
- Wykresy pomocnicze w analizach (jEksplor 0.1.4, jTestyT 0.4.3, jANOVA 0.5.1,
  jCzest 0.2.3, jRegr 0.1.4, jCI 0.3.4, jperm 0.2.4, jRol 0.1.3): bez tytułu
  z nazwą zmiennej nad wykresem i bez podtytułów/podpisów (opisy symboli, liczby
  typu r, AUC, różnica z CI) — wykresy mają być szybkie i poglądowe, a liczby są
  w tabelach. W Q-Q (jTestyT) i wykresach permutacyjnych (jperm) nazwa zmiennej
  przeniesiona na oś. Stałe tytuły sekcji i legendy bez zmian.

## 1.0.6 — 2026-10-03
Porównania post-hoc w jANOVA zgodne z rekomendacją przy nierównych wariancjach
(Games-Howell przy Welchu); moduł opcjonalny jWody wchodzi do macierzy wydania.
Wbudowana jANOVA wymaga nowej wersji aplikacji — sideload nie nadpisuje modułu
systemowego.

- jANOVA 0.5.0 (moduł wbudowany): porównania post-hoc przycięte do Tukeya,
  Dunnetta (vs kontrola) i „Brak”; usunięte NIR (LSD), Holm i Scheffé (dla par
  zdominowane przez Tukeya, NIR bez kontroli błędu rodziny porównań — zostaje
  w jRol). Przełącznik „Nierówne wariancje (Welch)” przy jednym czynniku zmienia
  też porównania: lista pokazuje Gamesa-Howella zamiast Tukeya, a Dunnett liczy
  osobne wariancje grup z df Welcha (Hasler i Hothorn 2008); średnie z SE
  i przedziałem z wariancji grupy. Przy Welchu i kilku czynnikach — średnie bez
  liter z notą. „Nieparametrycznie” przy jednym czynniku (Kruskal-Wallis /
  Friedman) ukrywa porównania parametryczne i wykres średnich, zostają litery
  Dunna / Nemenyiego. ANOVA powtórzonych pomiarów: lista Tukey / Dunnett / Brak.
  Stare pliki .omv z metodami lsd/holm/scheffe nie są obsługiwane (przed pilotażem).

- jWody: biblioteka danych z trzema wbudowanymi CSV, instrukcjami ćwiczeń,
  słownikiem zmiennych, jednostkami i pochodzeniem według wzorca jSpace.

- Opcjonalny jWody 0.1.1 (menu „Hydrologia”): kontrola szeregu, reżim i sezonowość,
  przepływy charakterystyczne, trendy oraz niżówki. Dobowe/miesięczne dane,
  wiele stacji, jawna kompletność i rok hydrologiczny; trzy syntetyczne zbiory
  dydaktyczne. Testy obliczeń, paneli i wykresów oraz skrypty `.jmo` dla
  Dockera, macOS i Windows. Wnioskowanie trendu pod założeniem niezależności
  wymaga jawnego włączenia. Zakres i walidacja: `jWody/README.md`.

## 1.0.5 — 2026-09-18
Nowy moduł opcjonalny jPomiar, rozszerzenia wykresów (scatr 2.9.2), marka jUPWR
w nagłówku okna i na ekranie startowym oraz poprawki instalatora Windows i builda
macOS. Domyślne zachowanie istniejących analiz i wykresów bez zmian.

### Aktualizacja modułu opcjonalnego jRISK 0.4.0 (2026-10-02)
Bez nowej wersji aplikacji: nowy `.jmo` instaluje się przez Moduły → Sideload na
zainstalowanym jUPWR 1.0.5 (zastępuje jRISK 0.3.4). Zgodność: macierz w
[`packaging/MODULES.md`](packaging/MODULES.md).

- jRISK 0.4.0 (luki z audytu pokrycia ćwiczeń „Analiza ryzyka”):
  „Zdarzenia i warunkowanie” i „Schemat Bernoulliego” przyjmują kolumnę liczności
  (tabela zagregowana, np. Bayes z 4 wierszy) i respektują wagi danych jamovi;
  w schemacie Bernoulliego jednostronna górna granica p (Clopper–Pearson,
  przy zerze sukcesów 1 − 0,05^(1/n)). „Niezawodność systemów”: bramka k-z-n
  w podsystemach w trybie danych, wspólna przyczyna jako element szeregowy
  (oba tryby, z ścieżkami, przekrojami, Birnbaumem i schematem) oraz tabela
  koherentności. „Drzewo błędów”: opcja „Powtórzona etykieta = to samo
  zdarzenie” liczy P(TOP), przekroje i ważność dokładnie (domyślnie powtórzenie
  nadal zatrzymuje analizę); małe P(TOP) na diagramie bez notacji wykładniczej.
  „Modele czasu życia”: opcjonalna „Grupa” w trybie danych (osobne dopasowanie
  i R(t) dla każdego typu urządzenia). Dane Bananpol jako jeden wątek kursu:
  typy urządzeń w `bananpol.csv` mają różne rozkłady Weibulla, `bananpol_system.csv`
  bierze niezawodności z ich dopasowań (R(6 mies.)), nowy surowy dziennik
  `bananpol_alarmy.csv` (6000 zmian, czujniki stary/nowy, sekcje) zastępuje gotowe
  tabele liczności. Dwa małe przykłady ćwiczeniowe (układ hamowania, drzewo
  z powtórzonym zdarzeniem; `jRISK/data-raw/cwiczenia.R`). Klucze zadań kursu
  „Analiza ryzyka” sprawdzane testami integracyjnymi.
  Domyślne zachowanie istniejących analiz bez zmian.

### Wydanie 1.0.5

- Nowy opcjonalny moduł jPomiar 0.3.0 (menu „Pomiary”): powtarzane pomiary,
  prosty budżet niepewności i propagacja dla sumy, różnicy, iloczynu i ilorazu
  z korelacją oraz kowariancja i elipsy 2D (standardowa, rozrzutu, ufności
  średniego położenia metodą Hotellinga). Każda analiza ma „Opis zastosowanych metod”
  (domyślnie wyłączony); objaśnienia założeń są w `jPomiar/README.md`.
  Zawiera dane, ćwiczenia i testy obliczeń.
  Dodano eksport `.jmo` z Dockera oraz kroki budowania Windows/macOS.
- Zbiory przykładowe jPomiar opowiadają konkretną historię, a nie są zestawem
  abstrakcyjnych liczb. `azotany.csv` (inżynieria środowiska) — 20 oznaczeń
  azotanów w certyfikowanym wzorcu 50,00 mg/L: laboratorium oczyszczalni
  sprawdza metodę przed serią oznaczeń w ściekach. Jeden zbiór prowadzi przez
  wszystkie trzy analizy jednowymiarowe: powtarzane pomiary → budżet niepewności
  (wzorcowanie odpowiada za około 96% wariancji, więc więcej powtórzeń nie pomoże)
  → propagacja ładunku = stężenie × przepływ. `lokalizacja.csv` (inżynieria
  bezpieczeństwa) — 30 wyznaczeń pozycji nieruchomego poszkodowanego przez
  lokalizator GNSS, gdzie elipsa rozrzutu ma znaczenie operacyjne: to obszar,
  który trzeba przeszukać. Dane są odtwarzalne skryptem `jPomiar/data-raw/datasets.R`.
- Ikona menu „Pomiary” dla jPomiar (`client/assets/analysis-jpomiar.svg`);
  wcześniej moduł dostawał domyślną ikonę.
- Wykresy (scatr 2.9.2), domyślne zachowanie bez zmian:
  - wykres waflowy ze zmienną grupującą dostaje rozmiar z siatki paneli —
    znikają białe pasy nad i pod rzędem paneli (kwadratowe kafelki
    w kwadratowym obrazie 500×500),
  - wykres radarowy, współrzędne równoległe i korelogram: przy zbyt małej
    liczbie zmiennych notka „Ten wykres wymaga co najmniej N zmiennych"
    zamiast pustego miejsca,
  - wykres radarowy: lista „Skalowanie osi" — min–maks obserwacji (jak dotąd),
    odporny min–maks (percentyle 5–95), procent największej średniej grupowej,
    rangi percentylowe; przy skośnych danych min–maks ściskał wielokąty
    do środka,
  - wykres chmurowy, punktowy ze średnią i skrzypcowy: lista „Układ punktów"
    z rojem (beeswarm, własna geometria, deterministyczna); w chmurowym
    dodatkowo stosy punktów,
  - wykres skrzypcowy z dwiema zmiennymi grupującymi: pudełko w środku
    rysuje się po jednym na podgrupę (było jedno na kategorię).
- Nazwa dystrybucji w miejscach, gdzie została jeszcze marka jamovi:
  nagłówek okna i tytuł karty/paska zadań pokazują „jUPWR" zamiast „jamovi",
  a ekran startowy (`electron/app/splash.html`) — znak „jU" w barwach jUPWR
  i napis „uruchamianie jUPWR…" zamiast niebieskiego logo jamovi
  i „starting jamovi...".
- Instalator Windows: dwie poprawki w ścieżce aktualizacji. Poprzedni
  deinstalator był uruchamiany z kopii w `%TEMP%` bez `_?=`, więc jego
  `$INSTDIR` wskazywał na `%TEMP%`, który sekcja deinstalacji kasuje przez
  `RMDir /r`. Dodatkowo `UninstallString` zapisany w cudzysłowach nie był
  rozpoznawany, więc stara wersja zostawała nadpisana zamiast odinstalowana.
  Przy okazji `/S` jest naprawdę cichy (`/SD` w oknach dialogowych).
- Build macOS: `20-modules.sh` dokłada do `modules/base/R` cztery zależności
  `Imports` z `jmv/DESCRIPTION` — `afex`, `mvnormtest`, `lavaan`, `Hmisc`.
  Dotąd przychodziły z systemowego R; gdy zniknęły z R.framework, build padał
  na `jmc --install jmv` („dependencies … are not available for package 'jmv'").
  `R CMD INSTALL` wymaga obecności `Imports` niezależnie od tego, czy analiza
  jest w jUPWR widoczna, a `jmc` ogranicza `R_LIBS` do `modules/base/R`, więc
  `--skip-deps` tego nie omija. Windows (`$deps13` w `build.ps1`) miał tę
  czwórkę od dawna — to był rozjazd między platformami, nie nowa zależność.
  Bez zmian w wynikach analiz.

## 1.0.4 — 2026-09-09
Wydanie poprawkowe: polska odmiana liczebników w komunikatach i spójny kolor
ikon belki statusu. Bez zmian w wynikach analiz.

- Komunikaty z liczbą (33 miejsca w 9 modułach) nie odmieniają już rzeczownika
  po liczebniku — forma dopełniacza pasowała tylko do 0 i 5+, więc „2 kompletnych
  obserwacji” albo „3 kategorii” były błędne przy typowych, małych liczbach.
  Wzorzec: liczba przed dwukropkiem („liczba kompletnych obserwacji: 2”) albo
  rzeczownik w nawiasie („N = 22 (pary bez braków)”).
  - komunikaty blokujące („nie da się policzyć, bo…”, gdzie liczba z definicji
    jest mała): jANOVA 0.4.2, jRol 0.1.2, jTestyT 0.4.2, jperm 0.2.3,
    jEksplor 0.1.2, jRegr 0.1.3, jSpace 0.3.2,
  - opisy zastosowanych metod i noty: jCzest 0.2.2 (liczba pomiarów, kategorii,
    porównań), jEksplor 0.1.2 (liczba klas, grup równolicznych),
    jCI 0.3.3 (liczba prób i replik bootstrapu), jRol 0.1.2 (plan doświadczenia:
    obiekty, powtórzenia, bloki, poletka),
  - „N = %d ⟨rzeczownik⟩”: jCI 0.3.3, jRegr 0.1.3, jANOVA 0.4.2, jCzest 0.2.2.
- Klient: interfejs przeszedł z niebieskiego na burgund jUPWR. Ikona „O zbiorze”
  jest pierwsza w obu belkach statusu (arkusz i zakładka Zmienne) i burgundowa;
  przekolorowane ikony belki i panelu filtrów (oko widoczności, dodawanie filtru,
  formuła, strzałki) oraz cała paleta chromu w CSS: zaznaczenie i podświetlenie
  nagłówków w arkuszu, panel filtrów, edytor zmiennych i przekształceń, backstage
  (Otwórz/Zapisz), wstążka, menu, sklep z modułami, panel opcji analizy
  i podświetlenie wybranej analizy w wynikach (82 podmiany w 22 plikach CSS),
  style pisane wprost w kodzie (`formatio.ts` — style kopiowanych i eksportowanych
  wyników, `references.ts`, `infobox.ts`, `clipboardprompt.ts`, `msgdialog.ts`,
  przeglądarka plików backstage) oraz ikony ekranu Otwórz/Zapisz (`place-*`,
  `document-*`, `bs-check-items`, wskaźnik pracy). Nagłówki wyników — na ekranie
  i w wyniku kopiowania — są burgundowe.
  Zasada: tła pod tekstem dostają jasny wariant (odcień, nie pełny kolor), więc
  kontrast tekstu nie spada; linie, obramowania i wypełnienia z białym tekstem
  dostają nasycony burgund. Kolory z dotychczasowej mapy jUPWR zachowane
  (`#3e6da9` → `#832034`, `#658ec2`/`#6b9de8` → `#b05263`, `#a9c6f2` → `#e9c6cd`),
  reszta przeliczona przez obrót odcienia na 350° z zachowaniem jasności
  i wytłumieniem nasycenia. Świadomie bez zmian: paleta kolorów tekstu w notatkach
  (przyciski nazwane wprost kolorem), ikony biblioteki metro, niebieska ikona
  powiadomienia „info” (znaczenie semantyczne, obok czerwonego błędu), logo jamovi,
  ikony modułów z biblioteki (cudze) i ikony zakładki Wykresy (decyzja z sierpnia).

## 1.0.3 — 2026-09-08
Wydanie poprawkowe: komunikaty „nie da się policzyć, bo…". Analiza, której nie
można wykonać na wskazanym wejściu, mówi dlaczego — z nazwą zmiennej, liczbą
obserwacji/grup/par i wymaganym minimum bieżącej implementacji. Bez nowych
ostrzeżeń metodologicznych i bez zmian w wynikach poprawnie wykonalnych analiz
(zakres: `docs/audits/2026-09-08-komunikaty-bledow.md`, kategoria 1).

- Klient: nota pod pustą tabelą jest widoczna. Renderer tabel kończył rysowanie
  pustego body przed stopką, więc komunikat błędu zapisany w R (np. „zmienna
  grupująca ma 3 poziomy") nie docierał na ekran; przejście poprawny wynik →
  błąd → poprawny wynik czyści stare noty. Test `client/resultsview/test/
  table-notes.test.ts`.
- jTestyT 0.4.1, jperm 0.2.2: komunikaty o liczbie poziomów, obserwacji w grupach
  i kompletnych par z konkretnymi liczbami; wyjątek testu głównego dla jednej
  zmiennej (np. stałe dane) daje notę dla tej zmiennej, nie kasując pozostałych
  wierszy.
- jANOVA 0.4.1, jRol 0.1.1: za mało kompletnych obserwacji zgłaszane jawnie
  zamiast cichego powrotu; błąd dopasowania modelu z prefiksem „Nie udało się
  dopasować modelu. Szczegóły:"; w jRol wskazana nieistniejąca kontrola Dunnetta
  jest błędem zamiast cichej podmiany na pierwszy poziom.
- jCI 0.3.2: jawnie wskazana nieistniejąca kategoria zdarzenia lub grupa jest
  błędem (bez podmiany na pierwszy poziom); brak kategorii w zmiennej zgłaszany.
- jRegr 0.1.2: nieistniejący poziom zdarzenia (logistyczna) lub poziom odniesienia
  predyktora jest błędem; niewyznaczalna korelacja pary (stała zmienna, n < 3)
  wyjaśniona zamiast pustego wiersza.
- jCzest 0.2.1: ujemne lub nieskończone liczności zatrzymują analizę zamiast być
  po cichu odfiltrowane.
- jEksplor 0.1.1: szereg rozdzielczy podaje liczbę obserwacji przy n < 2.
- distrACTION 1.3.3: parametry spoza dziedziny (ujemne SD, niecałkowite/ujemne n,
  p poza [0, 1], niedodatnie df F) wskazują parametr zamiast błędu R.
- scatr 2.9.1: samodzielny wykres, który nie powstaje (puste dane, suma liczności
  ≤ 0, za mało zmiennych, brak rozmieszczonych słów/kafelków), wyjaśnia przyczynę
  zamiast pustego obrazka; wykres współrzędnych równoległych nie przerywał się
  błędem `data.frame` przy zerowej liczbie wierszy.
- jRISK 0.3.4, jSpace 0.3.1 (opcjonalne): jawnie wskazany nieistniejący poziom
  jest błędem; w jSpace komunikaty o współrzędnych, grupach, rastrze, TLE
  i propagacji są błędami analizy, więc nie zależą od widoczności tabel
  dodatkowych.

## 1.0.2 — 2026-09-07
Wydanie poprawkowe interfejsu: czytelność wyników na rzutniku i polskie nazwy
w menu Analizy. Bez zmian w modułach i obliczeniach.

- Preferencje → Wyniki: „Rozmiar czcionki wyników" (100–200 %). Skaluje tabele,
  tytuły i noty w panelu wyników, nie zmieniając wielkości arkusza, wstążki
  ani panelu opcji — w odróżnieniu od Zoomu, który skaluje całą aplikację
  i zabiera obszar pracy. Ustawienie jest zapamiętywane. Wykresy zachowują
  swój rozmiar (renderuje je serwer).
- Menu Analizy: grupy „Eksploracja" i „Testy t" po polsku. Tytuł grupy tłumaczył
  moduł, który ją tworzy; gdy wszystkie analizy jmv w grupie są ukryte, tworzą ją
  moduły jUPWR bez własnych tłumaczeń i zostawał angielski klucz. Klient używa
  teraz własnego katalogu, gdy moduł nie ma tłumaczenia.

## 1.0.1 — 2026-09-06
Wydanie poprawkowe: naprawione obliczenia w trzech modułach kursu podstawowego.
Bez zmian w menu, panelach i modułach opcjonalnych.

- jperm 0.2.1: dokładne testy permutacyjne liczą p z pełnej enumeracji bez
  korekty Monte Carlo „+1”; opis metod rozróżnia oba schematy.
- jCI 0.3.1: regresja bootstrapowa pomija nieestymowalne pary współczynników,
  raportuje liczbę poprawnych replik i wymaga co najmniej połowy zadanych
  losowań (nie mniej niż 50); stały predyktor daje czytelny komunikat.
  Pasmo jest punktowo percentylowe i ma właściwy podpis także przy BCa dla
  współczynników; wyłączony wykres nie powoduje obliczania pasma.
- jRegr 0.1.1: df testu LR uwzględnia rangę modelu; automatyczna diagnostyka
  separacji całkowitej i quasi-całkowitej (lpSolve) ostrzega także przy pozornej
  zbieżności glm. Przy separacji lub braku zbieżności pomijane są SE, testy
  Walda, przedziały OR i krzywa z pasmem; b i OR oznaczono jako przybliżenia.
- CI: obowiązkowa kontrola metadanych obejmuje wersję instalatora Windows;
  sprawdzanie lokalnych .jmo można oddzielić przez --metadata-only.
  Dodano testy kontroli wydania oraz kompilację i testy R jperm/jCI/jRegr.

## 1.0.0 — 2026-09-04
Pierwsze wydanie „pełne": każde menu kursu podstawowego (Eksploracja, Przedziały
ufności, Testy t, ANOVA, Regresja, Częstości, Testy permutacyjne) ma analizy
jUPWR z panelem rdzeń → Zaawansowane i opisem zastosowanych metod. Numer 0.9.5.7
nadany tego samego dnia nie trafił do dystrybucji — jego zmiany są poniżej.
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
- Bibliografia: jUPWR jest cytowany jako pierwsza pozycja (przed jamovi i R),
  z numerem wersji braną z `JUPWR_VERSION`.
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
