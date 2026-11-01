# Handoff: ocena wykonalności redesignu interfejsu jUPWR (warianty A i C + menu, ustawienia, otwieranie danych)

**Dla:** Claude Code, repozytorium `jamovi-upwr-latest`, gałąź `main` (jUPWR 1.3.x, jamovi 28.17)
**Zadanie:** OCENA, nie wdrożenie. Nie zmieniaj kodu w `main`. Jeśli chcesz sprawdzić hipotezę
(np. czy coś da się zrobić samym CSS), zrób szybki szkic w osobnej gałęzi `spike/redesign-*`,
nie commituj do `main` i opisz wynik w raporcie.
**Wynik:** plik `docs/redesign/ocena-wykonalnosci.md` (struktura raportu: sekcja 6).

Przeczytaj najpierw `CLAUDE.md` (zasady projektu, ograniczenia) i `packaging/CHANNELS.md`.

---

## 1. Tło i założenia projektowe

jUPWR to narzędzie do NAUKI statystyki (UPWr). Zasady, których redesign nie może złamać:

- **Prostota obsługi.** Mało poziomów do przeklikania. Obecna głębokość to rozsądny kompromis
  i ma zostać: kategoria na wstążce → rozwijane menu → analiza oraz rdzeń panelu
  → zwinięte sekcje „Założenia” / „Zaawansowane”. Nie dokładamy nowych poziomów.
- **Bez asystentów i podpowiedzi interpretacyjnych.** Aplikacja nie mówi studentowi, co ma
  wywnioskować (por. zasada „co i jak, nigdy dlaczego” z zadania 19 w `CLAUDE.md`).
- **Trzy typy okien:** (1) dane (arkusz), (2) panel wyboru opcji analizy, (3) wyniki. Do tego
  doraźnie (4) edytor zmiennej (dziś pas na górze, który przesuwa cały widok w dół)
  i (5) panel ustawień aplikacji (prawa szuflada otwierana przyciskiem ⋮: powiększenie,
  format liczb i p, symbol dziesiętny, bibliografia, składnia, motyw i paleta wykresów,
  domyślne braki, język, tryb programisty).
- **Kolorystyka jUPWR jest ustalona i NIE podlega zmianie.** Paleta jest zdefiniowana i spójna
  z materiałami wykładowymi (motywy w `jmvcore/R/themes.R`, kolory klienta). Redesign dotyczy
  wyłącznie usprawnień wizualnych (układ, odstępy, hierarchia, czytelność, typografia
  w ramach istniejących krojów) i użytkowych (mniej kroków, mniej zasłaniania okien).
  Wszystkie kolory podane w makietach są orientacyjne. W implementacji zawsze używaj
  istniejących kolorów jUPWR i wskaż w raporcie, gdzie są dziś zdefiniowane.
- **Windows i macOS (Electron).** Referencyjny build to Docker (`docker compose --profile main build`).
- **Nie ruszamy `engine/` ani `server/`.** Kod jmv zostaje nietknięty (ukrywanie przez `jupwr.ts`).
- **Merge'e z upstreamem jamovi** zdarzają się regularnie (ostatnio 28.11 → 28.13 → 28.17).
  Każda zmiana w plikach upstreamu to przyszły konflikt. Preferuj nadpisania w osobnych plikach jUPWR
  (CSS, moduły TS) zamiast przepisywania plików jamovi.

Makiety są na płótnie w Claude (prywatne, Claude Code ich nie otworzy). Poniżej jest pełny opis.
Dane w makietach: zbiór „Długość zębów” (ToothGrowth: len, supp, dose), analiza jEksplor
„Zmienne ilościowe”: len według supp i dose, wykres pudełkowy.

---

## 2. Co już wiadomo o architekturze (sprawdzone pobieżnie, zweryfikuj)

- `client/index.html`: `#header` (tytuł + przyciski okna), `.silky-ribbon` (wstążka),
  `jmv-splitpanel#main-view`, wewnątrz niego `jmv-variable-editor#variable-editor`
  (domyślnie `.hidden`).
- `client/main/main.ts` (~1140 linii), ok. l. 516–562: `splitPanel.addPanel('main-table', …)`,
  `addPanel('main-options', { fixed: true, anchor: 'right', visible: false })`,
  `addPanel('results', …)`. Tryby splitpanelu: `mixed | data | results`
  (`client/main/splitpanel.ts`, ~670 linii; `setMode`, zdarzenie `mode-changed`,
  atrybut `data-splitpanel-mode` na `body`).
- Edytor zmiennej: `client/main/variableeditor.ts` (~450 linii) + widżety
  w `client/main/vareditor/` (datavarwidget, computedvarwidget, recodedvarwidget,
  conditionsbuilder, filterwidget, transformlist…). Przełączanie: `viewController._toggleVariableEditor()`
  / `showVariableEditor()` w `main.ts` (~l. 317–322, 928+).
- Panel opcji analizy to **iframe** (`client/main/optionspanel.ts`, `AnalysisResources`,
  `client/analysisui/`), współdzielony przez analizy tego samego typu. Kontrolki panelu
  (listy zmiennych, checkboxy, ComboBox, CollapseBox) budowane są z `.u.yaml` modułów przez
  analysisui. Ich wygląd to CSS w `client/analysisui/`.
- Wyniki to również iframe(y): `client/main/resultspanel.ts` (~1600 linii), `client/resultsview/`.
  Bibliografia: `client/main/references.ts`. Adnotacje (tekst w wynikach):
  `client/main/ribbon/annotationtab.ts`. **Sprawdź, co już umieją**, bo to fundament wariantu C.
- Wstążka: `client/main/ribbon.ts`, `ribbon/analysetab.ts` (filtr `JUPWR_HIDDEN_ANALYSES`,
  `JUPWR_MENU_ORDER`), `ribbon/plotstab.ts`, `ribbonmenu.ts`. Menu aplikacji/ustawienia:
  prawdopodobnie `ribbon/appmenu.ts` (zweryfikuj).
- Arkusz: `client/main/tableview.ts` (~3100 linii), `variablesview.ts`.
- Stałe jUPWR: `client/common/jupwr.ts`.
- **CSS prawie nie używa zmiennych CSS** (`var(--…)`: 0 w `ribbon.css`, `splitpanel.css`,
  `tableview.css`; pojedyncze w `variableeditor.css`, `backstage.css`). Kolory są wpisane na sztywno
  w wielu plikach. Istnieje `client/common/highcontrast.ts`. Sprawdź, jak dziś działa
  motyw/kontrast i czy wprowadzenie tokenów (`--jupwr-*`) to duża praca.

---

## 3. Wariant A: „Spokojna ewolucja”

Idea: ten sam model pracy i ten sam układ trzech okien, ale lżejszy wygląd i edytor zmiennej,
który nie zabiera miejsca wynikom.

| ID | Element | Opis docelowy |
|----|---------|---------------|
| A1 | Pasek tytułu | Pasek w obecnym kolorze jUPWR z nazwą pliku i liczbą wierszy pośrodku. Zakładki Zmienne / Dane / Analizy / Wykresy / Edytuj jako jeden przełącznik segmentowy (aktywna = białe tło, bordowy tekst). Po prawej „Moduły” (obrys) i przycisk ustawień aplikacji (ikona suwaków), który otwiera TĘ SAMĄ szufladę co dziś ⋮, tylko w nowym stylu. Natywne przyciski okna bez zmian. |
| A2 | Wstążka | Biała, niższa (ok. 52 px). Przyciski kategorii: ikona liniowa 18 px + podpis OBOK ikony, zaokrąglone tło przy najechaniu/aktywności. **Rozwijane menu kategorii zostają** (dzisiejsza struktura menu, podtytuły pozycji). |
| A3 | Trzy karty | Dane / Opcje / Wyniki jako osobne karty (białe, zaokrąglenie 12 px, cienkie obramowanie) z odstępem 10 px na jasnoszarym tle (odcienie z obecnej palety). Każda karta ma nagłówek: „Dane · 60 × 3 · filtry 0”, „Eksploracja / Zmienne ilościowe” + przycisk zwinięcia, „Zmienne ilościowe” + Kopiuj / Eksport. Szerokości: dane ~340–400 px, opcje ~320–360 px, wyniki reszta. Zmiana szerokości przeciąganiem jak dziś. |
| A4 | Edytor zmiennej w karcie danych | Po kliknięciu nagłówka kolumny NIE pojawia się pas nad całym oknem. Zamiast tego na górze karty danych otwiera się zwarty formularz: nazwa, przełącznik segmentowy Nominalna / Porządkowa / Ilościowa, poziomy jako przeciągane etykiety, link „Więcej ›” do pełnego edytora (typ danych, braki, opis, przekształcenia, warunki: dzisiejsze widżety). Zaznaczona kolumna jest wyróżniona w arkuszu. Wyniki i panel opcji pozostają widoczne. |
| A5 | Panel opcji | Wybrane zmienne jako etykiety (chipy) z × zamiast listy w ramce; obok pole „+ przeciągnij z danych” (dwukierunkowy przenośnik → zostaje jako alternatywa?). Statystyki w siatce dwukolumnowej. Wybór wykresu jako przełącznik segmentowy Brak / Histogram / Pudełkowy. Sekcje „Założenia”, „Zaawansowane”, „Opis metod” zwinięte jak dziś, z krótkim podglądem po prawej („24 opcje ›”, „wył. ›”). |
| A6 | Wyniki | Tabele bez zmian merytorycznych (linie APA). Wykres pudełkowy z grupami zagnieżdżonymi na osi (dawka, pod spodem supp) i kolorem = supp zamiast etykiet „OJ / 500”. |
| A7 | Pasek stanu | Jak dziś (Gotowy, wiersze, odfiltrowane, edytowane), tylko w nowym stylu. |

Pytania do oceny:

- A1–A3, A7: czy to głównie CSS (+ drobne zmiany DOM w `index.html` / `ribbon.ts`)? Ile plików
  upstreamu trzeba dotknąć? Czy da się to zamknąć w osobnym arkuszu `jupwr-theme.css`
  ładowanym po stylach jamovi? Jak to się ma do trybu wysokiego kontrastu i ewentualnego trybu ciemnego?
- A3: czy `jmv-splitpanel` pozwala na odstępy/karty bez zmiany logiki trybów `mixed/data/results`
  i animacji przejść? Co z zachowaniem przy małej szerokości okna?
- A4: największa niewiadoma wariantu A. Czy `jmv-variable-editor` można zamontować wewnątrz
  sekcji `main-table` (zamiast nad splitpanelem)? Czy da się zrobić tryb „zwarty” z linkiem do
  pełnego? Co z nawigacją ‹ › między zmiennymi, edycją wielu kolumn naraz, kolumnami
  wyliczanymi/przekształconymi/filtrami (te mają dużo treści: zostają w trybie pełnym na górze?).
  Skala zmian w `variableeditor.ts`, `main.ts`, `viewcontroller.ts`.
- A5: kontrolki są w iframe analysisui i są wspólne dla WSZYSTKICH modułów (jmv, scatr, nasze).
  Czy chipy zamiast listy to zmiana samego CSS kontrolki `VariablesListBox`/`TargetListBox`,
  czy nowa kontrolka? Wpływ na przeciąganie, klawiaturę i dostępność. Przełącznik segmentowy dla
  wykresu: czy istniejąca kontrolka (RadioButton/ComboBox) da się tak ostylować, czy to zmiana
  `.u.yaml` per moduł (jeśli tak, oszacuj liczbę plików).
- A6: zmiana w R (`jEksplor/R/*.b.R`, ggplot: `interaction()` + `facet_grid(switch = "x")` lub
  `ggh4x`?). Oddzielnie od klienta. Czy dotyczy też scatr `jmvbox`?

---

## 4. Wariant C: „Raport jako główna powierzchnia”

Idea: wyniki stają się dokumentem studenta (sprawozdanie), dane i opcje to narzędzia obok.

| ID | Element | Opis docelowy |
|----|---------|---------------|
| C1 | Dokument w centrum | Wyniki jako strona dokumentu (biała kartka ~700 px na szarym tle). Między blokami analiz akapity tekstu studenta (krój do decyzji; domyślnie obecne kroje jUPWR). Nagłówki sekcji („2. Opis”). Pod blokiem przycisk „+ analiza · tekst · wykres”. |
| C2 | Spis treści | Lewa kolumna z listą sekcji i analiz (klik = przewinięcie), „+ Nowa sekcja”. |
| C3 | Pole „Dodaj analizę…” | W pasku górnym pole wyszukiwania po nazwach analiz z menu (np. „test t”, „histogram”), wynik wstawia analizę w bieżące miejsce dokumentu. **Uzupełnienie** wstążki/menu, nie zamiennik. Do decyzji: czy wstążka zostaje w C. |
| C4 | Opcje jako szuflada bloku | Zaznaczony blok analizy ma bordową ramkę; jego opcje w prawej szufladzie (~290 px) „Ustawienia bloku”: zmienne, statystyki, „W raporcie pokaż” (tabela z numerem, wykres, opis metod), „Zaawansowane ›”. |
| C5 | Dane w dolnej szufladzie | Arkusz jako wysuwany panel na dole centralnej kolumny (~290 px, uchwyt do przeciągania, zwijany): nagłówek „Dane · 60 wierszy”, Filtry, Nowa zmienna. |
| C6 | Edytor zmiennej w dymku | Klik w nagłówek kolumny w szufladzie danych otwiera dymek (popover ~300 px): nazwa, poziom pomiaru (segmentowo), poziomy, „Przekształć…”, „Wszystkie ustawienia” (pełny edytor). |
| C7 | Numeracja | „Tabela 1. Statystyki opisowe długości (len)”: numer i podpis pod/nad tabelą, zachowany w eksporcie .docx/.odt/PDF. |
| C8 | Eksport | Przycisk „Eksportuj” w pasku górnym (cały dokument). |
| C9 | Ustawienia aplikacji | Przycisk w pasku górnym → ta sama szuflada ustawień co dziś. |

Pytania do oceny:

- C1: co dziś umieją adnotacje jamovi (`annotationtab.ts`, resultsview)? Czy tekst między
  analizami, nagłówki sekcji i ich zapis w `.omv` + eksport już istnieją? Wtedy C1 to głównie
  styl (kartka, krój), a nie nowa funkcja. Czy wyniki są jednym iframe'em czy wieloma (wpływ na
  ciągłość „dokumentu”, przewijanie, zaznaczanie)?
- C2: czy da się zbudować spis z istniejącego modelu wyników/adnotacji (`resultspanel.ts`)?
- C3: dane do wyszukiwania są w `analyses.ts` / menu wstążki (tytuły, podtytuły, `menuGroup`,
  po polsku z i18n). Oszacuj: komponent + wstawianie analizy w wybrane miejsce (czy jamovi pozwala
  wstawić analizę między istniejące, czy tylko na koniec?).
- C4: czy sekcję `main-options` da się przypiąć na stałe po prawej (`anchor: 'right'`), obok
  wyników zamiast nad arkuszem? Jak zachowa się logika trybów splitpanelu? „W raporcie pokaż”:
  czy to istniejące opcje modułów (widoczność elementów), czy nowy mechanizm (ukrywanie elementu
  wyniku z poziomu klienta)? To drugie dotyka serwera/protokołu, więc oznacz jako ryzyko.
- C5: największa niewiadoma wariantu C. Splitpanel jest dziś POZIOMY (lewo-prawo). Układ
  z danymi na dole wymaga panelu pionowego lub zagnieżdżenia. Skala zmian w `splitpanel.ts`,
  `main.ts`, `tableview.ts` (wirtualizacja wierszy przy zmiennej wysokości), skróty klawiszowe,
  tryby `data/results`. Czy prościej: arkusz jako trzecia kolumna jak dziś (C bez C5)?
- C6: dymek nad nagłówkiem: osadzenie widżetów z `vareditor/` w popoverze; konflikty z menu
  kontekstowym nagłówka (`contextmenu/`).
- C7: numeracja w resultsview + eksport (`client/main/formatio`, `common/formatio`?). Czy
  kolejność numerów da się liczyć w kliencie bez zmian w R?

---

## 4b. Elementy pomocnicze: menu, ustawienia, otwieranie danych (E1–E3)

Niezależne od wyboru A/C (pasują do obu). Struktura i głębokość menu bez zmian: tylko
czytelność i wygląd, z wyjątkiem E3c.

| ID | Element | Opis docelowy |
|----|---------|---------------|
| E1a | Rozwijane menu analiz (wstążka) | Panel ~340 px, zaokrąglony (12 px), z cieniem. Pozycja = ikona liniowa 22 px po lewej + tytuł (pogrubiony) + podtytuł pod tytułem, **wyrównany do lewej krawędzi tytułu** (dziś podtytuł jest wcięty i wygląda jak osobna pozycja). Większe pole kliknięcia (~9×12 px wypełnienia), wyraźne podświetlenie przy najechaniu/klawiaturze. |
| E1b | Nagłówek podgrupy | `menuSubgroup` (np. „Regresja logistyczna”) jako mały napis wersalikami z linią poziomą, NIE szary pas (dziś wygląda jak klikalna pozycja). |
| E1c | Ikony pozycji | Skąd brać ikony dla pozycji menu (dziś tylko kategorie mają ikony)? Opcje: nowe pole w `.a.yaml` (np. `menuIcon`), mapa w `client/common/jupwr.ts`, brak ikon dla modułów zewnętrznych. Oceń. |
| E1d | BŁĄD do naprawy niezależnie od redesignu | W menu Regresja ostatnia pozycja wyświetla tytuł „N wartości” i podtytuł „Wielomianowa” (jmv `logRegMulti`). Prawdopodobnie zła pozycja w i18n (tytuł przetłumaczony z innego klucza). Znajdź źródło (i18n jmv vs `JUPWR_*` w kliencie). Docelowo: „Wielomianowa” / „wiele kategorii bez kolejności”. Analogicznie sprawdź podtytuły „Porządkowa” i „Korelacja cząstkowa” (dziś puste). |
| E2a | Ustawienia: grupowanie | Szuflada po prawej (~420 px) z nagłówkiem „Ustawienia” i ×. Sekcje: Liczby w wynikach (symbol dziesiętny, cyfry znaczące, wartości p) · Wygląd wyników (powiększenie, czcionka wyników, bibliografia) · Wykresy (schemat, paleta) · Pliki, układ i język (braki przy imporcie, kopie odzyskiwania, „Interface” Klasyczny/…, język, jeśli dostępny) · Dla zaawansowanych (składnia R, tryb programisty). Stopka: wersja + „Przywróć domyślne”. |
| E2b | Kontrolki | Ustawienia dwuwartościowe jako przełącznik segmentowy (Przecinek/Kropka; układ interfejsu), wartości logiczne jako przełącznik/checkbox po prawej. Polskie etykiety bez anglicyzmów („Pokaż składnię R” zamiast „Pokaż składnię (syntax)”; „3 cyfry” zamiast „3 cyf.”). |
| E2c | Podgląd formatu liczb | Wiersz „Podgląd: 12,3 · 0,457 · p = 0,003 · p < 0,001” aktualizowany przy zmianie symbolu/cyfr. Użyj istniejącego formatowania z klienta (`client/common/formatting.ts`?). |
| E2d | Miniatury motywów wykresów | Wybór schematu/palety z miniatur (mały wykres słupkowy w kolorach palety) zamiast listy. Kolory pobierać z ISTNIEJĄCYCH definicji palet (palety jUPWR nie zmieniamy). Skąd klient zna kolory palet, czy musi pytać R? |
| E2e | „Przywróć domyślne” | Czy istnieje mechanizm resetu ustawień (`client/main/settings.ts`)? |
| E3a | Menu Plik | Zostaje lista akcji po lewej (Nowy, Otwórz, Importuj, Zapisz, Zapisz jako, Eksportuj). Bez zmian funkcjonalnych, tylko styl. |
| E3b | Widok „Otwórz” | Po prawej przełącznik: Ostatnie / Ten komputer / Biblioteka zbiorów + wyszukiwarka. „Ostatnie” jako tabela: znacznik formatu (OMV/CSV/XLSX), nazwa, folder (lub „Biblioteka · moduł”), data otwarcia. Dziś ostatnie pliki są małą listą w lewym dolnym rogu. Pole „Przeciągnij plik tutaj” z listą obsługiwanych formatów i przyciskiem „Wybierz z dysku…”. |
| E3c | Podgląd importu CSV (NOWA FUNKCJA) | Przed otwarciem CSV: wybór separatora (; , tab) i symbolu dziesiętnego, podgląd pierwszych wierszy, wykryte typy kolumn. Cel: polski Excel (średnik + przecinek dziesiętny). To dotyka importu po stronie serwera (`server/…/formatio`), więc oznacz wyraźnie, czy da się to zrobić bez zmian w `server/` i co mówi o tym upstream (czy jamovi ma już opcje importu CSV). |

Pytania do oceny: które z E1–E3 to sam CSS/DOM w kliencie (`ribbonmenu.ts`, `ribbon/appmenu.ts`?,
`backstage.ts`, `settings.ts`), a które wymagają logiki lub zmian poza `client/`. E1d i ewentualne
błędy w etykietach ustawień zgłoś osobno jako szybkie poprawki do zrobienia od razu.

---

## 5. Hybrydy do rozważenia (oceń osobno)

1. **A jako baza + C1, C2, C7** (dokument-like wyniki, spis, numeracja) bez szuflad C4/C5.
   Prawdopodobnie najlepszy stosunek efektu do kosztu.
2. **A + C3** (wyszukiwarka analiz jako uzupełnienie wstążki).
3. **A4 vs C6**: który sposób edycji zmiennej jest tańszy i mniej konfliktowy z upstreamem.

---

## 6. Oczekiwany raport (`docs/redesign/ocena-wykonalnosci.md`)

Dla KAŻDEGO elementu (A1–A7, C1–C9, E1a–E3c) wiersz tabeli:

| ID | Pliki (ścieżki) | Rodzaj zmiany (CSS / DOM / logika TS / R / serwer) | Wielkość S/M/L/XL | Konflikty z upstreamem (niskie/średnie/wysokie, dlaczego) | Ryzyka (dostępność, klawiatura, eksport, .omv, Windows/macOS) | Czy istnieje już coś podobnego w jamovi |

Potem:

1. **Podsumowanie wariantu A i C** (łączny koszt, co da się zrobić etapami, co wymaga zmian poza `client/`).
2. **Rekomendowana hybryda** i proponowana kolejność etapów (każdy etap osobnym PR, z możliwością
   wydania w 1.3.x/1.4.0).
3. **Strategia utrzymania przy merge'ach upstreamu** (np. jeden plik `client/jupwr/theme.css`
   + tokeny CSS; minimalne łaty w plikach jamovi; lista plików upstreamu, które trzeba dotknąć).
4. **Rzeczy, których NIE da się zrobić bez ruszania `server/`/`engine/`**, wyraźnie oznaczone.
5. **Otwarte pytania do mnie** (decyzje projektowe, które blokują ocenę).

Zasady: nie zgaduj, podawaj ścieżki i numery linii, a tam, gdzie nie sprawdziłeś, napisz
„niezweryfikowane”. Nie uruchamiaj pełnego builda Dockera tylko na potrzeby oceny; jeśli
robisz szkic w gałęzi `spike/…`, wystarczy `npm run build` w `client/` (lub odpowiednik
z `package.json`), a w raporcie opisz, na czym sprawdzałeś.

---

## 7. Stan wdrożenia (gałąź `spike/redesign`, aktualizacja 2026-11-01)

Dla projektującego: co z makiet już działa w aplikacji, czego nie da się zrobić (albo
tylko dużym kosztem) i jakie ograniczenia programu warto uwzględnić w kolejnych makietach.

### 7.1. Mechanizm: wygląd do wyboru

W ustawieniach jest pozycja „Interface”: **Klasyczny** albo **Lekki (A)** (etykiety na razie
po angielsku, tłumaczenia dojdą). Wybór zapisuje się i obejmuje całe okno, także panel opcji
i wyniki. Wygląd klasyczny pozostaje bez zmian (sprawdzone porównaniem zrzutów piksel
w piksel). Wariant A to osobny arkusz stylów zbudowany z kolorów, które jUPWR już ma,
plus dwóch ciepłych szarości z makiety. Wszystkie kolory interfejsu są zebrane w jednym
pliku `client/common/jupwr-colors.css` (tokeny `--jupwr-*`, z opisem roli każdego koloru);
kolory wykresów są osobno, w `jmvcore/R/themes.R`. Można testować nowy wygląd na zajęciach
bez ryzyka dla starego.

### 7.2. Wprowadzone (wariant A)

| ID | Co działa | Uwagi |
|----|-----------|-------|
| A1 | Zakładki jako przełącznik segmentowy; przycisk ustawień z ikoną suwaków; pośrodku paska nazwa zbioru i liczba wierszy („Długość zębów · 60 wierszy”) | Nazwa polska, gdy zbiór pochodzi z biblioteki z opisem; inaczej tytuł pliku |
| A2 | Biała wstążka 52 px; kategorie jako kolorowa ikona 24 px (te same co w wyglądzie klasycznym) + podpis w „pigułce”; otwarta kategoria bordowa na różowym tle; „Moduły” z obrysem | Wypróbowane też ikony liniowe czarno-białe (zbyt smutne) — zestaw 22 ikon zostaje w `assets/jupwr-line/` jako alternatywa (np. wersja dwukolorowa bordo + pomarańcz). Ikony Wykresów są blade (cienkie linie, jasne kolory) |
| A3 | Dane, opcje i wyniki jako białe karty (12 px) na jasnoszarym tle. Nagłówki kart: „Dane · nazwa · 60 × 3 · filtry 0” + „O zbiorze”; „Wyniki” + „Kopiuj” (cały dokument) i „Eksportuj” (Plik → Eksportuj) | Panel opcji ma własny nagłówek (tytuł analizy i przycisk zwinięcia) |
| A4 | Edytor w karcie danych: edytor zmiennej, filtry, zmienna wyliczona i przekształcona otwierają się pod nagłówkiem karty danych (wysokość 300 px), arkusz zsuwa się w dół, wyniki pozostają widoczne | To dzisiejsze edytory w węższym układzie, bez zwartego formularza z makiety (patrz 7.5) |
| A7 | Pasek stanu jako stopka karty danych | |
| arkusz | Białe nagłówki kolumn, szare numery wierszy bez tła, cienkie linie, bordowa ramka zaznaczonej komórki | Tylko kolory i linie |
| opcje | Karta opcji na ciepłej szarości (`#F4F2F1`), biały nagłówek, białe listy i pola z cienką zaokrągloną ramką, bordowe checkboxy, „Założenia” i „Zaawansowane” jako ciemniejsze zaokrąglone pola (`#EAE6E4`) | Całkiem biały panel męczył oczy i elementy się zlewały. Ciepłe szarości to JEDYNE nowe kolory (z makiety, za zgodą), tokeny `--jupwr-warm-*`. Tylko wygląd, układ kontrolek bez zmian |
| wyniki | Białe tło; przy zaznaczonej analizie pozostałe przyblakłe zamiast szarego panelu | |
| E1a, E1b | Menu: panel 12 px z cieniem, tytuł pogrubiony, podtytuł wyrównany pod tytułem, nagłówek podgrupy wersalikami z linią | |
| E1d | Naprawione w OBU wyglądach: „Wielomianowa / wiele kategorii bez kolejności”, „Porządkowa / kategorie uporządkowane”, „Korelacja cząstkowa / przy kontroli innych zmiennych”, „Dwie kategorie / zmienna zależna dwupoziomowa” | Źródło: angielskie etykiety jmv („N Outcomes”), nadpisane w kliencie |
| E2 (wygląd) | Szuflada „Ustawienia” z ×, biała, sekcje wersalikami, zaokrąglone pola | Kolejność i grupowanie bez zmian (7.3) |
| E3a | Menu Plik jako bordowa listwa z białym tekstem, zaznaczenie jako półprzezroczysta pigułka | |

### 7.3. W toku albo możliwe, ale wymaga programowania

| ID | Element | Stan / dlaczego nie sam styl | Koszt |
|----|---------|------------------------------|-------|
| A4+ | Zwarty edytor zmiennej z makiety (pola w jednej kolumnie, poziomy obok, ‹ ZMIENNA ›) | Dziś w karcie jest pełny edytor; zwarty układ pól to przestawienie widżetów w `vareditor/` | średni |
| E1c | Ikony przy pozycjach menu | Zestaw ikon i źródło (pozycje modułów zewnętrznych nie będą ich miały) | średni + grafika |
| E2a–b | Inne grupowanie ustawień, przełączniki segmentowe, „Pokaż składnię R”, „3 cyfry” | Zmiany w kodzie szuflady i w tłumaczeniach | mały–średni |
| E2c | Podgląd formatu liczb | Klient ma funkcję formatowania, którą można wykorzystać | mały |
| E2d | Miniatury palet | Klient zna tylko NAZWY palet; kolory są w R. Trzeba je skopiować do klienta (dwa miejsca do utrzymania) | mały |
| E2e | „Przywróć domyślne” | Mechanizmu nie ma; do napisania w kliencie | mały |
| E3b | Widok „Otwórz”: tabela „Ostatnie”, przełącznik Ostatnie/Ten komputer/Biblioteka, pole „Przeciągnij plik tutaj” | Dziś menu Plik to nakładka (~750 px), nie pełny ekran; „Ostatnie” to mała lista w lewym dolnym rogu. W wersji desktopowej „Ten komputer” otwiera systemowe okno wyboru pliku | średni |
| A6 | Wykres pudełkowy z grupami zagnieżdżonymi na osi | Zmiana w R (moduł jEksplor), niezależna od wyglądu | mały–średni |

### 7.4. Nie da się albo nie warto (ograniczenia programu)

1. **Trzy kolumny naraz (dane + opcje + wyniki).** Panel opcji ZASTĘPUJE arkusz (wysuwa się
   na jego miejsce). Treść panelu opcji jest generowana z definicji każdego modułu i
   zaprojektowana na ok. 585 px i dwie kolumny; zwężenie do ~340 px rozjechałoby wiele
   analiz. Układ: po lewej dane ALBO opcje, po prawej wyniki (przyjęte w makiecie).
2. **Chipy zmiennych i przełącznik „Brak / Histogram / Pudełkowy” (A5).** Kontrolki panelu
   opcji są wspólne dla wszystkich modułów i powstają z ich definicji. Chipy to nowa kontrolka
   (przeciąganie, klawiatura, dostępność), przełącznik segmentowy wymagałby zmian
   w definicjach wielu analiz. Wygląd list i pól da się zmieniać, sposób działania nie.
3. **Arkusz:** położenie i wysokość wierszy liczy program, więc zmieniać można kolory, linie
   i czcionkę, ale nie wysokość wierszy ani odstępy.
4. **Wyniki** to osobna ramka (iframe) dla każdej analizy. Wspólne tło i przewijanie działają,
   ale „dokument” z wariantu C nie jest jedną stroną.
5. **E3c podgląd importu CSV:** wymaga zmian w serwerze (poza zakresem). Stan dziś: separator
   (w tym średnik) serwer wykrywa sam, a symbol dziesiętny bierze z ustawienia „Symbol
   dziesiętny”. Dla polskiego Excela wystarczy ustawić przecinek.
6. **Kolory** zostają paletą jUPWR; kolory w makietach traktujemy jako orientacyjne.

### 7.5. Otwarte pytania do projektu

- E1c: czy ikony przy pozycjach menu są warte narysowania zestawu ikon?
- E2: docelowe grupowanie ustawień. Uwaga: część pozycji (powiększenie, język, wersja)
  istnieje tylko w aplikacji desktopowej, a w przeglądarce jest ukryta.
- A4: karta danych ma ok. 600 px szerokości. W edytorze zmiennej przekształconej reguła
  „jeżeli … użyj …” łamie się już na dwie linie (pole „wartość” pod spodem). Jak ma wyglądać
  w wąskiej karcie (łamanie, przewijanie, rozszerzanie karty na czas edycji)? Czy edytor
  ma mieć stałe 300 px, czy dopasowywać wysokość do treści (filtry z kilkoma warunkami)?

Znaleziony przy okazji błąd (wersja 1.3.0, także w wyglądzie klasycznym): gdy wyniki są
puste, logo z ekranu powitalnego rysuje się NAD otwartą szufladą ustawień. W wariancie A
naprawione; w klasycznym do decyzji.
