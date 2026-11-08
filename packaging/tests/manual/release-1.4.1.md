# Weryfikacja jUPWR 1.4.1 — 2026-11-08

Maszyna: Linux/WSL, Docker. Źródła w lokalnym drzewie roboczym, bez tagu wydania.
Binarki aplikacji i pliki `.jmo` jRISK 0.4.5 oraz jSpace 0.3.4 zostały
zbudowane natywnie na Windowsie i macOS — potwierdzenie użytkownika
z 2026-11-08. Poniższe testy automatyczne wykonano w Dockerze; potwierdzenie
buildów natywnych nie określa zakresu testów na tych platformach.

## Sprawdzone automatycznie

- `docker compose --profile main build`: obraz `jupwr/jupwr:1.4.1` zbudowany.
- `docker compose --profile main up -d`: uruchomiony, HTTP na 127.0.0.1:41337 odpowiada.
- Backend: 271 testów przeszło, w tym prawdziwy C++/R i test zapisywalnego
  deskryptora `fsync` emulujący wymagania Windows.
- Klient: 377 testów w 30 plikach przeszło.
- Kontrola wydania: 7 testów przeszło; `release-check.sh --metadata-only`
  bez ostrzeżeń, wersje jUPWR i listy modułów spójne między platformami.
- Testy audytu scatr, w tym heatmapa z samymi brakami: przeszły.
  Pozostaje wcześniejsze ostrzeżenie ggplot2 o `size` dla linii w stripmean.
- Testy R modułów i runtime: 159 przypadków, 5876 poprawnych asercji, bez błędów.
  Jeden test porównania z nortest pominięty (pakietu brak w obrazie; CI go
  instaluje). distrACTION zgłasza wcześniejsze ostrzeżenia precyzji `pnt`
  dla skrajnych parametrów rozkładu t (238 ostrzeżeń).
- 17 wykresów scatr: rzeczywisty silnik zwraca zasoby SVG ze ścieżkami
  wektorowymi; 51 eksportów PNG/SVG/PDF, zapis/otwarcie/przeliczenie OMV poprawne.
  Testy audytu i eksportów scatr uruchamiane są także przez CI: 9 przypadków,
  123 poprawne asercje.
- Catppuccin Latte/Mocha: poprawne kolory palety oraz gradientów sekwencyjnych
  i rozbieżnych w zbudowanym runtime.
- `test-release-141.py`: osiem analiz przez rzeczywisty C++/R; każda wyeksportowana
  do PNG, SVG i PDF (łącznie 24 pliki).
- Prawdziwy `.omv` z distrACTION 1.4.0: otwarcie, przeliczenie z p panelu,
  zapis i ponowne otwarcie w nowym runtime poprawne; zachowane 150 obserwacji,
  P(X = 0) dla Bin(12; 0,15) = 0,142.
- Oba nowe pliki audytu otwarte również przez HTTP `/open` działającej aplikacji
  z katalogu Dokumenty (odpowiedź `status: OK`). Istniejących plików nie nadpisywano.
- Percentyle: 15 testów jEksplor, 4061 asercji (jeden test nortest pominięty).
  Rzeczywisty silnik sprawdzony z `--percentiles-only`: zmiana ustawienia
  profilu kropka → przecinek → kropka przelicza wartości i podpisy oraz
  usuwa stare percentyle z pamięci podręcznej wyników.

- Dalsza stabilizacja: trzy testy fallbacku SVG (12 asercji: błąd, samo
  ostrzeżenie bez otwarcia urządzenia, poprawny SVG), trzy testy F/Weibulla
  (35 asercji) i test q Nemenyiego (8 asercji). Włączone do CI.
- `--boundaries-only`: rzeczywisty C++/R sprawdził PNG dla kwantyli F przy
  p = 0/1, komunikat Weibulla przy β = 0,001 bez utraty wyniku P(X ≤ 1),
  i q Nemenyiego dla 12 jednostek z 3 pomiarami.
- Konwencja q odpowiada [PMCMRplus](https://github.com/cran/PMCMRplus/blob/master/R/frdAllPairsNemenyiTest.R):
  dodatnia statystyka rozstępu studentyzowanego; p i grupy jednorodne bez zmiany.

## Dane i test ręczny

W interfejsie: **Otwórz → Ten komputer → Dokumenty → jupwr-test-audyt**.

- `distrACTION-1.4.1.csv`: 150 obserwacji, kolumny `kontrole`, `sukces`,
  `czas`, `skrajne`. `kontrole`: dla 0–5 liczności 19, 46, 47, 25, 11, 2.
  `sukces`: 41 „tak”, 109 „nie”; `skrajne` zawiera pojedyncze 10000000.
- `distrACTION-1.4.0.omv`: wygenerowany przez dawny moduł ze źródeł commita
  `b2090c5c`, z `empParams=estimated`, `empBand=true`, `empChiSq=true`.
  Nie jest to ręcznie zmodyfikowane archiwum nowej analizy. Po przeliczeniu
  w 1.5.x model używa p panelu zgodnie z decyzją opisaną w CHANGELOG.
- `plots-vector-1.4.1.csv`: 120 deterministycznych obserwacji, dwie zmienne
  jakościowe, trzy ilościowe i liczności; do weryfikacji 17 wykresów scatr.

- `nemenyi-1.4.1.csv`: 12 jednostek, trzy pomiary A/B/C; każda jednostka ma
  rangi 1/2/3, przy niezerowej zmienności różnic. Oczekiwane q: A–B i B–C
  około 3,4641, A–C około 6,9282.

Do końcowej kontroli wizualnej:

1. Dwumianowy, `kontrole`, n = 12, p = 0,15, P(X = 0): tabela jak w kluczu
   zadania 4.3; 19 obserwowanych wobec około 21,3 oczekiwanych.
2. Dwumianowy, `sukces`, tryb zero-jedynkowy: wybrać kategorię „tak”;
   p̂ = 41/150, model główny nadal z parametrów panelu.
3. Poissona, `skrajne`, λ = 2: domyślnie przedziały i nota. „Tabela liczności →
   pokaż wszystkie wartości” wyłącza grupowanie (dla tego pliku to bardzo duża
   tabela). Tryb „pojedyncze wartości (zapytaj przy dużej tabeli)” zamiast podziału
   pokazuje komunikat; momenty i punkty nadal dostępne. Do krótkiej próby pełnej
   tabeli użyć mniejszego zakresu. Obliczenie można anulować.
4. Eksploracja → Zmienne ilościowe: przy „Symbol dziesiętny → Przecinek”
   wpisać `2,5; 97,5`, przy „Kropka” — `2.5; 97.5`. Wyniki to dwa percentyle
   z podpisami odpowiadającymi ustawieniu. Domyślne `25;50;75` działa w obu
   trybach; dawne `25,50,75` pozostaje listą przy kropce dziesiętnej. Przy
   przecinku niewłaściwy zapis z kropką lub kilkoma przecinkami w liczbie
   zgłasza komunikat zamiast cichej zmiany interpretacji. Zmiana symbolu
   przelicza także istniejącą tabelę; przy wyłączonych percentylach zapis
   nie jest walidowany.
5. Przełączać widoczność wykresu/bootstrapu/opisu metod przy ziarnie 0 w jCI
   i jperm; sprawdzić zgodność wykresu z bieżącą tabelą. Wyłączenie Studenta
   w jTestyT usuwa jego wiersz.
6. Przełączać Classic / Lekki / Lekki przyciemniony, zmienić powiększenie
   i sprawdzić dostępność dolnych ustawień oraz eksport z obiema paletami Catppuccin.
7. Zakładka „Wykresy”: słupkowy, histogram, słupkowy zmiennej dyskretnej,
   gęstości, ridgeline, mozaikowy, skumulowany słupkowy, kolisty słupkowy,
   lizakowy, Pareto, mapa ciepła, korelogram, hexbin, radarowy, mapa drzewa,
   waflowy i profil średnich. Sprawdzić ostrość po powiększeniu, zmianę rozmiaru,
   grupowanie, tytuły i legendy, kopiowanie do dokumentu oraz eksport do PDF.
   Przy starych dokumentach zmienić opcję wykresu lub przeliczyć analizę, aby
   zastąpić zapamiętany wcześniej PNG nowym zasobem SVG.

8. Rozkład F: df₁ = 1, df₂ = 10, „Oblicz kwantyl”, p = 0, potem p = 1:
   wyniki 0 / Inf bez awarii. Przy p = 1 całe pokazane pole jest zacieniowane.
   Dla df₁ < 2 gęstość zaczyna się w dodatnim x, bez sztucznej podmiany szczytu.
9. Weibull: β = 0,001, η = 1, P(X ≤ 1): P = 0,632 w tabeli, przy wykresie
   komunikat o zakresie przekraczającym możliwości liczbowe. Nie zmieniono β
   ani nie podniesiono dolnej granicy dopuszczalnego kształtu.
10. ANOVA powtórzonych pomiarów na `nemenyi-1.4.1.csv`: jednostka `id`,
    zależna `wynik`, czynnik wewnątrz `czas`, „Nieparametrycznie” i porównania
    parami. Sprawdzić q wobec wartości wyżej. Obsługa PNG przy braku urządzenia
    SVG została zasymulowana w testach Linux; fizyczny czysty Mac nadal do kontroli.

## Powtarzanie testów

Testy kompilacji modułów bez zmieniania checkoutu (w Dockerze z R i zależnościami):

```sh
bash packaging/scripts/test-statistics.sh
```

Test rzeczywistego silnika, migracji i eksportów:

```sh
docker run --rm --network none -v "$PWD:/source:ro" --entrypoint python3 \
  jupwr/jupwr:1.4.1 /source/packaging/scripts/test-release-141.py \
  --data /source/packaging/tests/manual/distrACTION-1.4.1.csv \
  --legacy /source/packaging/tests/manual/distrACTION-1.4.0.omv
```

Skrypt korzysta z oddzielnej sesji i katalogu tymczasowego odzyskiwania.
Nie montuje ani nie czyści wolumenu odzyskiwania użytkownika.

Test zasobów wektorowych i eksportów scatr:

```sh
docker run --rm --network none -v "$PWD:/source:ro" --entrypoint python3 \
  jupwr/jupwr:1.4.1 /source/packaging/scripts/test-release-141.py \
  --plots-only --data /source/packaging/tests/manual/plots-vector-1.4.1.csv
```

Wyniki mają być zasobami `.svg` zawierającymi ścieżki wektorowe; test sprawdza
również eksport PNG/SVG/PDF i zapis, otwarcie oraz przeliczenie OMV. Cairo może
osadzić gradient legendy jako małą bitmapę; geometria wykresu pozostaje wektorowa.

Test F/Weibulla i tabeli Nemenyiego:

```sh
docker run --rm --network none -v "$PWD:/source:ro" --entrypoint python3 \
  jupwr/jupwr:1.4.1 /source/packaging/scripts/test-release-141.py \
  --boundaries-only --data /source/packaging/tests/manual/nemenyi-1.4.1.csv
```

## Warunki pozostałe przed publikacją

- Windows: potwierdzić rzeczywisty zapis/otwarcie OMV, także kopii odzyskiwania.
- macOS: sprawdzić eksport wektorowy na maszynie docelowej.
- Pełny `release-check.sh` na Linuxie obecnie zgłasza dwa braki: pliki
  `jRISK_0.4.5-linux.jmo` i `jSpace_0.3.4-linux.jmo`. Kontrola metadanych jest zielona;
  nie traktować jej jako potwierdzenia gotowych artefaktów opcjonalnych.
- Zmiany submodułu `plots` i repo głównego wypchnięte na `main`; macierz
  artefaktów Windows/macOS uzupełniona. Tag i publikacja wydania pozostają osobnym krokiem.
