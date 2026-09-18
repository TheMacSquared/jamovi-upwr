# jPomiar — Pomiary i niepewność

Opcjonalny moduł jUPWR, wersja 0.2.0. Instalacja pliku `.jmo` właściwego dla
platformy przez **Moduły → Sideload**; analizy trafiają do menu **Pomiary**.
Moduł jest przeznaczony do podstawowego kursu statystyki dla kierunków technicznych
oraz zajęć laboratoryjnych. Nie wymaga jSpace ani bibliotek GIS.

## Zakres

- **Powtarzane pomiary**: liczba pomiarów i braków, średnia, próbne SD, standardowa
  niepewność średniej `u_A = SD/√n`. Opcjonalne porównanie z odniesieniem i wykres
  w kolejności wierszy. Braki pomijane; nieskończoności odrzucane; co najmniej
  dwa poprawne pomiary. Odniesienie nie służy do testowania obciążenia.
- **Budżet niepewności**: wartość z opcjonalną poprawką; niezależne składniki:
  standardowa niepewność typu A, wzorcowanie `U/k`, rozdzielczość `d/√12`.
  Współczynniki wrażliwości wynoszą 1. Wynik: `u_c`, wybrany mnożnik `k`,
  `U = k u_c`, granice wynik ± U, wkłady do wariancji i ich udziały procentowe.
- **Propagacja niepewności**: suma, różnica, iloczyn i iloraz dwóch wielkości,
  z opcjonalną korelacją. Tabela pochodnych i trzech składników wariancji,
  w tym osobny składnik kowariancyjny (może być ujemny). Bez arbitralnego
  przypisywania kowariancji do udziałów poszczególnych wejść.

Opcjonalne funkcje i wykresy są domyślnie wyłączone. Budżet pozostaje pusty, dopóki
użytkownik nie włączy przynajmniej jednego składnika. Objaśnienia założeń są zawsze
widoczne. Wersja pilotażowa nie zawiera estymacji ważonej ani filtracji.

## Założenia i jednostki

Pomiary w serii dotyczą tej samej stałej wielkości; wzór SD/√n zakłada niezależność
i jednakową wariancję. Obserwacje kolejnych różnych obiektów nie są automatycznie
powtórzeniami tego samego pomiaru. Wykres pomaga zauważyć dryft, ale nie zastępuje
analizy autokorelacji.

Typ A i B oznaczają sposób oceny niepewności, a nie „losowy” i „systematyczny”.
W budżecie wprowadza się znaną poprawkę ze znakiem (jest **dodawana** do wyniku).
Niepewność tej poprawki musi być ujęta w składnikach. Wzorcowanie traktujemy tutaj
jako ocenę typu B na podstawie świadectwa.

Rozdzielczość to krok wskazania `d`, nie połowa przedziału. Przyjęto prostokątny
rozkład błędu zaokrąglenia na `[-d/2, d/2]`. Składnik przypisujemy do wyniku
bez dzielenia przez √n; jego zastosowanie trzeba uzasadnić modelem pomiaru.
Nie wolno podwójnie uwzględniać rozdzielczości już zawartej w innych składnikach.
Budżet obejmuje wyłącznie niezależne składniki addytywne, nie dowolną macierz
kowariancji.

Wszystkie składniki budżetu mają jednostkę wyniku. W propagacji `u(x)` ma
jednostkę `x`, a `u(y)` jednostkę `y`. Suma i różnica wymagają wspólnej jednostki;
iloczyn i iloraz mają odpowiednio iloczyn lub iloraz jednostek. Program nie
przelicza jednostek. Korelacja opisuje niepewności wejść, nie dowolną korelację
kolumn arkusza.

Propagacja używa `u_c² = c_x² u(x)² + c_y² u(y)² + 2 c_x c_y ρ u(x) u(y)`.
Dla sumy i różnicy jest to dokładna zależność wariancji, dla iloczynu i ilorazu —
linearyzacja pierwszego rzędu. Przy `u(y)/|y| ≥ 0,1` iloraz otrzymuje dodatkowy
komunikat; to wskazówka dydaktyczna, nie ścisłe kryterium poprawności.
Przy zerowej propagowanej wariancji modelu nieliniowego wynik jest wyraźnie
opisany jako przybliżenie, które może pomijać istotne wyrazy wyższego rzędu.

**Mnożnik k = 2 nie oznacza automatycznie 95% ufności.** Nie estymujemy efektywnych
stopni swobody ani współczynnika pokrycia. Do klasycznych przedziałów ufności
średniej z powtórzeń służy jCI. Zerowe składniki wprowadzone przez użytkownika
nie dowodzą braku niepewności pomiaru.

## Trzy ćwiczenia

### 1. Czy powtarzanie pomiaru usuwa przesunięcie?

Otwórz z biblioteki jPomiar **Powtarzane pomiary długości (syntetyczne)**
(`data/dlugosc.csv`). Zmienna: `dlugosc_mm`, jednostka: mm.
Wybierz **Pomiary → Powtarzane pomiary**, włącz wykres i odniesienie 100 mm.

Oczekiwane wyniki: n = 20, średnia = 100,111 mm, SD ≈ 0,026931 mm,
u_A ≈ 0,006022 mm, różnica względem odniesienia = 0,111 mm.

Porównaj wielkość rozrzutu z różnicą od odniesienia. Dlaczego duża liczba
powtórzeń nie musi dać prawidłowej wartości? Odniesienie jest tu dydaktycznym
punktem porównania — nie wykonujemy testu uwzględniającego jego niepewność.
Dane są sztuczne i nie pochodzą z rzeczywistego przyrządu.

### 2. Co najbardziej ogranicza niepewność wyniku?

W **Budżecie niepewności** wpisz średnią 100,111 mm, włącz poprawkę −0,100 mm.
Włącz typ A: 0,00602189 mm; wzorcowanie: U = 0,080 mm, k = 2;
rozdzielczość: d = 0,010 mm. Mnożnik wyniku: k = 2. Włącz wykres składników.
W tym ćwiczeniu umownie przyjmujemy niezależność i brak podwójnego liczenia
składnika rozdzielczości.

Oczekiwane: wynik = 100,011 mm, u_c ≈ 0,040554 mm, U ≈ 0,081107 mm.
Wzorcowanie dominuje w budżecie. Zmniejsz u_A dziesięciokrotnie i zobacz,
jak niewiele zmienia się U. Następnie zmniejsz U wzorcowania dwukrotnie.
Która poprawa daje większą korzyść i dlaczego?

### 3. Dlaczego korelacja może pomóc przy odejmowaniu?

W **Propagacji niepewności** wybierz różnicę: x = 10, y = 8,
u(x) = u(y) = 2 (jedna wspólna jednostka). Bez korelacji wynik to 2,
a u_c = √8 ≈ 2,828427. Włącz korelację ρ = 0,75: u_c = √2 ≈ 1,414214,
składniki wariancji wynoszą 4, 4 oraz −6.

Przełącz wzór na sumę: wynik = 18, u_c = √14 ≈ 3,741657. Wyjaśnij,
dlaczego ta sama zależność działa inaczej przy dodawaniu i odejmowaniu.

Rozszerzenie: prędkość jako iloraz x = 100 m, y = 20 s,
u(x) = 0,2 m, u(y) = 0,1 s, korelacja wyłączona.
Wynik ≈ 5 m/s, u_c ≈ 0,026926 m/s. Zwiększ niepewność czasu do 5 s
i omów komunikat o ograniczeniach linearyzacji.

## Budowanie i sprawdzanie

Najpierw referencyjny build aplikacji:

```sh
docker compose --profile main build
docker compose --profile main up -d
```

Moduł pozostaje opcjonalny; główny obraz nie instaluje go automatycznie.
Osobny target z tego samego Dockerfile eksportuje artefakt Linux:

```sh
bash packaging/scripts/build-jpomiar-docker.sh
```

Wynik: `packaging/build/dist/jPomiar_0.2.0-linux.jmo`. Zainstaluj przez Sideload
w uruchomionym jUPWR i wykonaj cztery ćwiczenia. Artefakt wymaga zgodnego środowiska
R i architektury kontenera. Budowa używa kopii źródeł wewnątrz obrazu.

Po poprawnym Dockerze, na odpowiednim systemie:

- macOS arm64: `bash packaging/scripts/macos/73-jmo-jpomiar.sh`;
- Windows x64: `packaging/scripts/windows/build.ps1`, krok 4h.

Wersja modułu ma być zgodna w `DESCRIPTION` i źródłowym `jamovi/0000.yaml`.
Nie dodawaj do commita wygenerowanych nagłówków R ani JS/metadanych kompilatora.

Pełna lokalna weryfikacja na Linux (wymaga R z zależnościami modułu i testthat,
Node oraz zależności w `jamovi-compiler/node_modules`):

```sh
bash packaging/scripts/test-jpomiar.sh
```

Skrypt kompiluje kopię źródeł w katalogu tymczasowym, uruchamia testy obliczeń
i integracyjne, a następnie usuwa pliki robocze. Nie zmienia źródeł w repozytorium.
Nie zastępuje testu sideloadu w aplikacji ani referencyjnego builda Docker.

Testy obliczeń można uruchomić bez kompilacji:

```sh
Rscript -e 'testthat::test_dir("jPomiar/tests/testthat", reporter="summary")'
```

Testy integracyjne uruchamiają się, gdy skompilowany przez jmc pakiet `jPomiar`
i `jmvcore` są na `.libPaths()` (np. przez `R_LIBS`). Sprawdzają tabele, przełączniki,
komunikaty, błędy wejścia i renderowanie wykresów. Bez zainstalowanego pakietu
są jawnie pomijane; urządzenie Cairo jest potrzebne do testu polskich etykiet wykresów.

## Podstawy metod

- [JCGM 100:2008 (GUM), szczególnie rozdziały 4–6](https://www.bipm.org/en/committees/jc/jcgm/publications).
- [NIST: budżety niepewności i współczynniki wrażliwości](https://www.itl.nist.gov/div898/handbook/mpc/section5/mpc56.htm).
- [NIST: terminologia i rozróżnienie ocen typu A/B](https://www.nist.gov/pml/nist-technical-note-1297/nist-tn-1297-appendix-d1-terminology).

## Kowariancja i elipsa 2D (od 0.2.0)

Analiza **Pomiary → Kowariancja i elipsa 2D** przyjmuje dwie różne kolumny
współrzędnych tego samego punktu. Wiersz oznacza sparowany pomiar X i Y;
wiersze z brakami usuwa się wspólnie. Wszystkie wyniki korzystają z tej samej
próby. Obie współrzędne muszą być w tej samej jednostce, w układzie płaskim.
Długość i szerokość geograficzna w stopniach wymagają wcześniejszego przeliczenia.

Tabele pokazują średnie położenie, korelację Pearsona, próbną macierz
kowariancji `S` (dzielnik `n−1`), oszacowaną kowariancję średniej `S/n`
i osie główne macierzy `S`. PCA nie standaryzuje współrzędnych: wartości własne
są wariancjami w kierunkach osi, a ich pierwiastki — odchyleniami standardowymi.
Kąt liczony jest od +X przeciwnie do wskazówek zegara, modulo 180°.
Wykres zachowuje proporcje 1:1; osie główne można włączyć osobno.

| Rodzaj | Półosie (λ — wartości własne S) | Znaczenie |
|---|---|---|
| Standardowa | `sqrt(λ)` | Elipsa rozrzutu 1 SD. Dla nieosobliwego rozkładu normalnego 2D o znanych parametrach pokrycie wynosi około 39,35%, nie 68%. |
| Rozrzutu | `sqrt(qchisq(p, 2) * λ)` | Przybliżony kontur rozkładu normalnego z parametrami oszacowanymi z danych. Nie gwarantuje odsetka punktów w próbie; nie jest dokładnym obszarem predykcji. |
| Ufności średniej | `sqrt(2*(n−1)/(n*(n−2)) * qf(p, 2, n−2) * λ)` | Wspólny obszar ufności dla dwóch składowych średniej metodą Hotellinga. Wymaga niezależnych pomiarów o dwuwymiarowym rozkładzie normalnym, n > 2 i nieosobliwej kowariancji. |

Poziom p dotyczy wyłącznie dwóch ostatnich wariantów. Elipsa standardowa nie
zależy od tego pola. Kowariancja średniej S/n zakłada niezależność powtórzeń
o jednakowej kowariancji. Nie obejmuje wspólnego błędu systematycznego przyrządu.

Dla punktów na prostej elipsa opisowa degeneruje się do odcinka; dla identycznych
pomiarów — do punktu. Takie dane nadal mają tabele opisowe, ale obszar Hotellinga
jest niedostępny. Za niemal osobliwą uznajemy macierz z mniejszą wartością własną
nie większą niż 10⁻¹² większej. Przy równych wartościach własnych kierunek osi
jest nieokreślony, więc nie podajemy kąta ani nie rysujemy arbitralnych osi.
Stała współrzędna oznacza nieokreśloną korelację, a nie korelację równą zero.

### Ćwiczenie 4. Rozrzut położenia a niepewność średniej

Otwórz z biblioteki **Położenie punktu 2D (syntetyczne)** (`polozenie2d.csv`).
Wybierz X = `x_m`, Y = `y_m`. Włącz wykres i osie główne.

1. Porównaj elementy pozadiagonalne macierzy S i kierunek dużej osi elipsy.
2. Wybierz elipsę standardową, a następnie rozrzut 95%. Sprawdź, że półosie
   zwiększyły się około 2,448 razy (`sqrt(qchisq(0.95, 2))`).
3. Wybierz ufność średniego położenia 95%. Wyjaśnij, dlaczego punkty pomiarowe
   nie muszą znajdować się wewnątrz tego obszaru.
4. Porównaj S z S/n. Która macierz opisuje pojedynczy pomiar, a która średnią?
   Czy zmniejszenie S/n usuwa błąd wspólny dla wszystkich pomiarów?

Dane są syntetyczne: 30 par utworzonych w R z `set.seed(2042)` i
`z <- matrix(rnorm(60), ncol=2)`, następnie
`x = 100 + 0.8*z[,1]`, `y = 200 + 0.5*z[,1] + 0.3*z[,2]`,
zaokrąglone do czterech miejsc po przecinku.

Podstawy: [NIST — statystyka T² Hotellinga](https://www.itl.nist.gov/div898/software/dataplot/refman2/auxillar/1samphot.htm),
[ESA — kowariancja i elipsa położenia](https://gssc.esa.int/navipedia/index.php/Positioning_Error).
