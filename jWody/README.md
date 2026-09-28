# jWody 0.1.1 — Hydrologia

Opcjonalny moduł jUPWR, instalowany przez **Moduły → Sideload**. Nie zmienia
zestawu analiz preinstalowanych ani wersji aplikacji. Menu: **Hydrologia**.

## Pięć analiz

| Panel | Obliczenia | Wykresy |
|---|---|---|
| Kontrola szeregu | błędne/powtórzone daty, braki jawne i nieobecne daty, najdłuższa luka, wartości ujemne, kompletność miesięcy i lat | hydrogram, mapa kompletności |
| Reżim i sezonowość | agregaty miesięczne/roczne, profil miesięczny, odniesienie i anomalie | przebieg agregatów, mapa rok × miesiąc |
| Przepływy charakterystyczne | NQ/SQ/WQ, NNQ/SNQ/SSQ/SWQ/WWQ, Q90/Q95, odpływ jednostkowy, objętość i warstwa | krzywa przewyższenia, charakterystyki roczne |
| Trendy i zmiany | MK i Sen rocznie albo sezonowo miesięcznie, korelacja reszt, opcjonalnie przybliżone wnioskowanie i Pettitt | agregaty i trend, korelogram reszt |
| Niżówki | próg podany / Qp stały / Qp miesięczny, katalog zdarzeń, czas i deficyt, zestawienie roczne | hydrogram z progiem i niżówkami, deficyty roczne |

Każdy panel zwraca dwie tabele, uwagi metodologiczne i opcjonalne wykresy.
Checkboxy wykresów i szczegółowego opisu metod są domyślnie wyłączone.

## Dane i wspólne zasady

Jedna obserwacja w wierszu. Kolumny: **data**, **wartość**, opcjonalnie **stacja**.
Datę należy wczytać jako tekst/zmienną nominalną w jednoznacznym formacie
`RRRR-MM-DD`. Miesięczne pomiary mają datę pierwszego dnia miesiąca. Nie
interpretujemy numerów seryjnych Excela ani dat zależnych od ustawień regionalnych.
Przykłady są dostępne po zainstalowaniu modułu przez **☰ → Otwórz →
Biblioteka danych → jWody**. Tak jak w jSpace, CSV znajdują się
w `data/`, a wpisy `datasets` w `jamovi/0000.yaml` zawierają opis ćwiczenia,
słownik zmiennych, jednostki i pochodzenie. Dane i ich metadane są pakowane do
`.jmo` i dostępne offline; nie trzeba ręcznie importować plików z repozytorium.
Biblioteka pokazuje nazwy i krótkie opisy. W obecnym kliencie panel „O zbiorze”
odczytuje pełną dokumentację tylko z jDane, więc dla jWody instrukcje ćwiczeń
znajdują się także poniżej; metadane są przygotowane do obsługi innych modułów.

Obsługiwane wielkości:

- przepływ dobowy lub średni miesięczny w m³/s;
- dobowa/miesięczna suma opadu w mm;
- stan wody w cm;
- głębokość zwierciadła w m p.p.t. (wzrost = obniżenie zwierciadła);
- rzędna zwierciadła w m n.p.m. (wzrost = podnoszenie zwierciadła).

Jednostki są deklarowane, nie automatycznie wykrywane ani przeliczane przy imporcie.
Dane nieregularne trzeba najpierw świadomie sprowadzić do dobowych/miesięcznych.
Przepływy charakterystyczne i niżówki wymagają dobowego Q w m³/s.
Wagi wierszy jamovi nie są obsługiwane.

Daty są sortowane osobno dla stacji. Domyślny rok hydrologiczny zaczyna się
1 listopada i jest oznaczony rokiem zakończenia; początek można zmienić na
miesiąc 1–12. Granice analizowanego okresu są opcjonalne. Bez nich zakres
wyznacza się osobno z dat każdej stacji. Jawne granice pozwalają uwzględnić
również brakujące pomiary na początku/końcu oczekiwanego szeregu.

**Brak ≠ zero.** Nie ma automatycznej interpolacji, usuwania wartości odstających
ani uśredniania duplikatów. Kontrola szeregu liczy nadmiarowe wiersze i wyłącza
wszystkie pomiary z powtórzonej daty z obliczeń kompletności. Pozostałe analizy
blokują duplikaty i błędne daty. Ujemny przepływ/opad/głębokość wymaga poprawienia
kodów braków lub wyboru właściwej wielkości; ujemny stan/rzędna może być poprawny.

Kompletność dotyczy **pełnego miesiąca/roku kalendarzowego**, także na brzegach
wybranego zakresu. Domyślnie wymagane jest 100%. Niższy próg jest świadomym
odstępstwem widocznym w uwagach. Odrzucone okresy pozostają w tabeli z brakami
wyników. Rok całkowicie bez danych zachowuje swoje miejsce na osi czasu.

Opad sumujemy, inne wielkości uśredniamy. Średnie miesięczne przy agregacji
rocznej ważymy długością miesięcy. Sumy niepełnych opadów nie są przeskalowywane.

## Definicje wyników

**Przepływy.** NQ/SQ/WQ to minimum/średnia/maksimum dobowych Q w przyjętym roku.
SNQ/SSQ/SWQ to średnie tych charakterystyk rocznych (każdy rok ma równą wagę),
NNQ i WWQ to ich skrajne wartości. Q90 i Q95 są kwantylami 0,10 i 0,05
(R `quantile`, typ 7) dobowych pomiarów z przyjętych lat: oznaczają przewyższenie
przez odpowiednio 90 i 95% czasu. Krzywa używa pozycji rangowych `r/(n+1)`.
Objętość = suma zmierzonych Q × 86400 s; warstwa mm = objętość/(1000 × km²).
Przy kompletności <100% oba wyniki dotyczą tylko zmierzonych dni.

**Reżim.** Anomalia jest różnicą agregatu i średniej odniesienia, a nie wskaźnikiem
standaryzowanym. Miesiące porównujemy z tym samym miesiącem kalendarzowym.
Dla rocznych agregatów odniesienie wybieramy według roku zakończenia. Brak
przyjętych okresów odniesienia daje brak anomalii. Zera w granicach odniesienia
oznaczają odpowiedni brzeg analizowanego szeregu.

**Trendy.** Podstawowy wynik to nachylenie Sena w jednostkach/rok. Odstępy czasu
pozostają rzeczywiste, nawet gdy brakuje całego roku. Sezonowo porównujemy tylko
pary z tego samego miesiąca. MK uwzględnia remisy i poprawkę ciągłości; wariant
sezonowy sumuje S i wariancje sezonów **bez kowariancji między sezonami**.

Domyślnie p i PU są wyłączone. Włączenie oznacza przyjęcie niezależności:
**nie zastosowano korekty autokorelacji ani prewhitening**. PU Sena jest
przybliżeniem rangowym normalnym (interpolacja rang granicznych, obcięcie do
zakresu nachyleń). Korelacje reszt liczymy dla dostępnych par na regularnej osi,
po odjęciu trendu Sena i median sezonowych; nie są formalnym testem niezależności.
Mała liczba agregatów ogranicza wiarygodność przybliżeń; minimum techniczne to
trzy agregaty. Przy zależności czasowej p/PU mogą być nadmiernie optymistyczne.
Test Pettitta dostępny jest tylko dla agregatów rocznych i włączonego
wnioskowania; p jest przybliżone (lepsze przy p ≤0,5), a rok wskazuje granicę
**po** tym roku. Zmiana nie określa przyczyny. Brak korekty wielokrotnych testów
przy wielu stacjach.

**Niżówki.** Warunek `Q < próg` jest ścisły. Qp estymujemy z miesięcy spełniających
próg kompletności w wybranym odniesieniu, minimum 10 pomiarów, a dla progu
sezonowego minimum to obowiązuje w każdym użytym miesiącu. Próg miesięczny
zmienia się skokowo na granicach miesięcy. Minimum 10 jest warunkiem technicznym,
nie gwarancją stabilnej estymacji: do interpretacji hydrologicznej potrzebny jest
reprezentatywny okres wieloletni.

Łączenie zdarzeń może objąć najwyżej wskazaną liczbę **znanych** dni nad progiem;
nigdy nie przekracza luki. Minimum dni pod progiem sprawdzamy po łączeniu.
Rozpiętość zdarzenia uwzględnia połączone przerwy, liczba dni niżówkowych nie.
Deficyt = suma dodatnich `(próg − Q)` × 86400. Zdarzenia przy brzegu danych/luki
są oznaczone jako ucięte; czas i deficyt obejmują wtedy tylko fragment obserwowany.
Wyniki roczne rozdzielają dni/deficyt po kalendarzu hydrologicznym, a początek
zdarzenia liczą tylko w jednym roku. Odrzucony rok nie otrzymuje fałszywego zera.

Limity ochronne: 200 000 wierszy, 50 stacji i 2400 agregatów w analizie trendu
(obliczanie wszystkich nachyleń wymaga pamięci kwadratowej).

## Dane dydaktyczne i ćwiczenia

Wszystkie trzy zbiory są **syntetyczne**, utworzone deterministycznie przez
`Rscript jWody/data-raw/generate.R` z katalogu repozytorium. Nie pochodzą z IMGW,
PIG ani pomiarów terenowych. Wzory generatora są źródłem danych, bez pobierania
zewnętrznego i bez losowego ziarna.

1. `przeplyw.csv`: dwie stacje, 2010–2024, Q m³/s. Wybierz `data`, `przeplyw_m3s`,
   `stacja`, dane dobowe. Porównaj kompletność 100% i 95%, potem Q90 i niżówki.
   Wyjaśnij różnicę między brakującym dniem a przepływem równym zero.
2. `opad.csv`: 2010–2024, suma dobowa mm. Wybierz `opad_mm`, rodzaj „Suma opadu”.
   Obejrzyj sumy miesięczne i rok z luką; pokaż, dlaczego brak opadu nie oznacza
   brakującego pomiaru. Nie używaj paneli przepływów i niżówek dla opadu.
3. `studnia.csv`: 1995–2024, dane miesięczne, głębokość m p.p.t. Wybierz
   `glebokosc_m`, rozdzielczość miesięczną, rodzaj „Głębokość zwierciadła”.
   Porównaj sezonowy trend z rocznym. Dodatnie nachylenie oznacza obniżanie
   zwierciadła; wzorzec zawiera trend, sezonowość i lukę.

## Budowanie i testowanie

Źródła: `jamovi/*.a.yaml` (opcje), `*.u.yaml` (panel), `*.r.yaml` (wyniki),
`R/*.b.R` (adaptery) i `R/utils-*.R` (niezależne funkcje obliczeniowe).
`jamovi/0000.yaml` jest źródłowym rejestrem modułu; wersja musi zgadzać się z
DESCRIPTION. Pliki `.h.R`, build i `.jmo` są generowane i nie trafiają do commita.

```sh
# Testy obliczeń i wygenerowanego API, bez pakowania; lokalny jmvcore z payloadu:
bash packaging/scripts/test-jwody.sh
# Alternatywna biblioteka z jmvcore:
JWODY_RLIBS=/sciezka/do/biblioteki bash packaging/scripts/test-jwody.sh

# Najpierw referencyjna aplikacja Docker:
docker compose --profile main build
docker compose --profile main up -d
# Opcjonalny artefakt Linux (nie instaluje modułu w aplikacji):
bash packaging/scripts/build-jwody-docker.sh
# Dopiero po sprawdzeniu Dockera — natywny macOS arm64:
bash packaging/scripts/macos/74-jmo-jwody.sh
# Windows x64: packaging/scripts/windows/build.ps1, krok 4i, na Windows.
```

Testy wymagają `testthat` oraz `ragg`; runtime modułu korzysta z jmvcore, R6 i
ggplot2 dostępnych w bazie jUPWR. Implementacje MK/Sena/Pettitta są lokalne,
z udokumentowanymi wzorami i testami; nie dodają skompilowanych zależności CRAN.
Nie deklarujemy pełnej równoważności z pakietem `trend` dla wszystkich danych.

## Podstawy metod

- [Dokumentacja MK, pakiet trend](https://search.r-project.org/CRAN/refmans/trend/html/mk.test.html): statystyka S, wariancja z remisami, poprawka ciągłości.
- [Sezonowy MK](https://search.r-project.org/CRAN/refmans/trend/html/smk.test.html), Hirsch, Slack i Smith (1982): sumowanie statystyk sezonowych.
- [Nachylenie Sena](https://search.r-project.org/CRAN/refmans/trend/html/sens.slope.html), Sen (1968): mediana nachyleń par obserwacji.
- [Test Pettitta](https://search.r-project.org/CRAN/refmans/trend/html/pettitt.test.html), Pettitt (1979): statystyka rangowa i przybliżone p.
- [lfstat / podręcznik WMO o niskich przepływach](https://stat.ethz.ch/CRAN/web/packages/lfstat/index.html): kontekst analizy niżówek. jWody nie deklaruje implementacji wszystkich procedur podręcznika.

## Status walidacji — 2026-09-28

- macOS arm64 (R 4.6.0): jmc, 111 sprawdzeń źródeł/paneli oraz 111 sprawdzeń
  gotowego `.jmo`, bez błędów i ostrzeżeń.
- Docker Linux arm64 (R 4.6.0): referencyjny build aplikacji, uruchomienie
  serwera (HTTP 302), eksport `.jmo` i 111 sprawdzeń gotowego artefaktu.
- Dziesięć wykresów renderowanych w testach; dodatkowa wizualna kontrola
  wybranych PNG dla dwóch stacji. Nie przeprowadzono klikanego testu paneli
  ani instalacji przez Sideload w uruchomionym jUPWR.
- Metadane wydania i siedem testów skryptu kontroli wydania: poprawne.
- Windows: dodany krok pakowania, bez natywnego builda/testu na Windows.
- Zewnętrzna kontrola liczb: asymptotyczne p MK porównane z `stats::cor.test`;
  ręcznie sprawdzalne wzorce dla sezonowego S, Sena, Pettitta, objętości,
  granic roku, braków i łączenia zdarzeń. Nie wykonano walidacji terenowej.

Artefakty są w `packaging/build/dist/`: `jWody_0.1.1-macos-arm64.jmo` oraz
`jWody_0.1.1-linux.jmo` (ten drugi zbudowany na Linux arm64). Należy wybierać
paczkę odpowiednią dla systemu i architektury. Sprawdzenie gotowej paczki:

```sh
Rscript packaging/scripts/test-jwody-artifact.R \
  packaging/build/dist/jWody_0.1.1-macos-arm64.jmo \
  jWody/tests/testthat packaging/build/stage/jamovi/modules/base/R
```

Testy wykryły i usunęły konflikt `format` z szerokiego importu `jmvcore`;
moduł importuje tylko potrzebną funkcję i jawnie używa `base::format`.
Samo renderowanie wykresów w R nie oznacza wizualnego testu paneli aplikacji.

Poza zakresem 0.1: automatyczna imputacja, dane godzinowe, korekty zależności
czasowej, GEV/GPD, SPI/SPEI, prognozowanie i modele opad–odpływ.

Wersja 0.1.1 uzupełnia dokumentację trzech zbiorów w bibliotece danych; same
pomiary i metody obliczeniowe pozostają takie jak w pierwszej wersji.

Kontrola biblioteki w gotowym archiwum (CSV, wersja, pełne metadane, zgodność
kolumn i identyczność danych ze źródłami):

```sh
node packaging/scripts/check-jwody-data.mjs packaging/build/dist/jWody_0.1.1-macos-arm64.jmo
```
