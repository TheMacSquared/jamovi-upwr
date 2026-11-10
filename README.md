# jUPWR

Fork [jamovi](https://www.jamovi.org) przygotowany na potrzeby dydaktyki statystyki na **Uniwersytecie Przyrodniczym we Wrocławiu**.

Bieżąca wersja dystrybucji: [`client/common/jupwr.ts`](client/common/jupwr.ts).
Wersje modułów i skład wydania: [`packaging/MODULES.md`](packaging/MODULES.md).
Historia wydań: [`CHANGELOG.md`](CHANGELOG.md).

## Co dodaje ten fork?

### Analizy kursu podstawowego w modułach jUPWR
Każde menu kursu podstawowego ma własne analizy jUPWR. Panel pokazuje od razu
opcje potrzebne na zajęciach, a pozostałe grupuje w sekcji „Zaawansowane”.
Każda analiza może dopisać do wyników „Opis zastosowanych metod”: co policzono,
z jakimi parametrami i jak przedstawiono.

Analizy jmv zastąpione przez moduły jUPWR są ukryte w menu, ale ich kod zostaje,
więc zapisane wcześniej pliki `.omv` nadal się otwierają. Analizy spoza kursu
(np. MANCOVA, regresja wielomianowa i porządkowa, modele log-liniowe) pozostają
widoczne jako jmv.

### Moduły preinstalowane
| Moduł | Menu | Opis |
|---|---|---|
| **jEksplor** | Eksploracja | Zmienne ilościowe (miary położenia, rozproszenia, kształtu, koncentracji; testy normalności), zmienne jakościowe (tabele liczności, także krzyżowe i w warstwach), szereg rozdzielczy |
| **jTestyT** | Testy t | Jedna próba, dwie grupy, próby sparowane: t Studenta, t Welcha, Mann-Whitney / Wilcoxon. Tylko testowanie; przedziały ufności są w jCI |
| **jANOVA** | ANOVA | ANOVA (czynniki, bloki, kowarianty) i ANOVA powtórzonych pomiarów na danych w formacie długim. Przełączniki „Nierówne wariancje” (Welch, Welch-James) i „Nieparametrycznie” (Kruskal-Wallis, ART, Friedman). Porównania wielokrotne z literami grup jednorodnych |
| **jRegr** | Regresja | Korelacja, regresja liniowa, regresja logistyczna dwumianowa z diagnostyką |
| **jCzest** | Częstości | Tabela kontyngencji z kontrolą liczebności oczekiwanych i miarami siły związku, test zgodności, próby zależne (McNemar, Q Cochrana) |
| **jCI** | Przedziały ufności | Przedziały klasyczne i bootstrapowe (percentylowy, BCa) dla średnich, proporcji, korelacji i regresji; d Cohena z przedziałem; analiza „Jak działa bootstrap”. Zawiera dawny moduł jboot |
| **jperm** | Testy permutacyjne | Testy permutacyjne |
| **distrACTION** | Rozkłady | 11 rozkładów prawdopodobieństwa po polsku, z E[X] i Var[X]; dane empiryczne można nałożyć na model |
| **scatr** | Wykresy | 30 wykresów w kategoriach dydaktycznych (patrz niżej) |
| **jDane** | Otwórz → Biblioteka | Zbiory danych do zajęć z tagami tematycznymi i cytowaniami. Bez analiz |
| **jmv** | pozostałe | Analizy jamovi spoza kursu podstawowego |

### Wykresy w kategoriach dydaktycznych
Zakładka **Wykresy** jest podzielona na kategorie według zastosowań (wzorzec:
[r-graph-gallery.com](https://r-graph-gallery.com)). O kategorii rozkładu
decyduje typ zmiennej głównej.

| Kategoria | Wykresy |
|---|---|
| **Rozkład ilościowe** | histogram, słupkowy dla zmiennej dyskretnej, gęstość, Q–Q, raincloud |
| **Rozkład jakościowe** | słupki skumulowane, waffle, treemap, mozaikowy |
| **Porównania** | pudełkowy, skrzypcowy, punktowy grup |
| **Ranking** | słupkowy, lollipop, chmura słów, kolisty słupkowy |
| **Profile** | radar, profil średnich, hantlowy, nachylenia |
| **Zależności** | rozrzutu, bąbelkowy, mapa ciepła, korelogram, hexbin |
| **Trendy** | liniowy, warstwowy |
| **Inne** | Pareto |

Dwa wykresy (ridgeline, współrzędne równoległe) są ukryte w menu i działają
tylko w zapisanych plikach. Wykresy pudełkowy, skrzypcowy, raincloud i punktowy
grup przyjmują też dane w formacie szerokim (kilka kolumn obok siebie).

Świadome decyzje dydaktyczne: **brak wykresu kołowego** (oduczamy go), a boxplot
występuje tylko w Porównaniach — rolę rozkładową pełni raincloud.

### Motywy, palety i wygląd
Motywy wykresów w stylu wykładowym oraz palety kuratorowane: barwy UPWr,
palety bezpieczne dla daltonistów (CVD-safe), viridis i Catppuccin. Skale ciągłe
podążają za wybraną paletą. Czcionki dołączone do dystrybucji. Wygląd interfejsu
wybiera się w ustawieniach: klasyczny (jak w jamovi), lekki albo lekki przyciemniony.

### Dane, dokumenty i bibliografia
- **Zmienne przekształcone** — tryb „Warunki” (kreator reguł bez pisania formuł).
- **Kopie odzyskiwania** dokumentów w wybranym odstępie czasu, atomowy zapis plików.
- **Bibliografia** zawiera cytowania pakietów R użytych w widocznych wynikach
  oraz źródeł zbiorów z biblioteki.
- **Statystyki opisowe jmv** (dla starych plików) zachowują średnią na boxplocie
  (`boxMean`) i współczynnik zmienności V (%).

### Moduły opcjonalne (`.jmo`, sideload)
Moduł obsługujący **jeden kurs** nie jest preinstalowany — studenci tego kursu
instalują go z pliku `.jmo` (**Moduły → Sideload**). Dzięki temu poprawki w takim
module nie wymagają reinstalacji całej aplikacji.

| Moduł | Opis |
|---|---|
| **jRISK** | Ryzyko i niezawodność: zdarzenia i warunkowanie z danych (2×2, Bayes, częstości naturalne, metryki detektora), schemat Bernoulliego, modele czasu życia (R(t)/h(t)/MTTF oraz tryb „Dane": Kaplan–Meier i MLE wykładniczy/gamma/Weibull z cenzorowaniem prawostronnym, AIC/BIC), niezawodność systemów (topologie kanoniczne z mostkiem, funkcja struktury, ścieżki/przekroje, schemat blokowy) i drzewo błędów FTA. Zawiera zbiory „Bananpol" |
| **jSpace** | Statystyka danych kosmicznych: orbity TLE/SGP4 (asteRisk), mapy punktowe i kartogramy oraz statystyki regionalne (sf), statystyki rastrów (terra), klasyfikacja k-NN i drzewo decyzyjne. Zawiera 4 zbiory danych |

W repozytorium są też moduły **wewnętrzne** — testowe, zamrożone i poza wydaniem:
jRol (doświadczalnictwo rolnicze), jPomiar (pomiary i niepewność) i jWody (hydrologia).
Nie są publikowane; zob. [`packaging/MODULES.md`](packaging/MODULES.md).

Plik `.jmo` zawiera pakiet R **skompilowany pod konkretną platformę**, dlatego każda
platforma wymaga osobnego builda:

| Platforma | Skrypt |
|---|---|
| macOS arm64 | `packaging/scripts/macos/70-jmo-jrisk.sh`, `71-jmo-jspace.sh` |
| Windows x64 | `packaging/scripts/windows/build.ps1` (kroki 4e–4f) |

Wynik trafia do `packaging/build/dist/`. Rejestr modułów i macierz zgodności:
[`packaging/MODULES.md`](packaging/MODULES.md).

### Branding
Aplikacja wyświetlana jako **jUPWR** — tytuł okna, nagłówek, ikony i wordmark.
Wersja dystrybucji: [`client/common/jupwr.ts`](client/common/jupwr.ts).

## Uruchomienie (Docker)

```bash
git clone https://github.com/TheMacSquared/jamovi-upwr.git jamovi-upwr-latest
cd jamovi-upwr-latest
git submodule update --init --recursive
docker compose --profile main build
docker compose --profile main up
```

Aplikacja dostępna pod http://localhost:41337 (nie `127.0.0.1`: serwer przyjmuje
połączenia tylko z originu `localhost`, patrz `JAMOVI_HOST_A` w `docker-compose.yaml`).

> Porty są publikowane wyłącznie na pętli zwrotnej (`127.0.0.1`). Serwer działa
> z pustym `JAMOVI_ACCESS_KEY` i ma podmontowany katalog Dokumenty użytkownika,
> więc **nie wystawiaj tych portów na 0.0.0.0** bez ustawienia klucza dostępu.

## Tryb deweloperski

```bash
docker compose --profile dev up
```

## Struktura repozytorium

```
client/          # interfejs (TypeScript, budowany przez vite)
server/          # serwer (Python/tornado)
engine/          # silnik C++ (nie modyfikować)
electron/        # punkt wejścia Electron
platform/        # ikony, pliki OS, env.conf
i18n/            # tłumaczenia (submoduł)
docker/          # Dockerfile'e
packaging/       # buildy natywne Windows/macOS, rejestr modułów
jamovi-compiler/ # kompilator modułów (jmc)
jmvcore/         # biblioteka R modułów (motywy, palety, opis metod)
docs/            # historia decyzji, instrukcje buildów, audyty

jmv/             # moduł analityczny (submoduł)
plots/           # moduł wykresów — scatr (submoduł)
jEksplor/        # eksploracja
jTestyT/         # testy t
jANOVA/          # ANOVA
jRegr/           # korelacja i regresja
jCzest/          # częstości
jCI/             # przedziały ufności (klasyczne i bootstrap)
jperm/           # testy permutacyjne
jdistrACTION/    # rozkłady prawdopodobieństwa
jDane/           # zbiory danych do zajęć
jRISK/           # ryzyko i niezawodność (opcjonalny — .jmo)
jSpace/          # statystyka danych kosmicznych (opcjonalny — .jmo)
jRol/ jPomiar/ jWody/   # moduły wewnętrzne (zamrożone, poza wydaniem)
```

## Zależności (build natywny Windows)

Visual Studio 2022 Build Tools (VCTools), RTools45, R 4.6.0, Boost 1.84,
Node.js 20+, cmake, Python 3.12 (python-build-standalone).

Docker buduje się niezależnie od powyższych — używa R 4.6.0 i Node 24.18
z obrazu `jamovi/jamovi-deps`. Szczegóły: [`packaging/README.md`](packaging/README.md).

## Dokumentacja

- [`CHANGELOG.md`](CHANGELOG.md) — historia wydań jUPWR
- [`packaging/MODULES.md`](packaging/MODULES.md) — rejestr modułów, macierz zgodności, procedura wydania
- [`packaging/README.md`](packaging/README.md) — buildy natywne, znane ograniczenia
- [`packaging/CHANNELS.md`](packaging/CHANNELS.md) — kanały wydań latest i stable
- [`AGENTS.md`](AGENTS.md) — architektura i zasady pracy dla agentów/współautorów
- [`docs/DECYZJE.md`](docs/DECYZJE.md) — kolejka zadań i historia decyzji

---

Bazuje na [jamovi](https://github.com/jamovi/jamovi) — wolne oprogramowanie na licencji AGPL-3.0.
