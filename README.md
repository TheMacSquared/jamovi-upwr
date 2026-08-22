# jUPWR

Fork [jamovi](https://www.jamovi.org) przygotowany na potrzeby dydaktyki statystyki na **Uniwersytecie Przyrodniczym we Wrocławiu**.

## Co dodaje ten fork?

### Modyfikacje jmv (moduł analityczny)
- **Boxplot** — opcja wyświetlania średniej jako kwadratu (`boxMean`)
- **Histogram** — facetowanie po grupie (`histFacet`)
- **Statystyki opisowe** — współczynnik zmienności V (%)

### Preinstalowane moduły dodatkowe
| Moduł | Opis |
|-------|------|
| **distrACTION** | Obliczanie i wizualizacja rozkładów prawdopodobieństwa (ciągłe i dyskretne). Pełny interfejs po polsku, wartość oczekiwana E[X] i wariancja Var[X] z wzorami dla wszystkich 11 rozkładów (w tym gamma, Weibulla i ujemny dwumianowy z wyborem konwencji zmiennej) |
| **jboot** | Testy bootstrapowe (przedziały ufności, korelacje, regresja, test dwóch grup i inne) |
| **jperm** | Testy permutacyjne |
| **jCI** | Przedziały ufności z wykresami dydaktycznymi (waffle, Gardner–Altman, scatter+regresja); wybór 2 kategorii z wielu poziomów; grupowanie |
| **jRISK** *(opcjonalny — plik `.jmo`, patrz niżej)* | Ryzyko i niezawodność: zdarzenia i warunkowanie z danych (2×2, Bayes, częstości naturalne, metryki detektora), schemat Bernoulliego (seria binarna), modele czasu życia (kalkulator R(t)/h(t)/MTTF oraz tryb „Dane": Kaplan–Meier i MLE wykładniczy/gamma/Weibull z cenzorowaniem prawostronnym, AIC/BIC), niezawodność systemów (topologie kanoniczne z mostkiem + komponenty z arkusza; funkcja struktury, ścieżki/przekroje, koherentność, schemat blokowy) i drzewo błędów FTA (bramki AND/OR, minimalne przekroje, ranking ważności). Zawiera przykładowe zbiory danych „Bananpol — urządzenia dojrzewalni" i „Bananpol — wypadki (skórki od banana)" (Otwórz → Biblioteka) |


### Moduł opcjonalny jRISK (analiza ryzyka)
jRISK obsługuje jeden kurs (analiza ryzyka), więc **nie jest preinstalowany** — studenci
tego kursu instalują go z pliku `.jmo` (**Moduły → Sideload** w jUPWR). Dzięki temu
poprawki i nowe funkcje jRISK nie wymagają reinstalacji całej aplikacji.
Plik `.jmo` zawiera pakiet R skompilowany pod konkretną platformę, dlatego budowany jest
osobno: `packaging/scripts/macos/70-jmo-jrisk.sh` (macOS arm64) i krok 4e w
`packaging/scripts/windows/build.ps1` (Windows x64); wynik trafia do `packaging/build/dist/`.

### Branding
Aplikacja wyświetlana jako **jUPWR** (tytuł okna, nagłówek)

## Uruchomienie (Docker)

```bash
git clone https://github.com/TheMacSquared/jamovi-upwr.git
cd jamovi-upwr
git submodule update --init --recursive
docker compose --profile main build
docker compose --profile main up
```

Aplikacja dostępna pod http://127.0.0.1:41337

## Tryb deweloperski

```bash
docker compose --profile dev up
```

## Struktura repozytorium

```
jamovi/          # aplikacja (Electron + Python + C++)
jmv/             # moduł analityczny R (submoduł)
jdistrACTION/       # moduł rozkładów — fork distrACTION
jboot/           # moduł bootstrap
jperm/           # moduł testów permutacyjnych
jCI/             # moduł przedziałów ufności
jRISK/           # moduł ryzyka i niezawodności (opcjonalny — dystrybuowany jako .jmo)

```

## Zależności (build natywny Windows)

Boost 1.84, RTools 4.5, Visual Studio 2022, Python 3.12, Node.js 22, R 4.5

---

Bazuje na [jamovi](https://github.com/jamovi/jamovi) — wolne oprogramowanie na licencji AGPL-3.0.
