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
| **distrACTION** | Obliczanie i wizualizacja rozkładów prawdopodobieństwa (ciągłe i dyskretne). Pełny interfejs po polsku, wartość oczekiwana E[X] i wariancja Var[X] z wzorami dla wszystkich 8 rozkładów |
| **jboot** | Testy bootstrapowe (przedziały ufności, korelacje, regresja, test dwóch grup i inne) |
| **jperm** | Testy permutacyjne |
| **jCI** | Przedziały ufności z wykresami dydaktycznymi (waffle, Gardner–Altman, scatter+regresja); wybór 2 kategorii z wielu poziomów; grupowanie |

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
jboot/           # moduł bootstrap (submoduł)
jperm/           # moduł testów permutacyjnych (submoduł)
jCI/             # moduł przedziałów ufności (submoduł)
```

## Zależności (build natywny Windows)

Boost 1.84, RTools 4.5, Visual Studio 2022, Python 3.12, Node.js 22, R 4.5

---

Bazuje na [jamovi](https://github.com/jamovi/jamovi) — wolne oprogramowanie na licencji AGPL-3.0.
