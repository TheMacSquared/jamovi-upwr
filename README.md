# jUPWR

Fork [jamovi](https://www.jamovi.org) przygotowany na potrzeby dydaktyki statystyki na **Uniwersytecie Przyrodniczym we Wrocławiu**.

## Co dodaje ten fork?

### Modyfikacje jmv (moduł analityczny)
- **Boxplot** — opcja wyświetlania średniej jako kwadratu (`boxMean`)
- **Statystyki opisowe** — współczynnik zmienności V (%)
- **Zmienne obliczane** — tryb „Warunki" (strukturalny kreator wyrażeń warunkowych)

> Facetowanie histogramów zapewnia wbudowany mechanizm „Podziel według" upstreamu —
> osobna opcja `histFacet` okazała się zbędna i nie istnieje.

### Wykresy w kategoriach dydaktycznych
Zakładka **Wykresy** jest podzielona na kategorie według zastosowań (wzorzec:
[r-graph-gallery.com](https://r-graph-gallery.com)), łącznie **26 wykresów**:

| Kategoria | Wykresy |
|---|---|
| **Rozkład** | histogram, gęstość, Q–Q, raincloud, ridgeline |
| **Porównania** | boxplot, skrzypce, punkty ze średnią |
| **Ranking** | słupkowy, lollipop, radar, współrzędne równoległe, chmura słów, słupki kołowe |
| **Zależności** | rozrzutu, bąbelkowy, mapa ciepła, korelogram, mozaikowy, hexbin |
| **Trendy** | liniowy, warstwowy |
| **Części całości** | słupki skumulowane, waffle, treemap |
| **Inne** | Pareto |

Świadome decyzje dydaktyczne: **brak wykresu kołowego** (oduczamy go), a boxplot
występuje tylko w Porównaniach — rolę rozkładową pełni raincloud.

### Motywy i palety
Motywy wykresów w stylu wykładowym oraz palety kuratorowane: barwy UPWr,
palety bezpieczne dla daltonistów (CVD-safe) i viridis. Skale ciągłe podążają
za wybraną paletą. Czcionki dołączone do dystrybucji.

### Moduły preinstalowane
| Moduł | Opis |
|-------|------|
| **scatr** | Wykresy — 26 pozycji w kategoriach dydaktycznych (patrz wyżej) |
| **distrACTION** | Rozkłady prawdopodobieństwa (ciągłe i dyskretne). Pełny interfejs po polsku, wartość oczekiwana E[X] i wariancja Var[X] ze wzorami dla wszystkich 11 rozkładów (w tym gamma, Weibulla i ujemny dwumianowy z wyborem konwencji zmiennej losowej) |
| **jboot** | Metody bootstrapowe (przedziały ufności, korelacje, regresja, porównania grup) |
| **jperm** | Testy permutacyjne |
| **jCI** | Przedziały ufności z wykresami dydaktycznymi (waffle, Gardner–Altman, scatter + regresja); wybór 2 kategorii z wielu poziomów; grupowanie |
| **jDane** | Zbiory danych do zajęć, z tagami tematycznymi (Otwórz → Biblioteka). Sam moduł nie wnosi analiz |
| **jANOVA** | ANOVA w jednym miejscu, na danych w formacie długim, z panelem pod dydaktyczną prostotę: „ANOVA" (czynniki; w „Zaawansowanych" czynniki blokujące, kowarianty, typ SS I/II/III, kontrasty) i „ANOVA powtórzonych pomiarów" (afex: test Mauchly'ego, poprawki GG/HF, split-plot). Przełącznik „Nierówne wariancje" daje Welcha lub Welcha-Jamesa, przełącznik „Nieparametrycznie" Kruskala-Wallisa z Dunnem, ART (aligned rank transform) dla układów czynnikowych albo Friedmana z Nemenyim. Porównania wielokrotne (Tukey, NIR, Holm, Dunnett, Scheffé) z grupami jednorodnymi (litery) i różnicą graniczną, wykresy średnich z literami. Analizy ANOVA z jmv (jednoczynnikowa, ANOVA, ANCOVA, powtórzonych pomiarów, Kruskal-Wallis, Friedman) są ukryte w menu, ale nadal otwierają się z zapisanych plików |
| **jTestyT** | Testy t (jedna próba, dwie grupy, próby sparowane) z płaskim panelem: przełączniki „Nierówne wariancje (Welch)" i „Nieparametrycznie (rangi)", różnica z przedziałem ufności, d Cohena z przedziałem, czytelny wykres estymacyjny Gardnera-Altmana (dwie grupy i pary z liniami) lub punktowy z H₀ (jedna próba); oraz test permutacyjny, przedział bootstrapowy, test znaków, K-S. Testy t z jmv są ukryte w menu |

### Moduły opcjonalne (`.jmo`, sideload)
Moduł obsługujący **jeden kurs** nie jest preinstalowany — studenci tego kursu
instalują go z pliku `.jmo` (**Moduły → Sideload**). Dzięki temu poprawki w takim
module nie wymagają reinstalacji całej aplikacji.

| Moduł | Opis |
|---|---|
| **jRISK** | Ryzyko i niezawodność: zdarzenia i warunkowanie z danych (2×2, Bayes, częstości naturalne, metryki detektora), schemat Bernoulliego, modele czasu życia (R(t)/h(t)/MTTF oraz tryb „Dane": Kaplan–Meier i MLE wykładniczy/gamma/Weibull z cenzorowaniem prawostronnym, AIC/BIC), niezawodność systemów (topologie kanoniczne z mostkiem, funkcja struktury, ścieżki/przekroje, schemat blokowy) i drzewo błędów FTA. Zawiera zbiory „Bananpol" |
| **jSpace** | Statystyka danych kosmicznych: orbity TLE/SGP4 (asteRisk), mapy punktowe i kartogramy oraz statystyki regionalne (sf), statystyki rastrów (terra), klasyfikacja k-NN i drzewo decyzyjne. Zawiera 4 zbiory danych |
| **jRol** | Doświadczalnictwo rolnicze: układ całkowicie losowy, losowanych bloków, kwadrat łaciński i split-plot (ANOVA z właściwymi błędami), porównania wielokrotne (Tukey, NIR, Scheffé, Dunnett, Holm) z grupami jednorodnymi i wartością NIR, wykresy średnich z literami i interakcji, diagnostyka oraz generator planu doświadczenia z mapą pola |

Plik `.jmo` zawiera pakiet R **skompilowany pod konkretną platformę**, dlatego każda
platforma wymaga osobnego builda:

| Platforma | Skrypt |
|---|---|
| macOS arm64 | `packaging/scripts/macos/70-jmo-jrisk.sh`, `71-jmo-jspace.sh` |
| Windows x64 | `packaging/scripts/windows/build.ps1` (kroki 4e i 4f) |

Wynik trafia do `packaging/build/dist/`. Rejestr modułów i macierz zgodności:
[`packaging/MODULES.md`](packaging/MODULES.md).

### Branding
Aplikacja wyświetlana jako **jUPWR** — tytuł okna, nagłówek, ikony i wordmark.
Wersja dystrybucji: [`client/common/jupwr.ts`](client/common/jupwr.ts).

## Uruchomienie (Docker)

```bash
git clone https://github.com/TheMacSquared/jamovi-upwr.git
cd jamovi-upwr
git submodule update --init --recursive
docker compose --profile main build
docker compose --profile main up
```

Aplikacja dostępna pod http://127.0.0.1:41337

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
jmvcore/         # biblioteka R modułów (motywy, palety)

jmv/             # moduł analityczny (submoduł)
plots/           # moduł wykresów — scatr (submoduł)
jdistrACTION/    # rozkłady prawdopodobieństwa
jboot/           # bootstrap
jperm/           # testy permutacyjne
jCI/             # przedziały ufności
jDane/           # zbiory danych do zajęć
jRISK/           # ryzyko i niezawodność (opcjonalny — .jmo)
jSpace/          # statystyka danych kosmicznych (opcjonalny — .jmo)
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
- [`CLAUDE.md`](CLAUDE.md) — architektura i konwencje dla agentów/współautorów

---

Bazuje na [jamovi](https://github.com/jamovi/jamovi) — wolne oprogramowanie na licencji AGPL-3.0.
