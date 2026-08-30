# Sesja B — merge submodułu `plots` (scatr) z upstreamem

**Maszyna:** dowolna z Dockerem (WSL wystarczy).
**Czas:** pół dnia — 12 commitów upstreamu, 7 plików `.b.R` do ręcznego scalenia,
26 wykresów do przeklikania.
**Repo:** `plots/` (submoduł → `github.com/TheMacSquared/jmvplots`), remote `upstream`
= `github.com/jamovi/jmvplots`.

> To zadanie zostało **świadomie odłożone** podczas audytu 30.08.2026 (wybrano opcję A:
> „zostaw `plots` na naszym 2.8.5"), żeby nie mieszać merge'a submodułu z resztą prac.
> Nic nie jest zepsute — to dług, nie awaria.

---

## Stan wyjściowy

| | commitów | wersja |
|---|---|---|
| my (`origin/main`) | 9 ponad bazę | scatr **2.8.5**, 26 wykresów, kategorie dydaktyczne |
| upstream (`896e7a0`) | 12 ponad bazę | scatr **2.9.0** |

Baza rozejścia: `00873623`.

### Co wnosi upstream

| commit | zmiana | wartość dla nas |
|---|---|---|
| `0a4127d` | **generowanie składni tidyverse/ggplot2** dla wykresów | **wysoka** — student widzi kod R, który odtwarza wykres |
| `2ebb94d` | testy równoważności generowanej składni | wysoka (chroni powyższe) |
| `962c498` | `linewidth` zamiast `size` w geomach linii | **pilna** — `size` jest deprecated w ggplot2 3.4+ |
| `0f14ed1` | brak notacji naukowej na osiach | dydaktycznie istotna |
| `e4e024d` | pusta składnia dla nieskonfigurowanych analiz | średnia |
| `05a1bd0` | snapshoty vdiffr jako lokalny check | organizacyjna |
| `da59dd8` | bump 2.9.0 | — |
| `2faefcb`, `20f03d5` | CI upstreamu | **do pominięcia** (mamy własne) |
| `99b8892`, `d8b49aa` | CLAUDE.md i docs upstreamu | do rozważenia (konflikt z naszym) |
| `896e7a0` | i18n | trywialna |

### Gdzie będzie boleć

Zero konfliktów w 18 naszych nowych wykresach — upstream ich nie zna. Konflikt jest
w 7 plikach, które **obie strony zmieniały**:

| plik | upstream | my |
|---|---|---|
| `R/jmvhist.b.R` | +109/−44 | +20/−2 |
| `R/jmvline.b.R` | +111/−62 | +12/−0 |
| `R/pareto.b.R` | +120/−36 | +1/−0 |
| `R/scat.b.R` | +90/−47 | +10/−2 |
| `R/jmvbox.b.R` | +82/−32 | +34/−2 |
| `R/jmvbar.b.R` | +47/−19 | +15/−3 |
| `R/utils.R` | +29/−13 | +17/−0 |

Asymetria jest kluczowa: **upstream przepisał te funkcje szeroko** (dodając generowanie
składni), my dołożyliśmy do nich wąskie rzeczy:

- `paletteFillGradient()` w `utils.R` — gradienty idą za paletą wybraną w motywie
  (commit `127095f`),
- wrap długich etykiet kategorii (`873c6ac`),
- `boxMean` — średnia na boxplocie (`25cc849`).

**Strategia:** brać wersję upstreamu jako bazę i **nakładać na nią nasze trzy zmiany**,
a nie odwrotnie. Odwrotnie znaczyłoby ręczne przepisywanie +588 linii upstreamu.

---

## Krok po kroku

### 1. Przygotowanie

```bash
cd /home/maciek/praca/warsztat/jamovi-upwr/plots
git fetch upstream
git checkout main && git pull
git branch backup/pre-2.9.0-$(date +%Y%m%d)     # punkt odwrotu
git checkout -b merge-upstream-2.9.0
```

### 2. Merge

```bash
git merge upstream/main
```

Spodziewaj się konfliktu w 7 plikach wyżej. Dla każdego:

```bash
git checkout --theirs R/<plik>.b.R    # baza = upstream
# potem ręcznie dołóż nasze: paletteFillGradient / wrap etykiet / boxMean
```

Punkty odniesienia dla naszych zmian:

```bash
git show 127095f -- R/utils.R R/jmvbox.b.R   # palety i gradienty
git show 873c6ac                              # wrap etykiet
git show 25cc849                              # boxMean
```

**CI upstreamu (`2faefcb`, `20f03d5`) pomiń** — mamy własny workflow, ich konfiguracja
celuje w monorepo jamovi. `99b8892` (CLAUDE.md) scal ręcznie albo zostaw nasz.

### 3. Wersja — **nie przegap**

`jmc` czyta wersję **wyłącznie** z `jamovi/0000.yaml`; `DESCRIPTION` jest tylko dla R CMD.
Zmień **oba** na `2.9.0` (albo `2.9.0-upwr`, jeśli chcesz odróżniać fork):

```bash
grep -m1 '^Version:' DESCRIPTION
grep -m1 '^version:' jamovi/0000.yaml
```

Bramka CI (`release-check.yml`) odrzuci push przy rozjeździe — to zamierzone.
Ta sama pułapka wyszła w audycie: commit 2.8.5 podbił `DESCRIPTION`, a `0000.yaml`
został na 2.8.3, przez co buildy stemplowały złą wersję.

### 4. Build i test — najważniejsza część

```bash
cd /home/maciek/praca/warsztat/jamovi-upwr
docker compose --profile main build
docker compose --profile main up
```

Sprawdź, że scatr ma **26 analiz** i komplet kategorii:

```bash
docker exec jamovi python3 -c "
import sys, os, asyncio, collections
sys.path.insert(0,'/usr/lib/jamovi/server')
os.environ.setdefault('JAMOVI_HOME','/usr/lib/jamovi')
from jamovi.server.settings import Settings
from jamovi.server.modules import Modules
async def main():
    m = Modules(Settings()); await m.read()
    for mod in m:
        if mod.name != 'scatr': continue
        g = collections.OrderedDict()
        for a in mod.analyses: g.setdefault(a.menuGroup, []).append(a.name)
        for k, v in g.items(): print('%-14s %2d: %s' % (k, len(v), ', '.join(sorted(v))))
asyncio.run(main())"
```

Oczekiwane (stan sprzed merge'a — musi się zgadzać co do wykresu):

```
Distribution    5: dens, jmvhist, qq, raincloud, ridge
Comparison      3: jmvbox, stripmean, violin
Ranking         6: circbar, jmvbar, lollipop, parcoord, radar, wordcloud
Correlation     6: bubble, corrgram, heatmap, hexbin, mosaic, scat
Evolution       2: area, jmvline
Composition     3: stackbar, treemap, waffle
Other           1: pareto
```

**Wizualnie przeklikaj wszystkie 26**, ze szczególną uwagą na:

- **7 plików z konfliktu** (histogram, boxplot, bar, line, pareto, scatter) — tu scalałeś ręcznie,
- **palety i motywy** — czy gradienty nadal idą za wybraną paletą (`paletteFillGradient`),
- **boxplot z opcją średniej** (`boxMean`),
- **długie etykiety kategorii** — czy nadal się zawijają,
- **osie liczbowe** — po `0f14ed1` bez notacji naukowej.

### 5. Push (kolejność ma znaczenie)

```bash
cd plots
git checkout main && git merge --no-ff merge-upstream-2.9.0
git push origin main                    # NAJPIERW submoduł

cd ..
git add plots
git commit -m "plots: merge upstream scatr 2.9.0 (składnia tidyverse, linewidth, osie)"
git push origin main                    # POTEM superprojekt
```

Odwrotna kolejność zostawia zawisły wskaźnik submodułu.

---

## Gdy scalanie okaże się za duże

Plan awaryjny — cherry-pick samych fixów, bez bumpu 2.9.0:

```bash
git cherry-pick 962c498    # linewidth (deprecated size) — konflikt jednolinijkowy
git cherry-pick 0f14ed1    # notacja naukowa na osiach
```

Załatwia najpilniejszy dług małym kosztem; generowanie składni zostaje na później.

---

## Definicja ukończenia

- [ ] merge scalony, `DESCRIPTION` = `0000.yaml` = 2.9.0
- [ ] Docker buduje się bez błędów
- [ ] scatr: 26 analiz, 7 kategorii — rozkład identyczny jak wyżej
- [ ] wszystkie 26 wykresów renderują się poprawnie
- [ ] palety, `boxMean` i wrap etykiet działają jak przed merge'em
- [ ] `plots` wypchnięty **przed** superprojektem
- [ ] CI (`Spójność wydania`) zielone

**Poza zakresem:** bump wersji jUPWR i wpis w CHANGELOG — to należy do procedury wydania
([`packaging/MODULES.md`](../../packaging/MODULES.md), sekcja *Procedura wydania*),
nie do tej sesji.
