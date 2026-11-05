# Test ręczny poprawek z audytu 2026-11-05

Dane: pliki CSV w tym katalogu (w Dockerze: `Dokumenty/jUPWR-audyt/`).
Każdy punkt: plik → analiza → ustawienia → **oczekiwany wynik**. W nawiasie
wartość sprzed poprawki, żeby było widać różnicę.

## jANOVA

1. **`01_anova_rm_kowarianta.csv`** → ANOVA powtórzonych pomiarów.
   Zmienna zależna `wynik`, jednostka `id`, wewnątrzobiektowy `czas`, kowarianta `wiek`.
   **Wiersz `czas`: F ≈ 230,8, p < 0,001** (było F = 0,34, p = 0,72).
   W „Opisie metod”: zdanie o wycentrowanych kowariantach.

2. **`02_anova_rm_porownania.csv`** → ANOVA powtórzonych pomiarów.
   Zależna `wynik`, jednostka `id`, wewnątrz `czas`, międzyobiektowy `grupa`;
   porównania Tukey, włączona tabela par.
   **Porównania `czas`: df = 18 w każdej parze; b–d p ≈ 0,045, b–c p ≈ 0,008**
   (było df ≈ 7,2; b–d p = 0,086, b–c p = 0,030). Litery: b i d w różnych grupach.

3. **`03_anova_reszty_filtr.csv`** → filtr `wiersz != 1`, potem ANOVA:
   zależna `y`, czynnik `grupa`, w Zaawansowanych zapis reszt do arkusza.
   **Kolumna reszt: wiersz 1 pusty, wiersz 2 = −1,0875, wiersz 12 = −1,845**
   (było: wartości przesunięte o jeden wiersz w górę, ostatni pusty).

## jRegr

4. **`04_regresja_wspolliniowa.csv`** → Regresja liniowa, zależna `y`,
   ilościowe `a`, `dup`, `c`, jakościowa `g`.
   **Każdy współczynnik leży w swoim przedziale.** Wiersz `c`: b = 0,0352,
   CI (−0,3119; 0,3824); wiersz `g: y`: b = −0,1225, CI (−0,8472; 0,6022)
   (było: przedziały przesunięte o wiersz, `c` bez przedziału). Nota o współczynniku NA.

5. Ten sam plik, filtr `wiersz != 1`, regresja `y` ~ `a`, zapis wartości przewidywanych.
   **Wiersz 1 pusty, wiersz 2 = 0,2273.**

## jEksplor

6. **`05_szereg_rozdzielczy.csv`** → Szereg rozdzielczy, zmienna `x`, reguła Sturgesa.
   **Razem = 19, ostatnia klasa zawiera 1,0** (było: Razem 17).
7. Zmienna `x2`, zadana szerokość klasy 0,1.
   **0,3 w klasie [0,3; 0,4), 0,7 w osobnej klasie [0,7; 0,8)** (było: 0,3 w [0,2; 0,3)).
8. **`03_anova_reszty_filtr.csv`** z filtrem `wiersz != 1` → Zmienne ilościowe, `y`,
   Zaawansowane → Wartości skrajne (po 1).
   **Najmniejsza 16,34 w wierszu 9, największa 23,8 w wierszu 10** (było: 8 i 9).

## jperm i jTestyT

9. **`06_perm_jedna_proba.csv`** → Test permutacyjny jednej próby, `x`, wartość testowa 0,1,
   hipoteza „większa”, test dokładny. **p = 0,0156** (było p = 0).
10. **`07_testy_t.csv`**, filtr `zestaw == "jedna"` → Test t jednej próby, `wynik`,
    wartość testowa 100, Wilcoxon. **Wiersz Wilcoxona: różnica 4,5** (było 104,5).
11. Filtr `zestaw == "dwie"` → Test t dwóch grup, `wynik` wg `grupa`, Mann-Whitney.
    **r rangowo-dwuseryjne = 1,00, ten sam znak co różnica i d Cohena** (było −1,00).

## jCzest

12. **`08_zgodnosc.csv`** → Zgodność, `kategoria`, χ² z poprawką ciągłości.
    **χ² z poprawką = 2,45, p ≈ 0,118** (było 3,20, p = 0,074).
13. **`09_zalezne_mcnemar_cochran.csv`** → Pomiary zależne: `p1`, `p2`, wielkość efektu.
    **OR = 3,00 (1,09; 8,25).** Następnie `p1`, `p2`, `p3` z porównaniami par:
    **para p1–p2: OR = 3,00** (było 0,333).
14. **`10_tabela_v_cramera.csv`** → Tabela, wiersze `wiersz`, kolumny `kolumna`,
    V Craméra z przedziałem. **V = 0,507, przedział (0,254; 0,760)** — zawsze ten sam.
15. **`11_tabela_v_nota.csv`** → jak wyżej. **V = 0,2295 powyżej górnej granicy 0,1844
    i nota „V jest obciążone w górę…”.**

## jCI

16. **`12_proporcja_bca.csv`** → Proporcja, `odpowiedz`, metoda bootstrap BCa, ziarno 1.
    **Przedział obejmuje 0,5 i jest bliski percentylowemu** (było ok. [0,25; 0,65]).

## jdistrACTION

17. Rozkład t, df = 1, λ = 0, „Pokaż E[X]”. **E[X]: nieokreślone (df ≤ 1)** (było 0).
18. df = 400, λ = 1, E[X] i Var[X]. **Liczby, nie NaN.**

## jSpace (moduł opcjonalny, tylko jeśli zainstalowany)

19. Własne dane: lon/lat = (−100; 49), (17; 51), (20; 52), (21; 52,5), (25; 22),
    wartość 1–5 → Mapa, statystyki regionalne.
    **Polska: n = 3, średnia 3; suma n = 5.**
