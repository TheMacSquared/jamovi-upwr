# Test ręczny — grupa 3 audytu (awarie)

1. 20_anova_pusta_komorka.csv → ANOVA: len, czynniki supp i dose (interakcje
   włączone, typ III). ✅ Polska nota „Puste komórki planu…” zamiast angielskiego błędu.
   Zmień typ SS na II → tabela się liczy.
2. 21_anova_welch_n1.csv → ANOVA: y, grupa, „Nierówne wariancje (Welch)”.
   ✅ Nota „…co najmniej 2 obserwacji w każdej grupie (grupa „c”: 1)”.
3. 22_anova_stala.csv → ANOVA: y, grupa, „Nieparametrycznie”, Założenia → normalność.
   ✅ Analiza się liczy (bez czerwonego błędu).
4. 23_regresja_gvif.csv → Regresja liniowa: y, ilościowe x1, x2, jakościowa g,
   Założenia → współliniowość. ✅ Wiersze x1 ≈ 5,51, x2 ≈ 5,58, **g ≈ 1,04 (df 2)**
   (było ok. 9 dla kolumn g).
5. Zmienne ilościowe (dowolne dane): włącz histogram, potem „Gęstość na histogramie”;
   pudełkowy, potem skrzypce/punkty/średnia. ✅ Wykres zmienia się od razu.
6. Rozkład t: df = 10, λ = −3. ✅ Cała krzywa widoczna.
7. 24_raincloud_brak_grupy.csv → Wykresy → raincloud: y według grupa.
   ✅ Wykres dwóch grup a, b (bez błędu).
8. 25_bubble_ujemne.csv → bubble: x, y, wielkość = wielkosc.
   ✅ Trzy bąble i komunikat „Pominięto punkty z wielkością ≤ 0 (3)…”.
9. 26_wide_listwise.csv → stripmean, tryb „kilka zmiennych”: a, b, „Wyklucz przypadki”.
   ✅ Średnia a = 2 (było 41,2).
10. Serwer: zapisz nowy plik .omv w Dokumentach. ✅ W WSL `ls -l ~/Documents`
    pokazuje `-rw-r--r--` (było `-rw-------`).
