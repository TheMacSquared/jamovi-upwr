# Import: zachowanie danych po błędzie odczytu

Pracuj na kopii dokumentu. Pliki testowe są wbudowane w obraz Dockera,
poza Biblioteką: **Dokumenty → jupwr-test-audyt**.

1. Otwórz `upstream-28.17.csv`; powinno być 6 wierszy i 8 kolumn.
2. Zmień pierwszą wartość w kolumnie „wynik (cm)” z 10 na 999.
3. Wybierz **Importuj** i wskaż `import-uszkodzony-test.omv` z tego folderu.
   Plik jest celowo uszkodzony. Powinien pojawić się błąd, ale arkusz ma nadal
   zawierać 6 wierszy, te same kolumny i wartość 999. Analizy mają pozostać.
4. Użyj Ctrl+Z: wartość ma wrócić do 10 (historia sprzed importu jest zachowana).
5. Ponownie zmień komórkę na 999. Przy imporcie kilku plików wybierz razem
   `upstream-28.17.csv` i `import-uszkodzony-test.omv`. Niezależnie od tego,
   który plik zostanie odczytany pierwszy, błąd ma pozostawić stary arkusz.
6. Dla porównania zaimportuj sam poprawny `upstream-28.17.csv`. Import powinien
   zastąpić dane i przywrócić wartość 10, zachowując dotychczasową obsługę analiz.

Testy automatyczne dodatkowo sprawdzają błędny pierwszy, drugi i trzeci plik,
przerwanie odczytu, limit łącznej liczby wierszy oraz zwalnianie buforów.
Ochrona dotyczy przygotowania i odczytu źródeł przed zastąpieniem danych.
