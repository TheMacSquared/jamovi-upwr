# Konfiguracja backendu jUPWR

Ustawienia można przekazać przez zmienne środowiskowe przed uruchomieniem
aplikacji lub jako klucze bez prefiksu `JAMOVI_` w sekcji `[ENV]` pliku `env.conf`.
W Docker Compose dodaj wybrane wartości do `environment` usługi `jamovi`.

| Zmienna | Domyślnie | Znaczenie |
|---|---:|---|
| `JAMOVI_R_PROCESSES` | `4` | Łączna liczba procesów R na sesję. Przy co najmniej dwóch procesach jeden slot pozostaje do inicjalizacji; przy jednym obsługuje on także obliczenia. |
| `JAMOVI_WEBSOCKET_QUEUE_MESSAGES` | `256` | Maksymalna liczba oczekujących wiadomości, poza jedną wysyłaną. |
| `JAMOVI_WEBSOCKET_QUEUE_BYTES` | `67108864` | Limit bajtów oczekujących i wysyłanej wiadomości razem (64 MiB). |
| `JAMOVI_WEBSOCKET_SEND_TIMEOUT` | `30` | Maksymalny czas wysłania jednej wiadomości, w sekundach. |

Wartości muszą być dodatnimi liczbami całkowitymi. Zwiększenie liczby procesów
R zwiększa także użycie pamięci; limit jest liczony na sesję serwera.

Każde połączenie ma jedno zadanie wysyłania. Oczekujące komunikaty postępu tego
samego żądania są zastępowane najnowszym, a odpowiedź końcowa usuwa jego stary
postęp. Odpowiedzi i dane zachowują kolejność. Przekroczenie limitu albo timeout
zamyka przeciążone połączenie kodem 1013; sesja pozostaje dostępna do ponownego
połączenia. Takie zamknięcie jest traktowane jak utrata połączenia, a nie
świadome zamknięcie dokumentu. Obowiązują nadal limity czasu życia sesji.

Anulowanie wysłanego obliczenia kończy zajęty proces R i uruchamia jego następcę,
zanim przyjmie następne obliczenie. Pozostałe procesy pracują dalej. Zamknięcie
sesji i globalny restart sprzątają procesy bez uruchamiania zbędnych następców.

## Zapis i odzyskiwanie

Zapis `.omv` używa unikalnego pliku tymczasowego w tym samym katalogu, opróżnia
bufory pliku przez `fsync` i dopiero wtedy podmienia docelowy plik. Błąd przed
podmianą zachowuje poprzednią wersję. Dwa zapisy nie współdzielą pliku `.tmp`.
Anulowany zapis nie zwalnia blokad danych, dopóki wątek zapisujący nie zakończy
pracy. Zakończonej już podmiany nie cofa się po późniejszym anulowaniu.

| Zmienna | Domyślnie | Znaczenie |
|---|---|---|
| `JAMOVI_RECOVERY_INTERVAL` | `120` | Odstęp kopii w sekundach; `0` wyłącza także kopię przy kończeniu sesji. |
| `JAMOVI_RECOVERY_PATH` | `recovery` w katalogu danych aplikacji | Katalog kopii; w Compose `/root/recovery`, na wolumenie `jupwr-recovery`. |

Kopie obejmują zmienione dokumenty i nowe dokumenty zawierające dane. Początkowy,
pusty i nieedytowany arkusz nie tworzy kopii. Mechanizm wymaga uprawnień do
lokalnego zapisu. Działa niezależnie od dotychczasowego autosave, które nadal
może zapisywać otwarty wcześniej plik, jeśli administrator je włączył.

Przy zamknięciu sesji powstaje jeszcze jedna kopia oczekujących zmian. Po awarii
można odzyskać stan z ostatniej zakończonej kopii. Kopia nie zmienia ścieżki,
tytułu ani oznaczenia niezapisanych zmian w otwartym dokumencie.

Wybierz **Otwórz → Ten komputer**, przejdź do poziomu głównego (nad Dokumentami)
i otwórz **Odzyskiwanie**. Pliki mają tytuł dokumentu i unikalny identyfikator.
Otwarta kopia ma niezapisane zmiany i wymaga wybrania docelowego pliku przy zapisie.
Kopie pozostają do ręcznego usunięcia; nie kasujemy ich przy otwarciu ani po
przebudowie kontenera. Usunięcie wolumenu Dockera usuwa również kopie.

Sprawdzone automatycznie w Dockerze/Linux. Zachowanie systemu plików i okien
plikowych paczek Windows/macOS wymaga sprawdzenia na tych systemach.
