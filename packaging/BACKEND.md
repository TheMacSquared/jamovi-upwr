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
