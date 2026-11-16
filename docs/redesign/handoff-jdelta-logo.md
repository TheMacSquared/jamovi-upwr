# Handoff: znak i ikona programu jDelta

**Dla:** agent projektowy (Claude Design)
**Zadanie:** zaprojektować znak, logotyp i ikonę aplikacji dla nowej nazwy programu.
Nie wdrażasz zmian w kodzie. Oddajesz pliki SVG i krótkie uzasadnienie.
**Kontekst w repozytorium:** zadanie 31 w `docs/DECYZJE.md` (plan zmiany nazwy).

---

## 1. Czym jest program

jDelta, dawniej jUPWR, to dystrybucja jamovi do nauki statystyki na Uniwersytecie
Przyrodniczym we Wrocławiu (UPWr). Korzystają z niej studenci kursu podstawowego:
biologia, rolnictwo, inżynieria środowiska, hydrologia, weterynaria. Program działa
w pracowniach, na prywatnych laptopach studentów (Windows, macOS) i na rzutniku
podczas zajęć.

Nazwa ma dwa znaczenia i znak powinien pokazać oba:

- **delta rzeki**, czyli rozlewisko u ujścia: hydrologia, środowisko, przyroda;
- **Δ**, czyli różnica, zmiana, błąd, efekt: to, czego szuka się w każdej analizie
  statystycznej.

Małe „j” nawiązuje do jamovi i do nazw modułów programu (jCI, jANOVA, jEksplor).
Na zajęciach mówi się po prostu „Delta”.

## 2. Charakter

- Poważny i rzeczowy, ale nie korporacyjny: narzędzie akademickie, nie produkt
  marketingowy.
- Spokojny i czytelny: student ma bez wahania rozpoznać ikonę na pulpicie w pracowni.
- Bez infantylizmu: żadnych maskotek, uśmiechniętych kropli ani clipartu.
- Bez dosłownej ilustracji mapy delty. Chodzi o znak, nie o rysunek.

## 3. Kolory (ustalone, nie zmieniamy)

Kolorystyka jest spójna z interfejsem i materiałami wykładowymi. Nie dodawaj nowych
kolorów marki.

| Rola | Kolor | Gdzie dziś |
|---|---|---|
| bordo UPWr (główny) | `#832034` | pasek tytułu, akcenty, tło ikony |
| bordo jaśniejsze | `#9A2A40` | stan najechania |
| złoty akcent | `#E6AC41` | pasek w obecnej ikonie |
| kremowa biel | `#FBF8F6` | znak na bordowym tle |
| tekst | `#333333` | |

Dopuszczalne są odcienie bordo i złotego (np. przejście w rozlewisku), jeśli nie
wprowadzają nowej barwy.

## 4. Obecny stan (do zastąpienia)

Ikona aplikacji (`platform/app-icon.svg`): bordowy kwadrat z zaokrąglonymi rogami,
kremowe litery „jU” szeryfowym krojem (Libre Baskerville), złoty pasek pod spodem.

```svg
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64" width="64" height="64">
  <rect width="64" height="64" rx="15" fill="#832034"></rect>
  <text x="32" y="42" text-anchor="middle" font-family="'Libre Baskerville',Georgia,serif"
        font-size="31.17" font-weight="700" fill="#FBF8F6">jU</text>
  <rect x="24.43" y="50" width="26.34" height="4" rx="2" fill="#E6AC41"></rect>
</svg>
```

Logotyp (`client/assets/logo-jupwr.svg`, 204×64): biały napis „jUPWR” bezszeryfowym
krojem systemowym na bordowym pasku ekranu „Otwórz/Zapisz”.

Forma bordowego kwadratu z kremowym znakiem i złotym akcentem może zostać, bo
użytkownicy ją znają. Zmienia się to, co jest w środku.

## 5. Kierunki do sprawdzenia

Przygotuj 2–3 koncepcje. Punkt wyjścia:

1. **Δ jako rozlewisko**: trójkąt, którego podstawa rozchodzi się w kilka odnóg
   (jak ujście rzeki). Złoty akcent może być jedną z odnóg.
2. **Δ z krzywą rozkładu**: krzywa Gaussa wpisana w trójkąt albo tworząca jego
   podstawę.
3. **Własna propozycja**, np. „j” połączone z Δ.

Przy każdej koncepcji napisz dwa, trzy zdania: co znak mówi i dlaczego działa
w małym rozmiarze.

## 6. Wymagania techniczne

**Ikona aplikacji**
- SVG z `viewBox="0 0 64 64"`, kształty zamiast tekstu (żadnych elementów `<text>`,
  bo font może nie być zainstalowany przy generowaniu PNG).
- Musi być czytelna w 16×16 px (pasek zadań Windows, karta przeglądarki). Pokaż
  podgląd w 16, 32 i 256 px. Jeśli przy 16 px szczegóły się zlewają, oddaj osobny,
  uproszczony wariant na małe rozmiary.
- Z tego SVG generujemy później `app-icon.ico` (Windows), `app-icon.icns`
  i `app-icon.iconset` (macOS) oraz `app-icon.png`. Nie musisz ich robić.

**Logotyp (napis + znak)**
- Wersja pozioma: znak + „jDelta”. Pisownia zawsze „jDelta”: małe „j”, wielkie „D”,
  bez spacji.
- Wariant biały na bordo, do paska ekranu „Otwórz/Zapisz”; obecny plik ma
  `viewBox="0 0 204 64"`, nowy może mieć inną szerokość.
- Wariant ciemny na jasnym tle, do ekranu startowego.
- Napis zamieniony na krzywe albo z podanym krojem. W programie są kroje
  IBM Plex Sans i IBM Plex Mono (licencja OFL); obecna ikona używa Libre Baskerville.
  Inny krój jest możliwy, jeśli ma wolną licencję.

**Wersja jednobarwna**
- Znak w jednym kolorze (bordo albo czarny) do wydruków i materiałów czarno-białych.

## 7. Ograniczenia

- **Logo UPWr zostaje osobno i bez zmian.** Ekran startowy i ustawienia pokazują
  logotyp uczelni (`upwr-logotyp-pl-poziomy.png`). Nie przerabiaj go, nie łącz go ze
  znakiem jDelta w jeden kształt i nie naśladuj go. Pokaż za to na makiecie, jak oba
  znaki wyglądają obok siebie na ekranie startowym (logo UPWr, pod nim nazwa jDelta,
  napis „uruchamianie…”, pasek postępu i wersja).
- Znak nie może przypominać logo jamovi ani innych programów statystycznych.
- Bez wykresu kołowego w znaku (dydaktyka świadomie go unika).

## 8. Co oddajesz

Pliki w `docs/redesign/jdelta/`:

- `koncepcje.md`: 2–3 koncepcje z podglądami i uzasadnieniem, z zaznaczeniem
  rekomendowanej;
- dla wybranej koncepcji (po akceptacji użytkownika):
  - `app-icon.svg` (64×64) i ewentualnie `app-icon-small.svg` (wariant na 16 px);
  - `logo-jdelta-white.svg` (biały na bordo), `logo-jdelta-dark.svg` (na jasnym tle);
  - `znak-mono.svg`;
  - makieta ekranu startowego (PNG albo SVG).

Pliki w `platform/` i `client/assets/` podmienia później agent wdrażający zadanie 31.
Nie nadpisuj ich sam.
