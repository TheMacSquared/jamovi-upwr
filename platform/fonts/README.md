# Fonty motywów jUPWR

Kroje używane przez motywy `jupwrSkrypt`, `jupwrCiemny` i `jupwrPastel`
(`jmvcore::theme_jupwr_*`) — te same co w interaktywnych wykładach (rodzina
IBM Plex; tytuły bez osobnego kroju szeryfowego). Pliki są **nietknięte** —
nie są subsetowane ani przepakowywane, więc Reserved Font Name pozostaje
nienaruszona.

| plik | rodzina | rola w motywie | źródło |
|---|---|---|---|
| `IBMPlexSans-Regular.ttf`, `-Bold.ttf`, `-Italic.ttf`, `-BoldItalic.ttf` | IBM Plex Sans | tekst, opisy osi, legenda, tytuły, podtytuły, nagłówki paneli | [IBM/plex](https://github.com/IBM/plex) 3.005 |
| `IBMPlexMono-Regular.ttf`, `-Bold.ttf`, `-Italic.ttf`, `-BoldItalic.ttf` | IBM Plex Mono | etykiety osi, podpisy | [IBM/plex](https://github.com/IBM/plex) |

Plex Sans ma grekę, indeksy i znaki łączące (x̄, p̂, μ₁, σ²), ale nie ma
znaków ∩ i ᶜ — te renderer bierze z fontu zastępczego systemu.

## Licencje

Obie rodziny są na **SIL Open Font License 1.1**, która wprost zezwala na
dołączanie fontów do oprogramowania i ich redystrybucję. Pełny tekst licencji
wraz z notą copyright: `licenses/IBMPlex-OFL.txt`.

Reserved Font Name: „Plex". **Nie modyfikuj tych plików** — modyfikacja pod
oryginalną nazwą narusza OFL. Jeżeli kiedyś zajdzie potrzeba subsetowania,
plik wynikowy musi dostać inną nazwę rodziny, a `jupwrFontFiles`
w `jmvcore/R/themes.R` trzeba zaktualizować.

## Jak trafiają do aplikacji

Nie są instalowane w systemie. `jmvcore::jupwrRegisterFonts()` rejestruje je
w runtime przez `systemfonts::add_fonts()` — jeden mechanizm, który działa
tak samo na Linuksie, macOS i Windows. Katalog jest wyszukiwany w kolejności:
`JUPWR_FONTS_PATH`, `<JAMOVI_HOME>/fonts`, a na końcu `<root>/fonts` wyliczony
ze ścieżki zainstalowanego jmvcore.

Miejsce docelowe w buildach (kopiowany cały katalog):

| build | ścieżka | krok |
|---|---|---|
| Docker | `/usr/lib/jamovi/fonts` | `docker/jamovi-Dockerfile`, stage `jamovi` |
| macOS | `<app>/Contents/Resources/jamovi/fonts` | `packaging/scripts/macos/50-assemble-app.sh` |
| Windows | `<app>\Resources\fonts` | `packaging/scripts/windows/build.ps1` |

Jeśli fontów zabraknie, motywy schodzą na `sans` / `mono` i wykresy nadal się
renderują — tylko innym krojem.
