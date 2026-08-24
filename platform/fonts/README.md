# Fonty motywów jUPWR

Kroje używane przez motywy `jupwrSkrypt`, `jupwrCiemny` i `jupwrPastel`
(`jmvcore::theme_jupwr_*`). Pliki są **nietknięte** — nie są subsetowane ani
przepakowywane, więc Reserved Font Names pozostają nienaruszone.

| plik | rodzina | rola w motywie | źródło |
|---|---|---|---|
| `SourceSerif4-Regular.ttf`, `-Bold.ttf`, `-It.ttf` | Source Serif 4 | tytuły, podtytuły, nagłówki paneli | [adobe-fonts/source-serif](https://github.com/adobe-fonts/source-serif) 4.005R |
| `AtkinsonHyperlegible-Regular.ttf` | Atkinson Hyperlegible | tekst, opisy osi, legenda | [googlefonts/atkinson-hyperlegible](https://github.com/googlefonts/atkinson-hyperlegible) 1.006 |
| `JetBrainsMono-Regular.ttf` | JetBrains Mono | etykiety osi, podpisy | [JetBrains/JetBrainsMono](https://github.com/JetBrains/JetBrainsMono) 2.304 |

## Licencje

Wszystkie trzy rodziny są na **SIL Open Font License 1.1**, która wprost
zezwala na dołączanie fontów do oprogramowania i ich redystrybucję. Pełne
teksty licencji wraz z notami copyright znajdują się w `licenses/`.

Reserved Font Names: „Source" (Source Serif 4) oraz „ATKINSON" i
„HYPERLEGIBLE" (Atkinson Hyperlegible). **Nie modyfikuj tych plików** —
modyfikacja pod oryginalną nazwą narusza OFL. Jeżeli kiedyś zajdzie potrzeba
subsetowania, plik wynikowy musi dostać inną nazwę rodziny, a `jupwrFontFiles`
w `jmvcore/R/themes.R` trzeba zaktualizować.

## Jak trafiają do aplikacji

Nie są instalowane w systemie. `jmvcore::jupwrRegisterFonts()` rejestruje je
w runtime przez `systemfonts::add_fonts()` — jeden mechanizm, który działa
tak samo na Linuksie, macOS i Windows. Katalog jest wyszukiwany w kolejności:
`JUPWR_FONTS_PATH`, `<JAMOVI_HOME>/fonts`, a na końcu `<root>/fonts` wyliczony
ze ścieżki zainstalowanego jmvcore.

Miejsce docelowe w buildach:

| build | ścieżka | krok |
|---|---|---|
| Docker | `/usr/lib/jamovi/fonts` | `docker/jamovi-Dockerfile`, stage `jamovi` |
| macOS | `<app>/Contents/Resources/jamovi/fonts` | `packaging/scripts/macos/50-assemble-app.sh` |
| Windows | `<app>\Resources\fonts` | `packaging/scripts/windows/build.ps1` |

Jeśli fontów zabraknie, motywy schodzą na `serif` / `sans` / `mono` i wykresy
nadal się renderują — tylko innym krojem.
