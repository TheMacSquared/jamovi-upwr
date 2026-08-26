#' @importFrom jmvcore .
tleorbitClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "tleorbitClass",
    inherit = tleorbitBase,
    private = list(
        # maps the 'satelita' option to names in inst/extdata/tle_przyklady.txt
        .nazwySatelitow = c(
            iss = "ISS (ZARYA)",
            noaa20 = "NOAA 20 (JPSS-1)",
            sentinel3a = "SENTINEL-3A",
            hst = "HST",
            meteosat10 = "METEOSAT-10 (MSG-3)",
            goes16 = "GOES 16",
            gps = "GPS BIII-1  (PRN 04)",
            galileo = "GSAT0101 (GALILEO-PFM)"
        ),

        .pobierzTLE = function() {
            if (self$options$zrodlo == "przyklad") {
                sats <- wczytajPrzykladyTLE()
                nazwa <- private$.nazwySatelitow[[self$options$satelita]]
                if (!nazwa %in% names(sats))
                    return(NULL)
                asteRisk::parseTLElines(sats[[nazwa]])
            } else {
                l1 <- trimws(self$options$tle1)
                l2 <- trimws(self$options$tle2)
                if (nchar(l1) < 60 || nchar(l2) < 60)
                    return(NULL)
                tryCatch(
                    asteRisk::parseTLElines(c("SATELITA", l1, l2)),
                    error = function(e) NULL)
            }
        },

        .run = function() {
            tle <- private$.pobierzTLE()
            if (is.null(tle)) {
                if (self$options$zrodlo == "wlasne")
                    self$results$elementy$setNote("err", paste(
                        "Wprowadz dwie pelne linie TLE (po 69 znakow).",
                        "Dane TLE znajdziesz np. na celestrak.org."))
                return()
            }

            # mean semi-major axis and altitude from Kepler's third law
            mu <- 398600.4418
            nRad <- tle$meanMotion * 2 * pi / 86400
            a <- (mu / nRad^2)^(1 / 3)

            el <- self$results$elementy
            el$addRow(rowKey = "incl", values = list(
                parametr = "Inklinacja", wartosc = tle$inclination,
                jednostka = "stopnie"))
            el$addRow(rowKey = "ecc", values = list(
                parametr = "Ekscentrycznosc", wartosc = tle$eccentricity,
                jednostka = ""))
            el$addRow(rowKey = "mm", values = list(
                parametr = "Obiegi na dobe", wartosc = tle$meanMotion,
                jednostka = "obr/doba"))
            el$addRow(rowKey = "okres", values = list(
                parametr = "Okres orbitalny", wartosc = 1440 / tle$meanMotion,
                jednostka = "min"))
            el$addRow(rowKey = "wys", values = list(
                parametr = "Srednia wysokosc (III prawo Keplera)",
                wartosc = a - 6371, jednostka = "km"))
            el$setNote("obj", paste0(
                "Obiekt: ", tle$objectName, ", epoka TLE: ",
                substr(tle$dateTime, 1, 19), " UTC"))

            # propagate with SGP4/SDP4 (asteRisk picks the algorithm)
            czasy <- seq(0, self$options$czasProp * 60, by = self$options$krok)
            if (length(czasy) > 5000) {
                self$results$staty$setNote("err", paste(
                    "Za wiele punktow propagacji (max 5000) —",
                    "zwieksz krok lub skroc czas."))
                return()
            }

            epoka <- as.POSIXct(tle$dateTime, tz = "UTC")
            n0 <- asteRisk::revDay2radMin(tle$meanMotion)
            wynik <- tryCatch({
                punkty <- lapply(czasy, function(m) {
                    st <- asteRisk::sgdp4(
                        n0 = n0,
                        e0 = tle$eccentricity,
                        i0 = asteRisk::deg2rad(tle$inclination),
                        M0 = asteRisk::deg2rad(tle$meanAnomaly),
                        omega0 = asteRisk::deg2rad(tle$perigeeArgument),
                        OMEGA0 = asteRisk::deg2rad(tle$ascension),
                        Bstar = tle$Bstar,
                        initialDateTime = tle$dateTime,
                        targetTime = m)
                    geo <- temeNaGeo(st$position, epoka + m * 60)
                    c(minuta = m, geo,
                      predkosc = sqrt(sum(st$velocity^2)))
                })
                as.data.frame(do.call(rbind, punkty))
            }, error = function(e) NULL)

            if (is.null(wynik)) {
                self$results$staty$setNote("err",
                    "Propagacja nie powiodla sie — sprawdz poprawnosc TLE.")
                return()
            }

            if (self$options$pokazStaty) {
                st <- self$results$staty
                for (w in list(
                        list(klucz = "wys", tytul = "Wysokosc [km]",
                             x = wynik$wysokosc),
                        list(klucz = "pred", tytul = "Predkosc [km/s]",
                             x = wynik$predkosc))) {
                    st$addRow(rowKey = w$klucz, values = list(
                        wielkosc = w$tytul,
                        n = length(w$x),
                        mean = mean(w$x),
                        sd = sd(w$x),
                        min = min(w$x),
                        max = max(w$x)))
                }
                st$setNote("info", paste0(
                    "Propagacja SGP4/SDP4 od epoki TLE, ",
                    self$options$czasProp, " h co ", self$options$krok,
                    " min. Wysokosc nad srednim promieniem Ziemi (6371 km); ",
                    "pozycje geograficzne w przyblizeniu GMST."))
            }

            stan <- list(nazwa = tle$objectName, trasa = wynik)
            self$results$mapa$setState(stan)
            self$results$profil$setState(stan)
        },

        .mapaPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state
            tr <- s$trasa

            # break the path where it crosses the date line to avoid
            # horizontal jumps across the whole map
            skok <- c(FALSE, abs(diff(tr$lon)) > 180)
            tr$segment <- cumsum(skok)

            plot <- buildMapaBazowa(theme = theme) +
                ggplot2::geom_path(
                    data = tr,
                    ggplot2::aes(x = lon, y = lat, group = segment),
                    color = theme$color[2], linewidth = 0.7) +
                ggplot2::geom_point(
                    data = tr[1, ],
                    ggplot2::aes(x = lon, y = lat),
                    color = theme$color[2], size = 3, shape = 17) +
                ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-90, 90),
                                  expand = FALSE) +
                ggplot2::labs(
                    title = paste("Trasa naziemna:", s$nazwa),
                    x = "Dlugosc geograficzna",
                    y = "Szerokosc geograficzna") +
                ggtheme
            plot
        },

        .profilPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state
            tr <- s$trasa

            plot <- ggplot2::ggplot(tr,
                    ggplot2::aes(x = minuta / 60, y = wysokosc)) +
                ggplot2::geom_line(color = theme$color[2], linewidth = 0.7) +
                ggplot2::labs(
                    title = paste("Wysokosc orbity:", s$nazwa),
                    x = "Czas od epoki TLE [h]",
                    y = "Wysokosc [km]") +
                ggtheme
            plot
        }
    )
)
