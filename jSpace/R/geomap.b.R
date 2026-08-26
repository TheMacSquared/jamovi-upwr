#' @importFrom jmvcore .
geomapClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "geomapClass",
    inherit = geomapBase,
    private = list(
        .run = function() {
            if (is.null(self$options$lon) || is.null(self$options$lat))
                return()

            lon <- jmvcore::toNumeric(self$data[[self$options$lon]])
            lat <- jmvcore::toNumeric(self$data[[self$options$lat]])

            wartoscVar <- self$options$wartosc
            grupaVar <- self$options$grupa
            wartosc <- if (!is.null(wartoscVar))
                jmvcore::toNumeric(self$data[[wartoscVar]]) else NULL
            grupa <- if (!is.null(grupaVar))
                as.factor(self$data[[grupaVar]]) else NULL

            validIdx <- !is.na(lon) & !is.na(lat) &
                lon >= -180 & lon <= 180 & lat >= -90 & lat <= 90
            if (sum(validIdx) < 1) {
                self$results$regiony$setNote("err", paste(
                    "Brak poprawnych wspolrzednych — lon w [-180, 180],",
                    "lat w [-90, 90]."))
                return()
            }
            lon <- lon[validIdx]
            lat <- lat[validIdx]
            if (!is.null(wartosc)) wartosc <- wartosc[validIdx]
            if (!is.null(grupa)) grupa <- droplevels(grupa[validIdx])

            # spatial join: which country does each point fall into
            # (teaching bridge: GeoPandas sjoin -> sf st_join); planar
            # predicates — Natural Earth polygons are not valid on the
            # S2 sphere and lon/lat point-in-polygon is fine planar
            s2Bylo <- sf::sf_use_s2()
            suppressMessages(sf::sf_use_s2(FALSE))
            on.exit(suppressMessages(sf::sf_use_s2(s2Bylo)), add = TRUE)
            kraje <- granicePanstw()
            punkty <- sf::st_as_sf(
                data.frame(lon = lon, lat = lat),
                coords = c("lon", "lat"), crs = sf::st_crs(kraje))
            zlacz <- suppressMessages(sf::st_join(punkty, kraje["panstwo"]))
            panstwo <- zlacz$panstwo
            panstwo[is.na(panstwo)] <- "(poza ladem)"

            if (self$options$pokazRegiony) {
                tab <- self$results$regiony
                for (p in sort(unique(panstwo))) {
                    idx <- panstwo == p
                    w <- if (!is.null(wartosc)) wartosc[idx] else NULL
                    tab$addRow(rowKey = p, values = list(
                        panstwo = p,
                        n = sum(idx),
                        mean = if (!is.null(w) && any(!is.na(w)))
                            mean(w, na.rm = TRUE) else NA,
                        sd = if (!is.null(w) && sum(!is.na(w)) > 1)
                            sd(w, na.rm = TRUE) else NA))
                }
                if (is.null(wartosc))
                    tab$setNote("info", paste(
                        "Przypisanie punktow do panstw przez zlaczenie",
                        "przestrzenne (sf::st_join). Wybierz zmienna Wartosc,",
                        "aby zobaczyc srednie i SD."))
                else
                    tab$setNote("info", paste0(
                        "Srednia i SD zmiennej: ", wartoscVar,
                        ". Zlaczenie przestrzenne: sf::st_join."))
            }

            if (self$options$korelacja) {
                if (is.null(wartosc)) {
                    self$results$kor$setNote("err",
                        "Wybierz zmienna Wartosc, aby policzyc korelacje.")
                } else {
                    ok <- !is.na(wartosc)
                    if (sum(ok) < 3) {
                        self$results$kor$setNote("err",
                            "Za malo obserwacji do korelacji.")
                    } else {
                        ct <- cor.test(lat[ok], wartosc[ok])
                        self$results$kor$setRow(rowNo = 1, values = list(
                            r = unname(ct$estimate),
                            t = unname(ct$statistic),
                            df = unname(ct$parameter),
                            p = ct$p.value,
                            ciLow = ct$conf.int[1],
                            ciUpp = ct$conf.int[2]))
                        self$results$kor$setNote("info", paste0(
                            "Korelacja Pearsona: ", wartoscVar,
                            " ~ szerokosc geograficzna (", self$options$lat,
                            ")."))
                    }
                }
            }

            stan <- list(
                lon = lon, lat = lat,
                wartosc = wartosc,
                grupa = if (is.null(grupa)) NULL else as.character(grupa),
                wartoscLabel = wartoscVar, grupaLabel = grupaVar,
                panstwo = panstwo)
            self$results$mapa$setState(stan)
            self$results$mapaKart$setState(stan)
        },

        .zakresMapy = function(lon, lat) {
            zakres <- self$options$zakres
            if (zakres == "europa")
                return(list(lon = c(-25, 45), lat = c(34, 72)))
            if (zakres == "swiat")
                return(list(lon = c(-180, 180), lat = c(-90, 90)))
            margLon <- max(2, diff(range(lon)) * 0.15)
            margLat <- max(2, diff(range(lat)) * 0.15)
            list(lon = c(min(lon) - margLon, max(lon) + margLon),
                 lat = c(max(-90, min(lat) - margLat),
                         min(90, max(lat) + margLat)))
        },

        .mapaPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state
            df <- data.frame(lon = s$lon, lat = s$lat)
            if (!is.null(s$wartosc)) df$wartosc <- s$wartosc
            if (!is.null(s$grupa)) df$grupa <- s$grupa

            plot <- buildMapaBazowa(private$.zakresMapy(s$lon, s$lat), theme)

            plot <- plot + ggplot2::labs(x = "Dlugosc geograficzna",
                                         y = "Szerokosc geograficzna")

            # ggtheme carries discrete colour scales — for a continuous
            # aesthetic add it first, then override with the theme gradient
            if (!is.null(s$grupa)) {
                plot <- plot + ggplot2::geom_point(
                    data = df,
                    ggplot2::aes(x = lon, y = lat, color = grupa),
                    size = 2, alpha = 0.85) +
                    ggplot2::labs(color = s$grupaLabel) +
                    ggtheme
            } else if (!is.null(s$wartosc)) {
                plot <- plot + ggplot2::geom_point(
                    data = df,
                    ggplot2::aes(x = lon, y = lat, color = wartosc),
                    size = 2, alpha = 0.85) +
                    ggplot2::labs(color = s$wartoscLabel) +
                    ggtheme
                suppressMessages(
                    plot <- plot + ggplot2::scale_color_gradientn(
                        colours = theme$gradient))
            } else {
                plot <- plot + ggplot2::geom_point(
                    data = df,
                    ggplot2::aes(x = lon, y = lat),
                    color = theme$color[2], size = 2, alpha = 0.85) +
                    ggtheme
            }

            plot
        },

        .kartogramPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state

            agregacja <- self$options$agregacja
            if (agregacja != "n" && is.null(s$wartosc))
                agregacja <- "n"

            dfp <- data.frame(panstwo = s$panstwo)
            if (!is.null(s$wartosc)) dfp$wartosc <- s$wartosc
            agr <- switch(agregacja,
                srednia = aggregate(wartosc ~ panstwo, dfp, mean),
                mediana = aggregate(wartosc ~ panstwo, dfp, median),
                n = aggregate(list(wartosc = dfp$panstwo),
                              by = list(panstwo = dfp$panstwo), FUN = length))
            etykieta <- switch(agregacja,
                srednia = paste("Srednia:", s$wartoscLabel),
                mediana = paste("Mediana:", s$wartoscLabel),
                n = "Liczba punktow")

            kraje <- granicePanstw()
            kraje <- merge(kraje, agr, by = "panstwo", all.x = TRUE)

            plot <- ggplot2::ggplot() +
                ggplot2::geom_sf(data = kraje,
                                 ggplot2::aes(fill = wartosc),
                                 color = theme$color[1], linewidth = 0.2) +
                ggplot2::labs(fill = etykieta,
                              x = "Dlugosc geograficzna",
                              y = "Szerokosc geograficzna")

            zakres <- private$.zakresMapy(s$lon, s$lat)
            plot <- plot + ggplot2::coord_sf(
                xlim = zakres$lon, ylim = zakres$lat, expand = FALSE)

            # continuous fill: theme gradient must land after ggtheme's
            # discrete scales
            plot <- plot + ggtheme
            suppressMessages(
                plot <- plot + ggplot2::scale_fill_gradientn(
                    colours = theme$gradient,
                    na.value = grDevices::adjustcolor(theme$color[1],
                                                     alpha.f = 0.12)))
            plot
        }
    )
)
