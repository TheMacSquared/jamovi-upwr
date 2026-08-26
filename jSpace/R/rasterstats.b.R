#' @importFrom jmvcore .
rasterstatsClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "rasterstatsClass",
    inherit = rasterstatsBase,
    private = list(
        .wczytajRaster = function() {
            zrodlo <- self$options$zrodlo
            if (zrodlo == "ndviWroclaw") {
                path <- system.file("extdata", "ndvi_wroclaw.tif",
                                    package = "jSpace")
                list(r = terra::rast(path), etykieta = "NDVI")
            } else if (zrodlo == "elewacja") {
                path <- system.file("ex", "elev.tif", package = "terra")
                list(r = terra::rast(path), etykieta = "Elewacja [m]")
            } else {
                if (is.null(self$options$xKol) || is.null(self$options$yKol) ||
                        is.null(self$options$wartoscKol))
                    return(NULL)
                x <- jmvcore::toNumeric(self$data[[self$options$xKol]])
                y <- jmvcore::toNumeric(self$data[[self$options$yKol]])
                w <- jmvcore::toNumeric(self$data[[self$options$wartoscKol]])
                ok <- !is.na(x) & !is.na(y)
                if (sum(ok) < 4)
                    return(NULL)
                # a regular x/y grid in a spreadsheet IS a raster —
                # teaching bridge to how terra/TorchGeo see imagery
                r <- tryCatch(
                    terra::rast(data.frame(x = x[ok], y = y[ok], z = w[ok]),
                                type = "xyz", crs = "EPSG:4326"),
                    error = function(e) NULL)
                if (is.null(r))
                    return(NULL)
                list(r = r, etykieta = self$options$wartoscKol)
            }
        },

        .run = function() {
            zr <- private$.wczytajRaster()
            if (is.null(zr)) {
                if (self$options$zrodlo == "kolumny")
                    self$results$staty$setNote("err", paste(
                        "Wybierz kolumny x, y i wartosci tworzace regularna",
                        "siatke (np. zbior 'NDVI — okolice Wroclawia')."))
                return()
            }

            w <- terra::values(zr$r)[, 1]
            w <- w[!is.na(w)]
            if (length(w) == 0)
                return()

            kw <- quantile(w, c(0.25, 0.5, 0.75))
            self$results$staty$setRow(rowNo = 1, values = list(
                n = length(w),
                mean = mean(w),
                sd = sd(w),
                min = min(w),
                q25 = kw[[1]],
                median = kw[[2]],
                q75 = kw[[3]],
                max = max(w)))
            self$results$staty$setNote("info", paste0(
                "Warstwa: ", zr$etykieta, ", ", nrow(zr$r), " x ", ncol(zr$r),
                " komorek (terra)."))

            if (self$options$pokazProg) {
                prog <- self$options$prog
                nPow <- sum(w > prog)
                self$results$progTab$setRow(rowNo = 1, values = list(
                    prog = prog,
                    n = nPow,
                    procent = nPow / length(w)))
            }

            df <- terra::as.data.frame(zr$r, xy = TRUE)
            names(df) <- c("x", "y", "wartosc")
            stan <- list(df = df, etykieta = zr$etykieta,
                         geograficzny = terra::is.lonlat(zr$r))
            self$results$histogram$setState(
                list(wartosci = w, etykieta = zr$etykieta))
            self$results$mapa$setState(stan)
        },

        .histPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state

            plot <- ggplot2::ggplot(data.frame(w = s$wartosci),
                    ggplot2::aes(x = w)) +
                ggplot2::geom_histogram(bins = self$options$klasyHist,
                                        fill = theme$fill[2],
                                        color = theme$color[1]) +
                ggplot2::labs(x = s$etykieta, y = "Liczba komorek") +
                ggtheme

            if (self$options$pokazProg)
                plot <- plot + ggplot2::geom_vline(
                    xintercept = self$options$prog,
                    color = theme$color[2], linetype = "dashed",
                    linewidth = 0.8)

            plot
        },

        .mapaPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state

            plot <- ggplot2::ggplot(s$df,
                    ggplot2::aes(x = x, y = y, fill = wartosc)) +
                ggplot2::geom_raster() +
                ggplot2::labs(fill = s$etykieta,
                              x = if (s$geograficzny)
                                  "Dlugosc geograficzna" else "x",
                              y = if (s$geograficzny)
                                  "Szerokosc geograficzna" else "y") +
                ggplot2::coord_fixed(
                    ratio = if (s$geograficzny)
                        1 / cos(mean(s$df$y) * pi / 180) else 1) +
                ggtheme
            # continuous fill: theme gradient must land after ggtheme's
            # discrete scales
            suppressMessages(
                plot <- plot + ggplot2::scale_fill_gradientn(
                    colours = theme$gradient))
            plot
        }
    )
)
