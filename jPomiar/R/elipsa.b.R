#' @importFrom jmvcore .
elipsaClass <- R6::R6Class("elipsaClass",
    inherit = elipsaBase,
    private = list(
        .run = function() {
            if (is.null(self$options$x) || !nzchar(self$options$x) ||
                is.null(self$options$y) || !nzchar(self$options$y)) return()
            if (self$options$x == self$options$y) stop("Wybierz dwie różne kolumny współrzędnych.", call. = FALSE)
            r <- measurementEllipse(jmvcore::toNumeric(self$data[[self$options$x]]),
                jmvcore::toNumeric(self$data[[self$options$y]]), self$options$ellipseType, self$options$level)
            self$results$summary$addRow(rowKey = 1, values = list(n = r$n, missing = r$missing,
                meanX = r$center[1], meanY = r$center[2], r = r$r))
            for (i in 1:2) {
                self$results$covariance$addRow(rowKey = i, values = list(coordinate = c("X", "Y")[i], x = r$covariance[i, 1], y = r$covariance[i, 2]))
                self$results$meanCovariance$addRow(rowKey = i, values = list(coordinate = c("X", "Y")[i], x = r$meanCovariance[i, 1], y = r$meanCovariance[i, 2]))
            }
            measurementRows(self$results$principal, r$principal)
            if (r$available) self$results$ellipse$addRow(rowKey = 1, values = list(kind = r$label,
                major = r$radii[1], minor = r$radii[2], angle = r$angle))
            measurementNotes(self$results$summary, r$notes[names(r$notes) %in% "correlation"])
            measurementNotes(self$results$principal, r$notes[names(r$notes) %in% c("singular", "isotropic")])
            measurementNotes(self$results$ellipse, r$notes[names(r$notes) %in% "mean"])
            o <- self$options
            md <- jmvcore::metodyNew()
            md$add("Dane", "Współrzędne: X = %s, Y = %s; każdy wiersz to jeden pomiar 2D, wiersze z brakiem którejkolwiek współrzędnej pominięto w całości.",
                   jmvcore::metodyCyt(o$x), jmvcore::metodyCyt(o$y))
            md$add("Model", "S = macierz kowariancji pomiarów (dzielnik n − 1); S/n = oszacowana kowariancja średniej; jednostka elementów = kwadrat jednostki współrzędnych.")
            md$add("Model", "Osie główne: wektory i wartości własne λ macierzy S bez standaryzacji; kąty od dodatniej osi X przeciwnie do ruchu wskazówek zegara, modulo 180°.")
            if (o$ellipseType == "standard")
                md$add("Model", "Elipsa standardowa: półosie √λ₁ i √λ₂ (rozrzut 1 SD).")
            else
                md$add("Model", switch(o$ellipseType,
                    scatter = "Elipsa rozrzutu %s%%: półosie √(χ²₂(p) · λ), parametry rozkładu normalnego oszacowane z próby.",
                    mean = "Obszar ufności średniego położenia %s%% (T² Hotellinga): półosie √[2(n−1)/(n(n−2)) · F₂,ₙ₋₂(p) · λ]; wymaga n &gt; 2 i nieosobliwej S."),
                    format(o$level))
            md$addIf(o$showPlot, "Wykres", "Punkty = pomiary, krzyżyk = średnie położenie, linia ciągła = elipsa%s; jednakowa skala obu osi.",
                     if (o$showAxes) ", linie przerywane = osie główne" else "")
            md$render(self$results$metody)
            if (self$options$showPlot) self$results$plot$setState(list(result = r,
                xLabel = self$options$x, yLabel = self$options$y, showAxes = self$options$showAxes))
        },
        .plot = function(image, ggtheme, theme, ...) measurementEllipsePlot(image$state, ggtheme, theme)
    )
)
