#' @importFrom jmvcore .
elipsaClass <- R6::R6Class("elipsaClass",
    inherit = elipsaBase,
    private = list(
        .run = function() {
            self$results$info$setContent("<p>Wybierz dwie współrzędne tego samego punktu, mierzone parami, w tej samej jednostce. Każdy wiersz stanowi jeden pomiar 2D. Wiersze z brakiem którejkolwiek współrzędnej pomijamy w całości.</p>")
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
            explanation <- switch(self$options$ellipseType,
                standard = "Elipsa standardowa ma półosie √λ₁ i √λ₂. To elipsa rozrzutu 1 SD, nie obszar ufności średniej. Dla nieosobliwego dwuwymiarowego rozkładu normalnego i znanych parametrów obejmuje około 39,35% prawdopodobieństwa, nie 68% ani 95%.",
                scatter = "Półosie wynoszą √(χ²₂(p) · λ). Parametry rozkładu zastępujemy oszacowaniami z próby: nominalny poziom jest przybliżonym pokryciem rozkładu normalnego, nie gwarancją odsetka punktów w próbie. To nie jest dokładny obszar predykcji nowego pomiaru ani obszar ufności średniej.",
                mean = "Półosie wynoszą √[2(n−1)/(n(n−2)) · F₂,n−₂(p) · λ]. Jest to wspólny obszar ufności dla dwóch składowych średniej (T² Hotellinga), nie obszar rozrzutu pomiarów. Wymaga niezależnych obserwacji z dwuwymiarowego rozkładu normalnego i nieosobliwej kowariancji.")
            self$results$info$setContent(paste0(self$results$info$content,
                "<p>S opisuje rozrzut pojedynczych pomiarów; S/n jest oszacowaniem kowariancji średniej przy niezależnych powtórzeniach o wspólnej kowariancji. Jednostką elementów macierzy jest kwadrat jednostki współrzędnych. Wspólne błędy systematyczne nie są tu uwzględnione.</p>",
                "<p>", explanation, "</p>",
                "<p>Osie PC1 i PC2 pochodzą z macierzy kowariancji bez standaryzacji. Wartości własne λ to wariancje w ich kierunkach. Kąty liczymy przeciwnie do ruchu wskazówek zegara od dodatniej osi X, modulo 180°. Wykres zachowuje jednakową skalę obu osi. Długość i szerokość geograficzną należy najpierw przeliczyć na lokalne współrzędne płaskie.</p>",
                if (length(r$notes)) paste0("<p>", paste(r$notes, collapse = " "), "</p>") else ""))
            if (self$options$showPlot) self$results$plot$setState(list(result = r,
                xLabel = self$options$x, yLabel = self$options$y, showAxes = self$options$showAxes))
        },
        .plot = function(image, ggtheme, theme, ...) measurementEllipsePlot(image$state, ggtheme, theme)
    )
)
