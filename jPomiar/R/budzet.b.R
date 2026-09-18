#' @importFrom jmvcore .
budzetClass <- R6::R6Class("budzetClass",
    inherit = budzetBase,
    private = list(
        .run = function() {
            o <- self$options
            md <- jmvcore::metodyNew()
            md$add("Model", "Wynik = wartość zmierzona%s; wszystkie składniki w jednostce wyniku.",
                   if (o$useCorrection) sprintf(" + znana poprawka (%s)", format(o$correction)) else " (bez poprawki)")
            md$add("Model", "Składniki niezależne, współczynniki wrażliwości równe 1: u_c = √Σu²; udział składnika = u² / u_c².")
            md$addIf(o$includeA, "Model", "Typ A: wprowadzona niepewność standardowa (dla średniej powtórzeń SD/√n).")
            md$addIf(o$includeCalibration, "Model", "Wzorcowanie (ocena typu B): u = U/k ze świadectwa, k = %s.", format(o$calibrationK))
            md$addIf(o$includeResolution, "Model", "Rozdzielczość (ocena typu B): u = d/√12, rozkład prostokątny na [−d/2, d/2], bez dzielenia przez √n.")
            md$add("Model", "Niepewność rozszerzona U = k · u_c, k = %s (mnożnik, nie poziom ufności); granice = wynik ± U.", format(o$coverageK))
            md$addIf(o$showPlot, "Wykres", "Słupki = wkłady składników do wariancji wyniku (u²).")
            md$render(self$results$metody)
            if (!o$includeA && !o$includeCalibration && !o$includeResolution) {
                self$results$summary$setNote("empty", "Włącz co najmniej jeden składnik budżetu i podaj jego wartość.")
                return()
            }
            r <- measurementBudget(self$options$value,
                correction = if (self$options$useCorrection) self$options$correction else 0,
                uA = if (self$options$includeA) self$options$uA else NULL,
                calibrationU = if (self$options$includeCalibration) self$options$calibrationU else NULL,
                calibrationK = self$options$calibrationK,
                resolution = if (self$options$includeResolution) self$options$resolution else NULL,
                k = self$options$coverageK)
            self$results$summary$addRow(rowKey = 1, values = r$summary)
            measurementRows(self$results$components, r$components)
            if (r$summary$uc == 0) self$results$summary$setNote("zero", "Wprowadzone składniki są zerowe: udziały procentowe są nieokreślone.")
            if (self$options$showPlot) self$results$plot$setState(r$components)
        },
        .plot = function(image, ggtheme, theme, ...) measurementVariancePlot(image$state, ggtheme, theme)
    )
)
