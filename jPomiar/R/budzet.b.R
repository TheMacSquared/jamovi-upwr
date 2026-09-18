#' @importFrom jmvcore .
budzetClass <- R6::R6Class("budzetClass",
    inherit = budzetBase,
    private = list(
        .run = function() {
            self$results$info$setContent(paste0(
                "<p>Model: wynik = wartość zmierzona + znana poprawka. Wszystkie składniki podaj w jednostce wyniku. ",
                "Budżet zakłada niezależność składników i współczynniki wrażliwości równe 1. ",
                "Typ A/B oznacza sposób oceny niepewności, a nie podział na błędy losowe/systematyczne.</p>",
                "<p>Typ A: dla średniej niezależnych powtórzeń wpisz SD/√n. Wzorcowanie: u = U/k ze świadectwa ",
                "(ocena typu B w tym budżecie). Rozdzielczość: u = d/√12 dla zaokrąglenia do kroku d ",
                "i rozkładu prostokątnego na [−d/2, d/2]. Składnika rozdzielczości nie dzielimy przez √n: ",
                "jest tu składnikiem przypisanym do wyniku. Nie dodawaj go ponownie, jeśli został już ujęty w innym składniku.</p>",
                "<p>Poprawka koryguje znane przesunięcie; jej niepewność należy uwzględnić w budżecie. ",
                "Uwzględnienie niepewności wzorcowania samo w sobie nie koryguje wyniku.</p>",
                measurementCoverageText()))
            if (!self$options$includeA && !self$options$includeCalibration && !self$options$includeResolution) {
                self$results$info$setContent(paste0("<p>Włącz co najmniej jeden składnik budżetu i podaj jego wartość.</p>", self$results$info$content))
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
            if (r$summary$uc == 0) self$results$summary$setNote("zero", "Wprowadzone składniki są zerowe; nie dowodzi to braku niepewności. Udziały procentowe są nieokreślone.")
            if (self$options$showPlot) self$results$plot$setState(r$components)
        },
        .plot = function(image, ggtheme, theme, ...) measurementVariancePlot(image$state, ggtheme, theme)
    )
)
