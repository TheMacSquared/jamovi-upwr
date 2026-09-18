#' @importFrom jmvcore .
propagacjaClass <- R6::R6Class("propagacjaClass",
    inherit = propagacjaBase,
    private = list(
        .run = function() {
            r <- measurementPropagation(self$options$model, self$options$x, self$options$y,
                self$options$ux, self$options$uy,
                rho = if (self$options$useCorrelation) self$options$rho else 0,
                k = self$options$coverageK)
            self$results$summary$addRow(rowKey = 1, values = r$summary)
            measurementRows(self$results$inputs, r$inputs)
            measurementRows(self$results$components, r$components)
            self$results$info$setContent(paste0(
                "<p>Model: ", r$formula, ". Korelacja wejść ρ = ", format(r$rho), ". ",
                "u_c² = c_x²u(x)² + c_y²u(y)² + 2c_xc_yρu(x)u(y). ",
                "c_x i c_y są pochodnymi wzoru względem wejść. Składnik kowariancyjny może być ujemny: ",
                "nie jest samodzielną wariancją. Nie rozdzielamy go arbitralnie na udziały wejść.</p>",
                "<p>Podaj niepewności standardowe, nie rozszerzone (U ze świadectwa najpierw podziel przez jego k). ",
                "Korelacja dotyczy niepewności wejść; korelacja kolumn opisujących różne obiekty nie jest automatycznie właściwą wartością. ",
                "Dla sumy i różnicy użyj tej samej jednostki; dla iloczynu i ilorazu jednostka wyniku wynika ze wzoru.</p>",
                if (length(r$notes)) paste0("<p>", paste(r$notes, collapse = " "), "</p>") else "",
                if (r$summary$uc == 0) "<p>Zerowy wynik propagacji dotyczy wyłącznie podanego modelu i jego składników.</p>" else "",
                measurementCoverageText()))
            if (self$options$showPlot) self$results$plot$setState(r$components)
        },
        .plot = function(image, ggtheme, theme, ...) measurementVariancePlot(image$state, ggtheme, theme)
    )
)
