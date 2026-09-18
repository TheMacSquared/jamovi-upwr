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
            measurementNotes(self$results$summary, r$notes)
            o <- self$options
            md <- jmvcore::metodyNew()
            md$add("Model", "%s; c_x i c_y to pochodne wzoru względem wejść w punkcie (x, y); wejścia jako niepewności standardowe.", r$formula)
            md$add("Model", "u_c² = c_x²u(x)² + c_y²u(y)² + 2c_xc_yρu(x)u(y); %s.",
                   if (o$useCorrelation) sprintf("korelacja niepewności wejść ρ = %s", format(r$rho)) else "korelacja wyłączona (ρ = 0)")
            md$addIf(o$model %in% c("product", "ratio"), "Model", "Iloczyn i iloraz: linearyzacja pierwszego rzędu; suma i różnica: zależność dokładna.")
            md$add("Model", "Składnik kowariancyjny podano osobno (może być ujemny), bez rozdzielania na udziały wejść.")
            md$add("Model", "Niepewność rozszerzona U = k · u_c, k = %s (mnożnik, nie poziom ufności); granice = wynik ± U.", format(o$coverageK))
            md$addIf(o$showPlot, "Wykres", "Słupki = trzy składniki wariancji wyniku: wejście x, wejście y i kowariancja.")
            md$render(self$results$metody)
            if (self$options$showPlot) self$results$plot$setState(r$components)
        },
        .plot = function(image, ggtheme, theme, ...) measurementVariancePlot(image$state, ggtheme, theme)
    )
)
