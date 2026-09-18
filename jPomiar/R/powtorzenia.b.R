#' @importFrom jmvcore .
powtorzeniaClass <- R6::R6Class("powtorzeniaClass",
    inherit = powtorzeniaBase,
    private = list(
        .run = function() {
            o <- self$options
            if (is.null(o$dep) || !nzchar(o$dep)) return()
            md <- jmvcore::metodyNew()
            md$add("Dane", "Zmienna %s; braki pominięto, pomiary w kolejności wierszy arkusza.", jmvcore::metodyCyt(o$dep))
            md$add("Model", "Średnia, SD pojedynczego pomiaru (dzielnik n − 1) i niepewność standardowa średniej typu A: u_A = SD/√n, przy niezależnych powtórzeniach o jednakowej wariancji.")
            md$addIf(o$useReference, "Model", "Różnica = średnia − wartość odniesienia (%s); opisowa, bez niepewności odniesienia.", format(o$reference))
            md$addIf(o$showPlot, "Wykres", "Pomiary w kolejności wierszy; linia przerywana = średnia%s.",
                     if (o$useReference) ", kropkowana = wartość odniesienia" else "")
            md$render(self$results$metody)
            result <- tryCatch(measurementSeries(jmvcore::toNumeric(self$data[[self$options$dep]])), error = identity)
            if (inherits(result, "error")) stop(conditionMessage(result), call. = FALSE)
            self$results$summary$addRow(rowKey = 1, values = result[c("n", "missing", "mean", "sd", "se")])
            if (self$options$useReference) {
                measurementScalar(self$options$reference, "Wartość odniesienia")
                difference <- result$mean - self$options$reference
                measurementScalar(difference, "Różnica od odniesienia")
                self$results$comparison$addRow(rowKey = 1, values = list(reference = self$options$reference, difference = difference))
                self$results$comparison$setNote("reference", "Różnica opisowa: nie uwzględnia niepewności odniesienia i nie jest testem obciążenia.")
            }
            if (result$sd == 0) self$results$summary$setNote("constant", "Brak rozrzutu w serii nie oznacza zerowej całkowitej niepewności pomiaru.")
            if (self$options$showPlot) self$results$plot$setState(list(x = result$x, index = result$index, mean = result$mean,
                reference = if (self$options$useReference) self$options$reference else NULL))
        },
        .plot = function(image, ggtheme, theme, ...) {
            s <- image$state
            if (is.null(s)) return(FALSE)
            d <- data.frame(index = s$index, value = s$x)
            plot <- ggplot2::ggplot(d, ggplot2::aes(x = index, y = value))
            plot <- plot + ggplot2::geom_line(colour = "#8296a3") + ggplot2::geom_point(colour = "#32678c") +
                ggplot2::geom_hline(yintercept = s$mean, colour = "#32678c", linetype = "dashed") +
                ggplot2::labs(x = "Kolejność wierszy", y = "Wynik pomiaru", caption = "Linia przerywana: średnia") + ggtheme
            if (!is.null(s$reference)) plot <- plot +
                ggplot2::geom_hline(yintercept = s$reference, colour = "#ae6030", linetype = "dotted") +
                ggplot2::labs(caption = "Linia przerywana: średnia; kropkowana: odniesienie")
            print(plot)
            TRUE
        }
    )
)
