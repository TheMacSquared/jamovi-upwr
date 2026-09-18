#' @importFrom jmvcore .
powtorzeniaClass <- R6::R6Class("powtorzeniaClass",
    inherit = powtorzeniaBase,
    private = list(
        .run = function() {
            self$results$info$setContent(paste0(
                "<p>Wybierz serię pomiarów tej samej, niezmiennej wielkości. ",
                "SD opisuje rozrzut pojedynczego pomiaru; u_A = SD/√n opisuje niepewność średniej. ",
                "Zakładamy niezależne powtórzenia o jednakowej wariancji. ",
                "Więcej powtórzeń nie usuwa wspólnego błędu przyrządu. ",
                "Wykres według kolejności wierszy pomaga zauważyć dryft, ale nie jest testem niezależności.</p>"))
            if (is.null(self$options$dep) || !nzchar(self$options$dep)) return()
            result <- tryCatch(measurementSeries(jmvcore::toNumeric(self$data[[self$options$dep]])), error = identity)
            if (inherits(result, "error")) stop(conditionMessage(result), call. = FALSE)
            self$results$summary$addRow(rowKey = 1, values = result[c("n", "missing", "mean", "sd", "se")])
            if (self$options$useReference) {
                measurementScalar(self$options$reference, "Wartość odniesienia")
                difference <- result$mean - self$options$reference
                measurementScalar(difference, "Różnica od odniesienia")
                self$results$comparison$addRow(rowKey = 1, values = list(reference = self$options$reference, difference = difference))
                self$results$comparison$setNote("reference", paste0(
                    "Różnica względem odniesienia jest opisowa. Nie uwzględnia niepewności odniesienia i nie jest testem obciążenia."))
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
