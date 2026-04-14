#' @importFrom jmvcore .
cionemeanClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "cionemeanClass",
    inherit = cionemeanBase,
    private = list(
        .run = function() {
            if (is.null(self$options$dep))
                return()

            dep <- self$options$dep
            ciWidth <- self$options$ciWidth / 100

            x <- jmvcore::toNumeric(self$data[[dep]])
            x <- x[!is.na(x)]
            n <- length(x)

            if (n < 2) {
                self$results$table$setNote("err", "Za malo obserwacji.")
                return()
            }

            est <- mean(x)
            se <- sd(x) / sqrt(n)
            tCrit <- qt(1 - (1 - ciWidth) / 2, df = n - 1)
            lower <- est - tCrit * se
            upper <- est + tCrit * se

            self$results$table$setRow(rowNo = 1, values = list(
                var = dep, estimate = est, se = se,
                lower = lower, upper = upper
            ))
            self$results$table$setNote("ci",
                paste0(self$options$ciWidth, "% CI (t-Studenta, df=", n - 1, ")"))

            self$results$plot$setState(list(
                label = dep, x = x,
                estimate = est, lower = lower, upper = upper,
                ciWidth = self$options$ciWidth
            ))
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state
            buildMeanCIPlot(s$label, s$x, s$estimate, s$lower, s$upper, s$ciWidth, ggtheme, theme)
        }
    )
)
