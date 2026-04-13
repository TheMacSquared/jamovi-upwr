#' @importFrom jmvcore .
ciproportionClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "ciproportionClass",
    inherit = ciproportionBase,
    private = list(
        .run = function() {
            if (is.null(self$options$dep))
                return()

            dep <- self$options$dep
            method <- self$options$method
            ciWidth <- self$options$ciWidth / 100
            level <- self$options$level

            x <- as.character(self$data[[dep]])
            x <- x[!is.na(x)]
            n <- length(x)

            if (n < 1) {
                self$results$table$setNote("err", "Brak obserwacji.")
                return()
            }

            successes <- sum(x == as.character(level))
            phat <- successes / n

            ci <- ciProportion(successes, n, ciWidth, method)

            self$results$table$setRow(rowNo = 1, values = list(
                var = dep, level = as.character(level),
                count = successes, total = n,
                estimate = phat,
                lower = ci$lower, upper = ci$upper
            ))

            methodLabel <- switch(method,
                wald = "Wald",
                wilson = "Wilson",
                clopperPearson = "Clopper-Pearson")
            self$results$table$setNote("ci",
                paste0(self$options$ciWidth, "% CI (metoda: ", methodLabel, ")"))

            self$results$plot$setState(list(
                label = paste0(dep, " = ", level),
                estimate = phat, lower = ci$lower, upper = ci$upper
            ))
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state
            buildCIPlot(s$label, s$estimate, s$lower, s$upper, ggtheme, theme)
        }
    )
)
