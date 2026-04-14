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

            # Auto-pick first level if user hasn't selected one
            if (is.null(level) || length(level) == 0 || nchar(as.character(level)) == 0) {
                column <- self$data[[dep]]
                if (is.factor(column))
                    level <- levels(column)[1]
                else
                    level <- as.character(sort(unique(column[!is.na(column)]))[1])
                if (is.null(level) || is.na(level))
                    return()
            }
            level <- as.character(level)

            x <- as.character(self$data[[dep]])
            x <- x[!is.na(x)]
            n <- length(x)

            if (n < 1) {
                self$results$table$setNote("err", "Brak obserwacji.")
                return()
            }

            successes <- sum(x == level)
            phat <- successes / n

            ci <- ciProportion(successes, n, ciWidth, method)

            self$results$table$setRow(rowNo = 1, values = list(
                var = dep, level = level,
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
                estimate = phat, lower = ci$lower, upper = ci$upper,
                ciWidth = self$options$ciWidth
            ))
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state
            buildProportionIconPlot(s$label, s$estimate, s$lower, s$upper, s$ciWidth, ggtheme, theme)
        }
    )
)
