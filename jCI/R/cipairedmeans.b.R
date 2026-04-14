#' @importFrom jmvcore .
cipairedmeansClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "cipairedmeansClass",
    inherit = cipairedmeansBase,
    private = list(
        .run = function() {
            if (is.null(self$options$var1) || is.null(self$options$var2))
                return()

            var1Name <- self$options$var1
            var2Name <- self$options$var2
            ciWidth <- self$options$ciWidth / 100

            x1 <- jmvcore::toNumeric(self$data[[var1Name]])
            x2 <- jmvcore::toNumeric(self$data[[var2Name]])

            complete <- !is.na(x1) & !is.na(x2)
            x1 <- x1[complete]
            x2 <- x2[complete]
            n <- length(x1)

            if (n < 2) {
                self$results$table$setNote("err",
                    "Za malo kompletnych par.")
                return()
            }

            d <- x1 - x2
            est <- mean(d)
            se <- sd(d) / sqrt(n)
            tCrit <- qt(1 - (1 - ciWidth) / 2, df = n - 1)
            lower <- est - tCrit * se
            upper <- est + tCrit * se

            self$results$table$setRow(rowNo = 1, values = list(
                var1 = var1Name, var2 = var2Name,
                estimate = est, se = se, lower = lower, upper = upper
            ))
            self$results$table$setNote("ci",
                paste0(self$options$ciWidth, "% CI (t-Studenta, df=", n - 1, ")"))

            self$results$plot$setState(list(
                label = paste0(var1Name, " - ", var2Name),
                x = d,
                estimate = est, lower = lower, upper = upper,
                ciWidth = self$options$ciWidth,
                refLine = 0
            ))
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state
            buildMeanCIPlot(s$label, s$x, s$estimate, s$lower, s$upper, s$ciWidth,
                ggtheme, theme, refLine = s$refLine)
        }
    )
)
