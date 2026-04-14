#' @importFrom jmvcore .
cicorrelationClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "cicorrelationClass",
    inherit = cicorrelationBase,
    private = list(
        .run = function() {
            if (is.null(self$options$var1) || is.null(self$options$var2))
                return()

            var1Name <- self$options$var1
            var2Name <- self$options$var2
            method <- self$options$method
            ciWidth <- self$options$ciWidth / 100

            x1 <- jmvcore::toNumeric(self$data[[var1Name]])
            x2 <- jmvcore::toNumeric(self$data[[var2Name]])

            complete <- !is.na(x1) & !is.na(x2)
            x1 <- x1[complete]
            x2 <- x2[complete]
            n <- length(x1)

            if (n < 4) {
                self$results$table$setNote("err",
                    "Potrzeba co najmniej 4 kompletne obserwacje.")
                return()
            }

            r <- cor(x1, x2, method = method)

            # Fisher Z transformation for CI
            z <- atanh(r)
            se_z <- 1 / sqrt(n - 3)
            zCrit <- qnorm(1 - (1 - ciWidth) / 2)
            lower <- tanh(z - zCrit * se_z)
            upper <- tanh(z + zCrit * se_z)

            self$results$table$setRow(rowNo = 1, values = list(
                var1 = var1Name, var2 = var2Name,
                estimate = r, lower = lower, upper = upper
            ))

            methodLabel <- ifelse(method == "pearson", "Pearson", "Spearman")
            self$results$table$setNote("ci",
                paste0(self$options$ciWidth, "% CI (", methodLabel,
                    ", transformacja Fishera Z)"))

            self$results$plot$setState(list(
                var1Name = var1Name, var2Name = var2Name,
                x1 = x1, x2 = x2,
                estimate = r, lower = lower, upper = upper,
                ciWidth = self$options$ciWidth,
                method = method
            ))
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state
            buildCorrelationPlot(s$x1, s$x2, s$var1Name, s$var2Name,
                s$estimate, s$lower, s$upper, s$ciWidth, s$method, ggtheme, theme)
        }
    )
)
