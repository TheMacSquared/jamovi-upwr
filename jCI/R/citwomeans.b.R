#' @importFrom jmvcore .
citwomeansClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "citwomeansClass",
    inherit = citwomeansBase,
    private = list(
        .run = function() {
            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            dep <- self$options$dep
            groupVar <- self$options$group
            ciWidth <- self$options$ciWidth / 100

            x <- jmvcore::toNumeric(self$data[[dep]])
            group <- factor(self$data[[groupVar]])

            complete <- !is.na(x) & !is.na(group)
            x <- x[complete]
            group <- droplevels(group[complete])
            levs <- levels(group)

            if (length(levs) != 2) {
                self$results$table$setNote("err",
                    "Zmienna grupujaca musi miec dokladnie 2 poziomy.")
                return()
            }

            x1 <- x[group == levs[1]]
            x2 <- x[group == levs[2]]
            n1 <- length(x1)
            n2 <- length(x2)

            if (n1 < 2 || n2 < 2) {
                self$results$table$setNote("err",
                    "Kazda grupa musi miec co najmniej 2 obserwacje.")
                return()
            }

            est <- mean(x1) - mean(x2)
            var1 <- var(x1)
            var2 <- var(x2)
            se <- sqrt(var1 / n1 + var2 / n2)

            # Welch-Satterthwaite degrees of freedom
            df <- (var1 / n1 + var2 / n2)^2 /
                ((var1 / n1)^2 / (n1 - 1) + (var2 / n2)^2 / (n2 - 1))

            tCrit <- qt(1 - (1 - ciWidth) / 2, df = df)
            lower <- est - tCrit * se
            upper <- est + tCrit * se

            self$results$table$setRow(rowNo = 1, values = list(
                var = dep, group1 = levs[1], group2 = levs[2],
                estimate = est, se = se, lower = lower, upper = upper
            ))
            self$results$table$setNote("ci",
                paste0(self$options$ciWidth, "% CI (Welch, df=",
                    formatC(df, format = "f", digits = 1), ")"))

            self$results$plot$setState(list(
                label = paste0(levs[1], " - ", levs[2]),
                estimate = est, lower = lower, upper = upper
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
