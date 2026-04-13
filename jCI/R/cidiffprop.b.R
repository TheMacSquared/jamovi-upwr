#' @importFrom jmvcore .
cidiffpropClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "cidiffpropClass",
    inherit = cidiffpropBase,
    private = list(
        .run = function() {
            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            dep <- self$options$dep
            groupVar <- self$options$group
            level <- self$options$level
            method <- self$options$method
            ciWidth <- self$options$ciWidth / 100

            outcome <- as.character(self$data[[dep]])
            group <- factor(self$data[[groupVar]])

            complete <- !is.na(outcome) & !is.na(group)
            outcome <- outcome[complete]
            group <- droplevels(group[complete])
            levs <- levels(group)

            if (length(levs) != 2) {
                self$results$table$setNote("err",
                    "Zmienna grupujaca musi miec dokladnie 2 poziomy.")
                return()
            }

            x1 <- sum(outcome[group == levs[1]] == as.character(level))
            n1 <- sum(group == levs[1])
            x2 <- sum(outcome[group == levs[2]] == as.character(level))
            n2 <- sum(group == levs[2])

            if (n1 < 1 || n2 < 1) {
                self$results$table$setNote("err",
                    "Kazda grupa musi miec co najmniej 1 obserwacje.")
                return()
            }

            p1 <- x1 / n1
            p2 <- x2 / n2
            est <- p1 - p2

            ci <- ciDiffProportion(x1, n1, x2, n2, ciWidth, method)

            self$results$table$setRow(rowNo = 1, values = list(
                var = dep, group1 = levs[1], group2 = levs[2],
                p1 = p1, p2 = p2, estimate = est,
                lower = ci$lower, upper = ci$upper
            ))

            methodLabel <- switch(method,
                wald = "Wald",
                newcombe = "Newcombe")
            self$results$table$setNote("ci",
                paste0(self$options$ciWidth, "% CI (metoda: ", methodLabel, ")"))

            self$results$plot$setState(list(
                label = paste0(levs[1], " - ", levs[2]),
                estimate = est, lower = ci$lower, upper = ci$upper
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
