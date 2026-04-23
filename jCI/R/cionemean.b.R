#' @importFrom jmvcore .
cionemeanClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "cionemeanClass",
    inherit = cionemeanBase,
    private = list(
        .run = function() {
            if (is.null(self$options$dep))
                return()

            dep <- self$options$dep
            groupVar <- self$options$group
            ciWidth <- self$options$ciWidth / 100

            x <- jmvcore::toNumeric(self$data[[dep]])

            # If grouping variable is provided
            if (!is.null(groupVar) && groupVar != "") {
                grp <- as.factor(self$data[[groupVar]])

                # Remove NA values
                validIdx <- !is.na(x) & !is.na(grp)
                x <- x[validIdx]
                grp <- grp[validIdx]

                groups <- levels(grp)
                rowNo <- 1

                plotData <- list(
                    label = dep,
                    groups = list(),
                    ciWidth = self$options$ciWidth
                )

                for (g in groups) {
                    xg <- x[grp == g]
                    n <- length(xg)

                    if (n < 2) {
                        self$results$table$addRow(rowKey = g, values = list(
                            var = dep, grp = g, estimate = NA, se = NA,
                            lower = NA, upper = NA
                        ))
                        self$results$table$setNote("warn",
                            paste("Grupa", g, "ma < 2 obserwacji"))
                        rowNo <- rowNo + 1
                        next
                    }

                    est <- mean(xg)
                    se <- sd(xg) / sqrt(n)
                    tCrit <- qt(1 - (1 - ciWidth) / 2, df = n - 1)
                    lower <- est - tCrit * se
                    upper <- est + tCrit * se

                    self$results$table$addRow(rowKey = g, values = list(
                        var = dep, grp = g, estimate = est, se = se,
                        lower = lower, upper = upper
                    ))

                    plotData$groups[[g]] <- list(
                        x = xg, estimate = est, lower = lower, upper = upper, n = n
                    )
                    rowNo <- rowNo + 1
                }

                self$results$table$setNote("ci",
                    paste0(self$options$ciWidth, "% CI (t-Studenta)"))
                self$results$plot$setState(plotData)

            } else {
                # No grouping - original single group behavior
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

                self$results$table$addRow(rowKey = dep, values = list(
                    var = dep, grp = "", estimate = est, se = se,
                    lower = lower, upper = upper
                ))
                self$results$table$setNote("ci",
                    paste0(self$options$ciWidth, "% CI (t-Studenta, df=", n - 1, ")"))

                self$results$plot$setState(list(
                    label = dep, x = x,
                    estimate = est, lower = lower, upper = upper,
                    ciWidth = self$options$ciWidth,
                    groups = NULL
                ))
            }
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state

            # If grouped data
            if (!is.null(s$groups) && length(s$groups) > 0) {
                buildGroupedMeanCIPlot(s$label, s$groups, s$ciWidth, ggtheme, theme)
            } else {
                buildMeanCIPlot(s$label, s$x, s$estimate, s$lower, s$upper, s$ciWidth, ggtheme, theme)
            }
        }
    )
)
