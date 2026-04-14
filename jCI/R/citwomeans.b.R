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
            allLevs <- levels(group)

            # Level selection logic:
            # - If user selected level1 and level2, use them
            # - Otherwise, if exactly 2 levels, use them automatically
            # - Otherwise, fall back to first 2 levels
            level1 <- self$options$level1
            level2 <- self$options$level2

            haveL1 <- !is.null(level1) && length(level1) > 0 &&
                nchar(as.character(level1)) > 0
            haveL2 <- !is.null(level2) && length(level2) > 0 &&
                nchar(as.character(level2)) > 0

            if (haveL1 && haveL2) {
                lev1 <- as.character(level1)
                lev2 <- as.character(level2)
                if (lev1 == lev2) {
                    self$results$table$setNote("err",
                        "Grupa 1 i Grupa 2 musza byc rozne.")
                    return()
                }
                if (!(lev1 %in% allLevs) || !(lev2 %in% allLevs)) {
                    self$results$table$setNote("err",
                        "Wybrane grupy nie istnieja w zmiennej.")
                    return()
                }
            } else if (length(allLevs) == 2) {
                lev1 <- allLevs[1]
                lev2 <- allLevs[2]
            } else if (length(allLevs) < 2) {
                self$results$table$setNote("err",
                    "Zmienna grupujaca musi miec co najmniej 2 poziomy.")
                return()
            } else {
                lev1 <- allLevs[1]
                lev2 <- allLevs[2]
                self$results$table$setNote("info",
                    paste0("Wiecej niz 2 grupy w zmiennej. Uzyto pierwszych dwoch: ",
                        lev1, " vs ", lev2, ". Wybierz r\u0119cznie w opcjach."))
            }

            # Filter to just the two selected levels
            keep <- group %in% c(lev1, lev2)
            x <- x[keep]
            group <- droplevels(group[keep])

            x1 <- x[group == lev1]
            x2 <- x[group == lev2]
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
                var = dep, group1 = lev1, group2 = lev2,
                estimate = est, se = se, lower = lower, upper = upper
            ))
            self$results$table$setNote("ci",
                paste0(self$options$ciWidth, "% CI (Welch, df=",
                    formatC(df, format = "f", digits = 1), ")"))

            self$results$plot$setState(list(
                x1 = x1, x2 = x2,
                group1 = lev1,
                group2 = lev2,
                estimate = est, lower = lower, upper = upper,
                ciWidth = self$options$ciWidth
            ))
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state
            buildTwoMeansCIPlot(s$x1, s$x2, s$group1, s$group2,
                s$estimate, s$lower, s$upper, s$ciWidth, ggtheme, theme)
        }
    )
)
