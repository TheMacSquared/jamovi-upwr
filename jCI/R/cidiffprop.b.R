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

            outcome <- as.character(self$data[[dep]])
            group <- factor(self$data[[groupVar]])

            complete <- !is.na(outcome) & !is.na(group)
            outcome <- outcome[complete]
            group <- droplevels(group[complete])
            allLevs <- levels(group)

            # Level selection for groups (same logic as citwomeans)
            gLev1 <- self$options$groupLevel1
            gLev2 <- self$options$groupLevel2

            haveG1 <- !is.null(gLev1) && length(gLev1) > 0 &&
                nchar(as.character(gLev1)) > 0
            haveG2 <- !is.null(gLev2) && length(gLev2) > 0 &&
                nchar(as.character(gLev2)) > 0

            if (haveG1 && haveG2) {
                lev1 <- as.character(gLev1)
                lev2 <- as.character(gLev2)
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
                    paste0("Wiecej niz 2 grupy. Uzyto pierwszych dwoch: ",
                        lev1, " vs ", lev2, ". Wybierz r\u0119cznie w opcjach."))
            }

            x1 <- sum(outcome[group == lev1] == level)
            n1 <- sum(group == lev1)
            x2 <- sum(outcome[group == lev2] == level)
            n2 <- sum(group == lev2)

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
                var = dep, group1 = lev1, group2 = lev2,
                p1 = p1, p2 = p2, estimate = est,
                lower = ci$lower, upper = ci$upper
            ))

            methodLabel <- switch(method,
                wald = "Wald",
                newcombe = "Newcombe")
            self$results$table$setNote("ci",
                paste0(self$options$ciWidth, "% CI (metoda: ", methodLabel, ")"))

            self$results$plot$setState(list(
                group1 = lev1,
                group2 = lev2,
                p1 = p1, p2 = p2, n1 = n1, n2 = n2,
                estimate = est, lower = ci$lower, upper = ci$upper,
                ciWidth = self$options$ciWidth
            ))
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state
            buildDiffPropPlot(s$group1, s$group2, s$p1, s$p2, s$n1, s$n2,
                s$estimate, s$lower, s$upper, s$ciWidth, ggtheme, theme)
        }
    )
)
