#' @importFrom jmvcore .
permtestpairedClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "permtestpairedClass",
    inherit = permtestpairedBase,
    private = list(
        .run = function() {
            if (is.null(self$options$var1) || is.null(self$options$var2))
                return()

            var1Name <- self$options$var1
            var2Name <- self$options$var2
            nPerm <- self$options$nPerm
            seed <- self$options$seed
            hypothesis <- self$options$hypothesis
            exact <- self$options$exact

            x1 <- jmvcore::toNumeric(self$data[[var1Name]])
            x2 <- jmvcore::toNumeric(self$data[[var2Name]])

            # Remove NAs pairwise
            complete <- !is.na(x1) & !is.na(x2)
            x1 <- x1[complete]
            x2 <- x2[complete]

            if (length(x1) < 2) {
                self$results$table$setNote(
                    "err",
                    "Za malo kompletnych par do przeprowadzenia testu."
                )
                return()
            }

            d <- x1 - x2
            observed <- mean(d)

            permDist <- permDistPaired(d, nPerm, seed, exact)
            pValue <- permPValue(observed, permDist, hypothesis)

            table <- self$results$table
            table$setRow(rowNo = 1, values = list(
                var1 = var1Name,
                var2 = var2Name,
                stat = observed,
                p = pValue,
                nPerm = length(permDist)
            ))

            hypNote <- switch(hypothesis,
                twoSided = paste0("H\u2090: \u03BC\u2081 \u2260 \u03BC\u2082"),
                greater  = paste0("H\u2090: \u03BC\u2081 > \u03BC\u2082"),
                less     = paste0("H\u2090: \u03BC\u2081 < \u03BC\u2082")
            )
            table$setNote("hyp", hypNote)

            if (exact)
                table$setNote("exact", "Test dokladny (wszystkie permutacje)")

            self$results$plot$setState(list(
                permDist = permDist,
                observed = observed,
                hypothesis = hypothesis
            ))
        },
        .permPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            state <- image$state
            p <- buildPermPlot(
                state$permDist,
                state$observed,
                state$hypothesis,
                ggtheme,
                theme
            )
            return(p)
        }
    )
)
