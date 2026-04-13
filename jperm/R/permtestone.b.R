#' @importFrom jmvcore .
permtestoneClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "permtestoneClass",
    inherit = permtestoneBase,
    private = list(
        .run = function() {
            if (is.null(self$options$dep))
                return()

            dep <- self$options$dep
            mu <- self$options$mu
            nPerm <- self$options$nPerm
            seed <- self$options$seed
            hypothesis <- self$options$hypothesis
            exact <- self$options$exact

            x <- jmvcore::toNumeric(self$data[[dep]])
            x <- x[!is.na(x)]

            if (length(x) < 2) {
                self$results$table$setNote(
                    "err",
                    "Za malo obserwacji do przeprowadzenia testu."
                )
                return()
            }

            observed <- mean(x) - mu

            permDist <- permDistOneSample(x, mu, nPerm, seed, exact)
            pValue <- permPValue(observed, permDist, hypothesis)

            table <- self$results$table
            table$setRow(rowNo = 1, values = list(
                var = dep,
                stat = observed,
                p = pValue,
                nPerm = length(permDist)
            ))

            hypNote <- switch(hypothesis,
                twoSided = paste0("H\u2090: \u03BC \u2260 ", mu),
                greater  = paste0("H\u2090: \u03BC > ", mu),
                less     = paste0("H\u2090: \u03BC < ", mu)
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
