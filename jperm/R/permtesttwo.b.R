#' @importFrom jmvcore .
permtesttwoClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "permtesttwoClass",
    inherit = permtesttwoBase,
    private = list(
        .run = function() {
            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            dep <- self$options$dep
            groupVar <- self$options$group
            nPerm <- self$options$nPerm
            seed <- self$options$seed
            hypothesis <- self$options$hypothesis
            exact <- self$options$exact

            x <- jmvcore::toNumeric(self$data[[dep]])
            group <- factor(self$data[[groupVar]])

            # Remove NAs pairwise
            complete <- !is.na(x) & !is.na(group)
            x <- x[complete]
            group <- droplevels(group[complete])

            levs <- levels(group)
            if (length(levs) != 2) {
                self$results$table$setNote(
                    "err",
                    "Zmienna grupujaca musi miec dokladnie 2 poziomy."
                )
                return()
            }

            n1 <- sum(group == levs[1])
            n2 <- sum(group == levs[2])
            if (n1 < 1 || n2 < 1) {
                self$results$table$setNote(
                    "err",
                    "Kazda grupa musi miec co najmniej 1 obserwacje."
                )
                return()
            }

            observed <- mean(x[group == levs[1]]) - mean(x[group == levs[2]])

            permDist <- permDistTwoSample(x, group, nPerm, seed, exact)
            pValue <- permPValue(observed, permDist, hypothesis)

            table <- self$results$table
            table$setRow(rowNo = 1, values = list(
                var = dep,
                group1 = levs[1],
                group2 = levs[2],
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
