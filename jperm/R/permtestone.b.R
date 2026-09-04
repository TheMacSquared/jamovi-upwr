#' @importFrom jmvcore .
permtestoneClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "permtestoneClass",
    inherit = permtestoneBase,
    private = list(
        .init = function() {
            for (v in self$options$vars) {
                self$results$plots$addItem(key = v); self$results$plots$get(key = v)$setTitle(v)
            }
        },
        .run = function() {
            o <- self$options
            if (length(o$vars) == 0) return()
            mu <- o$testValue; tt <- self$results$table
            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienne: %s; wartość testowa μ₀ = %g; braki pomijane osobno dla każdej zmiennej.",
                  jmvcore::metodyCyt(o$vars), mu)
            metodyPerm(m, o, "one", "Różnica = średnia − wartość testowa")
            m$render(self$results$metody)
            for (v in o$vars) {
                x <- jmvcore::toNumeric(self$data[[v]]); x <- x[!is.na(x)]
                if (length(x) < 2) { tt$setNote(paste0("n", v), sprintf("%s: za mało obserwacji.", v)); next }
                observed <- mean(x) - mu
                permDist <- permDistOneSample(x, mu, o$nPerm, o$seed, o$exact)
                tt$addRow(rowKey = v, values = list(var = v, stat = observed,
                    p = permPValue(observed, permDist, o$hypothesis), nPerm = length(permDist)))
                exactNote(tt, v, permDist, o$exact)
                self$results$plots$get(key = v)$setState(list(permDist = permDist, observed = observed, hypothesis = o$hypothesis))
            }
            tt$setNote("h", sprintf("Różnica = średnia − %g.", mu))
        },

        .permPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) return(FALSE)
            s <- image$state
            buildPermPlot(s$permDist, s$observed, s$hypothesis, ggtheme, theme)
        }
    )
)
