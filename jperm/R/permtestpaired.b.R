#' @importFrom jmvcore .
permtestpairedClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "permtestpairedClass",
    inherit = permtestpairedBase,
    private = list(
        .pairKeys = function() {
            keys <- character(0)
            for (p in self$options$pairs) if (!is.null(p$i1) && !is.null(p$i2)) keys <- c(keys, paste(p$i1, "−", p$i2))
            keys
        },
        .init = function() {
            for (k in private$.pairKeys()) {
                self$results$plots$addItem(key = k); self$results$plots$get(key = k)$setTitle(k)
            }
        },
        .run = function() {
            o <- self$options
            pairs <- Filter(function(p) !is.null(p$i1) && !is.null(p$i2), o$pairs)
            if (length(pairs) == 0) return()
            tt <- self$results$table
            m <- jmvcore::metodyNew()
            m$add("Dane", "Pary: %s; różnica = pierwsza − druga zmienna; tylko pary bez braków.",
                  paste(vapply(pairs, function(p) sprintf("„%s” − „%s”", jmvcore::htmlEscape(p$i1), jmvcore::htmlEscape(p$i2)), ""), collapse = ", "))
            metodyPerm(m, o, "paired", "Różnica = pierwsza − druga zmienna")
            m$render(self$results$metody)
            for (p in pairs) {
                k <- paste(p$i1, "−", p$i2)
                a <- jmvcore::toNumeric(self$data[[p$i1]]); b <- jmvcore::toNumeric(self$data[[p$i2]])
                ok <- !is.na(a) & !is.na(b); dif <- a[ok] - b[ok]
                if (length(dif) < 2) { tt$setNote(paste0("n", k), sprintf("%s: za mało par.", k)); next }
                observed <- mean(dif)
                permDist <- permDistPaired(dif, o$nPerm, o$seed, o$exact)
                tt$addRow(rowKey = k, values = list(var = k, stat = observed,
                    p = permPValue(observed, permDist, o$hypothesis), nPerm = length(permDist)))
                exactNote(tt, k, permDist, o$exact)
                self$results$plots$get(key = k)$setState(list(permDist = permDist, observed = observed, hypothesis = o$hypothesis))
            }
            tt$setNote("h", "Różnica = pierwsza − druga zmienna.")
        },

        .permPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) return(FALSE)
            s <- image$state
            buildPermPlot(s$permDist, s$observed, s$hypothesis, ggtheme, theme)
        }
    )
)
