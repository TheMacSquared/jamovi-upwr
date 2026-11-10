#' @importFrom jmvcore .
permtestpairedClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "permtestpairedClass",
    inherit = permtestpairedBase,
    private = list(
        # complete pairs of two different variables, each pair once: a variable
        # paired with itself or a repeated pair would give duplicate keys
        .validPairs = function() {
            pairs <- Filter(function(p) !is.null(p$i1) && !is.null(p$i2) && p$i1 != p$i2, self$options$pairs)
            pairs[!duplicated(vapply(pairs, function(p) paste(p$i1, "−", p$i2), ""))]
        },
        .pairKeys = function() vapply(private$.validPairs(), function(p) paste(p$i1, "−", p$i2), ""),
        .init = function() {
            for (k in private$.pairKeys()) {
                self$results$plots$addItem(key = k)
            }
        },
        .run = function() {
            o <- self$options
            tt <- self$results$table
            same <- unique(unlist(lapply(o$pairs, function(p) if (!is.null(p$i1) && !is.null(p$i2) && p$i1 == p$i2) p$i1)))
            if (length(same) > 0)
                tt$setNote("same", sprintf("Pominięto parę zmiennej z nią samą: %s.", paste0("„", same, "”", collapse = ", ")))
            pairs <- private$.validPairs()
            if (length(pairs) == 0) return()
            m <- jmvcore::metodyNew()
            m$add("Dane", "Pary: %s; różnica = pierwsza − druga zmienna; tylko pary bez braków.",
                  paste(vapply(pairs, function(p) sprintf("„%s” − „%s”", jmvcore::htmlEscape(p$i1), jmvcore::htmlEscape(p$i2)), ""), collapse = ", "))
            metodyPerm(m, o, "paired", "Różnica = pierwsza − druga zmienna")
            m$render(self$results$metody)
            for (p in pairs) {
                k <- paste(p$i1, "−", p$i2)
                a <- jmvcore::toNumeric(self$data[[p$i1]]); b <- jmvcore::toNumeric(self$data[[p$i2]])
                ok <- !is.na(a) & !is.na(b); dif <- a[ok] - b[ok]
                if (length(dif) < 2) { tt$setNote(paste0("n", k), sprintf("«%s»: liczba kompletnych par: %d; bieżąca implementacja wymaga co najmniej 2.", k, length(dif))); next }
                observed <- mean(dif)
                permDist <- permDistPaired(dif, o$nPerm, o$seed, o$exact)
                tt$addRow(rowKey = k, values = list(var = k, stat = observed,
                    p = permPValue(observed, permDist, o$hypothesis), nPerm = length(permDist)))
                exactNote(tt, k, permDist, o$exact)
                # the whole permutation distribution (up to 2^20 values) only when the plot is on
                if (isTRUE(o$plot)) self$results$plots$get(key = k)$setState(list(permDist = permDist, observed = observed, hypothesis = o$hypothesis, label = k))
            }
            tt$setNote("h", "Różnica = pierwsza − druga zmienna.")
        },

        .permPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) return(FALSE)
            s <- image$state
            buildPermPlot(s$permDist, s$observed, s$hypothesis, ggtheme, theme, s$label)
        }
    )
)
