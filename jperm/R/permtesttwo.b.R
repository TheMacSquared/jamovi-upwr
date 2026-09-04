#' @importFrom jmvcore .
permtesttwoClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "permtesttwoClass",
    inherit = permtesttwoBase,
    private = list(
        .init = function() {
            for (v in self$options$vars) {
                self$results$plots$addItem(key = v); self$results$plots$get(key = v)$setTitle(v)
            }
        },
        .run = function() {
            o <- self$options
            if (length(o$vars) == 0 || !optNonEmpty(o$group)) return()
            tt <- self$results$table
            gAll <- factor(self$data[[o$group]])
            lvAll <- levels(droplevels(gAll[!is.na(gAll)]))
            if (length(lvAll) != 2) {
                tt$setNote("g", "Zmienna grupująca musi mieć dokładnie 2 poziomy (odfiltruj pozostałe)."); return()
            }
            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienne: %s; zmienna grupująca „%s” z poziomami „%s” i „%s”; braki pomijane osobno dla każdej zmiennej.",
                  jmvcore::metodyCyt(o$vars), o$group, lvAll[1], lvAll[2])
            metodyPerm(m, o, "two", sprintf("Różnica = „%s” − „%s”", lvAll[1], lvAll[2]))
            m$render(self$results$metody)
            for (v in o$vars) {
                x <- jmvcore::toNumeric(self$data[[v]]); ok <- !is.na(x) & !is.na(gAll)
                x <- x[ok]; g <- droplevels(gAll[ok]); lv <- levels(g)
                if (nlevels(g) != 2 || any(table(g) < 1)) { tt$setNote(paste0("n", v), sprintf("%s: za mało obserwacji w grupie.", v)); next }
                observed <- mean(x[g == lv[1]]) - mean(x[g == lv[2]])
                permDist <- permDistTwoSample(x, g, o$nPerm, o$seed, o$exact)
                tt$addRow(rowKey = v, values = list(var = v, group1 = lv[1], group2 = lv[2], stat = observed,
                    p = permPValue(observed, permDist, o$hypothesis), nPerm = length(permDist)))
                exactNote(tt, v, permDist, o$exact)
                self$results$plots$get(key = v)$setState(list(permDist = permDist, observed = observed, hypothesis = o$hypothesis))
            }
            tt$setNote("h", sprintf("Różnica = %s − %s.", lvAll[1], lvAll[2]))
        },

        .permPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) return(FALSE)
            s <- image$state
            buildPermPlot(s$permDist, s$observed, s$hypothesis, ggtheme, theme)
        }
    )
)
