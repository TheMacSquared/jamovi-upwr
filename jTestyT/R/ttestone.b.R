#' @importFrom jmvcore .
ttestoneClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "ttestoneClass",
    inherit = ttestoneBase,
    private = list(
        .init = function() {
            for (v in self$options$vars) {
                self$results$plots$addItem(key = v); self$results$qq$addItem(key = v)
                self$results$plots$get(key = v)$setTitle(v); self$results$qq$get(key = v)$setTitle(v)
            }
        },
        .run = function() {
            o <- self$options
            if (length(o$vars) == 0) return()
            mu <- o$testValue
            tt <- self$results$ttest
            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienne: %s; wartość testowa μ₀ = %g; braki pomijane osobno dla każdej zmiennej.",
                  jmvcore::metodyCyt(o$vars), mu)
            m$addIf(o$student, "Testy", "t Studenta dla jednej próby, df = n − 1.")
            metodyWspolne(m, o, "one", "Różnica = średnia − wartość testowa")
            m$render(self$results$metody)
            for (v in o$vars) {
                x <- jmvcore::toNumeric(self$data[[v]]); x <- x[!is.na(x)]
                if (length(x) < 2) { tt$setNote(paste0("n", v), sprintf("%s: za mało obserwacji.", v)); next }
                if (isTRUE(o$student)) addTestRow(tt, paste(v, "t"), v, oneSampleT(x, mu, o$hypothesis))
                if (isTRUE(o$nonpar)) addTestRow(tt, paste(v, "w"), v, wilcoxOne(x, mu, o$hypothesis))
                if (isTRUE(o$desc)) self$results$desc$addRow(rowKey = v, values = c(list(var = v, group = ""), descRow(x)))
                if (isTRUE(o$norm)) self$results$norm$addRow(rowKey = v, values = c(list(var = v, group = ""), shapiroRow(x)))
                self$results$plots$get(key = v)$setState(list(groups = stats::setNames(list(x), v), refLine = mu,
                    refLabel = sprintf("wartość testowa %g", mu), ylab = v))
                self$results$qq$get(key = v)$setState(list(x = x, label = v))
            }
            tt$setNote("h", sprintf("Różnica = średnia − %g.", mu))
        },
        .plot = function(image, ggtheme, theme, ...) boxPlotTests(image, ggtheme, theme),
        .qq = function(image, ggtheme, theme, ...) qqPlotResid(image, ggtheme, theme)
    )
)
