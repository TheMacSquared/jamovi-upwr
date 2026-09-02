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
            level <- o$ciWidth / 100; mu <- o$testValue
            tt <- self$results$ttest
            for (v in o$vars) {
                x <- jmvcore::toNumeric(self$data[[v]]); x <- x[!is.na(x)]
                if (length(x) < 2) { tt$setNote(paste0("n", v), sprintf("%s: za mało obserwacji.", v)); next }
                if (isTRUE(o$student)) addTestRow(tt, paste(v, "t"), v, oneSampleT(x, mu, o$hypothesis, level))
                if (isTRUE(o$nonpar)) addTestRow(tt, paste(v, "w"), v, wilcoxOne(x, mu, o$hypothesis, level))
                if (isTRUE(o$desc)) self$results$desc$addRow(rowKey = v, values = c(list(var = v, group = ""), descRow(x)))
                if (isTRUE(o$norm)) self$results$norm$addRow(rowKey = v, values = c(list(var = v, group = ""), shapiroRow(x)))
                m <- stats::t.test(x, conf.level = level)
                self$results$plots$get(key = v)$setState(list(kind = "one", groups = stats::setNames(list(x), v),
                    means = stats::setNames(list(list(mean = mean(x), lower = m$conf.int[1], upper = m$conf.int[2])), v),
                    refLine = mu, level = level, ylab = v))
                self$results$qq$get(key = v)$setState(list(x = x, label = v))
            }
            tt$setNote("h", sprintf("H₁: %s; przedziały ufności %g%%. Wielkość efektu: d Cohena z przedziałem (niecentralny t)%s.",
                altLabel(o$hypothesis, "średnia − wartość testowa"), o$ciWidth,
                if (isTRUE(o$nonpar)) "; dla testu Wilcoxona r rangowo-dwuseryjne i pseudomediana z CI" else ""))
        },
        .plot = function(image, ggtheme, theme, ...) estimationPlot(image, ggtheme, theme),
        .qq = function(image, ggtheme, theme, ...) qqPlotResid(image, ggtheme, theme)
    )
)
