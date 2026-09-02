#' @importFrom jmvcore .
ttesttwoClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "ttesttwoClass",
    inherit = ttesttwoBase,
    private = list(
        .init = function() {
            for (v in self$options$vars) {
                self$results$plots$addItem(key = v); self$results$qq$addItem(key = v)
                self$results$plots$get(key = v)$setTitle(v); self$results$qq$get(key = v)$setTitle(v)
            }
        },
        .run = function() {
            o <- self$options
            if (length(o$vars) == 0 || !optNonEmpty(o$group)) return()
            level <- o$ciWidth / 100
            gAll <- factor(self$data[[o$group]])
            tt <- self$results$ttest; ex <- self$results$extra
            if (nlevels(droplevels(gAll[!is.na(gAll)])) != 2) {
                tt$setNote("g", "Zmienna grupująca musi mieć dokładnie 2 poziomy (odfiltruj pozostałe)."); return()
            }
            for (v in o$vars) {
                y <- jmvcore::toNumeric(self$data[[v]]); ok <- !is.na(y) & !is.na(gAll)
                y <- y[ok]; g <- droplevels(gAll[ok]); lv <- levels(g)
                if (nlevels(g) != 2 || any(table(g) < 2)) { tt$setNote(paste0("n", v), sprintf("%s: za mało obserwacji w grupie.", v)); next }
                addTestRow(tt, paste(v, "t"), v, twoSampleT(y, g, isTRUE(o$welch), o$hypothesis, level))
                if (isTRUE(o$nonpar)) addTestRow(tt, paste(v, "mw"), v, mannWhitney(y, g, o$hypothesis, level))
                if (isTRUE(o$ks)) addExtraRow(ex, paste(v, "ks"), v, ksTwo(y, g))
                if (isTRUE(o$perm)) addExtraRow(ex, paste(v, "p"), v, permTwo(y, g, o$hypothesis))
                if (isTRUE(o$boot)) addExtraRow(ex, paste(v, "b"), v, bootTwo(y, g, level = level))
                x1 <- y[g == lv[1]]; x2 <- y[g == lv[2]]
                if (isTRUE(o$desc)) for (l in lv)
                    self$results$desc$addRow(rowKey = paste(v, l), values = c(list(var = v, group = l), descRow(y[g == l])))
                if (isTRUE(o$norm)) for (l in lv)
                    self$results$norm$addRow(rowKey = paste(v, l), values = c(list(var = v, group = l), shapiroRow(y[g == l])))
                if (isTRUE(o$homog)) self$results$homog$addRow(rowKey = v, values = c(list(var = v), leveneTwo(y, g)))
                t1 <- stats::t.test(x1, conf.level = level); t2 <- stats::t.test(x2, conf.level = level)
                td <- stats::t.test(x1, x2, var.equal = !isTRUE(o$welch), conf.level = level)
                self$results$plots$get(key = v)$setState(list(kind = "two", groups = stats::setNames(list(x1, x2), lv),
                    means = stats::setNames(list(list(mean = mean(x1), lower = t1$conf.int[1], upper = t1$conf.int[2]),
                        list(mean = mean(x2), lower = t2$conf.int[1], upper = t2$conf.int[2])), lv),
                    est = mean(x1) - mean(x2), lower = td$conf.int[1], upper = td$conf.int[2], level = level, ylab = v))
                self$results$qq$get(key = v)$setState(list(x = c(x1 - mean(x1), x2 - mean(x2)), label = paste(v, "(reszty w grupach)")))
            }
            tt$setNote("h", sprintf("Różnica = %s − %s; H₁: %s; przedziały ufności %g%%. d Cohena z łączonym SD i przedziałem (niecentralny t)%s.",
                lv[1], lv[2], altLabel(o$hypothesis), o$ciWidth,
                if (isTRUE(o$nonpar)) "; dla Manna-Whitneya r rangowo-dwuseryjne i przesunięcie Hodgesa-Lehmanna z CI" else ""))
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (self$options$plotType == "box") boxPlotTwo(image, ggtheme, theme) else estimationPlot(image, ggtheme, theme)
        },
        .qq = function(image, ggtheme, theme, ...) qqPlotResid(image, ggtheme, theme)
    )
)
