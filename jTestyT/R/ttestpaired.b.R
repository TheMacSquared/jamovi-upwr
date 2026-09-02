#' @importFrom jmvcore .
ttestpairedClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "ttestpairedClass",
    inherit = ttestpairedBase,
    private = list(
        .pairKeys = function() {
            keys <- character(0)
            for (p in self$options$pairs) if (!is.null(p$i1) && !is.null(p$i2)) keys <- c(keys, paste(p$i1, "−", p$i2))
            keys
        },
        .init = function() {
            for (k in private$.pairKeys()) {
                self$results$plots$addItem(key = k); self$results$qq$addItem(key = k)
                self$results$plots$get(key = k)$setTitle(k); self$results$qq$get(key = k)$setTitle(paste("Różnice:", k))
            }
        },
        .run = function() {
            o <- self$options
            pairs <- Filter(function(p) !is.null(p$i1) && !is.null(p$i2), o$pairs)
            if (length(pairs) == 0) return()
            level <- o$ciWidth / 100
            tt <- self$results$ttest
            for (p in pairs) {
                k <- paste(p$i1, "−", p$i2)
                a <- jmvcore::toNumeric(self$data[[p$i1]]); b <- jmvcore::toNumeric(self$data[[p$i2]])
                ok <- !is.na(a) & !is.na(b); a <- a[ok]; b <- b[ok]; dif <- a - b
                if (length(dif) < 2) { tt$setNote(paste0("n", k), sprintf("%s: za mało par.", k)); next }
                if (isTRUE(o$student)) { r <- oneSampleT(dif, 0, o$hypothesis, level); r$test <- "t Studenta (pary)"; addTestRow(tt, paste(k, "t"), k, r) }
                if (isTRUE(o$nonpar)) addTestRow(tt, paste(k, "w"), k, wilcoxOne(dif, 0, o$hypothesis, level))
                if (isTRUE(o$desc)) {
                    self$results$desc$addRow(rowKey = paste(k, 1), values = c(list(var = k, group = p$i1), descRow(a)))
                    self$results$desc$addRow(rowKey = paste(k, 2), values = c(list(var = k, group = p$i2), descRow(b)))
                    self$results$desc$addRow(rowKey = paste(k, 3), values = c(list(var = k, group = "różnica"), descRow(dif)))
                }
                if (isTRUE(o$norm)) self$results$norm$addRow(rowKey = k, values = c(list(var = k, group = "różnice"), shapiroRow(dif)))
                t1 <- stats::t.test(a, conf.level = level); t2 <- stats::t.test(b, conf.level = level); td <- stats::t.test(dif, conf.level = level)
                self$results$plots$get(key = k)$setState(list(kind = "paired", groups = stats::setNames(list(a, b), c(p$i1, p$i2)),
                    means = stats::setNames(list(list(mean = mean(a), lower = t1$conf.int[1], upper = t1$conf.int[2]),
                        list(mean = mean(b), lower = t2$conf.int[1], upper = t2$conf.int[2])), c(p$i1, p$i2)),
                    est = mean(dif), lower = td$conf.int[1], upper = td$conf.int[2], diffs = dif, level = level,
                    ylab = "Wartość"))
                self$results$qq$get(key = k)$setState(list(x = dif, label = k))
            }
            tt$setNote("h", sprintf("Różnica = pierwsza − druga zmienna; H₁: %s; przedziały ufności %g%%. d Cohena = średnia różnica / SD różnic, z przedziałem (niecentralny t)%s.",
                altLabel(o$hypothesis), o$ciWidth,
                if (isTRUE(o$nonpar)) "; dla Wilcoxona r rangowo-dwuseryjne i pseudomediana z CI" else ""))
        },
        .plot = function(image, ggtheme, theme, ...) estimationPlot(image, ggtheme, theme),
        .qq = function(image, ggtheme, theme, ...) qqPlotResid(image, ggtheme, theme)
    )
)
