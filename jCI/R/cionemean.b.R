#' @importFrom jmvcore .
cionemeanClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "cionemeanClass",
    inherit = cionemeanBase,
    private = list(
        .statFun = function() {
            o <- self$options
            switch(o$stat, median = stats::median, trimmed = function(v) mean(v, trim = o$trimProp), mean)
        },
        .statLabel = function() {
            o <- self$options
            switch(o$stat, median = "Mediana", trimmed = sprintf("Średnia ucięta (%g%%)", 100 * o$trimProp), "Średnia")
        },
        .run = function() {
            o <- self$options
            if (!optNonEmpty(o$dep)) return()
            level <- o$ciWidth / 100
            t <- self$results$table
            statLab <- private$.statLabel(); fun <- private$.statFun()
            t$getColumn("estimate")$setTitle(statLab)
            # median / trimmed mean have no t interval: bootstrap percentile is forced
            method <- o$ciMethod
            forced <- o$stat != "mean" && !isBoot(method)
            if (forced) method <- "perc"

            x <- jmvcore::toNumeric(self$data[[o$dep]])
            grouped <- optNonEmpty(o$group)
            g <- if (grouped) factor(self$data[[o$group]]) else factor(rep(o$dep, length(x)))
            ok <- !is.na(x) & !is.na(g); x <- x[ok]; g <- droplevels(g[ok])
            if (length(x) == 0) { t$setNote("err", "Brak obserwacji."); return() }

            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienna „%s”%s; %s = %s; braki pominięte.", o$dep,
                  if (grouped) sprintf(", osobno w grupach zmiennej „%s” (%s)", o$group, jmvcore::metodyCyt(levels(g))) else "",
                  if (grouped) "N w grupach" else "N", if (grouped) paste(table(g), collapse = ", ") else length(x))
            m$add("Dane", "Estymowana miara: %s%s.", tolower(statLab),
                  if (o$stat == "trimmed") sprintf(" — średnia po odrzuceniu %g%% obserwacji z każdego końca", 100 * o$trimProp) else "")
            metodyPrzedzial(m, o, method, "Przedział t-Studenta: średnia ± t(df = n − 1) · SD/√n",
                            "losowanie n obserwacji ze zwracaniem, osobno w każdej grupie",
                            if (forced) "Dla mediany i średniej uciętej przedział klasyczny nie istnieje — użyto bootstrapu percentylowego." else NULL)
            m$addIf(o$plot, "Wykres", "Punkty = obserwacje, romb = %s, wąsy = przedział ufności.", tolower(statLab))
            m$addIf(o$bootPlot && isBoot(method), "Wykres", "Histogram replikacji bootstrapowych z estymatą i granicami przedziału.")
            m$render(self$results$metody)

            plotData <- list(label = o$dep, groups = list(), ciWidth = o$ciWidth, statLabel = statLab)
            fallback <- FALSE
            for (lv in levels(g)) {
                xg <- x[g == lv]; n <- length(xg); key <- if (grouped) lv else o$dep
                if (n < 2) {
                    t$addRow(rowKey = key, values = list(var = o$dep, grp = if (grouped) lv else "", n = n))
                    t$setNote(paste0("n", key), sprintf("%s: za mało obserwacji (n < 2).", key)); next
                }
                if (isBoot(method)) {
                    r <- bootCI(xg, function(d, i) fun(d[i]), o$nBoot, o$seed, method, level)
                    fallback <- fallback || r$fallback
                    if (isTRUE(o$bootPlot)) {
                        self$results$bootPlots$addItem(key = key)
                        img <- self$results$bootPlots$get(key = key); img$setTitle(key)
                        img$setState(list(reps = r$reps, est = r$est, lower = r$lower, upper = r$upper, xlab = statLab))
                    }
                } else r <- ciMeanT(xg, level)
                t$addRow(rowKey = key, values = list(var = o$dep, grp = if (grouped) lv else "", n = n,
                    estimate = r$est, se = r$se, lower = r$lower, upper = r$upper))
                plotData$groups[[key]] <- list(x = xg, estimate = r$est, lower = r$lower, upper = r$upper)
            }
            ciNote(t, o, method, "t-Studenta", fallback)
            if (length(plotData$groups)) self$results$plot$setState(plotData)
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildGroupedMeanCIPlot(s$label, s$groups, s$ciWidth, s$statLabel, ggtheme, theme)
        },
        .bootPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildBootHist(s$reps, s$est, s$lower, s$upper, s$xlab, ggtheme, theme)
        }
    )
)
