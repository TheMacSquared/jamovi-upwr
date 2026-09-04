#' @importFrom jmvcore .
cipairedmeansClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "cipairedmeansClass",
    inherit = cipairedmeansBase,
    private = list(
        .run = function() {
            o <- self$options
            if (!optNonEmpty(o$var1) || !optNonEmpty(o$var2)) return()
            level <- o$ciWidth / 100; t <- self$results$table; method <- o$ciMethod
            a <- jmvcore::toNumeric(self$data[[o$var1]]); b <- jmvcore::toNumeric(self$data[[o$var2]])
            ok <- !is.na(a) & !is.na(b); d <- a[ok] - b[ok]; n <- length(d)
            if (n < 2) { t$setNote("err", "Za mało kompletnych par (n < 2)."); return() }

            m <- jmvcore::metodyNew()
            m$add("Dane", "Pomiary „%s” i „%s” tych samych jednostek; różnica = „%s” − „%s”; N = %d par bez braków.",
                  o$var1, o$var2, o$var1, o$var2, n)
            metodyPrzedzial(m, o, method, "Przedział t-Studenta dla średniej różnic: średnia ± t(df = n − 1) · SD różnic/√n",
                            "losowanie n par (różnic) ze zwracaniem, statystyka = średnia różnic")
            m$addIf(o$plot, "Wykres", "Punkty = różnice w parach, romb = średnia różnic z przedziałem, linia przerywana = 0 (brak zmiany).")
            m$addIf(o$bootPlot && isBoot(method), "Wykres", "Histogram replikacji bootstrapowych średniej różnic.")
            m$render(self$results$metody)

            fallback <- FALSE
            if (isBoot(method)) {
                r <- bootCI(d, function(v, i) mean(v[i]), o$nBoot, o$seed, method, level); fallback <- r$fallback
                self$results$bootPlot$setState(list(reps = r$reps, est = r$est, lower = r$lower, upper = r$upper, xlab = "Średnia różnic"))
                clab <- NULL
            } else { r <- ciMeanT(d, level); clab <- sprintf("t-Studenta, df = %d", n - 1) }
            t$setRow(rowNo = 1, values = list(var1 = o$var1, var2 = o$var2, n = n, estimate = r$est, se = r$se, lower = r$lower, upper = r$upper))
            ciNote(t, o, method, clab, fallback)
            lab <- paste(o$var1, "−", o$var2)
            self$results$plot$setState(list(label = lab, groups = stats::setNames(list(list(x = d, estimate = r$est, lower = r$lower, upper = r$upper)), lab),
                ciWidth = o$ciWidth))
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildGroupedMeanCIPlot("Różnica w parze", s$groups, s$ciWidth, "Średnia różnic", ggtheme, theme, refLine = 0)
        },
        .bootPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildBootHist(s$reps, s$est, s$lower, s$upper, s$xlab, ggtheme, theme)
        }
    )
)
