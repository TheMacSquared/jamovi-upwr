#' @importFrom jmvcore .
citwomeansClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "citwomeansClass",
    inherit = citwomeansBase,
    private = list(
        .run = function() {
            o <- self$options
            if (!optNonEmpty(o$dep) || !optNonEmpty(o$group)) return()
            level <- o$ciWidth / 100; t <- self$results$table; method <- o$ciMethod
            x <- jmvcore::toNumeric(self$data[[o$dep]]); g <- factor(self$data[[o$group]])
            ok <- !is.na(x) & !is.na(g); x <- x[ok]; g <- droplevels(g[ok])
            sel <- pickTwoLevels(t, levels(g), o$level1, o$level2)
            if (is.null(sel)) return()
            keep <- g %in% sel; x <- x[keep]; g <- factor(g[keep], levels = sel)
            x1 <- x[g == sel[1]]; x2 <- x[g == sel[2]]
            if (length(x1) < 2 || length(x2) < 2) { t$setNote("err", "Każda grupa musi mieć co najmniej 2 obserwacje."); return() }

            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienna „%s”; grupy „%s” (n = %d) i „%s” (n = %d) ze zmiennej „%s”; różnica = „%s” − „%s”; braki pominięte.",
                  o$dep, sel[1], length(x1), sel[2], length(x2), o$group, sel[1], sel[2])
            metodyPrzedzial(m, o, method, if (method == "student") "Przedział t-Studenta ze wspólną wariancją: różnica ± t(df = n₁ + n₂ − 2) · s_p · √(1/n₁ + 1/n₂)"
                            else "Przedział Welcha: różnica ± t(df Welcha-Satterthwaite’a) · √(s₁²/n₁ + s₂²/n₂), bez założenia równych wariancji",
                            "losowanie ze zwracaniem osobno w każdej grupie (bootstrap warstwowy), statystyka = różnica średnich")
            m$addIf(o$effSize, "Wielkość efektu", "d Cohena = różnica średnich / łączone SD; przedział ufności %g%% z rozkładu niecentralnego t.", o$ciWidth)
            m$addIf(o$plot, "Wykres", "Estymacyjny (Gardner-Altman): punkty = obserwacje, romby = średnie grup, różnica z przedziałem na osi po prawej, zakotwiczonej w średniej „%s”.", sel[2])
            m$addIf(o$bootPlot && isBoot(method), "Wykres", "Histogram replikacji bootstrapowych różnicy z estymatą i granicami przedziału.")
            m$render(self$results$metody)

            fallback <- FALSE
            if (isBoot(method)) {
                d <- data.frame(x = x, g = g)
                r <- bootCI(d, function(dd, i) { s <- dd[i, ]; mean(s$x[s$g == sel[1]]) - mean(s$x[s$g == sel[2]]) },
                            o$nBoot, o$seed, method, level, strata = g)
                fallback <- r$fallback
                self$results$bootPlot$setState(list(reps = r$reps, est = r$est, lower = r$lower, upper = r$upper, xlab = "Różnica średnich"))
                clab <- NULL
            } else if (method == "student") { r <- ciTwoMeansStudent(x1, x2, level); clab <- sprintf("t-Studenta, df = %d", r$df)
            } else { r <- ciTwoMeansWelch(x1, x2, level); clab <- sprintf("Welcha, df = %.1f", r$df) }
            n1 <- length(x1); n2 <- length(x2)
            sp <- sqrt(((n1 - 1) * stats::var(x1) + (n2 - 1) * stats::var(x2)) / (n1 + n2 - 2))
            dC <- (mean(x1) - mean(x2)) / sp; dci <- if (isTRUE(o$effSize)) dInterval(dC, n1, n2, level) else c(NA_real_, NA_real_)
            t$setRow(rowNo = 1, values = list(var = o$dep, group1 = sel[1], group2 = sel[2],
                estimate = r$est, se = r$se, lower = r$lower, upper = r$upper, d = dC, dLower = dci[1], dUpper = dci[2]))
            ciNote(t, o, method, clab, fallback)
            self$results$plot$setState(list(x1 = x1, x2 = x2, group1 = sel[1], group2 = sel[2], ylab = o$dep,
                estimate = r$est, lower = r$lower, upper = r$upper, ciWidth = o$ciWidth))
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildTwoMeansCIPlot(s$x1, s$x2, s$group1, s$group2, s$estimate, s$lower, s$upper, s$ciWidth, s$ylab, ggtheme, theme)
        },
        .bootPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildBootHist(s$reps, s$est, s$lower, s$upper, s$xlab, ggtheme, theme)
        }
    )
)
