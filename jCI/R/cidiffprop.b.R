#' @importFrom jmvcore .
cidiffpropClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "cidiffpropClass",
    inherit = cidiffpropBase,
    private = list(
        .run = function() {
            o <- self$options
            if (!optNonEmpty(o$dep) || !optNonEmpty(o$group)) return()
            level <- o$ciWidth / 100; t <- self$results$table; method <- o$ciMethod
            column <- self$data[[o$dep]]
            lv <- pickLevel(column, o$level); if (is.null(lv)) return()
            outcome <- as.character(column); g <- factor(self$data[[o$group]])
            ok <- !is.na(outcome) & !is.na(g); outcome <- outcome[ok]; g <- droplevels(g[ok])
            sel <- pickTwoLevels(t, levels(g), o$groupLevel1, o$groupLevel2); if (is.null(sel)) return()
            keep <- g %in% sel; outcome <- outcome[keep]; g <- factor(g[keep], levels = sel)
            succ <- as.integer(outcome == lv)
            n1 <- sum(g == sel[1]); n2 <- sum(g == sel[2]); x1 <- sum(succ[g == sel[1]]); x2 <- sum(succ[g == sel[2]])
            if (n1 < 1 || n2 < 1) { t$setNote("err", "Każda grupa musi mieć co najmniej 1 obserwację."); return() }
            p1 <- x1 / n1; p2 <- x2 / n2

            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienna „%s”, „sukces” = kategoria „%s”%s; grupy „%s” (n = %d) i „%s” (n = %d) ze zmiennej „%s”; różnica = udział w „%s” − udział w „%s”; braki pominięte.",
                  o$dep, lv, if (!optNonEmpty(o$level)) " (pierwszy poziom — wybierz inny w panelu)" else "",
                  sel[1], n1, sel[2], n2, o$group, sel[1], sel[2])
            metodyPrzedzial(m, o, method, switch(method,
                newcombe = "Przedział Newcombe’a (hybrydowy score, metoda 10): granice z przedziałów Wilsona obu grup",
                wald = "Przedział Walda: różnica ± z · √(p̂₁(1 − p̂₁)/n₁ + p̂₂(1 − p̂₂)/n₂) — zawodny przy małych n", ""),
                "losowanie ze zwracaniem osobno w każdej grupie (bootstrap warstwowy), statystyka = różnica udziałów")
            m$addIf(o$plot, "Wykres", "Udziały w grupach z przedziałami Wilsona i różnica z przedziałem na osi po prawej (p.p. = punkty procentowe), zakotwiczonej w udziale „%s”.", sel[2])
            m$addIf(o$bootPlot && isBoot(method), "Wykres", "Histogram replikacji bootstrapowych różnicy udziałów.")
            m$render(self$results$metody)

            fallback <- FALSE
            if (isBoot(method)) {
                d <- data.frame(s = succ, g = g)
                r <- bootCI(d, function(dd, i) { z <- dd[i, ]; mean(z$s[z$g == sel[1]]) - mean(z$s[z$g == sel[2]]) },
                            o$nBoot, o$seed, method, level, strata = g)
                fallback <- r$fallback
                self$results$bootPlot$setState(list(reps = r$reps, est = r$est, lower = r$lower, upper = r$upper, xlab = "Różnica udziałów"))
                clab <- NULL
            } else { r <- ciDiffProportion(x1, n1, x2, n2, level, method); clab <- switch(method, newcombe = "Newcombe’a", wald = "Walda") }
            t$setRow(rowNo = 1, values = list(var = o$dep, group1 = sel[1], group2 = sel[2], p1 = p1, p2 = p2,
                estimate = p1 - p2, lower = r$lower, upper = r$upper))
            ciNote(t, o, method, clab, fallback)
            self$results$plot$setState(list(group1 = sel[1], group2 = sel[2], p1 = p1, p2 = p2,
                ci1 = ciProportion(x1, n1, level, "wilson"), ci2 = ciProportion(x2, n2, level, "wilson"),
                estimate = p1 - p2, lower = r$lower, upper = r$upper, ciWidth = o$ciWidth, level = lv))
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildDiffPropPlot(s$group1, s$group2, s$p1, s$p2, s$ci1, s$ci2, s$estimate, s$lower, s$upper, s$ciWidth, s$level, ggtheme, theme)
        },
        .bootPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildBootHist(s$reps, s$est, s$lower, s$upper, s$xlab, ggtheme, theme)
        }
    )
)
