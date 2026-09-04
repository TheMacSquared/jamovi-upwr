#' @importFrom jmvcore .
cicorrelationClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "cicorrelationClass",
    inherit = cicorrelationBase,
    private = list(
        .run = function() {
            o <- self$options
            if (!optNonEmpty(o$var1) || !optNonEmpty(o$var2)) return()
            level <- o$ciWidth / 100; t <- self$results$table; method <- o$ciMethod
            a <- jmvcore::toNumeric(self$data[[o$var1]]); b <- jmvcore::toNumeric(self$data[[o$var2]])
            ok <- !is.na(a) & !is.na(b); a <- a[ok]; b <- b[ok]; n <- length(a)
            if (n < 4) { t$setNote("err", "Potrzeba co najmniej 4 kompletnych obserwacji."); return() }
            corLab <- if (o$method == "pearson") "r Pearsona" else "ρ Spearmana"

            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienne „%s” i „%s”; N = %d par bez braków; współczynnik: %s%s.", o$var1, o$var2, n, corLab,
                  if (o$method == "spearman") " (korelacja rang)" else "")
            metodyPrzedzial(m, o, method,
                if (o$method == "pearson") "Transformacja Fishera z: tanh(atanh(r) ± z/√(n − 3))"
                else "Transformacja Fishera z z błędem Bonetta-Wrighta: tanh(atanh(ρ) ± z · √((1 + ρ²/2)/(n − 3)))",
                "losowanie n par obserwacji ze zwracaniem, statystyka = współczynnik korelacji")
            m$addIf(o$plot, "Wykres", "Rozrzut z prostą regresji (bez pasma — przedział w tabeli dotyczy współczynnika, nie prostej).")
            m$addIf(o$bootPlot && isBoot(method), "Wykres", "Histogram replikacji bootstrapowych współczynnika.")
            m$render(self$results$metody)

            fallback <- FALSE
            if (isBoot(method)) {
                d <- data.frame(a = a, b = b)
                r <- bootCI(d, function(dd, i) stats::cor(dd$a[i], dd$b[i], method = o$method), o$nBoot, o$seed, method, level)
                fallback <- r$fallback
                self$results$bootPlot$setState(list(reps = r$reps, est = r$est, lower = r$lower, upper = r$upper, xlab = corLab))
                clab <- NULL
            } else { r <- ciCorrelation(a, b, level, o$method); clab <- if (o$method == "pearson") "transformacja Fishera z" else "Fishera z z błędem Bonetta-Wrighta" }
            t$getColumn("estimate")$setTitle(if (o$method == "pearson") "r" else "ρ")
            t$setRow(rowNo = 1, values = list(var1 = o$var1, var2 = o$var2, n = n, estimate = r$est, lower = r$lower, upper = r$upper))
            ciNote(t, o, method, clab, fallback)
            self$results$plot$setState(list(a = a, b = b, var1 = o$var1, var2 = o$var2, estimate = r$est, lower = r$lower, upper = r$upper,
                ciWidth = o$ciWidth, method = o$method))
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildCorrelationPlot(s$a, s$b, s$var1, s$var2, s$estimate, s$lower, s$upper, s$ciWidth, s$method, ggtheme, theme)
        },
        .bootPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildBootHist(s$reps, s$est, s$lower, s$upper, s$xlab, ggtheme, theme)
        }
    )
)
