#' @importFrom jmvcore .
ciregressionClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "ciregressionClass",
    inherit = ciregressionBase,
    private = list(
        .run = function() {
            o <- self$options
            if (!optNonEmpty(o$dep) || !optNonEmpty(o$pred)) return()
            level <- o$ciWidth / 100; t <- self$results$table; method <- o$ciMethod
            y <- jmvcore::toNumeric(self$data[[o$dep]]); x <- jmvcore::toNumeric(self$data[[o$pred]])
            ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]; n <- length(x)
            if (n < 4) { t$setNote("err", "Potrzeba co najmniej 4 kompletnych obserwacji."); return() }
            d <- data.frame(x = x, y = y); fit <- stats::lm(y ~ x, data = d); cf <- stats::coef(fit)

            m <- jmvcore::metodyNew()
            m$add("Dane", "Regresja liniowa „%s” ~ „%s” (MNK); N = %d obserwacji bez braków; R² = %.3f.", o$dep, o$pred, n, summary(fit)$r.squared)
            metodyPrzedzial(m, o, method, "Przedziały t-Studenta dla współczynników: b ± t(df = n − 2) · SE(b)",
                            "losowanie n obserwacji (par x, y) ze zwracaniem i ponowne dopasowanie prostej; statystyki = wyraz wolny i nachylenie z tej samej replikacji")
            m$addIf(o$plot, "Wykres", "Rozrzut z prostą regresji i pasmem przedziału ufności dla wartości średniej (%s).",
                    if (isBoot(method)) "kwantyle replikowanych prostych w każdym x" else "klasyczne, z predict()")
            m$addIf(o$bootPlot && isBoot(method), "Wykres", "Histogram replikacji bootstrapowych nachylenia.")
            m$render(self$results$metody)

            xg <- seq(min(x), max(x), length.out = 100); fallback <- FALSE
            if (isBoot(method)) {
                r <- bootCI(d, function(dd, i) stats::coef(stats::lm(y ~ x, data = dd[i, ])), o$nBoot, o$seed, method, level)
                fallback <- r[[1]]$fallback || r[[2]]$fallback
                rows <- list(list(key = "b0", term = "Wyraz wolny", r = r[[1]]), list(key = "b1", term = paste0("Nachylenie (", o$pred, ")"), r = r[[2]]))
                lines <- outer(xg, r[[2]]$reps) + matrix(r[[1]]$reps, nrow = length(xg), ncol = length(r[[1]]$reps), byrow = TRUE)
                a <- (1 - level) / 2
                band <- data.frame(x = xg, fit = cf[1] + cf[2] * xg,
                                   lower = apply(lines, 1, stats::quantile, probs = a), upper = apply(lines, 1, stats::quantile, probs = 1 - a))
                self$results$bootPlot$setState(list(reps = r[[2]]$reps, est = cf[2], lower = r[[2]]$lower, upper = r[[2]]$upper, xlab = "Nachylenie"))
                clab <- NULL
            } else {
                ci <- stats::confint(fit, level = level); se <- summary(fit)$coefficients[, "Std. Error"]
                rows <- list(list(key = "b0", term = "Wyraz wolny", r = list(est = cf[1], se = se[1], lower = ci[1, 1], upper = ci[1, 2])),
                             list(key = "b1", term = paste0("Nachylenie (", o$pred, ")"), r = list(est = cf[2], se = se[2], lower = ci[2, 1], upper = ci[2, 2])))
                pr <- stats::predict(fit, newdata = data.frame(x = xg), interval = "confidence", level = level)
                band <- data.frame(x = xg, fit = pr[, "fit"], lower = pr[, "lwr"], upper = pr[, "upr"])
                clab <- sprintf("t-Studenta, df = %d", n - 2)
            }
            for (rw in rows) t$addRow(rowKey = rw$key, values = list(term = rw$term, estimate = rw$r$est, se = rw$r$se, lower = rw$r$lower, upper = rw$r$upper))
            ciNote(t, o, method, clab, fallback)
            t$setNote("fit", sprintf("R² = %.3f; N = %d.", summary(fit)$r.squared, n))
            self$results$plot$setState(list(x = x, y = y, xlab = o$pred, ylab = o$dep, band = band, ciWidth = o$ciWidth,
                bandLabel = if (isBoot(method)) bootLabel(method) else "klasyczne"))
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildRegressionPlot(s$x, s$y, s$xlab, s$ylab, s$band, s$ciWidth, s$bandLabel, ggtheme, theme)
        },
        .bootPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildBootHist(s$reps, s$est, s$lower, s$upper, s$xlab, ggtheme, theme)
        }
    )
)
