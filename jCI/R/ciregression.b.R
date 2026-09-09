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
            if (any(!is.finite(c(x, y))) || length(unique(x)) < 2) {
                t$setNote("err", "Predyktor musi być zmienny, a dane muszą zawierać wyłącznie wartości skończone."); return()
            }
            d <- data.frame(x = x, y = y); fit <- stats::lm(y ~ x, data = d); cf <- stats::coef(fit)

            m <- jmvcore::metodyNew()
            m$add("Dane", "Regresja liniowa „%s” ~ „%s” (MNK); N = %d (obserwacje bez braków); R² = %.3f.", o$dep, o$pred, n, summary(fit)$r.squared)
            metodyPrzedzial(m, o, method, "Przedziały t-Studenta dla współczynników: b ± t(df = n − 2) · SE(b)",
                            "losowanie n obserwacji (par x, y) ze zwracaniem i ponowne dopasowanie prostej; statystyki = wyraz wolny i nachylenie z tej samej replikacji")
            m$addIf(o$plot, "Wykres", "Rozrzut z prostą regresji i pasmem przedziału ufności dla wartości średniej (%s).",
                    if (isBoot(method)) "punktowe kwantyle percentylowe replikowanych prostych w każdym x, także przy BCa dla współczynników" else "klasyczne, z predict()")
            m$addIf(o$bootPlot && isBoot(method), "Wykres", "Histogram replikacji bootstrapowych nachylenia.")
            m$render(self$results$metody)

            xg <- seq(min(x), max(x), length.out = 100); fallback <- FALSE; band <- NULL
            if (isBoot(method)) {
                # Require at least half the requested replicates and at least 50.
                # This is a guard against a mostly undefined bootstrap, not a precision guarantee.
                minValid <- max(50, ceiling(o$nBoot / 2))
                r <- bootCI(d, regressionBootStatistic, o$nBoot, o$seed, method, level, minValid = minValid)
                if (r[[1]]$nFailed > 0) {
                    note <- sprintf("Poprawne repliki: %d z %d; repliki pominięte (brak estymowalnej pary współczynników): %d.", r[[1]]$nValid, o$nBoot, r[[1]]$nFailed)
                    t$setNote("replicas", note)
                    m$add("Przedział ufności", "%s", note)
                    m$render(self$results$metody)
                }
                if (!r[[1]]$sufficient || !r[[2]]$sufficient) {
                    t$setNote("err", sprintf("Za mało poprawnych replik (wymagane co najmniej %d) — nie wyznaczono przedziałów.", minValid))
                    return()
                }
                fallback <- r[[1]]$fallback || r[[2]]$fallback
                rows <- list(list(key = "b0", term = "Wyraz wolny", r = r[[1]]), list(key = "b1", term = paste0("Nachylenie (", o$pred, ")"), r = r[[2]]))
                if (isTRUE(o$plot))
                    band <- regressionBootBand(xg, cf, r[[1]]$reps, r[[2]]$reps, level)
                self$results$bootPlot$setState(list(reps = r[[2]]$reps, est = cf[2], lower = r[[2]]$lower, upper = r[[2]]$upper, xlab = "Nachylenie"))
                clab <- NULL
            } else {
                ci <- stats::confint(fit, level = level); se <- summary(fit)$coefficients[, "Std. Error"]
                rows <- list(list(key = "b0", term = "Wyraz wolny", r = list(est = cf[1], se = se[1], lower = ci[1, 1], upper = ci[1, 2])),
                             list(key = "b1", term = paste0("Nachylenie (", o$pred, ")"), r = list(est = cf[2], se = se[2], lower = ci[2, 1], upper = ci[2, 2])))
                if (isTRUE(o$plot)) {
                    pr <- stats::predict(fit, newdata = data.frame(x = xg), interval = "confidence", level = level)
                    band <- data.frame(x = xg, fit = pr[, "fit"], lower = pr[, "lwr"], upper = pr[, "upr"])
                }
                clab <- sprintf("t-Studenta, df = %d", n - 2)
            }
            for (rw in rows) t$addRow(rowKey = rw$key, values = list(term = rw$term, estimate = rw$r$est, se = rw$r$se, lower = rw$r$lower, upper = rw$r$upper))
            ciNote(t, o, method, clab, fallback)
            t$setNote("fit", sprintf("R² = %.3f; N = %d.", summary(fit)$r.squared, n))
            if (isTRUE(o$plot)) self$results$plot$setState(list(x = x, y = y, xlab = o$pred, ylab = o$dep, band = band, ciWidth = o$ciWidth,
                bandLabel = if (isBoot(method)) "bootstrap percentylowy, punktowe" else "klasyczne"))
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
