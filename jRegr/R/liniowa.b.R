#' @importFrom jmvcore .
liniowaClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "liniowaClass",
    inherit = liniowaBase,
    private = list(
        .run = function() {
            o <- self$options; dep <- o$dep; covs <- o$covs; factors <- o$factors
            if (!optNonEmpty(dep) || length(c(covs, factors)) == 0) return()
            level <- o$ciWidth / 100
            d <- regressionFrame(self$data, dep, covs, factors, o$refLevels)
            ft <- self$results$fit; ct <- self$results$coef
            for (v in factors) if (nlevels(d[[v]]) < 2) { ct$setNote("err", sprintf("Predyktor „%s” musi mieć co najmniej 2 poziomy.", v)); return() }
            p <- length(covs) + sum(vapply(factors, function(v) nlevels(d[[v]]) - 1, 1))
            if (nrow(d) < p + 2) { ct$setNote("err", "Za mało obserwacji kompletnych dla tej liczby predyktorów."); return() }
            fit <- stats::lm(regressionFormula(dep, covs, factors), data = d); sm <- summary(fit); n <- nrow(d)
            simple <- length(covs) == 1 && length(factors) == 0
            self$results$plot$setVisible(simple && isTRUE(o$plot))

            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienna zależna „%s”; predyktory ilościowe: %s; jakościowe: %s; N = %d obserwacji bez braków.", dep,
                  if (length(covs)) jmvcore::metodyCyt(covs) else "brak", if (length(factors)) jmvcore::metodyCyt(factors) else "brak", n)
            for (v in factors) m$add("Dane", "„%s” kodowana zero-jedynkowo, poziom odniesienia „%s” (współczynnik = różnica średnich wobec tego poziomu przy stałych pozostałych predyktorach).", v, levels(d[[v]])[1])
            m$add("Model", "Regresja liniowa metodą najmniejszych kwadratów (lm), bez interakcji; R² = SS modelu / SS ogółem, R² skorygowane = 1 − (1 − R²)(n − 1)/(n − p − 1); RMSE = √(SS reszt / n); test F całego modelu wobec modelu z samym wyrazem wolnym.")
            m$add("Model", "Współczynniki: test t (df = n − p − 1) i przedziały ufności %g%% (b ± t · SE).", o$ciWidth)
            m$addIf(o$stdEst, "Model", "β standaryzowane = b · SD(x) / SD(y) (dla zmiennych zero-jedynkowych SD kolumny wskaźnikowej).")
            m$addIf(o$anova, "Model", "Tabela ANOVA: sumy kwadratów typu II (car::Anova) — wkład każdego predyktora po uwzględnieniu pozostałych.")
            m$addIf(o$ic, "Model", "AIC i BIC: kryteria informacyjne, mniejsze = lepsze przy porównywaniu modeli na tych samych danych.")
            m$addIf(o$norm, "Założenia", "Normalność reszt: test Shapiro-Wilka (3–5000 reszt).")
            m$addIf(o$qq, "Założenia", "Wykres Q-Q reszt wobec rozkładu normalnego.")
            m$addIf(o$resPlot, "Założenia", "Reszty wobec wartości dopasowanych z wygładzeniem loess (liniowość i stałość wariancji).")
            m$addIf(o$durbin, "Założenia", "Autokorelacja reszt: statystyka Durbina-Watsona (≈ 2 = brak), autokorelacja rzędu 1, p z symulacji (car, 1000 powtórzeń, ziarno 1).")
            m$addIf(o$vif, "Założenia", "Współliniowość: VIF = 1/(1 − R²) każdego predyktora objaśnianego pozostałymi; tolerancja = 1/VIF; VIF &gt; 10 (tolerancja &lt; 0.1) = problem.")
            m$addIf(o$cooks, "Założenia", "Odległość Cooka: wpływ obserwacji na dopasowanie; liczba obserwacji powyżej progu 4/n.")
            m$addIf(simple && o$plot, "Wykres", "Rozrzut z prostą MNK i pasmem przedziału ufności %g%% dla wartości średniej (predict).", o$ciWidth)
            m$addIf(o$predictOV || o$residsOV, "Dodatkowe", "Do arkusza zapisane: %s (NA dla wierszy pominiętych).",
                    paste(c(if (o$predictOV) "wartości przewidywane", if (o$residsOV) "reszty"), collapse = " i "))
            m$render(self$results$metody)

            fs <- sm$fstatistic
            ft$setRow(rowNo = 1, values = list(r = sqrt(sm$r.squared), r2 = sm$r.squared, r2adj = sm$adj.r.squared,
                F = unname(fs[1]), df1 = unname(fs[2]), df2 = unname(fs[3]), p = stats::pf(fs[1], fs[2], fs[3], lower.tail = FALSE),
                rmse = sqrt(mean(stats::residuals(fit)^2)), aic = stats::AIC(fit), bic = stats::BIC(fit)))
            ft$setNote("n", sprintf("N = %d.", n))

            lab <- coefLabels(d, covs, factors); cf <- sm$coefficients; ci <- stats::confint(fit, level = level)
            beta <- if (isTRUE(o$stdEst)) stdBetas(fit) else NULL
            for (i in seq_len(nrow(cf))) {
                nm <- rownames(cf)[i]
                ct$addRow(rowKey = nm, values = list(term = if (!is.null(lab[nm]) && !is.na(lab[nm])) lab[[nm]] else nm,
                    b = cf[i, 1], se = cf[i, 2], t = cf[i, 3], p = cf[i, 4], lower = ci[i, 1], upper = ci[i, 2],
                    beta = if (is.null(beta)) NULL else unname(beta[nm])))
            }
            if (any(is.na(stats::coef(fit)))) ct$setNote("na", "Współczynniki NA: predyktor liniowo zależny od pozostałych (usunięty z modelu).")

            if (isTRUE(o$anova)) {
                at <- self$results$anova
                an <- tryCatch(car::Anova(fit, type = 2), error = function(e) NULL)
                if (is.null(an)) at$setNote("err", "Nie udało się policzyć tabeli ANOVA.")
                else for (i in seq_len(nrow(an))) {
                    tm <- rownames(an)[i]
                    at$addRow(rowKey = tm, values = list(term = if (tm == "Residuals") "Reszty" else tm, ss = an[i, "Sum Sq"], df = an[i, "Df"],
                        ms = an[i, "Sum Sq"] / an[i, "Df"], F = if (tm == "Residuals") NA else an[i, "F value"], p = if (tm == "Residuals") NA else an[i, "Pr(>F)"]))
                }
            }
            res <- stats::residuals(fit); fitted <- stats::fitted(fit)
            if (isTRUE(o$norm)) {
                nt <- self$results$norm
                if (n >= 3 && n <= 5000) { sw <- stats::shapiro.test(res); nt$setRow(rowNo = 1, values = list(w = unname(sw$statistic), p = sw$p.value)) }
                else nt$setNote("n", "Test Shapiro-Wilka wymaga od 3 do 5000 reszt.")
            }
            if (isTRUE(o$durbin)) { dw <- durbinWatson(fit); self$results$durbin$setRow(rowNo = 1, values = list(r = dw$r, dw = dw$dw, p = dw$p)) }
            if (isTRUE(o$vif)) {
                vt <- self$results$vif; v <- vifTable(fit)
                if (is.null(v)) vt$setNote("n", "VIF wymaga co najmniej dwóch kolumn predyktorów.")
                else for (i in seq_len(nrow(v))) vt$addRow(rowKey = v$term[i], values = list(term = if (!is.na(lab[v$term[i]])) lab[[v$term[i]]] else v$term[i], vif = v$vif[i], tol = v$tol[i]))
            }
            if (isTRUE(o$cooks)) { cs <- cooksSummary(fit); self$results$cooks$setRow(rowNo = 1, values = list(mean = cs$mean, max = cs$max, nHigh = cs$nHigh, thr = cs$thr)) }
            if (simple) {
                x <- d[[covs]]; xg <- seq(min(x), max(x), length.out = 100)
                nd <- data.frame(xg); names(nd) <- covs
                pr <- stats::predict(fit, newdata = nd, interval = "confidence", level = level)
                self$results$plot$setState(list(x = x, y = d[[dep]], xlab = covs, ylab = dep, band = data.frame(x = xg, fit = pr[, "fit"], lower = pr[, "lwr"], upper = pr[, "upr"])))
            }
            self$results$resPlot$setState(list(fitted = fitted, resid = res)); self$results$qq$setState(list(resid = res))
            complete <- stats::complete.cases(self$data[c(dep, covs, factors)])
            if (isTRUE(o$predictOV) && self$results$predictOV$isNotFilled()) { full <- rep(NA_real_, length(complete)); full[complete] <- fitted; self$results$predictOV$setValues(full) }
            if (isTRUE(o$residsOV) && self$results$residsOV$isNotFilled()) { full <- rep(NA_real_, length(complete)); full[complete] <- res; self$results$residsOV$setValues(full) }
        },
        .plot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); simpleRegressionPlot(s$x, s$y, s$xlab, s$ylab, s$band, ggtheme, theme) },
        .resPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); residFittedPlot(s$fitted, s$resid, ggtheme, theme) },
        .qqPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); qqResidPlot(s$resid, ggtheme, theme) }
    )
)
