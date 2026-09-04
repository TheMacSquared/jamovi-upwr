#' @importFrom jmvcore .
logistycznaClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "logistycznaClass",
    inherit = logistycznaBase,
    private = list(
        .run = function() {
            o <- self$options; dep <- o$dep; covs <- o$covs; factors <- o$factors
            if (!optNonEmpty(dep) || length(c(covs, factors)) == 0) return()
            level <- o$ciWidth / 100; ct <- self$results$coef; ft <- self$results$fit
            d <- regressionFrame(self$data, dep, covs, factors, o$refLevels, depFactor = TRUE)
            d[[dep]] <- droplevels(factor(d[[dep]]))
            if (nlevels(d[[dep]]) != 2) { ct$setNote("err", "Zmienna zależna musi mieć dokładnie 2 poziomy (odfiltruj pozostałe)."); return() }
            for (v in factors) if (nlevels(d[[v]]) < 2) { ct$setNote("err", sprintf("Predyktor „%s” musi mieć co najmniej 2 poziomy.", v)); return() }
            lv <- levels(d[[dep]])
            event <- if (optNonEmpty(o$event) && as.character(o$event) %in% lv) as.character(o$event) else lv[2]
            y <- as.integer(d[[dep]] == event); n <- nrow(d)
            if (sum(y) == 0 || sum(y) == n) { ct$setNote("err", "Zmienna zależna musi mieć obserwacje w obu kategoriach."); return() }
            d$.y <- y
            fit <- suppressWarnings(stats::glm(regressionFormula(".y", covs, factors), data = d, family = stats::binomial()))
            null <- stats::glm(.y ~ 1, data = d, family = stats::binomial())
            simple <- length(covs) == 1 && length(factors) == 0
            self$results$plot$setVisible(simple && isTRUE(o$plot))

            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienna zależna „%s”: zdarzenie = „%s” (kodowane 1), odniesienie = „%s”%s; predyktory ilościowe: %s; jakościowe: %s; N = %d obserwacji bez braków, zdarzeń = %d.",
                  dep, event, setdiff(lv, event)[1], if (!optNonEmpty(o$event)) " (domyślnie drugi poziom — wybierz w panelu)" else "",
                  if (length(covs)) jmvcore::metodyCyt(covs) else "brak", if (length(factors)) jmvcore::metodyCyt(factors) else "brak", n, sum(y))
            for (v in factors) m$add("Dane", "„%s” kodowana zero-jedynkowo, poziom odniesienia „%s”.", v, levels(d[[v]])[1])
            m$add("Model", "Regresja logistyczna dwumianowa (glm, logit, największa wiarygodność), bez interakcji; test modelu = test ilorazu wiarygodności wobec modelu z samym wyrazem wolnym (χ², df = liczba parametrów).")
            m$add("Model", "R² McFaddena = 1 − LL(model)/LL(zerowy); R² Nagelkerkego = R² Coxa-Snella / maksimum.")
            m$add("Model", "Współczynniki: test Walda z (b/SE), przedziały ufności %g%% b ± z · SE; iloraz szans OR = e^b z przedziałem e^(granice b) — zmiana szans zdarzenia na jednostkę predyktora (dla zero-jedynkowych: wobec poziomu odniesienia).", o$ciWidth)
            m$add("Klasyfikacja", "Przewidywane zdarzenie, gdy P ≥ %g; trafność = (TP + TN)/N, czułość = TP/(TP + FN), swoistość = TN/(TN + FP).", o$cutoff)
            m$addIf(o$roc, "Klasyfikacja", "Krzywa ROC: czułość wobec 1 − swoistość dla wszystkich progów; AUC = P(losowe zdarzenie ma większe P niż losowe niezdarzenie) (statystyka Manna-Whitneya).")
            m$addIf(o$ic, "Model", "AIC i BIC: kryteria informacyjne, mniejsze = lepsze.")
            m$addIf(o$vif, "Założenia", "Współliniowość: VIF z regresji liniowej każdej kolumny predyktorów na pozostałe; tolerancja = 1/VIF.")
            m$addIf(o$cooks, "Założenia", "Odległość Cooka dla obserwacji; liczba powyżej progu 4/n.")
            m$addIf(simple && o$plot, "Wykres", "Punkty = obserwacje (0/1, lekko rozproszone), krzywa = P(zdarzenie) z pasmem ±1.96 SE na skali logitu.")
            m$addIf(o$predictOV, "Dodatkowe", "Do arkusza zapisane prawdopodobieństwa zdarzenia (NA dla wierszy pominiętych).")
            m$render(self$results$metody)

            ll <- as.numeric(stats::logLik(fit)); ll0 <- as.numeric(stats::logLik(null))
            lr <- 2 * (ll - ll0); dfm <- length(stats::coef(fit)) - 1
            r2cs <- 1 - exp(-lr / n); r2n <- r2cs / (1 - exp(2 * ll0 / n))
            ft$setRow(rowNo = 1, values = list(dev = stats::deviance(fit), chi = lr, df = dfm, p = stats::pchisq(lr, dfm, lower.tail = FALSE),
                mcf = 1 - ll / ll0, nag = r2n, aic = stats::AIC(fit), bic = stats::BIC(fit)))
            ft$setNote("n", sprintf("N = %d; zdarzenie = „%s”.", n, event))

            lab <- coefLabels(d, covs, factors); cf <- summary(fit)$coefficients
            z <- stats::qnorm(1 - (1 - level) / 2)
            for (i in seq_len(nrow(cf))) {
                nm <- rownames(cf)[i]; b <- cf[i, 1]; se <- cf[i, 2]
                ct$addRow(rowKey = nm, values = list(term = if (!is.na(lab[nm])) lab[[nm]] else nm, b = b, se = se, z = cf[i, 3], p = cf[i, 4],
                    or = exp(b), orLower = exp(b - z * se), orUpper = exp(b + z * se)))
            }
            if (!fit$converged) ct$setNote("conv", "Algorytm nie osiągnął zbieżności — sprawdź separację (predyktor idealnie rozdziela kategorie).")

            prob <- stats::fitted(fit); cl <- classify(y, prob, o$cutoff); other <- setdiff(lv, event)[1]
            tt <- self$results$classTable
            tt$addRow(rowKey = "obs0", values = list(obs = other, pred0 = cl$tn, pred1 = cl$fp, pct = 100 * cl$tn / (cl$tn + cl$fp)))
            tt$addRow(rowKey = "obs1", values = list(obs = event, pred0 = cl$fn, pred1 = cl$tp, pct = 100 * cl$tp / (cl$tp + cl$fn)))
            tt$getColumn("pred0")$setTitle(other); tt$getColumn("pred1")$setTitle(event)
            self$results$classStats$setRow(rowNo = 1, values = list(cutoff = o$cutoff, acc = cl$acc, sens = cl$sens, spec = cl$spec, auc = aucValue(y, prob)))
            if (isTRUE(o$roc)) self$results$roc$setState(list(roc = rocCurve(y, prob), auc = aucValue(y, prob)))
            if (isTRUE(o$vif)) {
                vt <- self$results$vif; v <- vifTable(fit)
                if (is.null(v)) vt$setNote("n", "VIF wymaga co najmniej dwóch kolumn predyktorów.")
                else for (i in seq_len(nrow(v))) vt$addRow(rowKey = v$term[i], values = list(term = if (!is.na(lab[v$term[i]])) lab[[v$term[i]]] else v$term[i], vif = v$vif[i], tol = v$tol[i]))
            }
            if (isTRUE(o$cooks)) { cs <- cooksSummary(fit); self$results$cooks$setRow(rowNo = 1, values = list(mean = cs$mean, max = cs$max, nHigh = cs$nHigh, thr = cs$thr)) }
            if (simple) self$results$plot$setState(list(x = d[[covs]], y = y, xlab = covs, eventLabel = event, fit = fit, covName = covs))
            complete <- stats::complete.cases(self$data[c(dep, covs, factors)])
            if (isTRUE(o$predictOV) && self$results$predictOV$isNotFilled()) { full <- rep(NA_real_, length(complete)); full[complete] <- prob; self$results$predictOV$setValues(full) }
        },
        .plot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); logisticSimplePlot(s$x, s$y, s$xlab, s$eventLabel, s$fit, s$covName, ggtheme, theme) },
        .rocPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); rocPlot(s$roc, s$auc, ggtheme, theme) }
    )
)
