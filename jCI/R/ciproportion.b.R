#' @importFrom jmvcore .
ciproportionClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "ciproportionClass",
    inherit = ciproportionBase,
    private = list(
        .run = function() {
            o <- self$options
            if (!optNonEmpty(o$dep)) return()
            level <- o$ciWidth / 100; t <- self$results$table; method <- o$ciMethod
            column <- self$data[[o$dep]]
            lv <- pickLevel(column, o$level); if (is.null(lv)) return()
            x <- as.character(column); x <- x[!is.na(x)]; n <- length(x)
            if (n < 1) { t$setNote("err", "Brak obserwacji."); return() }
            succ <- as.integer(x == lv); k <- sum(succ); phat <- k / n

            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienna „%s”; „sukces” = kategoria „%s”%s; N = %d (braki pominięte), liczba sukcesów = %d.",
                  o$dep, lv, if (!optNonEmpty(o$level)) " (pierwszy poziom — wybierz inny w panelu)" else "", n, k)
            metodyPrzedzial(m, o, method, switch(method,
                wilson = "Przedział Wilsona (score): środek przesunięty ku ½, działa też przy małych n i skrajnych proporcjach",
                clopperPearson = "Przedział Cloppera-Pearsona (dokładny, z rozkładu dwumianowego; konserwatywny)",
                wald = "Przedział Walda: p̂ ± z · √(p̂(1 − p̂)/n) — zawodny przy małych n i p̂ blisko 0 lub 1", ""),
                "losowanie n obserwacji (sukces/porażka) ze zwracaniem, statystyka = udział sukcesów")
            m$addIf(o$plot, "Wykres", "Wafel 100 pól: ciemne = udział poniżej dolnej granicy, jasne = przedział ufności, szare = reszta.")
            m$addIf(o$bootPlot && isBoot(method), "Wykres", "Histogram replikacji bootstrapowych udziału.")
            m$render(self$results$metody)

            fallback <- FALSE
            if (isBoot(method)) {
                r <- bootCI(succ, function(v, i) mean(v[i]), o$nBoot, o$seed, method, level); fallback <- r$fallback
                self$results$bootPlot$setState(list(reps = r$reps, est = phat, lower = r$lower, upper = r$upper, xlab = "Udział"))
                clab <- NULL
            } else {
                ci <- ciProportion(k, n, level, method); r <- list(lower = ci$lower, upper = ci$upper)
                clab <- switch(method, wilson = "Wilsona", clopperPearson = "Cloppera-Pearsona", wald = "Walda")
            }
            t$setRow(rowNo = 1, values = list(var = o$dep, level = lv, count = k, total = n, estimate = phat, lower = r$lower, upper = r$upper))
            ciNote(t, o, method, clab, fallback)
            self$results$plot$setState(list(label = paste0(o$dep, " = ", lv), estimate = phat, lower = r$lower, upper = r$upper, ciWidth = o$ciWidth))
        },
        .ciPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildProportionIconPlot(s$label, s$estimate, s$lower, s$upper, s$ciWidth, ggtheme, theme)
        },
        .bootPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildBootHist(s$reps, s$est, s$lower, s$upper, s$xlab, ggtheme, theme)
        }
    )
)
