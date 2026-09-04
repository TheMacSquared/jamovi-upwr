#' @importFrom jmvcore .
korelacjaClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "korelacjaClass",
    inherit = korelacjaBase,
    private = list(
        .built = FALSE,
        .buildMatrix = function() {
            if (isTRUE(private$.built)) return()
            o <- self$options; t <- self$results$matrix; k <- length(o$vars)
            # lower triangle only: rows = variables 2..k, columns = variables 1..k-1
            t$addColumn(name = "var", title = "", type = "text", combineBelow = TRUE)
            t$addColumn(name = "kind", title = "", type = "text")
            for (v in o$vars[-k]) t$addColumn(name = paste0("c_", v), title = v, type = "text")
            private$.built <- TRUE
        },
        .init = function() {
            o <- self$options
            if (length(o$vars) >= 3) private$.buildMatrix()
        },
        .run = function() {
            o <- self$options; vars <- o$vars
            if (length(vars) < 2) return()
            level <- o$ciWidth / 100; pairMode <- length(vars) == 2
            self$results$pair$setVisible(pairMode); self$results$plot$setVisible(pairMode && isTRUE(o$plot))
            self$results$matrix$setVisible(!pairMode); self$results$plotMatrix$setVisible(!pairMode && isTRUE(o$plot))
            d <- lapply(vars, function(v) jmvcore::toNumeric(self$data[[v]])); names(d) <- vars

            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienne: %s; każda para liczona na obserwacjach bez braków w tej parze.", jmvcore::metodyCyt(vars))
            m$add("Testy", "Współczynnik: %s%s; p z cor.test (%s); H₁: %s.", corMethodLabel(o$method),
                  switch(o$method, spearman = " (korelacja rang)", kendall = " (zgodność par, z poprawką na wiązania)", ""),
                  switch(o$method, pearson = "test t, df = n − 2", spearman = "przybliżenie t bez wersji dokładnej", kendall = "przybliżenie normalne"),
                  switch(o$hypothesis, pos = "korelacja > 0", neg = "korelacja < 0", "korelacja ≠ 0 (dwustronna)"))
            m$addIf(o$ci && o$method != "kendall", "Testy", "Przedział ufności %g%%: transformacja Fishera z%s.", o$ciWidth,
                    if (o$method == "spearman") " z błędem standardowym Bonetta-Wrighta √((1 + ρ²/2)/(n − 3))" else ", błąd 1/√(n − 3)")
            m$addIf(o$ci && o$method == "kendall", "Testy", "Dla τ-b Kendalla przedział ufności nie jest liczony.")
            m$addIf(o$flag && !pairMode, "Testy", "Gwiazdki w macierzy: * p < 0.05, ** p < 0.01, *** p < 0.001.")
            m$addIf(o$plot, "Wykres", if (pairMode) "Rozrzut%s." else "Macierz rozrzutów: jeden panel na parę, w tytule współczynnik%s.",
                    if (o$method == "pearson") " z prostą regresji MNK" else " (bez prostej — współczynnik rangowy)")
            m$render(self$results$metody)

            if (pairMode) {
                r <- corPair(d[[1]], d[[2]], o$method, level, o$hypothesis)
                t <- self$results$pair
                t$getColumn("r")$setTitle(switch(o$method, pearson = "r", spearman = "ρ", kendall = "τ-b"))
                t$setRow(rowNo = 1, values = list(var1 = vars[1], var2 = vars[2], n = r$n, r = r$r, lower = r$lower, upper = r$upper, p = r$p))
                if (r$n < 3) t$setNote("n", "Za mało kompletnych par (n < 3).")
                if (isTRUE(o$ci) && o$method == "kendall") t$setNote("k", "Bez przedziału ufności dla τ-b.")
                ok <- !is.na(d[[1]]) & !is.na(d[[2]])
                self$results$plot$setState(list(x = d[[1]][ok], y = d[[2]][ok], xlab = vars[1], ylab = vars[2], r = r$r, method = o$method))
            } else {
                private$.buildMatrix(); t <- self$results$matrix; k <- length(vars)
                coefs <- list(); fmtP <- function(p) if (!is.finite(p)) "" else if (p < 0.001) "< .001" else sprintf("%.3f", p)
                kinds <- c(r = switch(o$method, pearson = "r", spearman = "ρ", kendall = "τ-b"), p = "p", n = "N")
                show <- c("r", "p", if (isTRUE(o$showN)) "n")
                for (i in 2:k) for (kk in show) {
                    vals <- list(var = vars[i], kind = kinds[[kk]])
                    for (j in seq_len(i - 1)) {
                        rr <- corPair(d[[i]], d[[j]], o$method, level, o$hypothesis)
                        coefs[[paste(vars[j], vars[i])]] <- rr$r
                        stars <- if (isTRUE(o$flag) && is.finite(rr$p)) (if (rr$p < 0.001) "***" else if (rr$p < 0.01) "**" else if (rr$p < 0.05) "*" else "") else ""
                        vals[[paste0("c_", vars[j])]] <- switch(kk, r = if (is.finite(rr$r)) paste0(sprintf("%.3f", rr$r), stars) else "",
                                                                 p = fmtP(rr$p), n = as.character(rr$n))
                    }
                    t$addRow(rowKey = paste(vars[i], kk), values = vals)
                }
                t$setNote("tri", "Dolny trójkąt macierzy; p pod współczynnikiem.")
                dd <- as.data.frame(d); dd <- dd[stats::complete.cases(dd), , drop = FALSE]
                self$results$plotMatrix$setState(list(d = dd, coefs = coefs, method = o$method))
            }
        },
        .pairPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); scatterPairPlot(s$x, s$y, s$xlab, s$ylab, s$r, s$method, ggtheme, theme) },
        .matrixPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); scatterMatrixPlot(s$d, s$coefs, s$method, ggtheme, theme) }
    )
)
