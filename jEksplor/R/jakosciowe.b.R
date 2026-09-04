#' @importFrom jmvcore .
jakoscioweClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "jakoscioweClass",
    inherit = jakoscioweBase,
    private = list(
        .built = FALSE,
        .groupLevels = function() {
            o <- self$options
            if (!optNonEmpty(o$splitBy)) return(NULL)
            col <- self$data[[o$splitBy]]; lv <- levels(col)
            if (is.null(lv) || length(lv) == 0) lv <- levels(droplevels(factor(col[!is.na(col)])))
            lv
        },
        .buildTables = function() {
            if (isTRUE(private$.built)) return()
            o <- self$options; gl <- private$.groupLevels()
            for (v in o$vars) {
                t <- self$results$freqs$get(key = v)
                t$addColumn(name = "level", title = v, type = "text")
                if (is.null(gl)) {
                    t$addColumn(name = "n", title = "Liczność", type = "integer")
                    t$addColumn(name = "pct", title = "%", type = "number")
                    t$addColumn(name = "cumN", title = "Liczność skumulowana", type = "integer", visible = "(cum)")
                    t$addColumn(name = "cumPct", title = "% skumulowany", type = "number", visible = "(cum)")
                } else {
                    for (kind in c("pcCol", "pcRow", "pcTotal"))
                        t$addColumn(name = paste0("type_", kind), title = "", type = "text", visible = paste0("(", kind, ")"))
                    for (i in seq_along(gl)) {
                        t$addColumn(name = paste0("n_", i), title = gl[i], superTitle = o$splitBy, type = "integer")
                        t$addColumn(name = paste0("pcCol_", i), title = gl[i], superTitle = o$splitBy, type = "number", visible = "(pcCol)")
                        t$addColumn(name = paste0("pcRow_", i), title = gl[i], superTitle = o$splitBy, type = "number", visible = "(pcRow)")
                        t$addColumn(name = paste0("pcTotal_", i), title = gl[i], superTitle = o$splitBy, type = "number", visible = "(pcTotal)")
                    }
                    t$addColumn(name = "n_tot", title = "Razem", type = "integer")
                    t$addColumn(name = "pcCol_tot", title = "Razem", type = "number", visible = "(pcCol)")
                    t$addColumn(name = "pcTotal_tot", title = "Razem", type = "number", visible = "(pcTotal)")
                }
            }
            private$.built <- TRUE
        },
        .init = function() {
            o <- self$options
            if (length(o$vars) == 0) return()
            for (v in o$vars) {
                for (arr in c("freqs", "bars", "mosaics")) { self$results[[arr]]$addItem(key = v); self$results[[arr]]$get(key = v)$setTitle(v) }
            }
            private$.buildTables()   # items must exist before their columns are added
        },
        .run = function() {
            o <- self$options
            if (length(o$vars) == 0) return()
            private$.buildTables()
            gl <- private$.groupLevels(); grouped <- !is.null(gl)
            gAll <- if (grouped) factor(self$data[[o$splitBy]], levels = gl) else NULL

            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienne: %s%s; kategorie w kolejności poziomów; braki pomijane osobno dla każdej zmiennej%s.", jmvcore::metodyCyt(o$vars),
                  if (grouped) sprintf("; podział według „%s” (%s)", o$splitBy, jmvcore::metodyCyt(gl)) else "",
                  if (grouped) " (obserwacja z brakiem w zmiennej grupującej pominięta)" else "")
            if (grouped) {
                m$addIf(o$pcCol, "Tabela", "% w kolumnie: liczność / suma kolumny (rozkład zmiennej w każdej grupie).")
                m$addIf(o$pcRow, "Tabela", "% w wierszu: liczność / suma wiersza (rozkład grup w każdej kategorii).")
                m$addIf(o$pcTotal, "Tabela", "% ogółem: liczność / N.")
            } else m$addIf(o$cum, "Tabela", "Skumulowane: suma liczności i procentów do danej kategorii włącznie (ma sens dla kategorii uporządkowanych).")
            m$addIf(o$summary, "Tabela", "Podsumowanie: liczba kategorii z obserwacjami, dominanta = kategoria najliczniejsza (przy remisie pierwsza) i jej udział.")
            m$addIf(o$bar, "Wykres", "Słupkowy: liczności kategorii%s.", if (grouped) ", słupki obok siebie = grupy" else "")
            m$addIf(o$mosaic, "Wykres", "Mozaikowy: szerokość kolumny ∝ udział kategorii%s.", if (grouped) ", podział w pionie = udziały grup w kategorii" else "")
            m$render(self$results$metody)

            for (v in o$vars) {
                raw <- self$data[[v]]; xf <- factor(raw)
                ok <- !is.na(xf); if (grouped) ok <- ok & !is.na(gAll)
                x <- droplevels(xf[ok]); lv <- levels(x); t <- self$results$freqs$get(key = v)
                if (length(x) == 0) { t$setNote("n", "Brak obserwacji."); next }
                if (!grouped) {
                    cnt <- as.integer(table(x)); N <- sum(cnt); cum <- cumsum(cnt)
                    for (i in seq_along(lv))
                        t$addRow(rowKey = lv[i], values = list(level = lv[i], n = cnt[i], pct = 100 * cnt[i] / N, cumN = cum[i], cumPct = 100 * cum[i] / N))
                    t$addRow(rowKey = ".total", values = list(level = "Razem", n = N, pct = 100))
                    t$addFormat(rowKey = ".total", col = 1, jmvcore::Cell.BEGIN_END_GROUP)
                } else {
                    g <- gAll[ok]; tab <- table(x, g); N <- sum(tab); rs <- rowSums(tab); cs <- colSums(tab)
                    for (i in seq_along(lv)) {
                        vals <- list(level = lv[i], type_pcCol = "% w kolumnie", type_pcRow = "% w wierszu", type_pcTotal = "% ogółem",
                                     n_tot = rs[[i]], pcCol_tot = 100 * rs[[i]] / N, pcTotal_tot = 100 * rs[[i]] / N)
                        for (j in seq_along(gl)) {
                            vals[[paste0("n_", j)]] <- tab[i, j]
                            vals[[paste0("pcCol_", j)]] <- if (cs[j] > 0) 100 * tab[i, j] / cs[j] else NA_real_
                            vals[[paste0("pcRow_", j)]] <- if (rs[i] > 0) 100 * tab[i, j] / rs[i] else NA_real_
                            vals[[paste0("pcTotal_", j)]] <- 100 * tab[i, j] / N
                        }
                        t$addRow(rowKey = lv[i], values = vals)
                    }
                    vals <- list(level = "Razem", type_pcCol = "% w kolumnie", type_pcRow = "% w wierszu", type_pcTotal = "% ogółem",
                                 n_tot = N, pcCol_tot = 100, pcTotal_tot = 100)
                    for (j in seq_along(gl)) {
                        vals[[paste0("n_", j)]] <- cs[[j]]; vals[[paste0("pcCol_", j)]] <- 100
                        vals[[paste0("pcRow_", j)]] <- 100 * cs[[j]] / N; vals[[paste0("pcTotal_", j)]] <- 100 * cs[[j]] / N
                    }
                    t$addRow(rowKey = ".total", values = vals)
                    t$addFormat(rowKey = ".total", col = 1, jmvcore::Cell.BEGIN_END_GROUP)
                }
                if (isTRUE(o$summary)) {
                    cnt <- table(x); md <- names(cnt)[which.max(cnt)]
                    self$results$summary$addRow(rowKey = v, values = list(var = v, n = length(x), missing = sum(is.na(xf)),
                        k = sum(cnt > 0), mode = md, modePct = 100 * max(cnt) / length(x)))
                }
                d <- data.frame(x = x); if (grouped) d$g <- gAll[ok]
                if (isTRUE(o$bar)) self$results$bars$get(key = v)$setState(list(d = d, var = v, groupVar = if (grouped) o$splitBy else NULL))
                if (isTRUE(o$mosaic)) self$results$mosaics$get(key = v)$setState(list(d = d, var = v, groupVar = if (grouped) o$splitBy else NULL))
            }
        },
        .barPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); barPlotQual(s$d, s$var, s$groupVar, ggtheme, theme) },
        .mosaicPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); mosaicPlotQual(s$d, s$var, s$groupVar, ggtheme, theme) }
    )
)
