#' @importFrom jmvcore .
jakoscioweClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "jakoscioweClass",
    inherit = jakoscioweBase,
    private = list(
        .built = FALSE,
        .levelsOf = function(name) {
            if (!optNonEmpty(name)) return(NULL)
            col <- self$data[[name]]; lv <- levels(col)
            if (is.null(lv) || length(lv) == 0) lv <- levels(droplevels(factor(col[!is.na(col)])))
            lv
        },
        .groupLevels = function() private$.levelsOf(self$options$splitBy),
        .layerLevels = function() private$.levelsOf(self$options$layer),
        # one table per variable, or per variable x layer level plus one for all layers together
        .parts = function(v) {
            ll <- private$.layerLevels()
            if (is.null(ll)) return(list(list(key = v, title = v, layer = NULL)))
            c(lapply(ll, function(l) list(key = paste0(v, " | ", l), title = paste0(v, " — ", self$options$layer, ": ", l), layer = l)),
              list(list(key = paste0(v, " | .all"), title = paste0(v, " — razem"), layer = NA_character_)))
        },
        .buildTables = function() {
            if (isTRUE(private$.built)) return()
            o <- self$options; gl <- private$.groupLevels()
            for (v in o$vars) for (pt in private$.parts(v)) {
                t <- self$results$freqs$get(key = pt$key)
                t$addColumn(name = "level", title = v, type = "text")
                if (is.null(gl)) {
                    t$addColumn(name = "n", title = "Liczność", type = "integer")
                    t$addColumn(name = "pct", title = "%", type = "number")
                    t$addColumn(name = "cumN", title = "Liczność skumulowana", type = "integer", visible = "(cum)")
                    t$addColumn(name = "cumPct", title = "% skumulowany", type = "number", visible = "(cum)")
                } else {
                    for (kind in c("pcCol", "pcRow", "pcTotal"))
                        t$addColumn(name = paste0("type_", kind), title = "", type = "text", visible = paste0("(", kind, ")"))
                    # one group header per level of the splitting variable, with n and % beneath it
                    for (i in seq_along(gl)) {
                        st <- paste0(o$splitBy, ": ", gl[i])
                        t$addColumn(name = paste0("n_", i), title = "n", superTitle = st, type = "integer")
                        t$addColumn(name = paste0("pcCol_", i), title = "%", superTitle = st, type = "number", visible = "(pcCol)")
                        t$addColumn(name = paste0("pcRow_", i), title = "%", superTitle = st, type = "number", visible = "(pcRow)")
                        t$addColumn(name = paste0("pcTotal_", i), title = "%", superTitle = st, type = "number", visible = "(pcTotal)")
                    }
                    t$addColumn(name = "n_tot", title = "n", superTitle = "Razem", type = "integer")
                    t$addColumn(name = "pcCol_tot", title = "%", superTitle = "Razem", type = "number", visible = "(pcCol)")
                    t$addColumn(name = "pcTotal_tot", title = "%", superTitle = "Razem", type = "number", visible = "(pcTotal)")
                }
            }
            private$.built <- TRUE
        },
        .init = function() {
            o <- self$options
            if (length(o$vars) == 0) return()
            nl <- length(private$.layerLevels())
            for (v in o$vars) {
                for (pt in private$.parts(v)) { self$results$freqs$addItem(key = pt$key); self$results$freqs$get(key = pt$key)$setTitle(pt$title) }
                for (arr in c("bars", "mosaics")) {
                    self$results[[arr]]$addItem(key = v)
                    # layers are drawn as panels side by side, at most three in a row
                    if (nl > 1) { nc <- min(nl, 3); self$results[[arr]]$get(key = v)$setSize(80 + 340 * nc, 100 + 320 * ceiling(nl / nc)) }
                }
            }
            private$.buildTables()   # items must exist before their columns are added
        },
        .fillTable = function(t, x, g, gl) {
            lv <- levels(x)
            if (length(x) == 0) { t$setNote("n", "Brak obserwacji."); return() }
            if (is.null(g)) {
                cnt <- as.integer(table(x)); N <- sum(cnt); cum <- cumsum(cnt)
                for (i in seq_along(lv))
                    t$addRow(rowKey = lv[i], values = list(level = lv[i], n = cnt[i], pct = 100 * cnt[i] / N, cumN = cum[i], cumPct = 100 * cum[i] / N))
                t$addRow(rowKey = ".total", values = list(level = "Razem", n = N, pct = 100))
            } else {
                tab <- table(x, g); N <- sum(tab); rs <- rowSums(tab); cs <- colSums(tab)
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
                    vals[[paste0("n_", j)]] <- cs[[j]]; vals[[paste0("pcCol_", j)]] <- if (cs[[j]] > 0) 100 else NA_real_
                    vals[[paste0("pcRow_", j)]] <- 100 * cs[[j]] / N; vals[[paste0("pcTotal_", j)]] <- 100 * cs[[j]] / N
                }
                t$addRow(rowKey = ".total", values = vals)
            }
            t$addFormat(rowKey = ".total", col = 1, jmvcore::Cell.BEGIN_END_GROUP)
        },
        .run = function() {
            o <- self$options
            if (length(o$vars) == 0) return()
            private$.buildTables()
            gl <- private$.groupLevels(); grouped <- !is.null(gl)
            ll <- private$.layerLevels(); layered <- !is.null(ll)
            gAll <- if (grouped) factor(self$data[[o$splitBy]], levels = gl) else NULL
            lAll <- if (layered) factor(self$data[[o$layer]], levels = ll) else NULL

            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienne: %s%s%s; kategorie w kolejności poziomów; braki pomijane osobno dla każdej zmiennej%s.", jmvcore::metodyCyt(o$vars),
                  if (grouped) sprintf("; podział według „%s” (%s)", o$splitBy, jmvcore::metodyCyt(gl)) else "",
                  if (layered) sprintf("; warstwy według „%s” (%s): osobna tabela dla każdego poziomu i tabela łączna", o$layer, jmvcore::metodyCyt(ll)) else "",
                  if (grouped || layered) " (obserwacja z brakiem w zmiennej grupującej pominięta)" else "")
            if (grouped) {
                m$addIf(o$pcCol, "Tabela", "% w kolumnie: liczność / suma kolumny (rozkład zmiennej w każdej grupie).")
                m$addIf(o$pcRow, "Tabela", "% w wierszu: liczność / suma wiersza (rozkład grup w każdej kategorii).")
                m$addIf(o$pcTotal, "Tabela", if (layered) "% ogółem: liczność / N warstwy." else "% ogółem: liczność / N.")
            } else m$addIf(o$cum, "Tabela", "Skumulowane: suma liczności i procentów do danej kategorii włącznie (ma sens dla kategorii uporządkowanych).")
            m$addIf(o$summary, "Tabela", "Podsumowanie: liczba kategorii z obserwacjami, dominanta = kategoria najliczniejsza (przy remisie pierwsza) i jej udział.")
            m$addIf(o$bar, "Wykres", "Słupkowy: liczności kategorii%s%s.", if (grouped) ", słupki obok siebie = grupy" else "", if (layered) ", panel dla każdej warstwy" else "")
            m$addIf(o$mosaic, "Wykres", "Mozaikowy: szerokość kolumny ∝ udział kategorii%s%s.", if (grouped) ", podział w pionie = udziały grup w kategorii" else "", if (layered) ", panel dla każdej warstwy" else "")
            m$render(self$results$metody)

            for (v in o$vars) {
                xf <- factor(self$data[[v]])
                base <- !is.na(xf); if (grouped) base <- base & !is.na(gAll); if (layered) base <- base & !is.na(lAll)
                lvX <- levels(droplevels(xf[base]))   # same categories in every layer, zeros included
                for (pt in private$.parts(v)) {
                    ok <- base; if (!is.null(pt$layer) && !is.na(pt$layer)) ok <- ok & lAll == pt$layer
                    x <- factor(xf[ok], levels = lvX)
                    private$.fillTable(self$results$freqs$get(key = pt$key), x, if (grouped) gAll[ok] else NULL, gl)
                    if (isTRUE(o$summary) && length(x) > 0) {
                        # missing values are counted against the layer they belong to
                        inLayer <- if (is.null(pt$layer) || is.na(pt$layer)) TRUE else (!is.na(lAll) & lAll == pt$layer)
                        cnt <- table(x); md <- names(cnt)[which.max(cnt)]
                        self$results$summary$addRow(rowKey = pt$key, values = list(var = v,
                            layer = if (is.null(pt$layer)) "" else if (is.na(pt$layer)) "Razem" else pt$layer,
                            n = length(x), missing = sum(is.na(xf) & inLayer),
                            k = sum(cnt > 0), mode = md, modePct = 100 * max(cnt) / length(x)))
                    }
                }
                x <- droplevels(xf[base]); d <- data.frame(x = x)
                if (grouped) d$g <- gAll[base]
                if (layered) d$l <- lAll[base]
                s <- list(d = d, var = v, groupVar = if (grouped) o$splitBy else NULL, layerVar = if (layered) o$layer else NULL)
                if (isTRUE(o$bar)) self$results$bars$get(key = v)$setState(s)
                if (isTRUE(o$mosaic)) self$results$mosaics$get(key = v)$setState(s)
            }
        },
        .barPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); barPlotQual(s$d, s$var, s$groupVar, ggtheme, theme, s$layerVar) },
        .mosaicPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); mosaicPlotQual(s$d, s$var, s$groupVar, ggtheme, theme, s$layerVar) }
    )
)
