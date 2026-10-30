#' @importFrom jmvcore .
tabelaClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "tabelaClass",
    inherit = tabelaBase,
    private = list(

        # kolumny tabeli liczności i reszt zależą od poziomów zmiennej kolumnowej.
        # UWAGA: w GUI `self$data` bywa jeszcze puste w .init (wtedy poziomów nie da
        # się odczytać), a przy wywołaniu z R dane są od razu. Dlatego budujemy
        # kolumny z OBU miejsc, a flaga chroni przed dodaniem ich dwa razy.
        .colsBuilt = FALSE,
        # opis zastosowanych metod (jmvcore::metodyNew, wspólny mechanizm jUPWR) — zbierany po drodze, renderowany na końcu .run
        .metody = NULL,

        .buildColumns = function() {
            if (isTRUE(private$.colsBuilt)) return(invisible(FALSE))
            o <- self$options
            if (!optNonEmpty(o$rows) || !optNonEmpty(o$cols)) return(invisible(FALSE))
            lv <- private$.colLevels()
            if (length(lv) == 0) return(invisible(FALSE))

            layered <- optNonEmpty(o$layer)
            freqs <- self$results$freqs
            if (layered) freqs$addColumn(name = "layer", title = o$layer, type = "text", combineBelow = TRUE)
            freqs$addColumn(name = "row", title = o$rows, type = "text", combineBelow = TRUE)
            freqs$addColumn(name = "kind", title = "", type = "text")
            for (l in lv)
                freqs$addColumn(name = paste0("c_", l), title = l, type = "number",
                                superTitle = o$cols)
            freqs$addColumn(name = "total", title = "Ogółem", type = "number")

            res <- self$results$resid
            if (layered) res$addColumn(name = "layer", title = o$layer, type = "text", combineBelow = TRUE)
            res$addColumn(name = "row", title = o$rows, type = "text")
            for (l in lv)
                res$addColumn(name = paste0("c_", l), title = l, type = "number",
                              superTitle = o$cols)

            private$.colsBuilt <- TRUE
            invisible(TRUE)
        },

        .init = function() {
            private$.buildColumns()
            if (optNonEmpty(self$options$layer))
                for (tn in c("tests", "effsize", "measures", "ordinal", "trend", "pairwise"))
                    self$results[[tn]]$getColumn("layer")$setTitle(self$options$layer)
            nl <- length(private$.layerLevels())
            if (nl > 1) { nc <- min(nl, 3); self$results$plot$setSize(60 + 340 * nc, 100 + 320 * ceiling(nl / nc)) }
        },

        .colLevels = function() {
            o <- self$options
            v <- try(self$data[[o$cols]], silent = TRUE)
            if (inherits(v, "try-error") || is.null(v)) return(character(0))
            # W .init jamovi podaje ramkę z kolumnami, ale BEZ wierszy — poziomy
            # trzeba wziąć z deklaracji czynnika, nie z obserwowanych wartości,
            # inaczej tabela liczności zostaje bez kolumn (i wygląda na pustą).
            lv <- levels(v)
            if (is.null(lv) || length(lv) == 0)
                lv <- levels(droplevels(factor(v[!is.na(v)])))
            lv
        },

        # layer = NULL: all observations (with a layer, only those with a non-missing layer)
        .table = function(layer = NULL) {
            o <- self$options
            r <- self$data[[o$rows]]; c0 <- self$data[[o$cols]]
            cnt <- if (optNonEmpty(o$counts)) jmvcore::toNumeric(self$data[[o$counts]]) else NULL
            if (optNonEmpty(o$layer)) {
                l <- factor(self$data[[o$layer]])
                keep <- !is.na(l); if (!is.null(layer)) keep <- keep & l == layer
                r <- r[keep]; c0 <- c0[keep]; if (!is.null(cnt)) cnt <- cnt[keep]
            }
            buildTable(r, c0, cnt)
        },

        .layerLevels = function() {
            o <- self$options
            if (!optNonEmpty(o$layer)) return(NULL)
            l <- self$data[[o$layer]]; lv <- levels(l)
            if (is.null(lv) || length(lv) == 0) lv <- levels(droplevels(factor(l[!is.na(l)])))
            lv
        },

        # row key and layer cell of a row in a given part (lay = NULL without layers)
        .rk = function(lay, key) if (is.null(lay)) key else paste0(lay, " | ", key),
        .lv = function(lay, vals) { if (!is.null(lay)) vals$layer <- lay; vals },

        .run = function() {
            o <- self$options
            if (!optNonEmpty(o$rows) || !optNonEmpty(o$cols)) return()
            private$.buildColumns()   # w GUI dane bywają dostępne dopiero tutaj
            tab <- private$.table()
            if (any(dim(tab) < 2) || sum(tab) == 0) {
                self$results$tests$setNote("n", "Każda zmienna musi mieć co najmniej 2 poziomy z obserwacjami.")
                return()
            }

            # warstwy: osobna tabela dla każdego poziomu, na końcu „Ogółem” (wszystkie warstwy razem);
            # opis metod zbierany tylko z tabeli łącznej (w warstwach te same metody)
            ll <- private$.layerLevels()
            parts <- c(lapply(ll, function(l) list(lay = l, tab = private$.table(l))),
                       list(list(lay = if (is.null(ll)) NULL else "Ogółem", tab = tab)))
            warn <- character(0); plotTabs <- list(); small <- character(0)
            for (pt in parts) {
                last <- identical(pt, parts[[length(parts)]])
                private$.metody <- jmvcore::metodyNew()
                if (last) private$.describeData(tab)
                private$.fillFreqs(pt$tab, pt$lay, ref = tab)
                if (any(dim(pt$tab) < 2) || sum(pt$tab) == 0) { small <- c(small, pt$lay); next }
                w <- private$.assumptionNotice(pt$tab)
                if (!is.null(w)) warn <- c(warn, if (is.null(pt$lay)) w else sprintf("Warstwa „%s”: %s", pt$lay, w))
                private$.fillTests(pt$tab, pt$lay)
                private$.fillEffSize(pt$tab, pt$lay)
                private$.fillMeasures(pt$tab, pt$lay)
                private$.fillOrdinal(pt$tab, pt$lay)
                private$.fillTrend(pt$tab, pt$lay)
                private$.fillResid(pt$tab, pt$lay)
                private$.fillPairwise(pt$tab, pt$lay)
                if (!last) plotTabs[[pt$lay]] <- pt$tab
            }
            if (length(small) > 0)
                self$results$tests$setNote("layers", sprintf("Warstwy bez testów (mniej niż 2 poziomy z obserwacjami): %s.",
                                                             paste0("„", small, "”", collapse = ", ")))
            if (length(warn) > 0)
                self$results$insert(1, jmvcore::Notice$new(self$options, name = ".assumption",
                                                           type = jmvcore::NoticeType$WARNING, content = paste(warn, collapse = " ")))

            if (o$plot != "none")
                self$results$plot$setState(list(tab = tab, tabs = if (length(plotTabs) > 0) plotTabs else NULL,
                                                layerVar = o$layer, kind = o$plot, pc = o$pc,
                                                xlab = o$rows, fill = o$cols))
            private$.describePlot()
            private$.metody$render(self$results$metody)
        },

        # --- opis metod: dane i prezentacja (reszta dopisywana w .fill*) ---
        .describeData = function(tab) {
            o <- self$options
            m <- private$.metody
            m$add("Dane", "Wiersze: „%s”, kolumny: „%s”; tabela %d × %d (poziomy w kolejności alfabetycznej / zadeklarowanej).",
                  o$rows, o$cols, nrow(tab), ncol(tab))
            m$addIf(optNonEmpty(o$counts), "Dane",
                    "Dane zagregowane: liczności z kolumny „%s”.", o$counts)
            m$addIf(optNonEmpty(o$layer), "Dane",
                    "Warstwy według „%s” (%s): tabela, testy i miary osobno w każdej warstwie oraz dla wszystkich warstw razem („Ogółem”).",
                    o$layer, paste0("„", private$.layerLevels(), "”", collapse = ", "))
            m$add("Dane", "N = %s; pominięto obserwacje z brakiem w którejkolwiek ze zmiennych.",
                  format(sum(tab), big.mark = " "))
            m$add("Dane", switch(o$pc,
                row = "Procenty liczone wierszami (mianownik: suma wiersza).",
                col = "Procenty liczone kolumnami (mianownik: suma kolumny).",
                total = "Procenty od ogółu (mianownik: N).",
                "Bez procentów — same liczności."))
            m$addIf(o$exp, "Dane",
                    "Liczebności oczekiwane przy niezależności: E = (suma wiersza × suma kolumny) / N.")
        },

        .describePlot = function() {
            o <- self$options
            if (o$plot == "bar")
                private$.metody$add("Wykres", "Wykres słupkowy: słupki = %s, grupy = „%s”, kolor = „%s”.",
                    switch(o$pc, row = "% wierszem", col = "% kolumną", "liczności"), o$rows, o$cols)
            else if (o$plot == "mosaic")
                private$.metody$add("Wykres", paste(
                    "Wykres mozaikowy: szerokość kolumny ∝ liczność wiersza (brzeg),",
                    "podział w pionie = rozkład warunkowy w wierszu."))
        },

        # --- tabela liczności: obserwowane, oczekiwane, procenty ---
        # ref: tabela łączna — warstwa pokazuje te same wiersze i kolumny (zera w pustych komórkach)
        .fillFreqs = function(tab, lay = NULL, ref = tab) {
            o <- self$options
            t <- self$results$freqs
            if (!identical(dimnames(tab), dimnames(ref))) {
                full <- ref; full[] <- 0
                ri <- intersect(rownames(tab), rownames(ref)); ci <- intersect(colnames(tab), colnames(ref))
                full[ri, ci] <- tab[ri, ci]; tab <- full
            }
            lv <- colnames(tab)
            e <- expectedCounts(tab)
            pcLab <- switch(o$pc, row = "% wierszem", col = "% kolumną", total = "% ogółu", NULL)

            for (i in seq_len(nrow(tab))) {
                rn <- rownames(tab)[i]
                vals <- list(row = rn, kind = "Liczność")
                for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- tab[i, j]
                vals$total <- sum(tab[i, ])
                t$addRow(rowKey = private$.rk(lay, paste0(rn, "_n")), values = private$.lv(lay, vals))

                if (isTRUE(o$exp)) {
                    vals <- list(row = rn, kind = "Oczekiwana")
                    for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- e[i, j]
                    vals$total <- sum(e[i, ])
                    t$addRow(rowKey = private$.rk(lay, paste0(rn, "_e")), values = private$.lv(lay, vals))
                }

                if (!is.null(pcLab)) {
                    den <- switch(o$pc, row = sum(tab[i, ]), total = sum(tab), col = NA)
                    vals <- list(row = rn, kind = pcLab)
                    for (j in seq_along(lv)) {
                        d <- if (o$pc == "col") sum(tab[, j]) else den
                        vals[[paste0("c_", lv[j])]] <- if (d > 0) 100 * tab[i, j] / d else NA_real_
                    }
                    vals$total <- if (o$pc == "row") { if (sum(tab[i, ]) > 0) 100 else NA_real_ } else
                        if (o$pc == "total") 100 * sum(tab[i, ]) / sum(tab) else NA_real_
                    t$addRow(rowKey = private$.rk(lay, paste0(rn, "_p")), values = private$.lv(lay, vals))
                }
            }

            vals <- list(row = "Ogółem", kind = "Liczność")
            for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- sum(tab[, j])
            vals$total <- sum(tab)
            t$addRow(rowKey = private$.rk(lay, "._total"), values = private$.lv(lay, vals))
            if (!is.null(lay)) t$addFormat(rowKey = private$.rk(lay, "._total"), col = 1, jmvcore::Cell.END_GROUP)
        },

        # --- SEDNO MODUŁU: automatyczna kontrola warunku E >= 5 ---
        .assumptionNotice = function(tab) {
            a <- checkAssumption(tab)
            private$.metody$add("Testy", paste(
                "Warunek stosowalności χ² sprawdzany automatycznie: dla 2×2 wszystkie E ≥ 5,",
                "dla większych tabel wszystkie E ≥ 1 i najwyżej 20%% komórek z E &lt; 5 (Cochran) — tu %s."),
                if (isTRUE(a$ok)) "spełniony" else "niespełniony (ostrzeżenie nad wynikami)")
            if (isTRUE(a$ok)) return(NULL)
            if (a$is2x2)
                sprintf(paste("W tabeli 2×2 najmniejsza liczebność oczekiwana wynosi %.2f (< 5),",
                              "więc przybliżenie χ² jest zawodne.%s"),
                        a$minExpected,
                        if (a$fisherFeasible) " Włącz dokładny test Fishera." else "")
            else
                sprintf(paste("%d z %d komórek (%.0f%%) ma liczebność oczekiwaną < 5,",
                              "a najmniejsza wynosi %.2f — przybliżenie χ² jest zawodne.%s"),
                        a$nBelow5, a$nCells, a$pctBelow5, a$minExpected,
                        if (a$fisherFeasible) " Rozważ dokładny test Fishera lub połączenie kategorii."
                        else " Rozważ połączenie kategorii.")
        },

        .fillTests = function(tab, lay = NULL) {
            o <- self$options
            t <- self$results$tests
            n <- sum(tab)
            m <- private$.metody
            if (isTRUE(o$chiSq)) {
                r <- chiSqTest(tab, correct = FALSE)
                t$addRow(rowKey = private$.rk(lay, "chi"), values = private$.lv(lay, list(test = "χ²", stat = r$stat, df = r$df, p = r$p)))
                m$add("Testy", "χ² Pearsona bez poprawki ciągłości, df = (r − 1)(c − 1) = %d.", r$df)
            }
            if (isTRUE(o$chiSqCorr)) {
                if (all(dim(tab) == c(2, 2))) {
                    r <- chiSqTest(tab, correct = TRUE)
                    m$add("Testy", "χ² z poprawką ciągłości Yatesa (|O − E| pomniejszone o 0.5).")
                    t$addRow(rowKey = private$.rk(lay, "chic"), values = private$.lv(lay, list(test = "χ² z poprawką ciągłości",
                                                           stat = r$stat, df = r$df, p = r$p)))
                } else {
                    t$setNote("cc", "Poprawka ciągłości dotyczy wyłącznie tabel 2×2.")
                }
            }
            if (isTRUE(o$likeRat)) {
                r <- likeRatTest(tab)
                t$addRow(rowKey = private$.rk(lay, "lr"), values = private$.lv(lay, list(test = "G² (iloraz wiarygodności)",
                                                     stat = r$stat, df = r$df, p = r$p)))
                m$add("Testy", "G² (iloraz wiarygodności) = 2 Σ O·ln(O/E), te same df co χ².")
            }
            if (isTRUE(o$fisher)) {
                ft <- try(stats::fisher.test(tab), silent = TRUE)
                if (inherits(ft, "try-error"))
                    t$setNote("f", "Dokładny test Fishera nie policzył się dla tej tabeli (za duża).")
                else {
                    t$addRow(rowKey = private$.rk(lay, "fi"), values = private$.lv(lay, list(test = "Dokładny test Fishera", p = ft$p.value)))
                    m$add("Testy", "Dokładny test Fishera (bez statystyki testowej; dla tabel &gt; 2×2 uogólnienie Freemana-Haltona).")
                }
            }
            # N w nocie, nie jako wiersz: liczebnosc dzielilaby kolumne ze statystykami
            # testowymi i dziedziczyla ich format (80.00000), a nie jest testem
            if (is.null(lay) || lay == "Ogółem") t$setNote("N", sprintf("N = %s.", format(n, big.mark = " ")))
        },

        .fillEffSize = function(tab, lay = NULL) {
            o <- self$options
            if (!isTRUE(o$effSize)) return()
            t <- self$results$effsize
            v <- cramersV(tab)
            ci <- if (isTRUE(o$effSizeCI)) cramersVCI(tab, level = o$ciWidth / 100) else c(NA_real_, NA_real_)
            # progi Cohena zależą od mniejszego wymiaru tabeli (df* = min(r,c) - 1)
            dfStar <- min(dim(tab)) - 1
            small <- 0.1 / sqrt(dfStar); med <- 0.3 / sqrt(dfStar); large <- 0.5 / sqrt(dfStar)
            interp <- if (!is.finite(v)) "" else if (v < small) "poniżej słabego"
                      else if (v < med) "słaby" else if (v < large) "umiarkowany" else "silny"
            t$addRow(rowKey = private$.rk(lay, "v"), values = private$.lv(lay, list(measure = "V Craméra", value = v,
                                                 lower = ci[1], upper = ci[2], interp = interp)))
            m <- private$.metody
            m$add("Wielkość efektu", paste(
                "V Craméra = √(χ² / (N · (min(r, c) − 1))); interpretacja wg progów Cohena",
                "dla tej tabeli: słaby %.2f, umiarkowany %.2f, silny %.2f."), small, med, large)
            m$addIf(o$effSizeCI, "Wielkość efektu", paste(
                "Przedział ufności %g%% dla V: bootstrap percentylowy, 1000 losowań",
                "z rozkładu wielomianowego, ziarno 1."), o$ciWidth)
        },

        .fillMeasures = function(tab, lay = NULL) {
            o <- self$options
            if (!(isTRUE(o$odds) || isTRUE(o$relRisk) || isTRUE(o$diffProp))) return()
            t <- self$results$measures
            if (!all(dim(tab) == c(2, 2))) {
                t$setNote("d", "Te miary są określone tylko dla tabel 2×2.")
                return()
            }
            m <- twoByTwo(tab, level = o$ciWidth / 100, compare = o$compare)
            add <- function(key, lab, x)
                t$addRow(rowKey = private$.rk(lay, key), values = private$.lv(lay, list(measure = lab, value = x$est,
                                                     lower = x$lower, upper = x$upper)))
            if (isTRUE(o$odds))     add("or", "Iloraz szans (OR)", m$or)
            if (isTRUE(o$relRisk))  add("rr", "Ryzyko względne (RR)", m$rr)
            if (isTRUE(o$diffProp)) add("dp", "Różnica proporcji", m$dp)

            # Kierunek miar zależy od kolejności poziomów, a ta jest alfabetyczna —
            # bez tego opisu student nie wie, czy dostał OR czy jego odwrotność.
            grp <- if (o$compare == "cols") colnames(tab) else rownames(tab)
            ev  <- if (o$compare == "cols") rownames(tab) else colnames(tab)
            m <- private$.metody
            m$add("Miary 2×2", paste(
                "Porównanie: „%s” względem „%s” pod względem udziału kategorii „%s”;",
                "poziomy w kolejności alfabetycznej — odwrotna kolejność odwraca OR i RR."),
                grp[1], grp[2], ev[1])
            m$add("Miary 2×2", "Przedziały ufności %g%% metodą Walda: na skali logarytmicznej dla OR i RR, na skali proporcji dla różnicy.",
                  o$ciWidth)
        },

        .fillOrdinal = function(tab, lay = NULL) {
            o <- self$options
            if (!(isTRUE(o$gamma) || isTRUE(o$taub))) return()
            t <- self$results$ordinal
            m <- ordinalMeasures(tab)
            if (isTRUE(o$gamma)) t$addRow(rowKey = private$.rk(lay, "g"), values = private$.lv(lay, list(measure = "Gamma", value = m$gamma)))
            if (isTRUE(o$taub))  t$addRow(rowKey = private$.rk(lay, "t"), values = private$.lv(lay, list(measure = "Tau-b Kendalla", value = m$taub)))
            private$.metody$add("Zmienne porządkowe", paste(
                "%s z par zgodnych i niezgodnych; kolejność kategorii = kolejność poziomów w tabeli",
                "— miary mają sens tylko dla kategorii uporządkowanych."),
                paste(c(if (isTRUE(o$gamma)) "Gamma Goodmana-Kruskala", if (isTRUE(o$taub)) "tau-b Kendalla"),
                      collapse = " i "))
        },

        .fillTrend = function(tab, lay = NULL) {
            if (!isTRUE(self$options$trend)) return()
            t <- self$results$trend
            ca <- cochranArmitage(tab)
            if (is.null(ca)) {
                t$setNote("t", "Test trendu wymaga tabeli 2×k (lub k×2) z co najmniej 3 uporządkowanymi kategoriami.")
                return()
            }
            t$addRow(rowKey = private$.rk(lay, "ca"), values = private$.lv(lay, list(z = ca$z, p = ca$p)))
            private$.metody$add("Testy", paste(
                "Test trendu Cochrana-Armitage’a: uporządkowanym kategoriom przypisano wartości 1…%d",
                "w kolejności poziomów; p dwustronne."), length(ca$scores))
        },

        .fillResid = function(tab, lay = NULL) {
            o <- self$options
            if (!isTRUE(o$resid)) return()
            t <- self$results$resid
            sr <- stdResiduals(tab)
            lv <- colnames(tab)
            for (i in seq_len(nrow(tab))) {
                vals <- list(row = rownames(tab)[i])
                for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- sr[i, j]
                t$addRow(rowKey = private$.rk(lay, rownames(tab)[i]), values = private$.lv(lay, vals))
            }
            private$.metody$add("Post-hoc", paste(
                "Skorygowane reszty standaryzowane (Habermana): (O − E) / √(E (1 − p<sub>w</sub>)(1 − p<sub>k</sub>)),",
                "w przybliżeniu N(0, 1); |z| &gt; 1.96 (α = 0.05) wskazuje komórki decydujące o zależności."))
        },

        .fillPairwise = function(tab, lay = NULL) {
            if (!isTRUE(self$options$pairwise)) return()
            t <- self$results$pairwise
            pw <- pairwiseRows(tab)
            if (is.null(pw)) {
                t$setNote("p", "Porównania par wymagają co najmniej 3 poziomów w wierszach.")
                return()
            }
            for (i in seq_len(nrow(pw)))
                t$addRow(rowKey = private$.rk(lay, i), values = private$.lv(lay, list(g1 = pw$g1[i], g2 = pw$g2[i],
                                                   stat = pw$stat[i], df = pw$df[i], p = pw$p[i])))
            private$.metody$add("Post-hoc", paste(
                "Porównania par wierszy: dla każdej pary χ² na podtabeli 2 × %d",
                "(liczba porównań: %d), p skorygowane metodą Holma."), ncol(tab), nrow(pw))
        },

        .plot = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            # warstwy: jeden panel na warstwę (bez „Ogółem”)
            tabs <- if (is.null(st$tabs)) list(st$tab) else st$tabs
            lab <- if (is.null(st$tabs)) NA_character_ else paste0(st$layerVar, ": ", names(tabs))
            layered <- !is.null(st$tabs)

            if (st$kind == "bar") {
                df <- do.call(rbind, lapply(seq_along(tabs), function(i) {
                    d <- as.data.frame(as.table(tabs[[i]]), stringsAsFactors = FALSE)
                    names(d) <- c("w", "k", "n")
                    if (st$pc == "row") d$val <- 100 * d$n / tapply(d$n, d$w, sum)[d$w]
                    else if (st$pc == "col") d$val <- 100 * d$n / tapply(d$n, d$k, sum)[d$k]
                    else d$val <- d$n
                    d$layer <- lab[i]; d
                }))
                ylab <- switch(st$pc, row = "% wierszem", col = "% kolumną", "Liczność")
                df$w <- factor(df$w, levels = rownames(st$tab)); df$k <- factor(df$k, levels = colnames(st$tab))
                df$layer <- factor(df$layer, levels = lab)
                p <- ggplot2::ggplot(df, ggplot2::aes(x = w, y = val, fill = k)) +
                    ggplot2::geom_col(colour = theme$color[1], position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
                    ggplot2::labs(x = st$xlab, y = ylab, fill = st$fill)
                if (layered) p <- p + ggplot2::facet_wrap(~ layer, ncol = min(length(tabs), 3))
            } else {
                # mozaika: szerokość kolumny ~ liczebność brzegowa wiersza,
                # podział wewnątrz ~ rozkład warunkowy — własna geometria (geom_rect)
                parts <- lapply(seq_along(tabs), function(i) {
                    tab <- tabs[[i]]; tab <- tab[rowSums(tab) > 0, , drop = FALSE]
                    if (nrow(tab) == 0) return(NULL)
                    xr <- cumsum(c(0, rowSums(tab) / sum(tab)))
                    rects <- do.call(rbind, lapply(seq_len(nrow(tab)), function(j) {
                        yr <- cumsum(c(0, tab[j, ] / sum(tab[j, ])))
                        data.frame(xmin = unname(xr[j]), xmax = unname(xr[j + 1]),
                                   ymin = unname(yr[-length(yr)]), ymax = unname(yr[-1]),
                                   w = rownames(tab)[j], k = colnames(tab), layer = lab[i], stringsAsFactors = FALSE)
                    }))
                    list(rects = rects, labs = data.frame(mid = (xr[-length(xr)] + xr[-1]) / 2,
                                                         label = rownames(tab), layer = lab[i], stringsAsFactors = FALSE))
                })
                parts <- Filter(Negate(is.null), parts)
                rects <- do.call(rbind, lapply(parts, `[[`, "rects")); labs <- do.call(rbind, lapply(parts, `[[`, "labs"))
                rects$k <- factor(rects$k, levels = colnames(st$tab))
                rects$layer <- factor(rects$layer, levels = lab); labs$layer <- factor(labs$layer, levels = lab)
                p <- ggplot2::ggplot(rects) +
                    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                                    fill = k), colour = "white", linewidth = 0.6) +
                    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
                    ggplot2::labs(x = st$xlab, y = "Udział w wierszu", fill = st$fill)
                if (!layered) {
                    p <- p + ggplot2::scale_x_continuous(breaks = labs$mid, labels = labs$label)
                } else {
                    # kolumny różnią się między warstwami, więc nazwy wierszy pod każdym panelem, nie na wspólnej osi
                    p <- p + ggplot2::geom_text(data = labs, ggplot2::aes(x = mid, y = -0.03, label = label),
                                                vjust = 1, size = 3.2, colour = theme$color[1]) +
                        ggplot2::facet_wrap(~ layer, ncol = min(length(tabs), 3)) +
                        ggplot2::scale_x_continuous(breaks = NULL) +
                        ggplot2::coord_cartesian(ylim = c(0, 1), clip = "off")
                }
            }
            p <- p + ggtheme + ggplot2::theme(legend.position = "bottom")
            if (layered && st$kind == "mosaic")
                p <- p + ggplot2::theme(axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 22)),
                                        panel.spacing.y = ggplot2::unit(3, "lines"))
            print(p)
            TRUE
        }
    )
)
