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

            freqs <- self$results$freqs
            freqs$addColumn(name = "row", title = o$rows, type = "text", combineBelow = TRUE)
            freqs$addColumn(name = "kind", title = "", type = "text")
            for (l in lv)
                freqs$addColumn(name = paste0("c_", l), title = l, type = "number",
                                superTitle = o$cols)
            freqs$addColumn(name = "total", title = "Ogółem", type = "number")

            res <- self$results$resid
            res$addColumn(name = "row", title = o$rows, type = "text")
            for (l in lv)
                res$addColumn(name = paste0("c_", l), title = l, type = "number",
                              superTitle = o$cols)

            private$.colsBuilt <- TRUE
            invisible(TRUE)
        },

        .init = function() private$.buildColumns(),

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

        .table = function() {
            o <- self$options
            r <- self$data[[o$rows]]; c0 <- self$data[[o$cols]]
            cnt <- if (optNonEmpty(o$counts)) jmvcore::toNumeric(self$data[[o$counts]]) else NULL
            buildTable(r, c0, cnt)
        },

        .run = function() {
            o <- self$options
            if (!optNonEmpty(o$rows) || !optNonEmpty(o$cols)) return()
            private$.buildColumns()   # w GUI dane bywają dostępne dopiero tutaj
            tab <- private$.table()
            if (any(dim(tab) < 2) || sum(tab) == 0) {
                self$results$tests$setNote("n", "Każda zmienna musi mieć co najmniej 2 poziomy z obserwacjami.")
                return()
            }

            private$.metody <- jmvcore::metodyNew()
            private$.describeData(tab)
            private$.fillFreqs(tab)
            private$.assumptionNotice(tab)
            private$.fillTests(tab)
            private$.fillEffSize(tab)
            private$.fillMeasures(tab)
            private$.fillOrdinal(tab)
            private$.fillTrend(tab)
            private$.fillResid(tab)
            private$.fillPairwise(tab)

            if (o$plot != "none")
                self$results$plot$setState(list(tab = tab, kind = o$plot, pc = o$pc,
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
        .fillFreqs = function(tab) {
            o <- self$options
            t <- self$results$freqs
            lv <- colnames(tab)
            e <- expectedCounts(tab)
            pcLab <- switch(o$pc, row = "% wierszem", col = "% kolumną", total = "% ogółu", NULL)

            for (i in seq_len(nrow(tab))) {
                rn <- rownames(tab)[i]
                vals <- list(row = rn, kind = "Liczność")
                for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- tab[i, j]
                vals$total <- sum(tab[i, ])
                t$addRow(rowKey = paste0(rn, "_n"), values = vals)

                if (isTRUE(o$exp)) {
                    vals <- list(row = rn, kind = "Oczekiwana")
                    for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- e[i, j]
                    vals$total <- sum(e[i, ])
                    t$addRow(rowKey = paste0(rn, "_e"), values = vals)
                }

                if (!is.null(pcLab)) {
                    den <- switch(o$pc, row = sum(tab[i, ]), total = sum(tab), col = NA)
                    vals <- list(row = rn, kind = pcLab)
                    for (j in seq_along(lv)) {
                        d <- if (o$pc == "col") sum(tab[, j]) else den
                        vals[[paste0("c_", lv[j])]] <- if (d > 0) 100 * tab[i, j] / d else NA_real_
                    }
                    vals$total <- if (o$pc == "row") 100 else
                        if (o$pc == "total") 100 * sum(tab[i, ]) / sum(tab) else NA_real_
                    t$addRow(rowKey = paste0(rn, "_p"), values = vals)
                }
            }

            vals <- list(row = "Ogółem", kind = "Liczność")
            for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- sum(tab[, j])
            vals$total <- sum(tab)
            t$addRow(rowKey = "._total", values = vals)
        },

        # --- SEDNO MODUŁU: automatyczna kontrola warunku E >= 5 ---
        .assumptionNotice = function(tab) {
            a <- checkAssumption(tab)
            private$.metody$add("Testy", paste(
                "Warunek stosowalności χ² sprawdzany automatycznie: dla 2×2 wszystkie E ≥ 5,",
                "dla większych tabel wszystkie E ≥ 1 i najwyżej 20%% komórek z E &lt; 5 (Cochran) — tu %s."),
                if (isTRUE(a$ok)) "spełniony" else "niespełniony (ostrzeżenie nad wynikami)")
            if (isTRUE(a$ok)) return()
            msg <- if (a$is2x2)
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

            notice <- jmvcore::Notice$new(self$options, name = ".assumption",
                                          type = jmvcore::NoticeType$WARNING, content = msg)
            self$results$insert(1, notice)
        },

        .fillTests = function(tab) {
            o <- self$options
            t <- self$results$tests
            n <- sum(tab)
            m <- private$.metody
            if (isTRUE(o$chiSq)) {
                r <- chiSqTest(tab, correct = FALSE)
                t$addRow(rowKey = "chi", values = list(test = "χ²", stat = r$stat, df = r$df, p = r$p))
                m$add("Testy", "χ² Pearsona bez poprawki ciągłości, df = (r − 1)(c − 1) = %d.", r$df)
            }
            if (isTRUE(o$chiSqCorr)) {
                if (all(dim(tab) == c(2, 2))) {
                    r <- chiSqTest(tab, correct = TRUE)
                    m$add("Testy", "χ² z poprawką ciągłości Yatesa (|O − E| pomniejszone o 0.5).")
                    t$addRow(rowKey = "chic", values = list(test = "χ² z poprawką ciągłości",
                                                           stat = r$stat, df = r$df, p = r$p))
                } else {
                    t$setNote("cc", "Poprawka ciągłości dotyczy wyłącznie tabel 2×2.")
                }
            }
            if (isTRUE(o$likeRat)) {
                r <- likeRatTest(tab)
                t$addRow(rowKey = "lr", values = list(test = "G² (iloraz wiarygodności)",
                                                     stat = r$stat, df = r$df, p = r$p))
                m$add("Testy", "G² (iloraz wiarygodności) = 2 Σ O·ln(O/E), te same df co χ².")
            }
            if (isTRUE(o$fisher)) {
                ft <- try(stats::fisher.test(tab), silent = TRUE)
                if (inherits(ft, "try-error"))
                    t$setNote("f", "Dokładny test Fishera nie policzył się dla tej tabeli (za duża).")
                else {
                    t$addRow(rowKey = "fi", values = list(test = "Dokładny test Fishera", p = ft$p.value))
                    m$add("Testy", "Dokładny test Fishera (bez statystyki testowej; dla tabel &gt; 2×2 uogólnienie Freemana-Haltona).")
                }
            }
            # N w nocie, nie jako wiersz: liczebnosc dzielilaby kolumne ze statystykami
            # testowymi i dziedziczyla ich format (80.00000), a nie jest testem
            t$setNote("N", sprintf("N = %s.", format(n, big.mark = " ")))
        },

        .fillEffSize = function(tab) {
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
            t$addRow(rowKey = "v", values = list(measure = "V Craméra", value = v,
                                                 lower = ci[1], upper = ci[2], interp = interp))
            m <- private$.metody
            m$add("Wielkość efektu", paste(
                "V Craméra = √(χ² / (N · (min(r, c) − 1))); interpretacja wg progów Cohena",
                "dla tej tabeli: słaby %.2f, umiarkowany %.2f, silny %.2f."), small, med, large)
            m$addIf(o$effSizeCI, "Wielkość efektu", paste(
                "Przedział ufności %g%% dla V: bootstrap percentylowy, 1000 losowań",
                "z rozkładu wielomianowego, ziarno 1."), o$ciWidth)
        },

        .fillMeasures = function(tab) {
            o <- self$options
            if (!(isTRUE(o$odds) || isTRUE(o$relRisk) || isTRUE(o$diffProp))) return()
            t <- self$results$measures
            if (!all(dim(tab) == c(2, 2))) {
                t$setNote("d", "Te miary są określone tylko dla tabel 2×2.")
                return()
            }
            m <- twoByTwo(tab, level = o$ciWidth / 100, compare = o$compare)
            add <- function(key, lab, x)
                t$addRow(rowKey = key, values = list(measure = lab, value = x$est,
                                                     lower = x$lower, upper = x$upper))
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

        .fillOrdinal = function(tab) {
            o <- self$options
            if (!(isTRUE(o$gamma) || isTRUE(o$taub))) return()
            t <- self$results$ordinal
            m <- ordinalMeasures(tab)
            if (isTRUE(o$gamma)) t$addRow(rowKey = "g", values = list(measure = "Gamma", value = m$gamma))
            if (isTRUE(o$taub))  t$addRow(rowKey = "t", values = list(measure = "Tau-b Kendalla", value = m$taub))
            private$.metody$add("Zmienne porządkowe", paste(
                "%s z par zgodnych i niezgodnych; kolejność kategorii = kolejność poziomów w tabeli",
                "— miary mają sens tylko dla kategorii uporządkowanych."),
                paste(c(if (isTRUE(o$gamma)) "Gamma Goodmana-Kruskala", if (isTRUE(o$taub)) "tau-b Kendalla"),
                      collapse = " i "))
        },

        .fillTrend = function(tab) {
            if (!isTRUE(self$options$trend)) return()
            t <- self$results$trend
            ca <- cochranArmitage(tab)
            if (is.null(ca)) {
                t$setNote("t", "Test trendu wymaga tabeli 2×k (lub k×2) z co najmniej 3 uporządkowanymi kategoriami.")
                return()
            }
            t$addRow(rowKey = "ca", values = list(z = ca$z, p = ca$p))
            private$.metody$add("Testy", paste(
                "Test trendu Cochrana-Armitage’a: uporządkowanym kategoriom przypisano wartości 1…%d",
                "w kolejności poziomów; p dwustronne."), length(ca$scores))
        },

        .fillResid = function(tab) {
            o <- self$options
            if (!isTRUE(o$resid)) return()
            t <- self$results$resid
            sr <- stdResiduals(tab)
            lv <- colnames(tab)
            for (i in seq_len(nrow(tab))) {
                vals <- list(row = rownames(tab)[i])
                for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- sr[i, j]
                t$addRow(rowKey = rownames(tab)[i], values = vals)
            }
            private$.metody$add("Post-hoc", paste(
                "Skorygowane reszty standaryzowane (Habermana): (O − E) / √(E (1 − p<sub>w</sub>)(1 − p<sub>k</sub>)),",
                "w przybliżeniu N(0, 1); |z| &gt; 1.96 (α = 0.05) wskazuje komórki decydujące o zależności."))
        },

        .fillPairwise = function(tab) {
            if (!isTRUE(self$options$pairwise)) return()
            t <- self$results$pairwise
            pw <- pairwiseRows(tab)
            if (is.null(pw)) {
                t$setNote("p", "Porównania par wymagają co najmniej 3 poziomów w wierszach.")
                return()
            }
            for (i in seq_len(nrow(pw)))
                t$addRow(rowKey = i, values = list(g1 = pw$g1[i], g2 = pw$g2[i],
                                                   stat = pw$stat[i], df = pw$df[i], p = pw$p[i]))
            private$.metody$add("Post-hoc", paste(
                "Porównania par wierszy: dla każdej pary χ² na podtabeli 2 × %d",
                "(%d porównań), p skorygowane metodą Holma."), ncol(tab), nrow(pw))
        },

        .plot = function(image, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            tab <- st$tab
            df <- as.data.frame(as.table(tab), stringsAsFactors = FALSE)
            names(df) <- c("w", "k", "n")

            if (st$kind == "bar") {
                if (st$pc == "row") {
                    tot <- tapply(df$n, df$w, sum)
                    df$val <- 100 * df$n / tot[df$w]
                    ylab <- "% wierszem"
                } else if (st$pc == "col") {
                    tot <- tapply(df$n, df$k, sum)
                    df$val <- 100 * df$n / tot[df$k]
                    ylab <- "% kolumną"
                } else {
                    df$val <- df$n; ylab <- "Liczność"
                }
                p <- ggplot2::ggplot(df, ggplot2::aes(x = w, y = val, fill = k)) +
                    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
                    ggplot2::labs(x = st$xlab, y = ylab, fill = st$fill)
            } else {
                # mozaika: szerokość kolumny ~ liczebność brzegowa wiersza,
                # podział wewnątrz ~ rozkład warunkowy — własna geometria (geom_rect)
                rw <- rowSums(tab) / sum(tab)
                xr <- cumsum(c(0, rw))
                rects <- do.call(rbind, lapply(seq_len(nrow(tab)), function(i) {
                    cw <- tab[i, ] / sum(tab[i, ])
                    yr <- cumsum(c(0, cw))
                    data.frame(xmin = xr[i], xmax = xr[i + 1],
                               ymin = yr[-length(yr)], ymax = yr[-1],
                               w = rownames(tab)[i], k = colnames(tab), stringsAsFactors = FALSE)
                }))
                mid <- (xr[-length(xr)] + xr[-1]) / 2
                p <- ggplot2::ggplot(rects) +
                    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                                    fill = k), colour = "white", linewidth = 0.6) +
                    ggplot2::scale_x_continuous(breaks = mid, labels = rownames(tab)) +
                    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
                    ggplot2::labs(x = st$xlab, y = "Udział w wierszu", fill = st$fill)
            }
            print(p + ggplot2::theme_minimal())
            TRUE
        }
    )
)
