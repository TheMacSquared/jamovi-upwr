#' @importFrom jmvcore .
zgodnoscClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "zgodnoscClass",
    inherit = zgodnoscBase,
    private = list(

        # opis zastosowanych metod (jmvcore::metodyNew, wspólny mechanizm jUPWR) — zbierany po drodze, renderowany na końcu .run
        .metody = NULL,

        .counts = function() {
            o <- self$options
            x <- self$data[[o$var]]
            cnt <- if (optNonEmpty(o$counts)) jmvcore::toNumeric(self$data[[o$counts]]) else NULL
            gofCounts(x, cnt)
        },

        .run = function() {
            o <- self$options
            if (!optNonEmpty(o$var)) return()
            obs <- private$.counts()
            obs <- obs[!is.na(names(obs))]
            if (length(obs) < 2) {
                self$results$tests$setNote("k", "Zmienna musi mieć co najmniej 2 kategorie.")
                return()
            }
            if (sum(obs) == 0) {
                self$results$tests$setNote("n", "Brak obserwacji.")
                return()
            }

            e <- gofExpected(obs, o$ratio)
            if (is.null(e)) {
                self$results$tests$setNote("r", paste(
                    "Proporcje oczekiwane muszą być nieujemne, nie wszystkie zerowe",
                    "i podane dla każdej kategorii."))
                return()
            }

            private$.metody <- jmvcore::metodyNew()
            private$.describeData(obs, e)
            private$.fillProps(obs, e)
            private$.assumptionNotice(obs, e)
            private$.fillTests(obs, e)
            private$.fillEffSize(obs, e)

            if (isTRUE(o$plot)) {
                self$results$plot$setState(list(obs = obs, e = e, var = o$var))
                private$.metody$add("Wykres", "Słupki obserwowane obok oczekiwanych dla każdej kategorii (liczności).")
            }
            private$.metody$render(self$results$metody)
        },

        .describeData = function(obs, e) {
            o <- self$options
            m <- private$.metody
            n <- sum(obs)
            m$add("Dane", "Zmienna „%s”, %d kategorii w kolejności poziomów; N = %s (braki pominięte).",
                  o$var, length(obs), format(n, big.mark = " "))
            m$addIf(optNonEmpty(o$counts), "Dane",
                    "Dane zagregowane: liczności z kolumny „%s”.", o$counts)
            m$add("Dane", "Proporcje oczekiwane (wagi z panelu znormalizowane do 1): %s.",
                  paste(sprintf("„%s” %.3f", jmvcore::htmlEscape(names(obs)), e / n), collapse = ", "))
        },

        .fillProps = function(obs, e) {
            t <- self$results$props
            n <- sum(obs)
            res <- gofResiduals(obs, e)
            for (i in seq_along(obs))
                t$addRow(rowKey = names(obs)[i], values = list(
                    level = names(obs)[i],
                    obs = obs[[i]], obsProp = obs[[i]] / n,
                    expCount = e[[i]], expProp = e[[i]] / n,
                    resid = res[[i]]))
            if (isTRUE(self$options$resid))
                private$.metody$add("Post-hoc", paste(
                    "Skorygowane reszty standaryzowane: (O − E) / √(E (1 − p)), w przybliżeniu N(0, 1);",
                    "|z| &gt; 1.96 (α = 0.05) wskazuje kategorie odstające od oczekiwań."))
        },

        # ten sam warunek co przy teście niezależności — przybliżenie χ² wymaga E >= 5
        .assumptionNotice = function(obs, e) {
            if (length(obs) == 2) return()   # dla 2 kategorii liczymy test dokładny
            a <- checkAssumptionE(e)
            private$.metody$add("Testy", paste(
                "Warunek stosowalności χ² sprawdzany automatycznie: wszystkie E ≥ 1",
                "i najwyżej 20%% kategorii z E &lt; 5 (Cochran) — tu %s."),
                if (isTRUE(a$ok)) "spełniony" else "niespełniony (ostrzeżenie nad wynikami)")
            if (isTRUE(a$ok)) return()
            msg <- sprintf(paste(
                "%d z %d kategorii (%.0f%%) ma liczebność oczekiwaną < 5,",
                "a najmniejsza wynosi %.2f — przybliżenie χ² jest zawodne.",
                "Rozważ połączenie kategorii%s."),
                a$nBelow5, a$nCells, a$pctBelow5, a$minExpected,
                if (!is.null(multinomExact(obs, e))) " albo dokładny test wielomianowy" else "")
            self$results$insert(1, jmvcore::Notice$new(
                self$options, name = ".assumption",
                type = jmvcore::NoticeType$WARNING, content = msg))
        },

        .fillTests = function(obs, e) {
            o <- self$options
            t <- self$results$tests
            k <- length(obs); n <- sum(obs)
            m <- private$.metody

            if (k == 2) {
                # dwie kategorie: test dwumianowy jest DOKŁADNY, więc jest domyślny
                p0 <- e[[1]] / n
                b <- binomGof(obs, p0, o$hypothesis)
                t$addRow(rowKey = "bin", values = list(
                    test = "Test dwumianowy (dokładny)", stat = obs[[1]], df = NA_integer_, p = b$p))
                # ktora kategoria jest testowana i wobec jakiego p0 — bez tego
                # wartosc statystyki (licznosc) nie ma odniesienia
                m$add("Testy", paste(
                    "Dwie kategorie → dokładny test dwumianowy: statystyka = liczba obserwacji",
                    "w kategorii „%s” (pierwszej) wobec p₀ = %.3f; hipoteza alternatywna: %s."),
                    names(obs)[1], p0,
                    switch(o$hypothesis, greater = "udział większy niż p₀",
                           less = "udział mniejszy niż p₀", "udział różny od p₀ (dwustronna)"))
                if (isTRUE(o$chiSqCorr)) {
                    ct <- suppressWarnings(stats::chisq.test(as.vector(obs), p = e / n, correct = TRUE))
                    t$addRow(rowKey = "cc", values = list(
                        test = "χ² z poprawką ciągłości", stat = unname(ct$statistic),
                        df = unname(ct$parameter), p = unname(ct$p.value)))
                    m$add("Testy", "χ² zgodności z poprawką ciągłości Yatesa, df = 1.")
                }
            } else {
                r <- chiSqGof(obs, e)
                t$addRow(rowKey = "chi", values = list(
                    test = "χ² zgodności", stat = r$stat, df = r$df, p = r$p))
                t$setNote("N", sprintf("N = %s.", format(n, big.mark = " ")))
                m$add("Testy", "%d kategorii → χ² zgodności Pearsona, df = k − 1 = %d.", k, r$df)
                if (isTRUE(o$chiSqCorr))
                    t$setNote("cc", "Poprawka ciągłości dotyczy wyłącznie dwóch kategorii.")
            }

            if (isTRUE(o$exact)) {
                me <- multinomExact(obs, e)
                if (is.null(me))
                    t$setNote("ex", "Dokładny test wielomianowy jest wykonalny tylko dla małych N i niewielu kategorii.")
                else {
                    t$addRow(rowKey = "mx", values = list(
                        test = "Dokładny test wielomianowy", stat = NA_real_, df = NA_integer_, p = me$p))
                    m$add("Testy", paste(
                        "Dokładny test wielomianowy: p = suma prawdopodobieństw wszystkich układów",
                        "liczności nie bardziej prawdopodobnych niż obserwowany (%s układów)."),
                        format(me$nStates, big.mark = " "))
                }
            }
        },

        .fillEffSize = function(obs, e) {
            if (!isTRUE(self$options$effSize)) return()
            t <- self$results$effsize
            w <- cohensW(obs, e)
            interp <- if (!is.finite(w)) "" else if (w < 0.1) "poniżej słabego"
                      else if (w < 0.3) "słaby" else if (w < 0.5) "umiarkowany" else "silny"
            t$addRow(rowKey = "w", values = list(measure = "w Cohena", value = w, interp = interp))
            private$.metody$add("Wielkość efektu",
                "w Cohena = √(χ² / N); interpretacja wg progów Cohena: słaby 0.10, umiarkowany 0.30, silny 0.50.")
        },

        .plot = function(image, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            df <- data.frame(
                kategoria = rep(names(st$obs), 2),
                typ = rep(c("Obserwowane", "Oczekiwane"), each = length(st$obs)),
                n = c(as.vector(st$obs), as.vector(st$e)),
                stringsAsFactors = FALSE)
            df$kategoria <- factor(df$kategoria, levels = names(st$obs))
            df$typ <- factor(df$typ, levels = c("Obserwowane", "Oczekiwane"))
            p <- ggplot2::ggplot(df, ggplot2::aes(x = kategoria, y = n, fill = typ)) +
                ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
                ggplot2::labs(x = st$var, y = "Liczność", fill = NULL) +
                ggplot2::theme_minimal()
            print(p)
            TRUE
        }
    )
)
