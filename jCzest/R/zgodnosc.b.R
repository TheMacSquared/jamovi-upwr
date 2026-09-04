#' @importFrom jmvcore .
zgodnoscClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "zgodnoscClass",
    inherit = zgodnoscBase,
    private = list(

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

            private$.fillProps(obs, e)
            private$.assumptionNotice(obs, e)
            private$.fillTests(obs, e)
            private$.fillEffSize(obs, e)

            if (isTRUE(o$plot))
                self$results$plot$setState(list(obs = obs, e = e, var = o$var))
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
                t$setNote("z", "Kategorie z |z| > 1,96 (α = 0,05) odstają od oczekiwań.")
        },

        # ten sam warunek co przy teście niezależności — przybliżenie χ² wymaga E >= 5
        .assumptionNotice = function(obs, e) {
            if (length(obs) == 2) return()   # dla 2 kategorii liczymy test dokładny
            a <- checkAssumptionE(e)
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

            if (k == 2) {
                # dwie kategorie: test dwumianowy jest DOKŁADNY, więc jest domyślny
                p0 <- e[[1]] / n
                b <- binomGof(obs, p0, o$hypothesis)
                t$addRow(rowKey = "bin", values = list(
                    test = "Test dwumianowy (dokładny)", stat = obs[[1]], df = NA_integer_, p = b$p))
                t$setNote("b", sprintf(
                    "Liczba obserwacji w kategorii „%s” wobec oczekiwanej proporcji %.3f; N = %d.",
                    names(obs)[1], p0, as.integer(n)))
                if (isTRUE(o$chiSqCorr)) {
                    ct <- suppressWarnings(stats::chisq.test(as.vector(obs), p = e / n, correct = TRUE))
                    t$addRow(rowKey = "cc", values = list(
                        test = "χ² z poprawką ciągłości", stat = unname(ct$statistic),
                        df = unname(ct$parameter), p = unname(ct$p.value)))
                }
            } else {
                r <- chiSqGof(obs, e)
                t$addRow(rowKey = "chi", values = list(
                    test = "χ² zgodności", stat = r$stat, df = r$df, p = r$p))
                t$setNote("N", sprintf("N = %s.", format(n, big.mark = " ")))
                if (isTRUE(o$chiSqCorr))
                    t$setNote("cc", "Poprawka ciągłości dotyczy wyłącznie dwóch kategorii.")
            }

            if (isTRUE(o$exact)) {
                me <- multinomExact(obs, e)
                if (is.null(me))
                    t$setNote("ex", "Dokładny test wielomianowy jest wykonalny tylko dla małych N i niewielu kategorii.")
                else
                    t$addRow(rowKey = "mx", values = list(
                        test = "Dokładny test wielomianowy", stat = NA_real_, df = NA_integer_, p = me$p))
            }
        },

        .fillEffSize = function(obs, e) {
            if (!isTRUE(self$options$effSize)) return()
            t <- self$results$effsize
            w <- cohensW(obs, e)
            interp <- if (!is.finite(w)) "" else if (w < 0.1) "poniżej słabego"
                      else if (w < 0.3) "słaby" else if (w < 0.5) "umiarkowany" else "silny"
            t$addRow(rowKey = "w", values = list(measure = "w Cohena", value = w, interp = interp))
            t$setNote("th", "Progi Cohena: słaby 0,10, umiarkowany 0,30, silny 0,50.")
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
