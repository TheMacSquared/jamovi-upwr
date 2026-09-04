#' @importFrom jmvcore .
cibootstrapClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "cibootstrapClass",
    inherit = cibootstrapBase,
    private = list(
        .run = function() {
            o <- self$options
            if (!optNonEmpty(o$dep)) return()
            level <- o$ciWidth / 100; a <- (1 - level) / 2
            x <- jmvcore::toNumeric(self$data[[o$dep]]); x <- x[!is.na(x)]; n <- length(x)
            if (n < 2) { self$results$summaryTable$setNote("err", "Za mało obserwacji (n < 2)."); return() }

            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienna „%s”; N = %d obserwacji bez braków.", o$dep, n)
            m$add("Przedział ufności", "Bootstrap „ręczny”: %d razy losowanych %d indeksów ze zwracaniem, z każdej próby liczona średnia; ziarno %d (te same próby przy każdym uruchomieniu).", o$nBoot, n, o$seed)
            m$add("Przedział ufności", "Przedział percentylowy %g%%: kwantyle %.3f i %.3f rozkładu średnich bootstrapowych; SE bootstrapowy = SD tych średnich; obciążenie = średnia bootstrapowa − średnia z danych.", o$ciWidth, a, 1 - a)
            m$addIf(o$showConvergence, "Przedział ufności", "Zbieżność: ten sam przedział percentylowy dla B = 50 … 5000 losowań (pakiet boot, to samo ziarno) — pokazuje, od jakiego B granice się stabilizują.")
            m$addIf(o$showConvergence, "Wykres", "Granice przedziału w funkcji B na osi logarytmicznej.")
            m$render(self$results$metody)

            ot <- self$results$origTable
            for (rw in list(list("n", "N", n), list("mean", "Średnia", mean(x)), list("sd", "Odchylenie standardowe", stats::sd(x)),
                            list("median", "Mediana", stats::median(x)), list("min", "Minimum", min(x)), list("max", "Maksimum", max(x))))
                ot$addRow(rowKey = rw[[1]], values = list(stat = rw[[2]], value = rw[[3]]))

            set.seed(o$seed)
            st <- self$results$samplesTable; means <- numeric(o$nBoot)
            for (b in seq_len(o$nBoot)) {
                idx <- sample.int(n, replace = TRUE); v <- x[idx]; means[b] <- mean(v)
                show <- if (n <= 20) seq_len(n) else 1:10
                st$addRow(rowKey = b, values = list(sample = b,
                    indices = paste(c(idx[show], if (n > 20) "…"), collapse = ", "),
                    values = paste(c(round(v[show], 2), if (n > 20) "…"), collapse = ", "), mean = means[b]))
            }
            su <- self$results$summaryTable
            for (rw in list(list("orig", "Średnia z danych", mean(x)), list("boot", "Średnia z prób bootstrapowych", mean(means)),
                            list("se", "SE bootstrapowy (SD średnich)", stats::sd(means)), list("bias", "Obciążenie", mean(means) - mean(x)),
                            list("lo", sprintf("Dolna granica CI %g%%", o$ciWidth), unname(stats::quantile(means, a))),
                            list("hi", sprintf("Górna granica CI %g%%", o$ciWidth), unname(stats::quantile(means, 1 - a)))))
                su$addRow(rowKey = rw[[1]], values = list(stat = rw[[2]], value = rw[[3]]))
            su$setNote("b", sprintf("B = %d prób; przy tak małym B przedział jest tylko ilustracją.", o$nBoot))

            if (isTRUE(o$showConvergence)) {
                bs <- c(50, 100, 200, 500, 1000, 2000, 5000); ct <- self$results$convTable
                pd <- data.frame(b = bs, lower = NA_real_, upper = NA_real_)
                for (i in seq_along(bs)) {
                    set.seed(o$seed)
                    reps <- boot::boot(x, function(v, ii) mean(v[ii]), R = bs[i])$t[, 1]
                    lo <- unname(stats::quantile(reps, a)); hi <- unname(stats::quantile(reps, 1 - a))
                    pd$lower[i] <- lo; pd$upper[i] <- hi
                    ct$addRow(rowKey = bs[i], values = list(b = bs[i], seBoot = stats::sd(reps), lower = lo, upper = hi, width = hi - lo))
                }
                self$results$convPlot$setState(pd)
            }
        },
        .convPlot = function(image, ggtheme, theme, ...) {
            s <- image$state; if (is.null(s)) return(FALSE)
            buildConvPlot(s, ggtheme, theme)
        }
    )
)
