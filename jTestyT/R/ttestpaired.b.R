#' @importFrom jmvcore .
ttestpairedClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "ttestpairedClass",
    inherit = ttestpairedBase,
    private = list(
        # complete pairs of two different variables, each pair once: a variable
        # paired with itself or a repeated pair would give duplicate keys
        .validPairs = function() {
            pairs <- Filter(function(p) !is.null(p$i1) && !is.null(p$i2) && p$i1 != p$i2, self$options$pairs)
            pairs[!duplicated(vapply(pairs, function(p) paste(p$i1, "−", p$i2), ""))]
        },
        .pairKeys = function() vapply(private$.validPairs(), function(p) paste(p$i1, "−", p$i2), ""),
        .init = function() {
            for (k in private$.pairKeys()) {
                self$results$plots$addItem(key = k); self$results$qq$addItem(key = k)
            }
        },
        .run = function() {
            o <- self$options
            tt <- self$results$ttest
            same <- unique(unlist(lapply(o$pairs, function(p) if (!is.null(p$i1) && !is.null(p$i2) && p$i1 == p$i2) p$i1)))
            if (length(same) > 0)
                tt$setNote("same", sprintf("Pominięto parę zmiennej z nią samą: %s.", paste0("„", same, "”", collapse = ", ")))
            pairs <- private$.validPairs()
            if (length(pairs) == 0) return()
            m <- jmvcore::metodyNew()
            m$add("Dane", "Pary: %s; różnica = pierwsza − druga zmienna; tylko pary bez braków.",
                  paste(vapply(pairs, function(p) sprintf("„%s” − „%s”", jmvcore::htmlEscape(p$i1), jmvcore::htmlEscape(p$i2)), ""), collapse = ", "))
            m$addIf(o$student, "Testy", "t Studenta dla par = t jednej próby na różnicach, df = n − 1.")
            metodyWspolne(m, o, "paired", "Różnica = pierwsza − druga zmienna")
            m$render(self$results$metody)
            for (p in pairs) {
                k <- paste(p$i1, "−", p$i2)
                a <- jmvcore::toNumeric(self$data[[p$i1]]); b <- jmvcore::toNumeric(self$data[[p$i2]])
                ok <- !is.na(a) & !is.na(b); a <- a[ok]; b <- b[ok]; dif <- a - b
                if (length(dif) < 2) { tt$setNote(paste0("n", k), sprintf("«%s»: liczba kompletnych par: %d; bieżąca implementacja wymaga co najmniej 2.", k, length(dif))); next }
                if (isTRUE(o$student)) addTestRow(tt, paste(k, "t"), k, { r <- oneSampleT(dif, 0, o$hypothesis); r$test <- "t Studenta (pary)"; r })
                if (isTRUE(o$nonpar)) addTestRow(tt, paste(k, "w"), k, wilcoxOne(dif, 0, o$hypothesis))
                if (isTRUE(o$desc)) {
                    self$results$desc$addRow(rowKey = paste(k, 1), values = c(list(var = k, group = p$i1), descRow(a)))
                    self$results$desc$addRow(rowKey = paste(k, 2), values = c(list(var = k, group = p$i2), descRow(b)))
                    self$results$desc$addRow(rowKey = paste(k, 3), values = c(list(var = k, group = "różnica"), descRow(dif)))
                }
                if (isTRUE(o$norm)) self$results$norm$addRow(rowKey = k, values = c(list(var = k, group = "różnice"), shapiroRow(dif)))
                self$results$plots$get(key = k)$setState(list(groups = stats::setNames(list(a, b, dif), c(p$i1, p$i2, "Różnica")),
                    refLine = 0, refLabel = "0 (brak różnicy)", ylab = "Wartość"))
                self$results$qq$get(key = k)$setState(list(x = dif, label = k))
            }
            tt$setNote("h", "Różnica = pierwsza − druga zmienna.")
        },
        .plot = function(image, ggtheme, theme, ...) boxPlotTests(image, ggtheme, theme),
        .qq = function(image, ggtheme, theme, ...) qqPlotResid(image, ggtheme, theme)
    )
)
