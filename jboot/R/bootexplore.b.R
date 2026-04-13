# This file is a generated template, your changes will not be overwritten

bootExploreClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bootExploreClass",
    inherit = bootExploreBase,
    private = list(
        .run = function() {

            if (is.null(self$options$dep))
                return()

            nBoot   <- self$options$nBoot
            ciWidth <- self$options$ciWidth / 100
            alpha   <- 1 - ciWidth
            seed    <- self$options$seed

            column <- jmvcore::toNumeric(self$data[[self$options$dep]])
            column <- column[!is.na(column)]
            n      <- length(column)

            if (n < 2)
                return()

            # Original data summary
            origTable <- self$results$origTable
            origTable$addRow(rowKey="n", values=list(
                stat = "N", value = n))
            origTable$addRow(rowKey="mean", values=list(
                stat = "Średnia", value = mean(column)))
            origTable$addRow(rowKey="sd", values=list(
                stat = "Odchylenie standardowe", value = sd(column)))
            origTable$addRow(rowKey="median", values=list(
                stat = "Mediana", value = median(column)))
            origTable$addRow(rowKey="min", values=list(
                stat = "Minimum", value = min(column)))
            origTable$addRow(rowKey="max", values=list(
                stat = "Maksimum", value = max(column)))

            # Bootstrap samples - show each step
            set.seed(seed)
            samplesTable <- self$results$samplesTable
            bootMeans    <- numeric(nBoot)

            for (b in seq_len(nBoot)) {
                idx    <- sample.int(n, replace = TRUE)
                vals   <- column[idx]
                bMean  <- mean(vals)
                bootMeans[b] <- bMean

                # Show max 20 indices/values to avoid overflow
                if (n <= 20) {
                    idxStr <- paste(idx, collapse = ", ")
                    valStr <- paste(round(vals, 2), collapse = ", ")
                } else {
                    idxStr <- paste(c(idx[1:10], "..."), collapse = ", ")
                    valStr <- paste(c(round(vals[1:10], 2), "..."), collapse = ", ")
                }

                samplesTable$addRow(rowKey=as.character(b), values=list(
                    sample  = b,
                    indices = idxStr,
                    values  = valStr,
                    mean    = bMean
                ))
            }

            # Summary
            ciLower <- as.numeric(quantile(bootMeans, alpha / 2))
            ciUpper <- as.numeric(quantile(bootMeans, 1 - alpha / 2))

            summaryTable <- self$results$summaryTable
            summaryTable$addRow(rowKey="origMean", values=list(
                stat = "Średnia oryginalna", value = mean(column)))
            summaryTable$addRow(rowKey="bootMean", values=list(
                stat = "Średnia z bootstrapu", value = mean(bootMeans)))
            summaryTable$addRow(rowKey="bootSE", values=list(
                stat = "Błąd standardowy (bootstrap)", value = sd(bootMeans)))
            summaryTable$addRow(rowKey="bias", values=list(
                stat = "Obciążenie", value = mean(bootMeans) - mean(column)))
            summaryTable$addRow(rowKey="ciLower", values=list(
                stat = paste0("Dolna granica PU (", self$options$ciWidth, "%)"),
                value = ciLower))
            summaryTable$addRow(rowKey="ciUpper", values=list(
                stat = paste0("Górna granica PU (", self$options$ciWidth, "%)"),
                value = ciUpper))

            summaryTable$setNote("info", paste0(
                "Ręczny bootstrap z B = ", nBoot,
                " próbami; ziarno = ", seed))
        }
    )
)
