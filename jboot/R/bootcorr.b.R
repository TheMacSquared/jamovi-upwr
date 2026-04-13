# This file is a generated template, your changes will not be overwritten

bootCorrClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bootCorrClass",
    inherit = bootCorrBase,
    private = list(
        .run = function() {

            vars <- self$options$vars

            if (length(vars) < 2)
                return()

            nBoot   <- self$options$nBoot
            ciWidth <- self$options$ciWidth / 100
            alpha   <- 1 - ciWidth
            method  <- self$options$method
            ciMethod <- self$options$ciMethod

            table <- self$results$corrTable

            bootDataList <- list()

            # Generate all pairs
            for (i in seq_along(vars)) {
                for (j in seq_along(vars)) {
                    if (j <= i)
                        next

                    var1Name <- vars[[i]]
                    var2Name <- vars[[j]]

                    col1 <- jmvcore::toNumeric(self$data[[var1Name]])
                    col2 <- jmvcore::toNumeric(self$data[[var2Name]])

                    # Pairwise complete observations
                    keep <- !is.na(col1) & !is.na(col2)
                    col1 <- col1[keep]
                    col2 <- col2[keep]
                    n    <- length(col1)

                    rowKey <- paste0(var1Name, "-", var2Name)

                    if (n < 3) {
                        table$addRow(rowKey=rowKey, values=list(
                            var1 = var1Name,
                            var2 = var2Name))
                        table$addFootnote(rowKey=rowKey, col="r",
                            "Za mało kompletnych obserwacji")
                        next
                    }

                    dat <- data.frame(x=col1, y=col2)

                    bootFun <- function(data, i) {
                        d <- data[i, ]
                        cor(d$x, d$y, method=method)
                    }

                    setSeedIfNeeded(self$options$seed)
                    bootResult <- boot::boot(data=dat, statistic=bootFun, R=nBoot)

                    reps    <- bootResult$t[, 1]
                    ci <- extractBootCI(bootResult, ciWidth, ciMethod)
                    ciLower <- ci$lower
                    ciUpper <- ci$upper

                    table$addRow(rowKey=rowKey, values=list(
                        var1    = var1Name,
                        var2    = var2Name,
                        r       = bootResult$t0,
                        seBoot  = sd(reps),
                        ciLower = ciLower,
                        ciUpper = ciUpper,
                        n       = n
                    ))

                    bootDataList[[rowKey]] <- list(
                        reps = reps, obs = bootResult$t0,
                        ciLower = ciLower, ciUpper = ciUpper,
                        label = paste0(var1Name, " & ", var2Name))

                    self$results$plots$addItem(key=rowKey)

                    if (self$options$showClassical) {
                        corResult <- cor.test(col1, col2, method=method, conf.level=ciWidth)
                        classTable <- self$results$classicalTable
                        classTable$addRow(rowKey=rowKey, values=list(
                            var1         = var1Name,
                            var2         = var2Name,
                            r            = corResult$estimate,
                            t            = if (!is.null(corResult$statistic)) corResult$statistic else NA,
                            df           = if (!is.null(corResult$parameter)) corResult$parameter else NA,
                            p            = corResult$p.value,
                            ciLowerClass = if (!is.null(corResult$conf.int)) corResult$conf.int[1] else NA,
                            ciUpperClass = if (!is.null(corResult$conf.int)) corResult$conf.int[2] else NA
                        ))
                    }
                }
            }

            methodLabel <- ifelse(method == "pearson", "Pearsona", "Spearmana")
            table$setNote("boot", paste0("Bootstrapowy PU dla korelacji ", methodLabel, " (metoda: ", ciMethodLabel(ciMethod), "); B = ", nBoot))

            private$.bootData <- bootDataList
        },

        .plot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.bootData))
                return(FALSE)

            key <- image$key
            if (! key %in% names(private$.bootData))
                return(FALSE)

            info    <- private$.bootData[[key]]
            reps    <- info$reps
            obs     <- info$obs
            ciLower <- info$ciLower
            ciUpper <- info$ciUpper
            label   <- info$label

            df <- data.frame(x = reps)

            plot <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
                ggplot2::geom_histogram(bins = 40, fill = "steelblue",
                    color = "white", alpha = 0.7) +
                ggplot2::geom_vline(xintercept = obs,
                    color = "red", linewidth = 1) +
                ggplot2::geom_vline(xintercept = c(ciLower, ciUpper),
                    color = "darkgreen", linetype = "dashed", linewidth = 0.8) +
                ggplot2::annotate("rect",
                    xmin = ciLower, xmax = ciUpper,
                    ymin = -Inf, ymax = Inf,
                    fill = "green", alpha = 0.1) +
                ggplot2::labs(
                    x = paste0("Bootstrapowe korelacje (", label, ")"),
                    y = "Częstość",
                    subtitle = "Czerwona linia = wartość obserwowana | Zielone linie = granice PU") +
                ggtheme

            print(plot)
            TRUE
        },

        .bootData = NULL
    )
)
