# This file is a generated template, your changes will not be overwritten

bootPairedClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bootPairedClass",
    inherit = bootPairedBase,
    private = list(
        .run = function() {

            if (is.null(self$options$pair1) || is.null(self$options$pair2))
                return()

            nBoot    <- self$options$nBoot
            ciWidth  <- self$options$ciWidth / 100
            alpha    <- 1 - ciWidth
            ciMethod <- self$options$ciMethod

            col1 <- jmvcore::toNumeric(self$data[[self$options$pair1]])
            col2 <- jmvcore::toNumeric(self$data[[self$options$pair2]])

            # Pairwise complete
            keep <- !is.na(col1) & !is.na(col2)
            col1 <- col1[keep]
            col2 <- col2[keep]
            n    <- length(col1)

            # Descriptive table
            descTable <- self$results$descTable
            descTable$addRow(rowKey="v1", values=list(
                var  = self$options$pair1,
                n    = n,
                mean = mean(col1),
                sd   = sd(col1)
            ))
            descTable$addRow(rowKey="v2", values=list(
                var  = self$options$pair2,
                n    = n,
                mean = mean(col2),
                sd   = sd(col2)
            ))

            if (n < 3) {
                testTable <- self$results$testTable
                testTable$addRow(rowKey="res", values=list(n = n))
                testTable$addFootnote(rowKey="res", col="meanDiff",
                    "Za mało kompletnych par obserwacji")
                return()
            }

            diffs <- col1 - col2

            bootFun <- function(data, i) mean(data[i])
            setSeedIfNeeded(self$options$seed)
            bootResult <- boot::boot(data=diffs, statistic=bootFun, R=nBoot)

            reps <- bootResult$t[, 1]
            ci   <- extractBootCI(bootResult, ciWidth, ciMethod)

            testTable <- self$results$testTable
            testTable$addRow(rowKey="res", values=list(
                meanDiff = bootResult$t0,
                seBoot   = sd(reps),
                ciLower  = ci$lower,
                ciUpper  = ci$upper,
                n        = n
            ))

            testTable$setNote("boot", paste0(
                "Bootstrapowy CI dla średniej różnic (metoda: ",
                ciMethodLabel(ciMethod), "); B = ", nBoot))

            private$.bootData <- list(
                reps = reps, obs = bootResult$t0,
                ciLower = ci$lower, ciUpper = ci$upper)
        },

        .plot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.bootData))
                return(FALSE)

            info    <- private$.bootData
            reps    <- info$reps
            obs     <- info$obs
            ciLower <- info$ciLower
            ciUpper <- info$ciUpper

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
                    x = "Bootstrapowe średnie różnic",
                    y = "Częstość",
                    subtitle = "Czerwona linia = wartość obserwowana | Zielone linie = granice CI") +
                ggtheme

            print(plot)
            TRUE
        },

        .bootData = NULL
    )
)
