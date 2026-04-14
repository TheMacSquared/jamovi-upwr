# This file is a generated template, your changes will not be overwritten

bootConvergenceClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bootConvergenceClass",
    inherit = bootConvergenceBase,
    private = list(
        .run = function() {

            if (is.null(self$options$dep))
                return()

            ciWidth <- self$options$ciWidth / 100
            alpha   <- 1 - ciWidth
            seed    <- self$options$seed

            column <- jmvcore::toNumeric(self$data[[self$options$dep]])
            column <- column[!is.na(column)]
            n      <- length(column)

            if (n < 2)
                return()

            bValues <- c(50, 100, 200, 500, 1000, 2000, 5000)
            bootFun <- function(data, i) mean(data[i])

            table    <- self$results$convTable
            plotData <- data.frame(
                b = integer(0), ciLower = numeric(0), ciUpper = numeric(0))

            for (bVal in bValues) {
                set.seed(seed)
                bootResult <- boot::boot(data=column, statistic=bootFun, R=bVal)
                reps <- bootResult$t[, 1]

                ciLower  <- as.numeric(quantile(reps, alpha / 2))
                ciUpper  <- as.numeric(quantile(reps, 1 - alpha / 2))
                ciWidthV <- ciUpper - ciLower

                table$addRow(rowKey=as.character(bVal), values=list(
                    b       = bVal,
                    seBoot  = sd(reps),
                    ciLower = ciLower,
                    ciUpper = ciUpper,
                    ciWidth = ciWidthV
                ))

                plotData <- rbind(plotData, data.frame(
                    b = bVal, ciLower = ciLower, ciUpper = ciUpper))
            }

            table$setNote("info", paste0(
                "Percentylowy CI (", self$options$ciWidth,
                "%); ziarno = ", seed))

            private$.plotData <- plotData
        },

        .plot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.plotData))
                return(FALSE)

            pd <- private$.plotData

            # Reshape for ggplot: two lines (lower and upper)
            dfLong <- data.frame(
                b     = rep(pd$b, 2),
                value = c(pd$ciLower, pd$ciUpper),
                bound = rep(c("Dolna granica", "Górna granica"), each = nrow(pd))
            )

            plot <- ggplot2::ggplot(dfLong,
                    ggplot2::aes(x = b, y = value, color = bound)) +
                ggplot2::geom_line(linewidth = 1) +
                ggplot2::geom_point(size = 2) +
                ggplot2::scale_x_log10(breaks = pd$b) +
                ggplot2::labs(
                    x = "Liczba prób bootstrapowych (B)",
                    y = "Granice CI",
                    color = "") +
                ggtheme

            print(plot)
            TRUE
        },

        .plotData = NULL
    )
)
