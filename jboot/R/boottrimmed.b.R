# This file is a generated template, your changes will not be overwritten

bootTrimmedClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bootTrimmedClass",
    inherit = bootTrimmedBase,
    private = list(
        .run = function() {

            if (length(self$options$dep) == 0)
                return()

            nBoot    <- self$options$nBoot
            ciWidth  <- self$options$ciWidth / 100
            ciMethod <- self$options$ciMethod
            trimProp <- self$options$trimProp

            table <- self$results$trimTable
            bootDataList <- list()

            for (depName in self$options$dep) {

                column <- jmvcore::toNumeric(self$data[[depName]])
                column <- column[!is.na(column)]
                n <- length(column)

                if (n < 3) {
                    table$setRow(rowKey=depName, values=list(var=depName))
                    table$addFootnote(rowKey=depName, col="trimMean",
                        "Za mało obserwacji")
                    next
                }

                bootFun <- function(data, i) mean(data[i], trim=trimProp)
                setSeedIfNeeded(self$options$seed)
                bootResult <- boot::boot(data=column, statistic=bootFun, R=nBoot)

                reps <- bootResult$t[, 1]
                ci   <- extractBootCI(bootResult, ciWidth, ciMethod)

                table$setRow(rowKey=depName, values=list(
                    var      = depName,
                    trimMean = bootResult$t0,
                    mean     = mean(column),
                    seBoot   = sd(reps),
                    ciLower  = ci$lower,
                    ciUpper  = ci$upper,
                    n        = n
                ))

                bootDataList[[depName]] <- list(
                    reps = reps, obs = bootResult$t0,
                    ciLower = ci$lower, ciUpper = ci$upper)
            }

            table$setNote("boot", paste0(
                "Bootstrapowy PU dla średniej uciętej (",
                trimProp * 100, "%, metoda: ",
                ciMethodLabel(ciMethod), "); B = ", nBoot))

            private$.bootData <- bootDataList
        },

        .plot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.bootData))
                return(FALSE)

            depName <- image$key
            if (! depName %in% names(private$.bootData))
                return(FALSE)

            info <- private$.bootData[[depName]]
            df   <- data.frame(x = info$reps)

            plot <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
                ggplot2::geom_histogram(bins = 40, fill = "steelblue",
                    color = "white", alpha = 0.7) +
                ggplot2::geom_vline(xintercept = info$obs,
                    color = "red", linewidth = 1) +
                ggplot2::geom_vline(xintercept = c(info$ciLower, info$ciUpper),
                    color = "darkgreen", linetype = "dashed", linewidth = 0.8) +
                ggplot2::annotate("rect",
                    xmin = info$ciLower, xmax = info$ciUpper,
                    ymin = -Inf, ymax = Inf,
                    fill = "green", alpha = 0.1) +
                ggplot2::labs(
                    x = paste0("Bootstrapowe średnie ucięte (", depName, ")"),
                    y = "Częstość",
                    subtitle = "Czerwona linia = wartość obserwowana | Zielone linie = granice PU") +
                ggtheme

            print(plot)
            TRUE
        },

        .bootData = NULL
    )
)
