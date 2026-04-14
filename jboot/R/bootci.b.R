# This file is a generated template, your changes will not be overwritten

bootCIClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bootCIClass",
    inherit = bootCIBase,
    private = list(
        .run = function() {

            if (length(self$options$dep) == 0)
                return()

            nBoot   <- self$options$nBoot
            ciWidth <- self$options$ciWidth / 100
            alpha   <- 1 - ciWidth
            ciMethod <- self$options$ciMethod
            stat    <- self$options$stat
            trimProp <- self$options$trimProp

            # Select statistic function based on `stat` option
            bootFun <- switch(stat,
                mean    = function(data, i) mean(data[i]),
                median  = function(data, i) median(data[i]),
                trimmed = function(data, i) mean(data[i], trim = trimProp)
            )
            statLabel <- switch(stat,
                mean = "\u015brednia",
                median = "Mediana",
                trimmed = paste0("\u015arednia uci\u0119ta (trim = ", trimProp, ")")
            )

            table <- self$results$ciTable
            bootDataList <- list()

            for (depName in self$options$dep) {

                column <- jmvcore::toNumeric(self$data[[depName]])
                column <- column[!is.na(column)]
                n <- length(column)

                if (n < 2) {
                    table$setRow(rowKey=depName, values=list(var=depName))
                    table$addFootnote(rowKey=depName, col="mean",
                        "Za mało obserwacji")
                    next
                }

                setSeedIfNeeded(self$options$seed)
                bootResult <- boot::boot(data=column, statistic=bootFun, R=nBoot)

                reps <- bootResult$t[, 1]
                ci <- extractBootCI(bootResult, ciWidth, ciMethod)
                ciLower <- ci$lower
                ciUpper <- ci$upper

                table$setRow(rowKey=depName, values=list(
                    var     = depName,
                    mean    = bootResult$t0,
                    seBoot  = sd(reps),
                    ciLower = ciLower,
                    ciUpper = ciUpper,
                    bias    = mean(reps) - bootResult$t0,
                    n       = n
                ))

                bootDataList[[depName]] <- list(
                    reps = reps, obs = bootResult$t0,
                    ciLower = ciLower, ciUpper = ciUpper)

                if (self$options$showClassical && stat == "mean") {
                    tResult <- t.test(column, conf.level = ciWidth)
                    classTable <- self$results$classicalTable
                    classTable$setRow(rowKey=depName, values=list(
                        var          = depName,
                        t            = tResult$statistic,
                        df           = tResult$parameter,
                        p            = tResult$p.value,
                        ciLowerClass = tResult$conf.int[1],
                        ciUpperClass = tResult$conf.int[2]
                    ))
                }
            }

            table$setNote("stat", paste0("Miara centralna: ", statLabel))
            table$setNote("boot",
                paste0("Bootstrapowy CI (metoda: ", ciMethodLabel(ciMethod), "); B = ", nBoot))

            private$.bootData <- bootDataList
        },

        .plot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.bootData))
                return(FALSE)

            depName <- image$key
            if (! depName %in% names(private$.bootData))
                return(FALSE)

            info    <- private$.bootData[[depName]]
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
                    x = paste0("Bootstrapowe średnie (", depName, ")"),
                    y = "Częstość",
                    subtitle = "Czerwona linia = wartość obserwowana | Zielone linie = granice CI") +
                ggtheme

            print(plot)
            TRUE
        },

        .bootData = NULL
    )
)
