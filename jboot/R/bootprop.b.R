# This file is a generated template, your changes will not be overwritten

bootPropClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bootPropClass",
    inherit = bootPropBase,
    private = list(
        .run = function() {

            if (is.null(self$options$var))
                return()

            nBoot   <- self$options$nBoot
            ciWidth <- self$options$ciWidth / 100
            alpha   <- 1 - ciWidth
            ciMethod <- self$options$ciMethod

            varName <- self$options$var
            column  <- self$data[[varName]]
            column  <- column[!is.na(column)]
            n       <- length(column)

            if (n < 2) {
                self$results$propTable$addRow(rowKey="err", values=list(
                    level = "—"))
                self$results$propTable$addFootnote(rowKey="err", col="level",
                    "Za mało obserwacji")
                return()
            }

            levels <- sort(unique(as.character(column)))
            table  <- self$results$propTable
            bootDataList <- list()

            for (lvl in levels) {

                vec <- as.character(column)

                bootFun <- function(data, i) {
                    mean(data[i] == lvl)
                }

                setSeedIfNeeded(self$options$seed)
                bootResult <- boot::boot(data=vec, statistic=bootFun, R=nBoot)

                reps    <- bootResult$t[, 1]
                ci <- extractBootCI(bootResult, ciWidth, ciMethod)
                ciLower <- ci$lower
                ciUpper <- ci$upper

                table$addRow(rowKey=lvl, values=list(
                    level   = lvl,
                    count   = sum(vec == lvl),
                    prop    = bootResult$t0,
                    seBoot  = sd(reps),
                    ciLower = ciLower,
                    ciUpper = ciUpper
                ))

                bootDataList[[lvl]] <- list(
                    reps = reps, obs = bootResult$t0,
                    ciLower = ciLower, ciUpper = ciUpper)

                self$results$plots$addItem(key=lvl)
            }

            table$setNote("boot",
                paste0("Bootstrapowy PU dla proporcji (metoda: ", ciMethodLabel(ciMethod), "); B = ", nBoot))

            # Store bootstrap results for plots
            private$.bootData <- bootDataList
        },

        .plot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.bootData))
                return(FALSE)

            lvl <- image$key
            if (! lvl %in% names(private$.bootData))
                return(FALSE)

            info    <- private$.bootData[[lvl]]
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
                    x = paste0("Bootstrapowe proporcje (", lvl, ")"),
                    y = "Częstość",
                    subtitle = "Czerwona linia = wartość obserwowana | Zielone linie = granice PU") +
                ggtheme

            print(plot)
            TRUE
        },

        .bootData = NULL
    )
)
