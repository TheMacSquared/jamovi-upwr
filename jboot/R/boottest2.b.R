# This file is a generated template, your changes will not be overwritten

bootTest2Class <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bootTest2Class",
    inherit = bootTest2Base,
    private = list(
        .run = function() {

            if (length(self$options$dep) == 0 || is.null(self$options$group))
                return()

            nBoot   <- self$options$nBoot
            ciWidth <- self$options$ciWidth / 100
            alpha   <- 1 - ciWidth
            ciMethod <- self$options$ciMethod

            groupName <- self$options$group
            groupCol  <- self$data[[groupName]]
            allLevels <- levels(groupCol)

            if (is.null(allLevels))
                allLevels <- sort(unique(as.character(groupCol)))

            if (length(allLevels) < 2)
                jmvcore::reject("Zmienna grupująca musi mieć co najmniej 2 poziomy",
                    code="error")

            # Use user-selected levels, or default to first two
            lev1 <- self$options$group1Level
            lev2 <- self$options$group2Level

            if (is.null(lev1) || lev1 == "")
                lev1 <- allLevels[1]
            if (is.null(lev2) || lev2 == "")
                lev2 <- allLevels[2]

            if (lev1 == lev2)
                jmvcore::reject("Grupa 1 i Grupa 2 muszą być różnymi poziomami",
                    code="error")

            levels <- c(lev1, lev2)

            testTable <- self$results$testTable
            descTable <- self$results$descTable
            bootDataList <- list()

            for (depName in self$options$dep) {

                column <- jmvcore::toNumeric(self$data[[depName]])
                grp    <- as.character(groupCol)

                # Keep only selected levels, remove NAs
                keep   <- !is.na(column) & !is.na(grp) & (grp %in% levels)
                column <- column[keep]
                grp    <- grp[keep]

                g1 <- column[grp == levels[1]]
                g2 <- column[grp == levels[2]]

                # Descriptive statistics table
                descTable$addRow(rowKey=paste0(depName, "_1"), values=list(
                    var   = depName,
                    group = levels[1],
                    n     = length(g1),
                    mean  = mean(g1),
                    sd    = sd(g1)
                ))
                descTable$addRow(rowKey=paste0(depName, "_2"), values=list(
                    var   = depName,
                    group = levels[2],
                    n     = length(g2),
                    mean  = mean(g2),
                    sd    = sd(g2)
                ))

                if (length(g1) < 2 || length(g2) < 2) {
                    testTable$setRow(rowKey=depName, values=list(var=depName))
                    testTable$addFootnote(rowKey=depName, col="meanDiff",
                        "Za mało obserwacji w jednej lub obu grupach")
                    next
                }

                # Bootstrap: resample within groups, compute difference of means
                dat <- data.frame(value=column, group=grp, stringsAsFactors=FALSE)

                bootFun <- function(data, i) {
                    d <- data[i, ]
                    mean(d$value[d$group == levels[1]]) -
                        mean(d$value[d$group == levels[2]])
                }

                setSeedIfNeeded(self$options$seed)

                bootResult <- boot::boot(data=dat, statistic=bootFun,
                    R=nBoot, strata=factor(dat$group))

                reps    <- bootResult$t[, 1]
                ci <- extractBootCI(bootResult, ciWidth, ciMethod)
                ciLower <- ci$lower
                ciUpper <- ci$upper

                testTable$setRow(rowKey=depName, values=list(
                    var      = depName,
                    meanDiff = bootResult$t0,
                    seBoot   = sd(reps),
                    ciLower  = ciLower,
                    ciUpper  = ciUpper,
                    n1       = length(g1),
                    n2       = length(g2)
                ))

                bootDataList[[depName]] <- list(
                    reps = reps, obs = bootResult$t0,
                    ciLower = ciLower, ciUpper = ciUpper)

                if (self$options$showClassical) {
                    tResult <- t.test(g1, g2, conf.level = ciWidth)
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

            testTable$setNote("boot", paste0(
                "Bootstrapowy PU dla różnicy średnich (metoda: ", ciMethodLabel(ciMethod), ", ",
                levels[1], " \u2212 ", levels[2], "); B = ", nBoot))

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
                    x = paste0("Bootstrapowe różnice średnich (", depName, ")"),
                    y = "Częstość",
                    subtitle = "Czerwona linia = wartość obserwowana | Zielone linie = granice PU") +
                ggtheme

            print(plot)
            TRUE
        },

        .bootData = NULL
    )
)
