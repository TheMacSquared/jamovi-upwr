# This file is a generated template, your changes will not be overwritten

bootMultiGroupClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bootMultiGroupClass",
    inherit = bootMultiGroupBase,
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
            levels    <- levels(groupCol)

            if (is.null(levels))
                levels <- sort(unique(as.character(groupCol)))

            if (length(levels) < 2)
                jmvcore::reject("Zmienna grupująca musi mieć co najmniej 2 poziomy",
                    code="error")

            table <- self$results$groupTable
            bootDataList <- list()

            for (depName in self$options$dep) {

                column <- jmvcore::toNumeric(self$data[[depName]])
                grp    <- as.character(groupCol)
                groupReps <- list()

                for (lvl in levels) {

                    values <- column[grp == lvl & !is.na(column) & !is.na(grp)]
                    n      <- length(values)
                    rowKey <- paste0(depName, "_", lvl)

                    if (n < 2) {
                        table$addRow(rowKey=rowKey, values=list(
                            var   = depName,
                            group = lvl,
                            n     = n))
                        table$addFootnote(rowKey=rowKey, col="mean",
                            "Za mało obserwacji")
                        next
                    }

                    bootFun <- function(data, i) mean(data[i])
                    setSeedIfNeeded(self$options$seed)
                    bootResult <- boot::boot(data=values, statistic=bootFun, R=nBoot)

                    reps    <- bootResult$t[, 1]
                    ci <- extractBootCI(bootResult, ciWidth, ciMethod)
                    ciLower <- ci$lower
                    ciUpper <- ci$upper

                    table$addRow(rowKey=rowKey, values=list(
                        var     = depName,
                        group   = lvl,
                        n       = n,
                        mean    = bootResult$t0,
                        seBoot  = sd(reps),
                        ciLower = ciLower,
                        ciUpper = ciUpper
                    ))

                    groupReps[[lvl]] <- reps
                }

                bootDataList[[depName]] <- groupReps
            }

            table$setNote("boot", paste0("Bootstrapowe PU per grupa (metoda: ", ciMethodLabel(ciMethod), "); B = ", nBoot, ". Niepokrywające się PU sugerują istotną różnicę."))

            private$.bootData  <- bootDataList
            private$.groupName <- groupName
        },

        .plot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.bootData))
                return(FALSE)

            depName <- image$key
            if (! depName %in% names(private$.bootData))
                return(FALSE)

            groupReps <- private$.bootData[[depName]]
            if (length(groupReps) == 0)
                return(FALSE)

            allReps <- data.frame(x = numeric(0), group = character(0),
                stringsAsFactors = FALSE)

            for (lvl in names(groupReps)) {
                allReps <- rbind(allReps, data.frame(
                    x = groupReps[[lvl]], group = lvl,
                    stringsAsFactors = FALSE))
            }

            plot <- ggplot2::ggplot(allReps,
                    ggplot2::aes(x = x, fill = group)) +
                ggplot2::geom_histogram(bins = 40, alpha = 0.5,
                    position = "identity", color = "white") +
                ggplot2::labs(
                    x = paste0("Bootstrapowe średnie (", depName, ")"),
                    y = "Częstość",
                    fill = private$.groupName) +
                ggtheme

            print(plot)
            TRUE
        },

        .bootData = NULL,
        .groupName = NULL
    )
)
