# This file is a generated template, your changes will not be overwritten

bootRegressionClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bootRegressionClass",
    inherit = bootRegressionBase,
    private = list(
        .run = function() {

            if (is.null(self$options$dep) || is.null(self$options$pred))
                return()

            nBoot    <- self$options$nBoot
            ciWidth  <- self$options$ciWidth / 100
            ciMethod <- self$options$ciMethod

            y <- jmvcore::toNumeric(self$data[[self$options$dep]])
            x <- jmvcore::toNumeric(self$data[[self$options$pred]])

            keep <- !is.na(x) & !is.na(y)
            x <- x[keep]
            y <- y[keep]
            n <- length(x)

            if (n < 4) {
                coefTable <- self$results$coefTable
                coefTable$addRow(rowKey="err", values=list(
                    term = "—"))
                coefTable$addFootnote(rowKey="err", col="term",
                    "Za mało kompletnych obserwacji")
                return()
            }

            # Observed model
            fit <- lm(y ~ x)
            coefs <- coef(fit)
            r2 <- summary(fit)$r.squared

            # Bootstrap slope (case resampling)
            dat <- data.frame(x = x, y = y)

            bootFunSlope <- function(data, i) {
                d <- data[i, ]
                coef(lm(y ~ x, data = d))[2]
            }
            bootFunIntercept <- function(data, i) {
                d <- data[i, ]
                coef(lm(y ~ x, data = d))[1]
            }

            setSeedIfNeeded(self$options$seed)
            bootSlope     <- boot::boot(data=dat, statistic=bootFunSlope, R=nBoot)
            setSeedIfNeeded(self$options$seed)
            bootIntercept <- boot::boot(data=dat, statistic=bootFunIntercept, R=nBoot)

            ciSlope     <- extractBootCI(bootSlope, ciWidth, ciMethod)
            ciIntercept <- extractBootCI(bootIntercept, ciWidth, ciMethod)

            # Coefficients table
            coefTable <- self$results$coefTable
            coefTable$addRow(rowKey="intercept", values=list(
                term     = "Wyraz wolny (intercept)",
                estimate = coefs[1],
                seBoot   = sd(bootIntercept$t[, 1]),
                ciLower  = ciIntercept$lower,
                ciUpper  = ciIntercept$upper
            ))
            coefTable$addRow(rowKey="slope", values=list(
                term     = paste0("Nachylenie (", self$options$pred, ")"),
                estimate = coefs[2],
                seBoot   = sd(bootSlope$t[, 1]),
                ciLower  = ciSlope$lower,
                ciUpper  = ciSlope$upper
            ))

            coefTable$setNote("boot", paste0(
                "Bootstrapowy PU (metoda: ", ciMethodLabel(ciMethod),
                "); B = ", nBoot))

            # Fit table
            fitTable <- self$results$fitTable
            fitTable$addRow(rowKey="r2", values=list(
                stat  = "R²",
                value = r2
            ))
            fitTable$addRow(rowKey="n", values=list(
                stat  = "N",
                value = n
            ))

            private$.bootData <- list(
                reps = bootSlope$t[, 1], obs = coefs[2],
                ciLower = ciSlope$lower, ciUpper = ciSlope$upper)

            # Store data for scatter plot
            private$.scatterData <- list(
                x = x, y = y,
                intercepts = bootIntercept$t[, 1],
                slopes     = bootSlope$t[, 1],
                obsIntercept = coefs[1],
                obsSlope     = coefs[2],
                ciWidth = ciWidth)
        },

        .plot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.bootData))
                return(FALSE)

            info <- private$.bootData
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
                    x = "Bootstrapowe nachylenia (slope)",
                    y = "Częstość",
                    subtitle = "Czerwona linia = wartość obserwowana | Zielone linie = granice PU") +
                ggtheme

            print(plot)
            TRUE
        },

        .plotScatter = function(image, ggtheme, theme, ...) {

            if (is.null(private$.scatterData))
                return(FALSE)

            sd <- private$.scatterData
            dat <- data.frame(x = sd$x, y = sd$y)

            # Generate prediction grid
            xRange <- range(sd$x)
            xGrid  <- seq(xRange[1], xRange[2], length.out = 100)

            # Compute fitted line for each bootstrap replicate
            nBoot <- length(sd$slopes)
            fitMatrix <- matrix(NA, nrow = length(xGrid), ncol = nBoot)
            for (b in seq_len(nBoot)) {
                fitMatrix[, b] <- sd$intercepts[b] + sd$slopes[b] * xGrid
            }

            # Percentile CI band at each x
            alpha <- 1 - sd$ciWidth
            ciLower <- apply(fitMatrix, 1, quantile, probs = alpha / 2)
            ciUpper <- apply(fitMatrix, 1, quantile, probs = 1 - alpha / 2)
            yFit    <- sd$obsIntercept + sd$obsSlope * xGrid

            bandDf <- data.frame(x = xGrid, yFit = yFit,
                ciLower = ciLower, ciUpper = ciUpper)

            depName  <- self$options$dep
            predName <- self$options$pred

            plot <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y)) +
                ggplot2::geom_point(alpha = 0.5, color = "grey40") +
                ggplot2::geom_ribbon(data = bandDf,
                    ggplot2::aes(x = x, ymin = ciLower, ymax = ciUpper, y = NULL),
                    fill = "steelblue", alpha = 0.3) +
                ggplot2::geom_line(data = bandDf,
                    ggplot2::aes(x = x, y = yFit),
                    color = "red", linewidth = 1) +
                ggplot2::labs(
                    x = predName,
                    y = depName,
                    subtitle = paste0("Czerwona linia = regresja obserwowana | ",
                        "Niebieskie pasmo = bootstrapowy PU (",
                        round(sd$ciWidth * 100), "%)")) +
                ggtheme

            print(plot)
            TRUE
        },

        .bootData = NULL,
        .scatterData = NULL
    )
)
