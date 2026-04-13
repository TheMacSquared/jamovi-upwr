# CI computation and plotting utilities for jCI module

#' Confidence interval for a proportion
#'
#' @param x integer, number of successes
#' @param n integer, total observations
#' @param ciWidth numeric, confidence level (0-1)
#' @param method character, one of "wald", "wilson", "clopperPearson"
#' @return list with lower, upper
ciProportion <- function(x, n, ciWidth, method) {
    alpha <- 1 - ciWidth
    phat <- x / n

    if (method == "wald") {
        z <- qnorm(1 - alpha / 2)
        se <- sqrt(phat * (1 - phat) / n)
        lower <- max(0, phat - z * se)
        upper <- min(1, phat + z * se)

    } else if (method == "wilson") {
        z <- qnorm(1 - alpha / 2)
        z2 <- z^2
        denom <- 1 + z2 / n
        centre <- (phat + z2 / (2 * n)) / denom
        margin <- z * sqrt((phat * (1 - phat) / n + z2 / (4 * n^2))) / denom
        lower <- max(0, centre - margin)
        upper <- min(1, centre + margin)

    } else {
        # Clopper-Pearson (exact)
        lower <- if (x == 0) 0 else qbeta(alpha / 2, x, n - x + 1)
        upper <- if (x == n) 1 else qbeta(1 - alpha / 2, x + 1, n - x)
    }

    return(list(lower = lower, upper = upper))
}

#' Confidence interval for difference of two proportions
#'
#' @param x1 integer, successes in group 1
#' @param n1 integer, total in group 1
#' @param x2 integer, successes in group 2
#' @param n2 integer, total in group 2
#' @param ciWidth numeric, confidence level (0-1)
#' @param method character, one of "wald", "newcombe"
#' @return list with lower, upper
ciDiffProportion <- function(x1, n1, x2, n2, ciWidth, method) {
    alpha <- 1 - ciWidth
    p1 <- x1 / n1
    p2 <- x2 / n2
    est <- p1 - p2

    if (method == "wald") {
        z <- qnorm(1 - alpha / 2)
        se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
        lower <- est - z * se
        upper <- est + z * se

    } else {
        # Newcombe (method 10) — based on Wilson intervals for each proportion
        z <- qnorm(1 - alpha / 2)

        ci1 <- ciProportion(x1, n1, ciWidth, "wilson")
        ci2 <- ciProportion(x2, n2, ciWidth, "wilson")

        lower <- est - z * sqrt(ci1$lower * (1 - ci1$lower) / n1 +
            ci2$upper * (1 - ci2$upper) / n2)
        upper <- est + z * sqrt(ci1$upper * (1 - ci1$upper) / n1 +
            ci2$lower * (1 - ci2$lower) / n2)
    }

    return(list(lower = lower, upper = upper))
}

#' Build error bar plot for CI
#'
#' @param label character, label for the estimate
#' @param estimate numeric, point estimate
#' @param lower numeric, lower CI bound
#' @param upper numeric, upper CI bound
#' @param ggtheme ggplot2 theme from jamovi
#' @param theme list with jamovi theme colors
#' @return ggplot object
buildCIPlot <- function(label, estimate, lower, upper, ggtheme, theme) {
    df <- data.frame(
        label = label,
        estimate = estimate,
        lower = lower,
        upper = upper
    )

    p <- ggplot2::ggplot(df, ggplot2::aes(x = label, y = estimate)) +
        ggplot2::geom_point(
            size = 4,
            color = theme$color[1]
        ) +
        ggplot2::geom_errorbar(
            ggplot2::aes(ymin = lower, ymax = upper),
            width = 0.15,
            linewidth = 0.8,
            color = theme$color[1]
        ) +
        ggplot2::labs(x = "", y = "Estymata") +
        ggtheme

    return(p)
}
