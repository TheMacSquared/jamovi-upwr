# CI computation and plotting utilities for jCI module

#' Confidence interval for a proportion
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
        lower <- if (x == 0) 0 else qbeta(alpha / 2, x, n - x + 1)
        upper <- if (x == n) 1 else qbeta(1 - alpha / 2, x + 1, n - x)
    }

    return(list(lower = lower, upper = upper))
}

#' Confidence interval for difference of two proportions
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

#' Plot for single/paired mean CI: raw data + mean point + CI
#'
#' @param label character, label for x-axis (variable name or difference name)
#' @param x numeric vector of data
#' @param estimate mean
#' @param lower, upper CI bounds
#' @param ciWidth confidence level (percentage, e.g. 95)
#' @param ggtheme jamovi theme
#' @param theme jamovi theme colors
#' @param refLine optional numeric, draws horizontal reference line (e.g. 0 for paired)
buildMeanCIPlot <- function(label, x, estimate, lower, upper, ciWidth,
                             ggtheme, theme, refLine = NULL) {
    df <- data.frame(group = label, y = x)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = y)) +
        # Jittered raw data
        ggplot2::geom_jitter(
            width = 0.15, height = 0,
            alpha = 0.4,
            size = 2,
            color = theme$color[1]
        )

    # Optional reference line (e.g. 0 for paired difference)
    if (!is.null(refLine))
        p <- p + ggplot2::geom_hline(
            yintercept = refLine,
            linetype = "dashed", color = "grey40"
        )

    p <- p +
        # CI error bar (drawn next to data)
        ggplot2::geom_errorbar(
            data = data.frame(group = label, est = estimate,
                              lower = lower, upper = upper),
            ggplot2::aes(x = group, ymin = lower, ymax = upper, y = est),
            width = 0.3,
            linewidth = 1.2,
            color = "firebrick",
            inherit.aes = FALSE
        ) +
        # Mean point
        ggplot2::geom_point(
            data = data.frame(group = label, est = estimate),
            ggplot2::aes(x = group, y = est),
            shape = 18,  # diamond
            size = 6,
            color = "firebrick",
            inherit.aes = FALSE
        ) +
        ggplot2::labs(
            x = NULL,
            y = label,
            subtitle = sprintf("\u015brednia = %.3f; %g%% CI: [%.3f, %.3f]",
                estimate, ciWidth, lower, upper)
        ) +
        ggtheme

    return(p)
}

#' Gardner-Altman estimation plot: two groups + difference with CI
buildTwoMeansCIPlot <- function(x1, x2, group1, group2,
                                 estimate, lower, upper, ciWidth,
                                 ggtheme, theme) {
    # Combine raw data for both groups
    rawDF <- data.frame(
        group = factor(c(rep(group1, length(x1)), rep(group2, length(x2))),
                       levels = c(group1, group2)),
        y = c(x1, x2)
    )

    means <- data.frame(
        group = factor(c(group1, group2), levels = c(group1, group2)),
        mean = c(mean(x1), mean(x2))
    )

    # Reference: mean of group2 (baseline)
    ref <- mean(x2)

    # Difference "panel" — put at x = group2 + offset, plotted as error bar
    # Rescale so that zero difference aligns with group2's mean (reference)
    diffDF <- data.frame(
        group = factor("Różnica", levels = c(group1, group2, "Różnica")),
        y = ref + estimate,
        lower_y = ref + lower,
        upper_y = ref + upper
    )

    # Update factor levels to include difference column
    rawDF$group <- factor(rawDF$group, levels = c(group1, group2, "Różnica"))
    means$group <- factor(means$group, levels = c(group1, group2, "Różnica"))

    p <- ggplot2::ggplot(rawDF, ggplot2::aes(x = group, y = y)) +
        ggplot2::geom_jitter(
            width = 0.15, height = 0,
            alpha = 0.4,
            size = 2,
            color = theme$color[1]
        ) +
        # Horizontal reference line at group2 mean (baseline for difference)
        ggplot2::geom_hline(
            yintercept = ref,
            linetype = "dashed", color = "grey50"
        ) +
        # Group means
        ggplot2::geom_point(
            data = means,
            ggplot2::aes(x = group, y = mean),
            shape = 18, size = 5,
            color = "firebrick",
            inherit.aes = FALSE
        ) +
        # Difference with CI
        ggplot2::geom_errorbar(
            data = diffDF,
            ggplot2::aes(x = group, ymin = lower_y, ymax = upper_y),
            width = 0.3,
            linewidth = 1.2,
            color = "firebrick",
            inherit.aes = FALSE
        ) +
        ggplot2::geom_point(
            data = diffDF,
            ggplot2::aes(x = group, y = y),
            shape = 18, size = 6,
            color = "firebrick",
            inherit.aes = FALSE
        ) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::labs(
            x = NULL,
            y = "Warto\u015b\u0107 / r\u00f3\u017cnica",
            subtitle = sprintf("R\u00f3\u017cnica \u015brednich = %.3f; %g%% CI: [%.3f, %.3f]",
                estimate, ciWidth, lower, upper)
        ) +
        ggtheme

    return(p)
}

#' Icon plot (waffle) for a proportion with CI
#' 100 squares in 10x10 grid; colored = proportion; CI range shown as lighter color
buildProportionIconPlot <- function(label, estimate, lower, upper, ciWidth,
                                     ggtheme, theme) {
    # Grid positions
    n <- 100
    grid <- expand.grid(x = 1:10, y = 1:10)
    grid$idx <- 1:n  # 1 to 100

    # Determine color categories:
    # - certain success (idx <= round(lower*100))  -> dark
    # - within CI (round(lower*100) < idx <= round(upper*100)) -> medium
    # - certain failure (idx > round(upper*100)) -> light grey
    lowerCount <- round(lower * 100)
    upperCount <- round(upper * 100)
    estCount <- round(estimate * 100)

    grid$cat <- "Pora\u017cka"
    grid$cat[grid$idx <= upperCount] <- "W przedziale CI"
    grid$cat[grid$idx <= estCount] <- "Sukces (estymata)"
    grid$cat[grid$idx <= lowerCount] <- "Sukces (estymata)"

    # Re-categorize properly: bottom = successes up to estimate; middle = CI band
    grid$cat <- "Poza CI"
    grid$cat[grid$idx <= lowerCount] <- "Sukces (poni\u017cej CI)"
    grid$cat[grid$idx > lowerCount & grid$idx <= upperCount] <- "W przedziale CI"
    # The estimate itself is within or at boundary

    grid$cat <- factor(grid$cat,
        levels = c("Sukces (poni\u017cej CI)", "W przedziale CI", "Poza CI"),
        labels = c("Sukces", "CI", "Pora\u017cka"))

    p <- ggplot2::ggplot(grid, ggplot2::aes(x = x, y = y, fill = cat)) +
        ggplot2::geom_tile(color = "white", linewidth = 1.5) +
        ggplot2::scale_fill_manual(
            values = c(
                "Sukces" = "firebrick",
                "CI" = "#E89090",
                "Pora\u017cka" = "#E0E0E0"
            ),
            drop = FALSE
        ) +
        ggplot2::coord_fixed() +
        ggplot2::scale_y_reverse() +
        ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
        ggtheme +
        ggplot2::theme(
            axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.line = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            legend.position = "bottom",
            legend.title = ggplot2::element_blank(),
            legend.key.size = ggplot2::unit(0.8, "lines")
        )

    return(p)
}

#' Plot for difference of two proportions: two stacked bars + difference CI
buildDiffPropPlot <- function(group1, group2, p1, p2, n1, n2,
                               estimate, lower, upper, ciWidth,
                               ggtheme, theme) {
    # Two bars: one per group showing success proportion
    df_labels <- data.frame(
        group = factor(c(group1, group2), levels = c(group1, group2)),
        success = c(p1, p2),
        nTotal = c(n1, n2)
    )
    df_long <- data.frame(
        group = factor(rep(c(group1, group2), 2),
                       levels = c(group1, group2)),
        category = factor(rep(c("Sukces", "Porazka"), each = 2),
                          levels = c("Porazka", "Sukces")),
        proportion = c(p1, p2, 1 - p1, 1 - p2)
    )

    p <- ggplot2::ggplot(df_long,
                         ggplot2::aes(x = group, y = proportion, fill = category)) +
        ggplot2::geom_col(width = 0.6, color = "white") +
        ggplot2::scale_fill_manual(
            values = c("Sukces" = "firebrick", "Porazka" = "#E0E0E0"),
            labels = c("Sukces" = "Sukces", "Porazka" = "Pora\u017cka"),
            name = NULL
        ) +
        ggplot2::geom_text(
            data = df_labels,
            ggplot2::aes(x = group, y = success / 2,
                         label = sprintf("%.1f%%\n(%d/%d)",
                                         100 * success, round(success * nTotal), nTotal)),
            color = "black", fontface = "bold",
            inherit.aes = FALSE
        ) +
        ggplot2::scale_y_continuous(
            labels = function(x) paste0(round(x * 100), "%"),
            limits = c(0, 1)) +
        ggplot2::labs(x = NULL, y = "Proporcja", fill = NULL) +
        ggtheme +
        ggplot2::theme(
            legend.position = "bottom",
            legend.title = ggplot2::element_blank()
        )

    return(p)
}

#' Scatterplot with regression line and confidence band for correlation
buildCorrelationPlot <- function(x1, x2, var1Name, var2Name,
                                  estimate, lower, upper, ciWidth, method,
                                  ggtheme, theme) {
    df <- data.frame(x = x1, y = x2)
    methodLabel <- ifelse(method == "pearson", "Pearson", "Spearman")

    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(
            alpha = 0.5,
            size = 2.5,
            color = theme$color[1]
        ) +
        ggplot2::geom_smooth(
            method = "lm",
            formula = y ~ x,
            se = TRUE,
            level = ciWidth / 100,
            color = "firebrick",
            fill = "#E89090",
            alpha = 0.3
        ) +
        ggplot2::labs(
            x = var1Name,
            y = var2Name,
            subtitle = sprintf(
                "r (%s) = %.3f; %g%% CI: [%.3f, %.3f]",
                methodLabel, estimate, ciWidth, lower, upper)
        ) +
        ggtheme

    return(p)
}

#' Legacy fallback (kept for compatibility — just a simple error bar plot)
buildCIPlot <- function(label, estimate, lower, upper, ggtheme, theme) {
    df <- data.frame(
        label = label,
        estimate = estimate,
        lower = lower,
        upper = upper
    )

    p <- ggplot2::ggplot(df, ggplot2::aes(x = label, y = estimate)) +
        ggplot2::geom_point(size = 4, color = theme$color[1]) +
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
