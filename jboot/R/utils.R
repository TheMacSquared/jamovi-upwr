# Shared utility functions for jboot module

# Extract CI bounds from boot::boot.ci() result based on method
# Returns list(lower, upper)
extractBootCI <- function(bootResult, ciWidth, method) {
    ciObj <- boot::boot.ci(bootResult, conf = ciWidth, type = method)

    if (method == "perc") {
        vals <- ciObj$percent
        return(list(lower = vals[4], upper = vals[5]))
    } else if (method == "bca") {
        vals <- ciObj$bca
        return(list(lower = vals[4], upper = vals[5]))
    } else if (method == "basic") {
        vals <- ciObj$basic
        return(list(lower = vals[4], upper = vals[5]))
    }

    # Fallback to percentile from replicates
    reps <- bootResult$t[, 1]
    alpha <- 1 - ciWidth
    list(lower = as.numeric(quantile(reps, alpha / 2)),
         upper = as.numeric(quantile(reps, 1 - alpha / 2)))
}

# CI method label in Polish
ciMethodLabel <- function(method) {
    switch(method,
        perc  = "percentylowy",
        bca   = "BCa (skorygowany)",
        basic = "bazowy (basic)",
        method)
}

# Set seed if user provided one (> 0)
setSeedIfNeeded <- function(seed) {
    if (!is.null(seed) && seed > 0) set.seed(seed)
}

# Common bootstrap histogram plot with legend
bootHistPlot <- function(reps, obs, ciLower, ciUpper, xlab, ggtheme) {
    df <- data.frame(x = reps)

    ggplot2::ggplot(df, ggplot2::aes(x = x)) +
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
            x = xlab,
            y = "Częstość",
            subtitle = "Czerwona linia = wartość obserwowana | Zielone linie = granice PU") +
        ggtheme
}
