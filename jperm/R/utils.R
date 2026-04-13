# Permutation test engine and plot utilities for jperm module
# Pure R implementation using sample()/replicate() for didactic clarity

#' Calculate permutation p-value
#' Uses (sum + 1) / (B + 1) correction so p is never exactly 0
#'
#' @param observed numeric, observed test statistic
#' @param permDist numeric vector, permutation distribution
#' @param hypothesis character, one of "twoSided", "greater", "less"
#' @return numeric p-value
permPValue <- function(observed, permDist, hypothesis) {
    B <- length(permDist)

    count <- switch(hypothesis,
        twoSided = sum(abs(permDist) >= abs(observed)),
        greater  = sum(permDist >= observed),
        less     = sum(permDist <= observed)
    )

    return((count + 1) / (B + 1))
}

#' Generate permutation distribution for one-sample test
#' H0: mean(x) = mu0, using sign-flip of centered values
#'
#' @param x numeric vector of observations
#' @param mu0 numeric, hypothesized mean
#' @param nPerm integer, number of Monte Carlo permutations
#' @param seed integer, random seed (0 = no seed)
#' @param exact logical, use exact enumeration of all sign-flips
#' @return numeric vector of permuted test statistics
permDistOneSample <- function(x, mu0, nPerm, seed, exact) {
    centered <- x - mu0
    n <- length(centered)

    if (exact && n <= 20) {
        # Enumerate all 2^n sign-flip combinations
        nAll <- 2^n
        permDist <- numeric(nAll)
        for (i in seq_len(nAll)) {
            # Convert integer to sign pattern: 0 -> -1, 1 -> +1
            bits <- as.integer(intToBits(i - 1)[1:n])
            signs <- ifelse(bits == 1, 1, -1)
            permDist[i] <- mean(signs * centered)
        }
        return(permDist)
    }

    # Monte Carlo approximation
    if (seed != 0) set.seed(seed)

    permDist <- replicate(nPerm, {
        signs <- sample(c(-1, 1), n, replace = TRUE)
        mean(signs * centered)
    })

    return(permDist)
}

#' Generate permutation distribution for two independent samples
#' H0: mean(group1) = mean(group2), using label shuffling
#'
#' @param x numeric vector of all observations
#' @param group factor with exactly 2 levels
#' @param nPerm integer, number of Monte Carlo permutations
#' @param seed integer, random seed (0 = no seed)
#' @param exact logical, use exact enumeration
#' @return numeric vector of permuted test statistics
permDistTwoSample <- function(x, group, nPerm, seed, exact) {
    levs <- levels(group)
    n <- length(x)
    n1 <- sum(group == levs[1])

    if (exact && choose(n, n1) <= 100000) {
        # Enumerate all possible group assignments
        combos <- utils::combn(n, n1)
        nAll <- ncol(combos)
        permDist <- numeric(nAll)
        for (i in seq_len(nAll)) {
            idx1 <- combos[, i]
            permDist[i] <- mean(x[idx1]) - mean(x[-idx1])
        }
        return(permDist)
    }

    # Monte Carlo approximation
    if (seed != 0) set.seed(seed)

    permDist <- replicate(nPerm, {
        shuffled <- sample(group)
        mean(x[shuffled == levs[1]]) - mean(x[shuffled == levs[2]])
    })

    return(permDist)
}

#' Generate permutation distribution for paired samples
#' Delegates to one-sample sign-flip on differences with mu0 = 0
#'
#' @param d numeric vector of paired differences
#' @param nPerm integer, number of Monte Carlo permutations
#' @param seed integer, random seed (0 = no seed)
#' @param exact logical, use exact enumeration
#' @return numeric vector of permuted test statistics
permDistPaired <- function(d, nPerm, seed, exact) {
    return(permDistOneSample(d, mu0 = 0, nPerm = nPerm, seed = seed, exact = exact))
}

#' Build permutation distribution plot
#' Histogram with observed value (red dashed line) and shaded rejection region
#'
#' @param permDist numeric vector of permutation distribution
#' @param observed numeric, observed test statistic
#' @param hypothesis character, one of "twoSided", "greater", "less"
#' @param ggtheme ggplot2 theme object from jamovi
#' @param theme list with jamovi theme colors
#' @return ggplot object
buildPermPlot <- function(permDist, observed, hypothesis, ggtheme, theme) {
    df <- data.frame(stat = permDist)

    # Determine rejection region for shading
    df$reject <- switch(hypothesis,
        twoSided = abs(df$stat) >= abs(observed),
        greater  = df$stat >= observed,
        less     = df$stat <= observed
    )

    p <- ggplot2::ggplot(df, ggplot2::aes(x = stat)) +
        ggplot2::geom_histogram(
            bins = 40,
            fill = theme$fill[2],
            color = theme$color[1],
            alpha = 0.7
        ) +
        ggplot2::geom_histogram(
            data = df[df$reject, , drop = FALSE],
            ggplot2::aes(x = stat),
            bins = 40,
            fill = "firebrick",
            color = theme$color[1],
            alpha = 0.7
        ) +
        ggplot2::geom_vline(
            xintercept = observed,
            linetype = "dashed",
            color = "red",
            linewidth = 1.2
        ) +
        ggplot2::labs(
            x = "Statystyka permutacyjna",
            y = "Liczebnosc"
        ) +
        ggtheme

    return(p)
}
