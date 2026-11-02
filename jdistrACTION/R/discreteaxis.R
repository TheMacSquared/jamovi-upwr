# Axis breaks for the discrete distributions. Every integer value on the
# x axis gives hundreds of labels when n is large, so the labels are thinned
# to about `n` round values (pretty()), and only whole numbers are kept.
discreteAxisBreaks <- function(x, n = 10) {
    x <- unique(x[is.finite(x)])
    if (length(x) <= n)
        return(sort(x))
    breaks <- pretty(range(x), n = n)
    breaks[breaks == round(breaks) & breaks >= min(x) & breaks <= max(x)]
}
