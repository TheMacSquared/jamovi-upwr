# Axis breaks for the discrete distributions. Every integer value on the
# x axis gives hundreds of labels when n is large, so the labels are thinned
# to about `n` round values (pretty()), and only whole numbers are kept.
discreteAxisBreaks <- function(x, n = 10) {
    x <- unique(x[is.finite(x)])
    # long labels (large values) need fewer breaks to stay apart
    if (length(x) && max(abs(x)) >= 1e4) n <- min(n, 5)
    if (length(x) <= n)
        return(sort(x))
    breaks <- pretty(range(x), n = n)
    breaks[breaks == round(breaks) & breaks >= min(x) & breaks <= max(x)]
}

# jUPWR: the x range of a discrete plot. Unchanged while the support shown so
# far has at most `limit` values (all usual parameters); beyond that only the
# 0.0001-0.9999 quantile range, widened to x1/x2 when they are not in the far
# tails -- a Poisson with lambda = 1e6 or a binomial with n = 1e7 drew (and
# stored) millions of bars. The probabilities in the tables are unaffected.
discretePlotRange <- function(lo, hi, qfun, extra = numeric(0), limit = 2000) {
    if (hi - lo <= limit) return(c(lo, hi))
    lo2 <- max(lo, qfun(1e-4))
    hi2 <- min(hi, qfun(1 - 1e-4))
    ex <- extra[is.finite(extra)]
    ex <- ex[ex >= qfun(1e-9) & ex <= qfun(1 - 1e-9)]
    if (length(ex)) {
        lo2 <- max(lo, min(lo2, floor(min(ex))))
        hi2 <- min(hi, max(hi2, ceiling(max(ex))))
    }
    c(lo2, hi2)
}

# columns of a discrete distribution; with many values (more than 2000)
# a filled area instead (touching columns showed white anti-aliasing gaps):
# the base colour for the whole distribution, the highlight colour where the
# bars are highlighted
discreteColumns <- function(x, fill, outline, base = "#9f9f9f") {
    if (length(x) <= 2000)
        return(ggplot2::geom_col(fill = fill, colour = outline, width = 0.7))
    hl <- fill != base
    hlFill <- if (any(hl)) fill[hl][1] else base
    list(
        ggplot2::geom_area(fill = base, colour = NA),
        if (any(hl)) ggplot2::geom_area(
            mapping = ggplot2::aes(y = ifelse(hl, .data$Prob, 0)),
            fill = hlFill, colour = NA)
    )
}
