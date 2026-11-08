# Logarithms avoid Inf - Inf in the variance at very small shape values.
# An unrepresentable finite moment is reported as Inf, never an artificial NaN.
weibullMoments <- function(shape, scale) {
    logFirst <- lgamma(1 + 1 / shape)
    logSecond <- lgamma(1 + 2 / shape)
    list(mean = exp(log(scale) + logFirst),
         variance = exp(2 * log(scale) + logSecond + log1p(-exp(2 * logFirst - logSecond))))
}
