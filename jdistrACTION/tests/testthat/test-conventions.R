# Pure-formula contract tests for the convention mapping used in
# negbinomialdistribution.b.R and geometricdistribution.b.R.
# They pin down the offset identities against R's d/p/q functions,
# including the discrete edge cases (non-integer x, values before the
# support, closed interval bounds, p = 1 degeneracy).

# negative binomial, trials convention: X = trial number of the r-th success
nbTrialsD <- function(k, r, p)
  ifelse(k == floor(k) & k >= r, dnbinom(k - r, size = r, prob = p), 0)
nbTrialsP <- function(k, r, p)
  pnbinom(floor(k) - r, size = r, prob = p)
nbTrialsQ <- function(q, r, p)
  qnbinom(q, size = r, prob = p) + r

test_that("negative binomial trials convention matches the shifted R functions", {
  r <- 3; p <- 0.4
  # pmf sums to 1 over the shifted support
  expect_equal(sum(nbTrialsD(r:200, r, p)), 1, tolerance = 1e-9)
  # P(X = k) equals choose(k-1, r-1) p^r (1-p)^(k-r) — the lecture formula
  for (k in c(3, 5, 10)) {
    expect_equal(nbTrialsD(k, r, p),
                 choose(k - 1, r - 1) * p^r * (1 - p)^(k - r),
                 tolerance = 1e-12)
  }
  # CDF consistent with pmf accumulation
  expect_equal(nbTrialsP(7, r, p), sum(nbTrialsD(r:7, r, p)), tolerance = 1e-12)
  # quantile function undoes the CDF on the shifted support
  q <- nbTrialsQ(0.8, r, p)
  expect_gte(nbTrialsP(q, r, p), 0.8)
  expect_lt(nbTrialsP(q - 1, r, p), 0.8)
})

test_that("negative binomial trials convention: discrete edge cases", {
  r <- 3; p <- 0.4
  expect_equal(nbTrialsD(4.5, r, p), 0)     # non-integer x -> P(X = x) = 0
  expect_equal(nbTrialsD(2, r, p), 0)       # x < r is before the support
  expect_equal(nbTrialsP(2, r, p), 0)       # CDF is 0 before the support
  expect_equal(nbTrialsP(4.7, r, p), nbTrialsP(4, r, p))  # CDF steps at integers
  # p = 1 degenerates to X = r
  expect_equal(nbTrialsD(r, r, 1), 1)
  expect_equal(nbTrialsQ(0.5, r, 1), r)
  # closed interval [x1, x2]: P = F(x2) - F(ceil(x1) - 1) includes both ends
  x1 <- 4; x2 <- 6
  expect_equal(nbTrialsP(x2, r, p) - nbTrialsP(ceiling(x1) - 1, r, p),
               sum(nbTrialsD(4:6, r, p)), tolerance = 1e-12)
})

test_that("negative binomial failures convention is plain dnbinom/pnbinom/qnbinom", {
  r <- 3; p <- 0.4
  expect_equal(dnbinom(2, r, p), nbTrialsD(2 + r, r, p), tolerance = 1e-12)
  expect_equal(pnbinom(5, r, p), nbTrialsP(5 + r, r, p), tolerance = 1e-12)
  expect_equal(qnbinom(0.8, r, p), nbTrialsQ(0.8, r, p) - r)
  # moments differ between conventions by the constant shift r
  expect_equal(r / p, r * (1 - p) / p + r, tolerance = 1e-12)
})

# geometric, trials convention: X = trial number of the first success
geomTrialsD <- function(k, p)
  ifelse(k == floor(k) & k >= 1, dgeom(k - 1, p), 0)
geomTrialsP <- function(k, p)
  pgeom(floor(k) - 1, p)
geomTrialsQ <- function(q, p)
  qgeom(q, p) + 1

test_that("geometric trials convention matches the shifted R functions", {
  p <- 0.3
  expect_equal(geomTrialsD(1, p), p, tolerance = 1e-12)          # P(X = 1) = p
  expect_equal(geomTrialsD(4, p), (1 - p)^3 * p, tolerance = 1e-12)
  expect_equal(geomTrialsP(4, p), 1 - (1 - p)^4, tolerance = 1e-12)
  expect_equal(geomTrialsD(0, p), 0)                             # before support
  expect_equal(geomTrialsD(2.5, p), 0)                           # non-integer
  expect_equal(1 / p, (1 - p) / p + 1, tolerance = 1e-12)        # mean shift
  q <- geomTrialsQ(0.9, p)
  expect_gte(geomTrialsP(q, p), 0.9)
  expect_lt(geomTrialsP(q - 1, p), 0.9)
})

test_that("geometric failures convention (module default) is plain dgeom/pgeom", {
  p <- 0.3
  expect_equal(dgeom(0, p), p, tolerance = 1e-12)
  expect_equal(pgeom(3, p), 1 - (1 - p)^4, tolerance = 1e-12)
})

# continuous additions: reference identities used by the new analyses
test_that("gamma shape+rate parametrization and moments", {
  a <- 2; l <- 1.5
  expect_equal(pgamma(2, a, l), pgamma(2, shape = a, rate = l))
  expect_equal(a / l, a * (1 / l), tolerance = 1e-12)  # E[X] = alpha * theta
  # alpha = 1 reduces to the exponential distribution
  expect_equal(pgamma(2, 1, l), pexp(2, l), tolerance = 1e-12)
  # upper tail computed directly is stable far in the right tail
  x <- qgamma(1 - 1e-12, a, l)
  expect_gt(pgamma(x, a, l, lower.tail = FALSE), 0)
})

test_that("weibull shape+scale parametrization and moments", {
  b <- 1.5; eta <- 2
  expect_equal(pweibull(2, shape = b, scale = eta),
               1 - exp(-(2 / eta)^b), tolerance = 1e-12)
  expect_equal(eta * gamma(1 + 1 / b),
               integrate(function(t) t * dweibull(t, b, eta), 0, Inf)$value,
               tolerance = 1e-6)
  # beta = 1 reduces to the exponential with rate 1/eta
  expect_equal(pweibull(2, 1, eta), pexp(2, 1 / eta), tolerance = 1e-12)
  # shape < 1: density is unbounded at 0, finite at any epsilon
  expect_true(is.infinite(dweibull(0, 0.5, eta)))
  expect_true(is.finite(dweibull(1e-8, 0.5, eta)))
})
