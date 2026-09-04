skip_if_not_installed("jCzest")

txt <- function(r) paste(capture.output(print(r)), collapse = "\n")

# dwa pomiary tej samej jednostki, dane zagregowane (40 / 25 / 10 / 25)
pary <- data.frame(przed = factor(c("tak", "tak", "nie", "nie")),
                   po    = factor(c("tak", "nie", "tak", "nie")),
                   n     = c(40, 25, 10, 25))
# trzy pomiary binarne, format szeroki
set.seed(11)
n <- 80
trzy <- data.frame(
    p1 = factor(ifelse(rbinom(n, 1, 0.60) == 1, "tak", "nie")),
    p2 = factor(ifelse(rbinom(n, 1, 0.45) == 1, "tak", "nie")),
    p3 = factor(ifelse(rbinom(n, 1, 0.25) == 1, "tak", "nie")))

test_that("dwa pomiary: McNemar zgodny z mcnemar.test", {
    res <- jCzest:::zalezne(data = pary, vars = c("przed", "po"), counts = "n")
    tests <- res$tests$asDF
    # poziomy alfabetycznie (nie, tak), wiec tabela par to:
    #        nie tak          b = [nie->tak] = 10, c = [tak->nie] = 25
    #   nie   25  10
    #   tak   25  40
    tab <- matrix(c(25, 10, 25, 40), 2, 2, byrow = TRUE)
    expect_match(tests$test[1], "McNemar")
    expect_equal(tests$stat[1], unname(mcnemar.test(as.table(tab), correct = FALSE)$statistic))
    expect_equal(tests$df[1], 1)

    zPopr <- jCzest:::zalezne(data = pary, vars = c("przed", "po"), counts = "n", corr = TRUE)
    expect_equal(zPopr$tests$asDF$stat[1],
                 unname(mcnemar.test(as.table(tab), correct = TRUE)$statistic))
})

test_that("OR par niezgodnych z przedzialem", {
    res <- jCzest:::zalezne(data = pary, vars = c("przed", "po"), counts = "n")
    e <- res$effsize$asDF
    expect_equal(nrow(e), 1)
    expect_true(is.finite(e$value[1]))
    expect_lt(e$lower[1], e$value[1])
    expect_gt(e$upper[1], e$value[1])
})

test_that("dokladny test dwumianowy na parach niezgodnych", {
    res <- jCzest:::zalezne(data = pary, vars = c("przed", "po"), counts = "n", exact = TRUE)
    tests <- res$tests$asDF
    p <- tests$p[tests$test == "Dokładny test dwumianowy"]
    expect_equal(p, binom.test(10, 35, 0.5)$p.value)
})

test_that("ostrzezenie gdy par niezgodnych za malo", {
    duzo <- jCzest:::zalezne(data = pary, vars = c("przed", "po"), counts = "n")
    expect_false(grepl("zawodne", txt(duzo)))

    malo <- data.frame(a = factor(c("tak", "tak", "nie", "nie")),
                       b = factor(c("tak", "nie", "tak", "nie")),
                       n = c(50, 5, 3, 50))
    out <- txt(jCzest:::zalezne(data = malo, vars = c("a", "b"), counts = "n"))
    expect_true(grepl("zawodne", out))
    expect_true(grepl("dwumianow", out))
})

test_that("trzy pomiary: Q Cochrana z post-hoc", {
    res <- jCzest:::zalezne(data = trzy, vars = c("p1", "p2", "p3"), posthoc = TRUE)
    tests <- res$tests$asDF
    expect_match(tests$test[1], "Cochran")
    expect_equal(tests$df[1], 2)
    expect_true(tests$stat[1] >= 0)

    ph <- res$posthoc$asDF
    expect_equal(nrow(ph), 3)                 # trzy pary pomiarow
    expect_true(all(ph$p >= 0 & ph$p <= 1, na.rm = TRUE))

    marg <- res$marg$asDF                     # udzialy w kazdym pomiarze
    expect_equal(nrow(marg), 3)
})

test_that("Q dla dwoch pomiarow pokrywa sie z McNemarem (kontrola spojnosci)", {
    dwa <- trzy[, c("p1", "p2")]
    mc <- jCzest:::zalezne(data = dwa, vars = c("p1", "p2"))$tests$asDF$stat[1]
    m <- cbind(as.integer(dwa$p1 == levels(dwa$p1)[1]),
               as.integer(dwa$p2 == levels(dwa$p2)[1]))
    expect_equal(mc, cochranQ(m)$stat)
})

test_that("przypadki brzegowe nie wywalaja analizy", {
    # pomiary identyczne — Q/McNemar nieokreslone, ma byc komunikat, nie blad
    ident <- data.frame(a = factor(c("tak", "nie", "tak")), b = factor(c("tak", "nie", "tak")))
    expect_error(jCzest:::zalezne(data = ident, vars = c("a", "b")), NA)
    expect_true(grepl("identyczne|niezgodnych", txt(jCzest:::zalezne(data = ident, vars = c("a", "b")))))

    # pomiar o trzech kategoriach przy Q
    trzyKat <- data.frame(a = factor(c("x", "y", "z")), b = factor(c("x", "y", "x")),
                          c = factor(c("y", "x", "x")))
    expect_error(jCzest:::zalezne(data = trzyKat, vars = c("a", "b", "c")), NA)

    # jeden pomiar
    expect_error(jCzest:::zalezne(data = trzy, vars = "p1"), NA)

    # braki danych
    braki <- trzy; braki$p1[1:5] <- NA
    expect_error(jCzest:::zalezne(data = braki, vars = c("p1", "p2", "p3")), NA)
})
