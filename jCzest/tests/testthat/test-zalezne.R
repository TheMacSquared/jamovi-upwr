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

test_that("OR przy dwoch pomiarach jest w GLOWNEJ tabeli, nie w osobnej", {
    res <- jCzest:::zalezne(data = pary, vars = c("przed", "po"), counts = "n")
    t <- res$tests$asDF
    expect_true(all(c("or", "lower", "upper") %in% names(t)))
    expect_true(is.finite(t$or[1]))
    expect_lt(t$lower[1], t$or[1])
    expect_gt(t$upper[1], t$or[1])
    # OR = b/c, czyli pary "nie -> tak" do par "tak -> nie" (10 / 25), zgodnie
    # z nota pod tabela; odwrotna kolejnosc dalaby 2.5
    expect_equal(t$or[1], 10 / 25)

    # wylaczona wielkosc efektu: komorka pozostaje pusta (nie liczba)
    bez <- jCzest:::zalezne(data = pary, vars = c("przed", "po"), counts = "n", effSize = FALSE)
    expect_false(isTRUE(is.finite(bez$tests$asDF$or[1])))
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

test_that("wykres udzialow dziala dla obu metod", {
    r3 <- jCzest:::zalezne(data = trzy, vars = c("p1", "p2", "p3"), plot = TRUE)
    st <- r3$plot$state
    expect_false(is.null(st))
    expect_equal(st$vars, c("p1", "p2", "p3"))
    expect_equal(length(st$prop), 3)
    expect_true(all(st$prop >= 0 & st$prop <= 1))
    expect_equal(st$level, levels(trzy$p1)[2])   # domyslnie druga kategoria („tak”)

    r2 <- jCzest:::zalezne(data = pary, vars = c("przed", "po"), counts = "n", plot = TRUE)
    expect_false(is.null(r2$plot$state))
    expect_equal(length(r2$plot$state$prop), 2)

    # bez opcji stan nie jest ustawiany
    expect_null(jCzest:::zalezne(data = trzy, vars = c("p1", "p2"))$plot$state)
})

test_that("wielkosc efektu przy k >= 3 jest podawana parami, nie pusta tabela", {
    res <- jCzest:::zalezne(data = trzy, vars = c("p1", "p2", "p3"), posthoc = TRUE)
    ph <- res$posthoc$asDF
    expect_true("or" %in% names(ph))
    expect_equal(nrow(ph), 3)
    # OR jest skonczony tam, gdzie sa pary niezgodne w obie strony
    expect_true(any(is.finite(ph$or)))
    expect_true(all(ph$lower[is.finite(ph$or)] < ph$or[is.finite(ph$or)]))

    # bez post-hoc: ma byc wskazowka, gdzie szukac wielkosci efektu
    bez <- jCzest:::zalezne(data = trzy, vars = c("p1", "p2", "p3"))
    expect_true(grepl("parami", txt(bez)))
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

test_that("kategoria „wystąpiło” steruje udzialami, wykresem i opisem metod", {
    # domyslnie druga kategoria: przy (nie, tak) udzialy dotycza „tak”
    d2 <- jCzest:::zalezne(data = pary, vars = c("przed", "po"), counts = "n",
                           plot = TRUE, metody = TRUE)
    expect_equal(d2$plot$state$level, "tak")
    expect_equal(d2$plot$state$prop, c((40 + 25) / 100, (40 + 10) / 100))
    expect_match(d2$metody$content, "„tak”")

    # jawny wybor odwraca udzialy (dopelnienie do 1), statystyka bez zmian
    e2 <- jCzest:::zalezne(data = pary, vars = c("przed", "po"), counts = "n",
                           plot = TRUE, event = "nie")
    expect_equal(e2$plot$state$level, "nie")
    expect_equal(e2$plot$state$prop, 1 - d2$plot$state$prop)
    expect_equal(e2$tests$asDF$stat[1], d2$tests$asDF$stat[1])

    # Q Cochrana: udzialy wybranej kategorii, Q niezalezne od wyboru
    q1 <- jCzest:::zalezne(data = trzy, vars = c("p1", "p2", "p3"), plot = TRUE, event = "tak")
    q2 <- jCzest:::zalezne(data = trzy, vars = c("p1", "p2", "p3"), plot = TRUE, event = "nie")
    expect_equal(q1$plot$state$prop, vapply(trzy, function(x) mean(x == "tak"), numeric(1)),
                 ignore_attr = TRUE)
    expect_equal(q2$plot$state$prop, 1 - q1$plot$state$prop)
    expect_equal(q1$tests$asDF$stat[1], q2$tests$asDF$stat[1])

    # kategoria spoza poziomow = jak pusta (druga)
    x2 <- jCzest:::zalezne(data = pary, vars = c("przed", "po"), counts = "n",
                           plot = TRUE, event = "brak")
    expect_equal(x2$plot$state$level, "tak")
})
