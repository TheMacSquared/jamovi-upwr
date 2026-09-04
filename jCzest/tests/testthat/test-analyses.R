skip_if_not_installed("jCzest")

# dane z zapasem liczebnosci — zalozenie chi2 spelnione
set.seed(1)
duze <- data.frame(
    plec = factor(sample(c("K", "M"), 300, TRUE)),
    wybor = factor(sample(c("A", "B", "C"), 300, TRUE, prob = c(.5, .3, .2)))
)
# dane zagregowane 2x2
zagregowane <- data.frame(w = c("A", "A", "B", "B"), k = c("tak", "nie", "tak", "nie"),
                          n = c(30, 10, 15, 25))

test_that("rdzen panelu dziala bez zadnego klikania", {
    res <- jCzest:::tabela(data = duze, rows = "plec", cols = "wybor")
    tests <- res$tests$asDF
    ref <- suppressWarnings(chisq.test(table(duze$plec, duze$wybor), correct = FALSE))
    expect_equal(tests$stat[1], unname(ref$statistic))
    expect_equal(tests$p[1], unname(ref$p.value))

    # V Cramera ma byc widoczne domyslnie (inaczej niz w jmv)
    eff <- res$effsize$asDF
    expect_equal(nrow(eff), 1)
    expect_equal(eff$value[1], cramersV(table(duze$plec, duze$wybor)))

    # procenty wierszami domyslnie
    freqs <- res$freqs$asDF
    expect_true(any(grepl("wierszem", freqs$kind)))
})

test_that("ostrzezenie o liczebnosciach oczekiwanych pojawia sie tylko gdy trzeba", {
    txt <- function(r) paste(capture.output(print(r)), collapse = "\n")

    expect_false(grepl("zawodne", txt(jCzest:::tabela(data = duze, rows = "plec", cols = "wybor"))))

    rzadkie <- data.frame(
        a = factor(c("x", "x", "x", "y", "y", "z", "z", "z", "z", "z")),
        b = factor(c("p", "q", "p", "q", "p", "q", "p", "q", "p", "q"))
    )
    out <- txt(jCzest:::tabela(data = rzadkie, rows = "a", cols = "b"))
    expect_true(grepl("zawodne", out))
    expect_true(grepl("Fisher", out))
})

test_that("dane zagregowane i miary 2x2 z nota o kierunku", {
    res <- jCzest:::tabela(data = zagregowane, rows = "w", cols = "k", counts = "n",
                           odds = TRUE, relRisk = TRUE, diffProp = TRUE)
    m <- res$measures$asDF
    expect_equal(nrow(m), 3)
    # poziomy sortowane alfabetycznie: kolumny to (nie, tak)
    expect_equal(m$value[1], (10 * 15) / (30 * 25))
    expect_true(grepl("odwraca OR", paste(capture.output(print(res)), collapse = "\n")))
})

test_that("opcje zaawansowane licza sie i nie przerywaja analizy", {
    res <- jCzest:::tabela(data = duze, rows = "plec", cols = "wybor",
                           exp = TRUE, fisher = TRUE, likeRat = TRUE, resid = TRUE,
                           pairwise = TRUE, gamma = TRUE, taub = TRUE, effSizeCI = TRUE)
    expect_true(nrow(res$tests$asDF) >= 3)          # chi2 + G2 + Fisher + N
    expect_equal(nrow(res$resid$asDF), 2)           # dwa poziomy plci
    expect_equal(nrow(res$ordinal$asDF), 2)
    eff <- res$effsize$asDF
    expect_true(is.finite(eff$lower[1]) && is.finite(eff$upper[1]))
})

test_that("poprawka ciaglosci i miary 2x2 sa odrzucane poza tabela 2x2", {
    res <- jCzest:::tabela(data = duze, rows = "plec", cols = "wybor",
                           chiSqCorr = TRUE, odds = TRUE)
    # 2x3 — poprawka nie ma zastosowania, wiec nie dokladamy wiersza
    expect_false(any(grepl("poprawk", res$tests$asDF$test)))
    expect_equal(nrow(res$measures$asDF), 0)
})

test_that("kolumny powstaja tez gdy .init dostaje ramke bez wierszy (warunek z GUI)", {
    # W jamovi .init bywa wolane z ramka, ktora ma kolumny i zadeklarowane poziomy,
    # ale ZERO wierszy. Wyprowadzanie poziomow z obserwowanych wartosci dawalo wtedy
    # tabele licznosci bez kolumn — wygladala na pusta, choc testy sie liczyly.
    pusta <- data.frame(plec = factor(character(0), levels = c("K", "M")),
                        wybor = factor(character(0), levels = c("A", "B", "C")))
    a <- jCzest:::tabelaClass$new(
        options = jCzest:::tabelaOptions$new(rows = "plec", cols = "wybor"),
        data = pusta)
    a$init()
    nm <- vapply(a$results$freqs$columns, function(x) x$name, character(1))
    expect_true(all(c("c_A", "c_B", "c_C") %in% nm))

    # init + run na tym samym obiekcie nie moze zdublowac kolumn
    b <- jCzest:::tabelaClass$new(
        options = jCzest:::tabelaOptions$new(rows = "plec", cols = "wybor"),
        data = duze)
    b$init(); b$run()
    nm2 <- vapply(b$results$freqs$columns, function(x) x$name, character(1))
    expect_false(any(duplicated(nm2)))
    expect_gt(nrow(b$results$freqs$asDF), 0)
})

test_that("przypadki brzegowe nie wywalaja analizy", {
    jeden <- data.frame(a = factor(rep("x", 10)), b = factor(rep("p", 10)))
    expect_error(jCzest:::tabela(data = jeden, rows = "a", cols = "b"), NA)

    braki <- data.frame(a = factor(c("x", "y", NA, "x")), b = factor(c("p", NA, "q", "q")))
    expect_error(jCzest:::tabela(data = braki, rows = "a", cols = "b"), NA)
})
