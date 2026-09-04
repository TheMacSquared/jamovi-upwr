skip_if_not_installed("jCzest")

txt <- function(r) paste(capture.output(print(r)), collapse = "\n")

# cztery kategorie, liczebnosci z zapasem
set.seed(2)
cztery <- data.frame(kat = factor(sample(c("A", "B", "C", "D"), 200, TRUE)))
# klasyczne dane Mendla jako dane zagregowane
mendel <- data.frame(fenotyp = factor(c("gladkie_zolte", "gladkie_zielone",
                                        "pomarszczone_zolte", "pomarszczone_zielone"),
                                      levels = c("gladkie_zolte", "gladkie_zielone",
                                                 "pomarszczone_zolte", "pomarszczone_zielone")),
                     n = c(315, 108, 101, 32))

test_that("rdzen: chi2 zgodnosci i w Cohena bez klikania", {
    res <- jCzest:::zgodnosc(data = cztery, var = "kat")
    tests <- res$tests$asDF
    ref <- chisq.test(table(cztery$kat))
    expect_equal(tests$stat[1], unname(ref$statistic))
    expect_equal(tests$p[1], unname(ref$p.value))
    expect_equal(tests$df[1], 3)

    eff <- res$effsize$asDF          # w Cohena widoczne domyslnie
    expect_equal(nrow(eff), 1)
    expect_equal(eff$value[1], sqrt(unname(ref$statistic) / 200))

    props <- res$props$asDF
    expect_equal(nrow(props), 4)
    expect_equal(sum(props$obs), 200)
})

test_that("wlasne proporcje: 9:3:3:1 na danych Mendla", {
    res <- jCzest:::zgodnosc(data = mendel, var = "fenotyp", counts = "n",
                             ratio = c(9, 3, 3, 1), exp = TRUE)
    tests <- res$tests$asDF
    ref <- chisq.test(c(315, 108, 101, 32), p = c(9, 3, 3, 1) / 16)
    expect_equal(tests$stat[1], unname(ref$statistic))
    expect_equal(tests$p[1], unname(ref$p.value))
    expect_gt(tests$p[1], 0.9)       # klasyczny wynik: brak podstaw do odrzucenia

    props <- res$props$asDF
    expect_equal(sum(props$expCount), 556)
    expect_equal(props$expCount[1], 556 * 9 / 16)
})

test_that("dwie kategorie: test dwumianowy zamiast chi2", {
    dwa <- data.frame(w = factor(c("tak", "nie")), n = c(30, 20))
    res <- jCzest:::zgodnosc(data = dwa, var = "w", counts = "n")
    tests <- res$tests$asDF
    expect_match(tests$test[1], "dwumianowy")
    # poziomy alfabetycznie: "nie" pierwsze, wiec testujemy 20 z 50
    expect_equal(tests$p[1], binom.test(20, 50, 0.5)$p.value)

    jedn <- jCzest:::zgodnosc(data = dwa, var = "w", counts = "n", hypothesis = "greater")
    expect_equal(jedn$tests$asDF$p[1], binom.test(20, 50, 0.5, alternative = "greater")$p.value)
})

test_that("ostrzezenie o E<5 tylko przy chi2 i tylko gdy trzeba", {
    expect_false(grepl("zawodne", txt(jCzest:::zgodnosc(data = cztery, var = "kat"))))

    # UWAGA: przy rownych proporcjach oczekiwanych E = n/k dla KAZDEJ kategorii,
    # wiec o zlamaniu zalozenia decyduje male n wzgledem liczby kategorii,
    # a nie skosnosc obserwacji (2,3,2,40 daje E = 11,75 i jest w porzadku)
    duzeSkosne <- data.frame(k = factor(c("a", "b", "c", "d")), n = c(2, 3, 2, 40))
    expect_false(grepl("zawodne", txt(jCzest:::zgodnosc(data = duzeSkosne, var = "k", counts = "n"))))

    maleN <- data.frame(k = factor(c("a", "b", "c", "d")), n = c(2, 3, 2, 4))
    expect_true(grepl("zawodne", txt(jCzest:::zgodnosc(data = maleN, var = "k", counts = "n"))))

    # przy 2 kategoriach test jest dokladny, wiec ostrzezenie nie ma sensu
    male <- data.frame(k = factor(c("a", "b")), n = c(2, 3))
    expect_false(grepl("zawodne", txt(jCzest:::zgodnosc(data = male, var = "k", counts = "n"))))
})

test_that("reszty wskazuja kategorie odstajaca", {
    skos <- data.frame(k = factor(c("a", "b", "c")), n = c(60, 20, 20))
    res <- jCzest:::zgodnosc(data = skos, var = "k", counts = "n", resid = TRUE)
    r <- res$props$asDF
    # nadwyzka w jednej kategorii WYMUSZA niedobor w pozostalych (suma O-E = 0),
    # wiec nie testujemy "tylko a odstaje", tylko kierunek i wielkosc
    expect_gt(r$resid[1], 1.96)               # "a" ma nadwyzke
    expect_lt(r$resid[2], 0)                  # "b" i "c" maja niedobor
    expect_lt(r$resid[3], 0)
    expect_true(which.max(abs(r$resid)) == 1) # to "a" odstaje najmocniej
})

test_that("dokladny test wielomianowy zgadza sie z dwumianowym przy k=2", {
    dwa <- data.frame(w = factor(c("tak", "nie")), n = c(7, 3))
    res <- jCzest:::zgodnosc(data = dwa, var = "w", counts = "n", exact = TRUE)
    tests <- res$tests$asDF
    mx <- tests$p[tests$test == "Dokładny test wielomianowy"]
    expect_equal(mx, binom.test(3, 10, 0.5)$p.value, tolerance = 1e-6)
})

test_that("bledne proporcje i przypadki brzegowe nie wywalaja analizy", {
    # zla liczba wag
    res <- jCzest:::zgodnosc(data = cztery, var = "kat", ratio = c(1, 2))
    expect_equal(nrow(res$tests$asDF), 0)
    expect_match(txt(res), "Proporcje oczekiwane")

    # ujemna waga
    expect_error(jCzest:::zgodnosc(data = cztery, var = "kat", ratio = c(1, 1, 1, -1)), NA)

    # jedna kategoria
    jedna <- data.frame(k = factor(rep("x", 10)))
    expect_error(jCzest:::zgodnosc(data = jedna, var = "k"), NA)

    # braki danych
    braki <- data.frame(k = factor(c("a", "b", NA, "a")))
    expect_error(jCzest:::zgodnosc(data = braki, var = "k"), NA)
})
