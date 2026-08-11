# Generates data/bananpol_wypadki.csv — banana-peel slip accidents among
# plantation workers. Deterministic (fixed seed); rerun after editing and
# commit the regenerated CSV together with this script.
#
# Dependency structure (the didactic point):
#   sekcja -> number of peels encountered (Poisson; section C is next to
#             the canteen), each peel is a Bernoulli slip trial whose
#             per-peel probability depends on anti-slip shoes,
#   slip severity (given a slip) is milder with anti-slip shoes,
#   sick-leave days grow with severity.
set.seed(1906)
n <- 200
sekcja <- sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.35, 0.40, 0.25))
staz <- round(rgamma(n, shape = 2, rate = 0.5), 1)
buty_antyposlizgowe <- sample(c("tak", "nie"), n, replace = TRUE, prob = c(0.6, 0.4))

# peels encountered in a week; the canteen effect
lambdaSekcja <- c(A = 1, B = 2, C = 5)
skorki_tydzien <- rpois(n, lambdaSekcja[sekcja])

# each peel is one Bernoulli trial; shoes lower the per-peel slip risk
pPerPeel <- ifelse(buty_antyposlizgowe == "tak", 0.04, 0.12)
pSlip <- 1 - (1 - pPerPeel)^skorki_tydzien
slip <- rbinom(n, 1, pSlip)
poslizgniecie <- ifelse(slip == 1, "tak", "nie")

# severity given a slip; anti-slip shoes also soften the landing
ciezkosc <- rep("brak", n)
for (i in which(slip == 1)) {
  probs <- if (buty_antyposlizgowe[i] == "tak") c(0.70, 0.25, 0.05)
           else c(0.50, 0.30, 0.20)
  ciezkosc[i] <- sample(c("lekki", "sredni", "ciezki"), 1, prob = probs)
}

dni_zwolnienia <- integer(n)
for (i in which(slip == 1))
  dni_zwolnienia[i] <- switch(ciezkosc[i],
    lekki  = rpois(1, 1),
    sredni = 2 + rpois(1, 5),
    ciezki = 7 + rpois(1, 10))

d <- data.frame(
  pracownik = 1:n,
  sekcja,
  staz,
  buty_antyposlizgowe,
  skorki_tydzien,
  poslizgniecie,
  ciezkosc,
  dni_zwolnienia)
write.csv(d, file.path("..", "data", "bananpol_wypadki.csv"),
          row.names = FALSE, quote = TRUE)
