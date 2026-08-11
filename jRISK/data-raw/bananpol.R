# Generates data/bananpol.csv — the synthetic teaching dataset for the risk
# analysis course: ripening-room equipment at the Bananpol banana distributor. Deterministic (fixed seed); rerun after editing and commit
# the regenerated CSV together with this script.
set.seed(2026)
n <- 150
sekcja <- sample(c("A", "B", "C"), n, replace = TRUE)
urzadzenie <- sample(c("agregat", "wentylator", "nawilzacz"), n, replace = TRUE,
                     prob = c(0.3, 0.5, 0.2))
# lifetime: Weibull(shape 1.5, scale 24 months), right-censored at 36 months
czas <- round(rweibull(n, shape = 1.5, scale = 24), 1)
awaria <- ifelse(czas > 36, 0L, 1L)
czas_pracy <- pmin(czas, 36)
# 12 inspections a year, 15% failure chance each -> binomial
kontrole_niezaliczone <- rbinom(n, size = 12, prob = 0.15)
# minor defects per year -> Poisson
usterki_rok <- rpois(n, lambda = 2)
# irrigation output as percent of nominal -> normal
wydajnosc <- round(rnorm(n, mean = 100, sd = 8), 1)

d <- data.frame(id = 1:n, sekcja, urzadzenie, czas_pracy, awaria,
                kontrole_niezaliczone, usterki_rok, wydajnosc)
write.csv(d, file.path("..", "data", "bananpol.csv"),
          row.names = FALSE, quote = TRUE)
