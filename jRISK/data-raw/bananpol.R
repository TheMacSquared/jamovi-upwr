# Generates data/bananpol.csv — the synthetic teaching dataset for the risk
# analysis course: ripening-room equipment at the Bananpol banana distributor. Deterministic (fixed seed); rerun after editing and commit
# the regenerated CSV together with this script.
set.seed(2026)
n <- 150
sekcja <- sample(c("A", "B", "C"), n, replace = TRUE)
urzadzenie <- sample(c("agregat", "wentylator", "nawilzacz"), n, replace = TRUE,
                     prob = c(0.3, 0.5, 0.2))
# lifetime: Weibull per device type (months), right-censored at 36 months;
# one uniform per draw, so the later columns keep their random stream.
# agregat: wear-out (beta 2.2), wentylator: nearly random failures and short
# life (beta 1.2), nawilzacz: in between. bananpol_system.csv takes its
# component reliabilities from the fits of these data (data-raw/bananpol-system.R)
weibullPar <- list(agregat    = c(shape = 2.2, scale = 30),
                   wentylator = c(shape = 1.2, scale = 18),
                   nawilzacz  = c(shape = 1.6, scale = 26))
shape <- vapply(urzadzenie, function(u) weibullPar[[u]][["shape"]], 0)
scale <- vapply(urzadzenie, function(u) weibullPar[[u]][["scale"]], 0)
czas <- round(rweibull(n, shape = shape, scale = scale), 1)
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
