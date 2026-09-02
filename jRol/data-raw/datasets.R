# Provenance of the jRol datasets (teaching examples for the designs).

# Yates (1935) split-plot oats trial: blocks B, variety V on whole plots,
# nitrogen N on sub plots, yield Y (1/4 lb per sub plot). Exported as-is.
data("oats", package = "MASS")
write.csv(oats, "data/oats.csv", row.names = FALSE, na = "")

# Synthetic 4x4 Latin square (jRol teaching example): winter wheat yield
# (t/ha) under four fertilisation treatments with a row gradient (moisture)
# and a column gradient (slope). Seed fixed so the file is reproducible.
set.seed(2026)
k <- 4
d <- expand.grid(wiersz = 1:k, kolumna = 1:k)
lev <- c("kontrola", "NPK", "obornik", "NPK+obornik")
d$nawoz <- lev[(d$wiersz + d$kolumna - 2) %% k + 1]
perm <- sample(k); d$nawoz <- lev[perm[match(d$nawoz, lev)]]
eff <- c(kontrola = 0, NPK = 0.9, obornik = 0.5, "NPK+obornik" = 1.3)
d$plon <- round(5.2 + 0.25 * d$wiersz - 0.3 * d$kolumna + eff[d$nawoz] + rnorm(16, 0, 0.3), 2)
d <- d[order(d$wiersz, d$kolumna), ]
write.csv(d, "data/pszenica_latin.csv", row.names = FALSE)
