# Provenance of the jDane datasets.
#
# Big5.csv, ToothGrowth.csv, bugs.csv, iris.csv are copied verbatim from
# the jmv module (jmv/data/), so the library entries stay identical to the
# examples jamovi ships with.
#
# The remaining four are exported from their R packages. CASchools carries
# float32 noise from the original Stata file, hence the rounding.

data("CASchools", package = "AER")
num <- sapply(CASchools, is.numeric)
CASchools[num] <- lapply(CASchools[num], round, 4)
write.csv(CASchools, "data/CASchools.csv", row.names = FALSE, na = "")

data("Affairs", package = "AER")
write.csv(Affairs, "data/affairs.csv", row.names = FALSE, na = "")

data("RiceFarms", package = "plm")
write.csv(RiceFarms, "data/RiceFarms.csv", row.names = FALSE, na = "")

write.csv(palmerpenguins::penguins, "data/penguins.csv",
          row.names = FALSE, na = "")

write.csv(PlantGrowth, "data/PlantGrowth.csv", row.names = FALSE, na = "")

write.csv(npk, "data/npk.csv", row.names = FALSE, na = "")

# birthwt ships with 0/1 and 1/2/3 codes; recoded to labels the same way
# the MASS documentation does, so jamovi picks the columns up as nominal
data("birthwt", package = "MASS")
birthwt$low   <- factor(birthwt$low,   levels = 0:1, labels = c("no", "yes"))
birthwt$race  <- factor(birthwt$race,  levels = 1:3, labels = c("white", "black", "other"))
birthwt$smoke <- factor(birthwt$smoke, levels = 0:1, labels = c("no", "yes"))
birthwt$ht    <- factor(birthwt$ht,    levels = 0:1, labels = c("no", "yes"))
birthwt$ui    <- factor(birthwt$ui,    levels = 0:1, labels = c("no", "yes"))
write.csv(birthwt, "data/birthwt.csv", row.names = FALSE, na = "")

# passenger names live in the rownames; keep them as a regular column
data("TitanicSurvival", package = "carData")
titanic <- data.frame(name = rownames(TitanicSurvival), TitanicSurvival)
write.csv(titanic, "data/titanic.csv", row.names = FALSE, na = "")

# Yates (1935) split-plot oats trial: blocks B, variety V on whole plots,
# nitrogen N on sub plots, yield Y (1/4 lb per sub plot). Exported as-is.
data("oats", package = "MASS")
write.csv(oats, "data/oats.csv", row.names = FALSE, na = "")

# Synthetic 4x4 Latin square (jDosw teaching example): winter wheat yield
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
