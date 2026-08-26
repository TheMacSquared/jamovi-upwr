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
