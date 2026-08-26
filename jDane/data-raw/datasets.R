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
