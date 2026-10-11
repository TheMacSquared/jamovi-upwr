# TIPI: heplots::TIPI (Jones i in. 2020) with item-level missing answers restored.
# In the raw data (networktree::tipi) a skipped item is coded 0; heplots averaged
# those zeros into the scale scores. Scores are rebuilt from the items and set
# to NA wherever X == 0 or X_r == 0. Run from the jDane directory.
library(heplots)
library(networktree)
data(tipi)
raw <- tipi[!is.na(tipi$gender) & tipi$gender %in% c("Male", "Female"), ]
stopifnot(nrow(raw) == nrow(TIPI))

scales <- c(Extraversion = "E", Neuroticism = "N", Conscientiousness = "C",
            Agreeableness = "A", Openness = "O")
out <- TIPI
replaced <- integer(0)
for (s in names(scales)) {
  k <- scales[[s]]
  x <- raw[[k]]; xr <- raw[[paste0(k, "_r")]]
  calc <- (x + (8 - xr)) / 2
  stopifnot(isTRUE(all.equal(calc, TIPI[[s]])))  # reproduces heplots exactly
  bad <- x == 0 | xr == 0
  out[[s]][bad] <- NA
  replaced[s] <- sum(bad)
}
stopifnot(all(unlist(out[names(scales)]) >= 1 & unlist(out[names(scales)]) <= 7, na.rm = TRUE))
anyrow <- Reduce(`|`, lapply(names(scales), function(s) is.na(out[[s]]) & !is.na(TIPI[[s]])))
cat("Zastapione wartosci:", paste(names(replaced), replaced), "\n")
cat("Osoby z co najmniej jednym zastapieniem:", sum(anyrow), "\n")
write.csv(out, "data/TIPI.csv", row.names = FALSE, na = "")
