# Generates data/bananpol_alarmy.csv — a raw shift log of the overheating
# alarm in the six Bananpol ripening chambers. One row = one shift in one
# chamber (1000 shifts per chamber). Deterministic (fixed seed); rerun after
# editing and commit the regenerated CSV together with this script.
#
# Structure (the didactic point):
#   - base rate of overheating depends on the section (A 0.5%, B 1%, C 2%;
#     section C is next to the loading dock), so the same sensor has a
#     different PPV in different sections,
#   - chambers K2, K4, K6 got the new sensor model: same sensitivity (0.95),
#     fewer false alarms (1% instead of 5%),
#   - given the true state, the alarm depends only on the sensor model.
set.seed(303)
komory <- data.frame(
  komora = paste0("K", 1:6),
  sekcja = rep(c("A", "B", "C"), each = 2),
  czujnik = rep(c("stary", "nowy"), 3))
baseRate <- c(A = 0.005, B = 0.01, C = 0.02)
sens <- c(stary = 0.95, nowy = 0.95)
falseAlarm <- c(stary = 0.05, nowy = 0.01)

nShift <- 1000
start <- as.Date("2025-01-01")
d <- do.call(rbind, lapply(seq_len(nrow(komory)), function(k) {
  i <- seq_len(nShift)
  data.frame(
    data = format(start + (i - 1) %/% 3),
    zmiana = c("I", "II", "III")[(i - 1) %% 3 + 1],
    sekcja = komory$sekcja[k],
    komora = komory$komora[k],
    czujnik = komory$czujnik[k])
}))
d <- d[order(d$data, match(d$zmiana, c("I", "II", "III")), d$komora), ]
over <- rbinom(nrow(d), 1, baseRate[d$sekcja])
alarm <- rbinom(nrow(d), 1, ifelse(over == 1, sens[d$czujnik], falseAlarm[d$czujnik]))
d$przegrzanie <- ifelse(over == 1, "tak", "nie")
d$alarm <- ifelse(alarm == 1, "tak", "nie")
write.csv(d, file.path("..", "data", "bananpol_alarmy.csv"),
          row.names = FALSE, quote = TRUE)
