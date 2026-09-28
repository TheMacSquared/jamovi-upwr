# Synthetic deterministic teaching data; no observational source or RNG state.
# Run from the repository root: Rscript jWody/data-raw/generate.R
out <- "jWody/data"
dir.create(out, showWarnings = FALSE, recursive = TRUE)
date <- seq(as.Date("2010-01-01"), as.Date("2024-12-31"), by = "day")
t <- seq_along(date)
flow <- do.call(rbind, lapply(c("Rzeka_A", "Rzeka_B"), function(station) {
    q <- 12 + 6 * sin(2 * pi * (t + 45) / 365.25) + 1.4 * sin(t / 17) + .0003 * t
    if (station == "Rzeka_B") q <- .65 * q + cos(t / 31)
    q[seq(600, length(q), 911)] <- NA_real_
    q[1200:1214] <- NA_real_
    data.frame(data = as.character(date), stacja = station, przeplyw_m3s = round(q, 3))
}))
utils::write.csv(flow, file.path(out, "przeplyw.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
p <- ifelse(t %% 7 %in% c(0, 1, 3), pmax(0, 4 + 4 * sin(t / 11) + 2 * cos(t / 61)), 0)
p[2000:2005] <- NA_real_
utils::write.csv(data.frame(data = as.character(date), stacja = "Opad_A", opad_mm = round(p, 2)), file.path(out, "opad.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
dm <- seq(as.Date("1995-01-01"), as.Date("2024-12-01"), by = "month")
tm <- seq_along(dm)
z <- 3 + .35 * sin(2 * pi * tm / 12) + .002 * tm + .07 * cos(tm / 7)
z[150:152] <- NA_real_
utils::write.csv(data.frame(data = as.character(dm), stacja = "Studnia_A", glebokosc_m = round(z, 3)), file.path(out, "studnia.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
