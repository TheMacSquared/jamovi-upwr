# Generates data/bananpol_system.csv — components of one Bananpol ripening
# line. The reliabilities are not hand-set: they are R(6 months) (mission =
# time to the half-yearly service) from Weibull fits to data/bananpol.csv per
# device type, exactly as Ryzyko → Modele czasu życia (tryb danych, grupa =
# urzadzenie, t = 6) reports them. Only the controller STER has no field data
# and keeps the manufacturer's value 0.98. Run from data-raw/ after
# bananpol.R. The same file feeds two analyses:
#   - relsystem (data mode): relVar = niezawodnosc, groupVar = podsystem,
#     gates: parallel within a subsystem, series between subsystems;
#     STER is deliberately a single point of failure,
#   - fta: probVar = p_awarii, branchVar = podsystem, gates AND + OR —
#     the dual of the structure above, so P(top) = 1 - R_sys.
source(file.path("..", "R", "utils.R"))
mission <- 6
b <- read.csv(file.path("..", "data", "bananpol.csv"))
rType <- vapply(c(agregat = "agregat", wentylator = "wentylator", nawilzacz = "nawilzacz"),
  function(u) {
    s <- b[b$urzadzenie == u, ]
    fit <- riskLtFit(s$czas_pracy, s$awaria, "weibull")
    riskLtReliability(mission, "weibull", fit$par)
  }, 0)
print(round(rType, 4))

d <- data.frame(
  komponent = c("AGR1", "AGR2", "WEN1", "WEN2", "WEN3", "NAW1", "NAW2", "STER"),
  podsystem = c("chlodzenie", "chlodzenie",
                "wentylacja", "wentylacja", "wentylacja",
                "nawilzanie", "nawilzanie",
                "sterowanie"),
  typ = c("agregat", "agregat", "wentylator", "wentylator", "wentylator",
          "nawilzacz", "nawilzacz", "sterownik"),
  niezawodnosc = round(c(rep(rType[["agregat"]], 2), rep(rType[["wentylator"]], 3),
                         rep(rType[["nawilzacz"]], 2), 0.98), 3))
d$p_awarii <- round(1 - d$niezawodnosc, 3)
write.csv(d, file.path("..", "data", "bananpol_system.csv"),
          row.names = FALSE, quote = TRUE)
