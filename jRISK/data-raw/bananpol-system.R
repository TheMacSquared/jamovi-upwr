# Generates data/bananpol_system.csv — components of one Bananpol ripening
# line with mission reliabilities (hand-crafted, no RNG). The same file
# feeds two analyses:
#   - relsystem (data mode): relVar = niezawodnosc, groupVar = podsystem,
#     gates: parallel within a subsystem, series between subsystems;
#     STER is deliberately a single point of failure,
#   - fta: probVar = p_awarii, branchVar = podsystem, gates AND + OR —
#     the dual of the structure above, so P(top) = 1 - R_sys.
d <- data.frame(
  komponent = c("AGR1", "AGR2", "WEN1", "WEN2", "WEN3", "NAW1", "NAW2", "STER"),
  podsystem = c("chlodzenie", "chlodzenie",
                "wentylacja", "wentylacja", "wentylacja",
                "nawilzanie", "nawilzanie",
                "sterowanie"),
  niezawodnosc = c(0.90, 0.92, 0.85, 0.88, 0.86, 0.93, 0.95, 0.98))
d$p_awarii <- round(1 - d$niezawodnosc, 4)
write.csv(d, file.path("..", "data", "bananpol_system.csv"),
          row.names = FALSE, quote = TRUE)
