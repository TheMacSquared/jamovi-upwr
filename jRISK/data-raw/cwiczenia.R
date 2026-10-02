# Generates the small exercise datasets of the "Analiza ryzyka" course
# (hand-crafted values, no RNG). Run from data-raw/. One ready-made example
# per kind of input; further variants students build by editing the values.
# (Raw Bananpol data come from bananpol*.R; aggregated 2x2 tables students
# build themselves from counts read off bananpol_alarmy.)
#   - uklad_hamowania: small structure for relsystem (data mode),
#   - dwie_bariery: fault tree for fta with a repeated event (needs repeated
#     labels treated as one event).
out <- function(d, name)
  write.csv(d, file.path("..", "data", paste(name, ".csv", sep = "")),
            row.names = FALSE, quote = TRUE, na = "")

# braking: common element C in series with two parallel lines A, B
out(data.frame(
  element = c("C", "A", "B"),
  podsystem = c("C", "AB", "AB"),
  niezawodnosc = c(0.95, 0.90, 0.90)), "uklad_hamowania")

# two independent barriers B1, B2 with common cause C:
# TOP = I AND (C OR (B1 AND B2)) written through its minimal cut sets
# {I, C} OR {I, B1, B2}; I is repeated, so it must be one event
out(data.frame(
  zdarzenie = c("I", "C", "I", "B1", "B2"),
  p = c(0.005, 0.01, 0.005, 0.05, 0.08),
  galaz = c("przekroj_IC", "przekroj_IC", "przekroj_IB", "przekroj_IB", "przekroj_IB")),
  "dwie_bariery")
