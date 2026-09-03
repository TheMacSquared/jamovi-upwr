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

# --- zbiory "lekkie", dodane w 0.6.0 -----------------------------------------

# uroda wykladowcy vs ocena zajec (Hamermesh & Parker 2005)
data("TeachingRatings", package = "AER")
write.csv(TeachingRatings, "data/TeachingRatings.csv", row.names = FALSE, na = "")

# postacie Star Wars; list-columns (films/vehicles/starships) sie nie
# eksportuja do csv, wiec wypadaja
sw <- dplyr::starwars
sw <- sw[, !sapply(sw, is.list)]
write.csv(sw, "data/starwars.csv", row.names = FALSE, na = "")

# kwartet Anscombe'a w formacie dlugim (jeden wiersz = jeden punkt)
ans <- do.call(rbind, lapply(1:4, function(i) data.frame(
    zbior = paste0("zbior_", i),
    x = anscombe[[paste0("x", i)]],
    y = anscombe[[paste0("y", i)]])))
write.csv(ans, "data/anscombe.csv", row.names = FALSE, na = "")

# opoznienia autobusow -- zbior syntetyczny przygotowany na zajecia;
# "-0" z zaokraglenia normalizowane do 0
bus <- read.csv("data-raw/spoznienia_autobusy.csv", stringsAsFactors = FALSE)
bus$opoznienie_min <- bus$opoznienie_min + 0
bus$nr_linii <- as.integer(bus$nr_linii)
write.csv(bus, "data/autobusy.csv", row.names = FALSE, na = "")

# fastfood: wartosci odzywcze 515 pozycji menu 8 sieci (openintro)
data("fastfood", package = "openintro")
write.csv(as.data.frame(fastfood), "data/fastfood.csv", row.names = FALSE, na = "")

# Datasaurus Dozen: 13 zbiorow o niemal identycznych statystykach
data("datasaurus_dozen", package = "datasauRus")
write.csv(as.data.frame(datasaurus_dozen), "data/datasaurus.csv",
          row.names = FALSE, na = "")

# filmy IMDb (stan na 2005). Pelny zbior ma 58 788 wierszy i 24 kolumny --
# za duzo na zajecia, wiec: filmy pelnometrazowe (>= 60 min) od 1970 r.
# z co najmniej 100 glosami i znanym budzetem, losowa probka 2000 sztuk;
# 7 kolumn wskaznikowych gatunku scalone w jedna zmienna nominalna
data("movies", package = "ggplot2movies")
# gatunek glowny: pierwszy pasujacy wg priorytetu ponizej, bo w zbiorze
# zrodlowym film moze miec kilka etykiet naraz (n_genres liczy ile)
gen <- c("Documentary", "Animation", "Action", "Romance", "Comedy",
         "Drama", "Short")
mv <- subset(movies, year >= 1970 & length >= 60 & votes >= 100 &
                     !is.na(budget) & budget > 0 & !is.na(rating))
ind <- as.matrix(mv[, gen])
mv$n_genres <- rowSums(ind)
mv$genre <- ifelse(mv$n_genres == 0, "Other",
                   gen[max.col(ind, ties.method = "first")])
mv$mpaa[mv$mpaa == ""] <- NA
mv <- mv[, c("title", "year", "length", "budget", "rating", "votes",
             "mpaa", "genre", "n_genres")]
set.seed(2026)
mv <- mv[sort(sample(nrow(mv), 2000)), ]
write.csv(mv, "data/movies.csv", row.names = FALSE, na = "")
