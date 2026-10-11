# Music & Mental Health Survey Results (Rasgaitis 2022, Kaggle, CC0 1.0).
# Source file: data-raw/mxmh_survey_results.csv (version 1, 2022-11-21).
# Run from the jDane directory. Writes data/mxmh.csv and the old->new column
# name table data-raw/mxmh_nazwy.csv.
d <- read.csv("data-raw/mxmh_survey_results.csv", check.names = FALSE,
              na.strings = "", stringsAsFactors = FALSE, encoding = "UTF-8")
stopifnot(nrow(d) == 736, ncol(d) == 33)

# columns without analytical content
d$Timestamp <- NULL
d$Permissions <- NULL

new <- c(
  "Age" = "age", "Primary streaming service" = "streaming_service",
  "Hours per day" = "hours_per_day", "While working" = "while_working",
  "Instrumentalist" = "instrumentalist", "Composer" = "composer",
  "Fav genre" = "fav_genre", "Exploratory" = "exploratory",
  "Foreign languages" = "foreign_languages", "BPM" = "bpm",
  "Anxiety" = "anxiety", "Depression" = "depression",
  "Insomnia" = "insomnia", "OCD" = "ocd", "Music effects" = "music_effects")
fr <- grep("^Frequency \\[", names(d), value = TRUE)
gen <- sub("^Frequency \\[(.*)\\]$", "\\1", fr)
new[fr] <- paste0("freq_", gsub("[^a-z0-9]+", "_", tolower(gsub("&", "n", gen))))
stopifnot(all(names(d) %in% names(new)), !anyDuplicated(new))
map <- data.frame(stara_nazwa = names(d), nowa_nazwa = unname(new[names(d)]))
names(d) <- map$nowa_nazwa
write.csv(map, "data-raw/mxmh_nazwy.csv", row.names = FALSE, fileEncoding = "UTF-8")

# physically impossible BPM (outside 20-300) -> missing
bad <- !is.na(d$bpm) & (d$bpm < 20 | d$bpm > 300)
cat("BPM poza zakresem 20-300:", sum(bad), "\n")
d$bpm[bad] <- NA

write.csv(d, "data/mxmh.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")
