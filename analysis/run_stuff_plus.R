library(readr)

source("R/helpers.R")
source("R/clean_pitches.R")
source("R/stuff_weights.R")
source("R/score_stuff.R")
source("R/reports.R")

raw_pitches <- read_csv("data/sample_pitches.csv", show_col_types = FALSE)

cleaned_pitches <- standardize_pitch_data(
  df = raw_pitches,
  prefer_auto_pitch_type = TRUE
)

scored_pitches <- cleaned_pitches %>%
  add_stuff_traits() %>%
  score_pitch_rows()

pitch_grades <- scored_pitches %>%
  summarize_pitch_grades(min_pitcher_pitch_n = 5) %>%
  add_trait_percentiles() %>%
  add_development_notes()

arsenal_grades <- summarize_arsenal_grades(
  pitch_grades = pitch_grades,
  min_pitcher_pitch_n = 5
)

pitch_leaderboard <- pitch_grades %>%
  filter(Pitches >= 5) %>%
  arrange(desc(PitchStuffPlus))

validation_summary <- summarize_validation(pitch_grades)

dir.create("outputs", showWarnings = FALSE)

write_csv(pitch_grades, "outputs/pitch_grades.csv")
write_csv(arsenal_grades, "outputs/arsenal_grades.csv")
write_csv(pitch_leaderboard, "outputs/pitch_leaderboard.csv")
write_csv(validation_summary, "outputs/validation_summary.csv")

print(arsenal_grades)
print(validation_summary)
