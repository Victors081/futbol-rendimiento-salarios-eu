

library(worldfootballR)
library(dplyr)
#eng_match_results <- load_match_results(country = "ENG", gender = c("M"), season_end_year = c(2023), tier = "1st")

#countries <- c("ESP", "ITA","FRA","GER","ENG")

countries <- c("ENG")

years <- c(2022)

filename <- "data/raw/data_points.csv"

if (!file.exists(filename)) {

  league_stats <- fb_season_team_stats(
    country = countries,
    gender = "M",
    season_end_year = years,
    tier = "1st",
    stat_type = "league_table"
  )


  write.csv(league_stats, file = filename, row.names=FALSE)

} else {

  league_stats <- read.csv(filename)
}


England_seasontable <-
  league_stats |>
  filter(Competition_Name == "Premier League", Season_End_Year == 2022)


eng_seasontable <- England_seasontable |>
  mutate(
    W_mp = (W/MP),
    WL_ratio = (W/L)
  ) |>
  select(Squad, Pts, Pts.MP, W_mp, WL_ratio, GD)


#England_seasontable <- read_csv(file = "input/EPL_2223.csv")

write.csv(eng_seasontable, file = "data/processed/results_league.csv", row.names=FALSE)

rm(list = ls())
