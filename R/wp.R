library(nflverse)
library(tidyverse)

szn <- 1999:2022
pbp <- load_pbp(seasons = szn)
pbp_stat <- calculate_player_stats(pbp, weekly = TRUE)

write_csv(pbp_stat, "csv_statistics/player_stats_1999_2022.csv")
# Load team information
team_info <- load_teams()

# Load QB rosters and player information
qb <- load_rosters(szn) %>%
  filter(position == "QB") %>%
  select(season, gsis_id, team)

qbs <- load_players() %>%
  filter(position == "QB") |>
  select(gsis_id, display_name, short_name, team_abbr, headshot)

# Load play-by-play data
pbp <- load_pbp(seasons = szn)

pbp_stat <- calculate_player_stats(pbp, weekly = TRUE) |>
  rename(team = recent_team) |>
  filter(position == "QB") |>
  select(player_id:week,sacks:rushing_epa)

# Load next-gen stats for QBs
ngs <-nflreadr::load_pft_stats(stat_type = "passing", seasons = 2022)
write.csv(ngs, 'csv_statistics/ngs_passing_16_22.csv')

ngs_qbs <- ngs %>%
  group_by(season) %>%
  arrange(desc(attempts)) %>%
  slice_head(n = 35) %>%
  select(season, week, display_name, player_id = gsis_id) %>%
  ungroup()


ngs_wk <- ngs %>%
  filter(gsis_id %in% ngs_qbs$player_id) %>%
  filter(week != 0) %>%
  select(
    season,week,player_id=gsis_id,player_display_name = display_name,
    attempts:completion_percentage,rating = passer_rating,
    cpoe = completion_percentage_above_expectation,avg_tt = avg_time_to_throw) |>
  mutate(week = if_else((season %in% 2021:2022 & week == 23), 22, week)) |>
  mutate(week = if_else((season %in% 2016:2020 & week == 22), 21, week))

write_csv(qbr, 'csv_statistics/qb_ngs_weekly.csv')

# Combine all data and return the final dataframe
qb_wk_stats <- ngs_wk |>
  left_join(pbp_stat, by =c("season" = "season", "week" = "week", "player_id" = "player_id", "player_display_name" = "player_display_name")) |>
  rename(display_name = player_display_name)

qbr <- get_weekly_qbr_all_years(years = szn) |>
  select(season, display_name = name_display, team = team_abb, week = game_week, team_name = team, opponent = opp_name, qb_plays, epa_total, pts_added,qbr_total)

write_csv(qbr, 'csv_statistics/qb_qbr_weekly.csv')

quarterback_statistics <- qb_wk_stats |>
  left_join(qbr, by = c("season", "week", "display_name")) |>
  select(name = display_name, season, week, team = team_name, opponent, qb_plays, pts_added, qbr = qbr_total, epa_total, attempts, completions, pass_yards, pass_td = pass_touchdowns, pass_int = interceptions, comp_per = completion_percentage, cpoe, rating, time_pocket = avg_tt, sacks, sack_fumbles_lost, carries, rush_yds = rushing_yards, rush_td = rushing_tds, rushing_fumbles_lost, rushing_first_downs, rushing_epa) |>
  mutate(ypa = pass_yards / attempts,
         ypa = round(ypa, 2),
         comp_per = round(comp_per, 1),  # round column1 to 2 decimal places
         cpoe = round(cpoe, 1),
         rating = round(rating, 2),
         time_pocket = round(time_pocket, 4),
         rushing_epa = round(rushing_epa, 3)) |>
  replace_na(list(rush_epa = 0)) |>
  mutate(season_type = case_when(
    (season %in% 2021:2022 & week %in% 19:23) ~ "POST",
    (season %in% 2016:2020 & week %in% 18:22) ~ "POST",
    TRUE ~ "REG"
  )) %>%
  mutate(playoff_type = case_when(
    (season %in% 2021:2022 & week == 22) ~ "Super Bowl",
    (season %in% 2021:2022 & week == 21) ~ "Championship",
    (season %in% 2021:2022 & week == 20) ~ "Divisional",
    (season %in% 2021:2022 & week == 19) ~ "Wild Card",
    (season %in% 2016:2020 & week == 21) ~ "Super Bowl",
    (season %in% 2016:2020 & week == 20) ~ "Championship",
    (season %in% 2016:2020 & week == 19) ~ "Divisional",
    (season %in% 2016:2020 & week == 18) ~ "Wild Card",
    TRUE ~ "0"
  )) %>%
  relocate(season_type, .before = 4) |>
  relocate(playoff_type, .before = 5) |>
  relocate(ypa, .before = 20) |>
  group_by(name, season, week) |>
  mutate(fumbles = sum(sack_fumbles_lost, rushing_fumbles_lost)) |>
  ungroup() |>
  select(-sack_fumbles_lost, -rushing_fumbles_lost) |>
  arrange(-pts_added)

playoff_leaders <- quarterback_statistics |>
  filter(season_type == "POST") |>
  filter(!is.na(qb_plays)) |>
  group_by(name) |>
  summarize(
    qb_plays = sum(qb_plays),
    pts_added = sum(pts_added),
    epa_total = sum(epa_total),
    attempts = sum(attempts),
    completions = sum(completions),
    pass_yards = sum(pass_yards),
    pass_td = sum(pass_td),
    pass_int = sum(pass_int),
    time_pocket = mean(time_pocket, na.rm = TRUE),
    sacks = sum(sacks),
    fumbles = sum(fumbles),
    carries = sum(carries),
    rush_yds = sum(rush_yds),
    rush_td = sum(rush_td)) |>
  mutate(time_pocket = round(time_pocket, 3),
         ypa = pass_yards / attempts,
         ypa = round(ypa, 2),
         comp_perc = round(completions / attempts, 4),
         pts_per_100play = (pts_added / qb_plays) * 100,
         pts_per_100play = round(pts_per_100play, 3))|>
  relocate(pts_per_100play, .before = 4) |>
  relocate(comp_perc, .before = 11) |>
  relocate(ypa, .before = 12) |>
  arrange(-pts_per_100play)
write_csv(playoff_leaders, 'csv_statistics/qb_stats_postseason.csv')
write_csv(quarterback_statistics, 'csv_statistics/qb_stats_allseasons.csv')

pbp <- load_pbp(1999:2000)
write_csv(pbp, 'csv_statistics/pbp_1999_2022.csv')

part <- load_participation(2016:2022)
write_csv(pbp, 'csv_statistics/participation_2016_2022.csv')
library(readr)
raw_metrics <- espnscrapeR::get_espn_wr_metrics()
write_csv(raw_metrics, 'csv_statistics/wrmetrics_2017_2022.csv')


years <- 2016:2022
get_win_rate_all_years <- function(years) {

  espn_qbr_all_years <- data.frame()  # Initialize an empty dataframe to store the combined data for all years
  scrape_espn_win_rate(season = 2022)
  for (year in years) {
    espn_qbr_year <- get_weekly_qbr(year)  # Get the data for this year
    espn_qbr_all_years <- rbind(espn_qbr_all_years, espn_qbr_year)  # Combine the data
  }

  return(espn_qbr_all_years)
}
winrate <- get_win_rate_all_years(years)
write_csv(winrate, 'csv_statistics/winrate_2006_2022.csv')
