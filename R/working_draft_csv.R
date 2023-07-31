library(nflverse)
library(tidyverse)

szn <- 2016:2022
elo <- get_538_elo(szn)
# Load team information
team_info <- load_teams() %>%
  select(team = team_abbr,
         team_conf,
         team_color,
         team_division,
         team_wordmark,
         team_nick)

pbp <- load_pbp(seasons = szn)

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

########## Calculate player stats based on play-by-play data----------------------
pbp_rp <- pbp %>%
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(epa)) %>%
  filter(penalty == 0) %>%
  filter(qb_spike == 0 | qb_kneel == 0) %>%
  mutate(player_id = ifelse(
    !is.na(passer_player_id),
    passer_player_id,
    ifelse(
      !is.na(rusher_player_id),
      rusher_player_id,
      ifelse(!is.na(fumbled_1_player_id),
             fumbled_1_player_id, NA)))) %>%
  mutate(player_name = ifelse(
    !is.na(passer_player_name),
    passer_player_name,
    ifelse(
      !is.na(rusher_player_name),
      rusher_player_name,
      ifelse(!is.na(fumbled_1_player_name),
             fumbled_1_player_name, NA)))) %>%
  filter(!is.na(player_name) & !is.na(player_id))

df <- pbp_rp %>%
  select(player_id, player_name, posteam, epa) %>%
  filter(player_name %in% qbs$short_name) %>%
  group_by(player_id, player_name, posteam) %>%
  mutate(play_n = row_number(),
         cum_epa = cumsum(epa)) %>%
  ungroup() %>%
  rename(team = posteam) %>%
  arrange(player_name, play_n) %>%
  left_join(team_info, by = "team") %>%
  mutate(
    player_name = gsub(".", ". ", player_name, fixed = TRUE),
    player_name2 = player_name
  ) # To plot individual lines in all facets.

top <- df %>%
  group_by(player_id, player_name, team) %>%
  summarise(plays = n(),
            cum_epa = last(cum_epa)) %>%
  arrange(-cum_epa)
########## Calculate player stats based on play-by-play data----------------------

# Load next-gen stats for QBs
ngs <-nflreadr::load_nextgen_stats(stat_type = "passing", seasons = szn) |>
  select(season, gsis_id = player_gsis_id, display_name = player_display_name, short_name = player_short_name, season, week, team = team_abbr, avg_time_to_throw:completion_percentage_above_expectation) |>
  left_join(qb, by = c("season" = "season", "gsis_id" = "gsis_id",  "team" = "team"))


ngs_qbs <- ngs %>%
  filter(week == 0) %>%
  group_by(season, team) %>%
  arrange(desc(attempts)) %>%
  slice_head() %>%
  select(season, display_name, team, player_id = gsis_id) %>%
  ungroup()

ngs_szn <- ngs %>%
  filter(gsis_id %in% ngs_qbs$player_id) %>%
  filter(week == 0) %>%
  select(
    season,
    week,
    player_display_name = display_name,
    team,
    attempts:completion_percentage,
    rating = passer_rating,
    cpoe = completion_percentage_above_expectation,
    avg_tt = avg_time_to_throw)

ngs_wk <- ngs %>%
  filter(gsis_id %in% ngs_qbs$player_id) %>%
  filter(week != 0) %>%
  select(
    season,
    week,
    player_display_name = display_name,
    team,
    attempts:completion_percentage,
    rating = passer_rating,
    cpoe = completion_percentage_above_expectation,
    avg_tt = avg_time_to_throw)

ngs_szn_data <- ngs_qbs %>%
  left_join(ngs_szn, by = c("season" = "season", "display_name" = "player_display_name", "team" = "team")) |>
  select(-week)

ngs_wkly_data <- ngs_qbs %>%
  left_join(ngs_wk, by = c("season" = "season", "display_name" = "player_display_name", "team" = "team"))


# Combine all data and return the final dataframe
qb_wk_stats <- pbp_stat |>
  left_join(ngs_wkly_data, by =c("season" = "season", "week" = "week", "player_display_name" = "display_name", "player_id" = "player_id", "team" = "team"))


year <- 2016
weekly_qbr_data1 <- get_weekly_qbr(year)
weekly_qbr_data2 <- get_weekly_qbr(year)
weekly_qbr_data3 <- get_weekly_qbr(year)
weekly_qbr_data4 <- get_weekly_qbr(year)
weekly_qbr_data5 <- get_weekly_qbr(year)
weekly_qbr_data6 <- get_weekly_qbr(year)
weekly_qbr_data7 <- get_weekly_qbr(year)
qbr_wkly <- rbind(weekly_qbr_data1,weekly_qbr_data2,weekly_qbr_data3,weekly_qbr_data4,weekly_qbr_data5,weekly_qbr_data6,weekly_qbr_data7)
rm(weekly_qbr_data1,weekly_qbr_data2,weekly_qbr_data3,weekly_qbr_data4,weekly_qbr_data5,weekly_qbr_data6,weekly_qbr_data7)

qbr_wkly <- qbr_wkly |>
  select(season,season_type,game_id, week = game_week, team = team_abb, player_display_name = name_display, team_name = team, opp_name, qbr_total:epa_total)


quarterback_wkly <- qbr_wkly |>
  left_join(qb_wk_stats, by = c("player_display_name", "season" = "season", "week" = "week", "team" = "team")) |>
  select(season, season_type, week, player_name = player_display_name, team_abbr = team, team = team_name, opp = opp_name, qb_plays, qbr_total, pts_added, epa_total, attempts:avg_tt, sacks:passing_epa, dakota:rushing_epa)
