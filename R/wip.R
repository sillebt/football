team_info <- load_teams() |>
  rename(team = team_abbr)

szn <- 1999:2022

## Load QB rosters and player information
roster <- load_rosters()

# Load play-by-play data
pbp <- load_pbp(seasons = szn)

pbp_stat <- calculate_player_stats(pbp, weekly = TRUE) |>
  rename(team = recent_team) |>
  filter(position == "QB" | position == "WR" | position == "TE" | position == "RB") |>
  select(-position_group, -player_name,-headshot_url)

# Calculate player stats based on play-by-play data
pbp_rp <- pbp |>
  filter(rush == 1 | pass == 1) |>
  filter(!is.na(epa)) |>
  filter(penalty == 0) |>
  filter(qb_spike == 0 | qb_kneel == 0) |>
  mutate(player_id = ifelse(
    !is.na(passer_player_id), passer_player_id,
    ifelse(!is.na(rusher_player_id),rusher_player_id,
           ifelse(!is.na(fumbled_1_player_id),fumbled_1_player_id, NA)) )) |>
  mutate(player_name = ifelse(
    !is.na(passer_player_name),passer_player_name,
    ifelse(!is.na(rusher_player_name),rusher_player_name,
           ifelse(!is.na(fumbled_1_player_name),fumbled_1_player_name, NA)) )) |>
  filter(!is.na(player_name) & !is.na(player_id))



kc_stats <- pbp_stat |>
  filter(team == "KC")

kc_rbs <- kc_stats |>
  filter(position == "RB") |>
  arrange(season,week)

kc_rbs$gm_count <- ave(kc_rbs$carries, kc_rbs$player_display_name, FUN = seq_along)
kc_rbs$cry <- ave(kc_rbs$carries, kc_rbs$player_display_name, FUN = cumsum)

kc_rbs2 <- kc_rbs |>
  select(player_display_name, gm_count, csum_run,csum_td,csum_rec,csum_rec_td,cry)

