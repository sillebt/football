#' QB Stats Various Metrics
#'
#' @param szn
#'
#' @import nflverse
#' @import tidyverse
#' @import dplyr
#'
#' @return dataframe
#' @export
#'
#' @examples
#' szn = 2022
#' stats <- get_combined_qb_stats(szn)
get_combined_qb_stats <- function(szn) {
<<<<<<< HEAD
=======
  setwd("~/Desktop/nfl_plots")
>>>>>>> 69e67c13cb630b540c3442b8e32ef642c64e5b7b

  # Load team information
  team_info <- load_teams() %>%
    select(team = team_abbr,
           team_conf,
           team_color,
           team_division,
           team_wordmark,
           team_nick)

  # Load QB rosters and player information
  qb <- load_rosters(szn) %>%
    filter(position == "QB") %>%
    select(gsis_id, team)

  qbs <- load_players() %>%
    filter(position == "QB")

  # Load play-by-play data
  pbp <- load_pbp(seasons = szn)

  pbp_stat <- calculate_player_stats(pbp, weekly = FALSE) |>
    select(player_id:rushing_epa) |>
    rename(team = recent_team)

  # Calculate player stats based on play-by-play data
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
               fumbled_1_player_id, NA)
      )
    )) %>%
    mutate(player_name = ifelse(
      !is.na(passer_player_name),
      passer_player_name,
      ifelse(
        !is.na(rusher_player_name),
        rusher_player_name,
        ifelse(!is.na(fumbled_1_player_name),
               fumbled_1_player_name, NA)
      )
    )) %>%
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

  # Load next-gen stats for QBs
  ngs <-
    nflreadr::load_nextgen_stats(stat_type = "passing", seasons = szn) %>%
    mutate(team_abbr = ifelse(team_abbr == "LAR", "LA", team_abbr)) %>%
    left_join(qb, by = c("player_gsis_id" = "gsis_id"))

  ngs_qbs <- ngs %>%
    filter(week == 0) %>%
    group_by(team) %>%
    arrange(desc(attempts)) %>%
    slice_head() %>%
    select(player_display_name, team, player_id = player_gsis_id) %>%
    ungroup()

  ngs_top <- ngs %>%
    filter(player_gsis_id %in% ngs_qbs$player_id) %>%
    filter(week == 0) %>%
    select(
      player_id = player_gsis_id,
      player_display_name,
      team,
      rating = passer_rating,
      cpoe = completion_percentage_above_expectation,
      avg_tt = avg_time_to_throw
    )

  ngs_top$nfl_tt <- mean(ngs$avg_time_to_throw)

  ngs_data <- ngs_qbs %>%
    left_join(ngs_top, by = c("player_display_name", "team", "player_id"))

  # Load advanced stats
  stats <-
    load_pfr_advstats(szn, stat_type = "pass", summary_level = "season") |>
    mutate(team = ifelse(team == "LVR", "LV", team)) |>
    mutate(team = ifelse(team == "LAR", "LA", team))

  # Combine all data and return the final dataframe
  qb_szn_stats <- ngs_data |>
    left_join(top, by = c("player_id")) |>
    select(!team.x) |>
    rename(team = team.y)  |>
    left_join(stats, by = c("player_display_name" = "player", "team")) |>
    left_join(pbp_stat, by = c("player_id", "player_display_name", "team")) |>
    inner_join(team_info, by = "team") |>
    separate(team_division,
             into = c("conference", "division"),
             sep = " ") |>
    select(
      -player_name.x,
      -player_name.y,
      -pfr_id,
      -pocket_time,
      -position,
      -position_group,
      -season
    ) |>
    arrange(-cum_epa) |>
    select(
      player_id,
      player = player_display_name,
      team,
      headshot = headshot_url,
      games,
      plays,
      cum_epa,
      passing_epa,
      rushing_epa,
      dakota,
      rating,
      cpoe,
      completions:passing_first_downs,
      throwaways,
      drops,
      on_tgt_throws,
      bad_throws,
      batted_balls,
      avg_tt,
      nfl_tt,
      times_blitzed,
      times_hurried,
      times_hit,
      times_pressured,
      carries:rushing_first_downs,
      team_nick,
      conference,
      division,
      team_wordmark
    )

  qb_szn_stats$season = szn

  return(qb_szn_stats)
}


#' Plot QB Division Table
#'
#' @param szn
#'
#' @import tidyverse
#' @import nflverse
#' @import gt
#' @import gtExtras
#' @import ggimage
#' @import RColorBrewer
#' @import ggtext
#' @import sysfonts
#' @import showtext
#' @import glue
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' szn <- 2022
#' create_qb_summary_table(szn)
create_qb_summary_table <- function(szn) {
  # Import font from sysfonts
  sysfonts::font_add_google("chivo", "chivo")
  showtext_auto()

  # Get QB stats data
  qb_stats <- get_combined_qb_stats(szn)

  current_szn <- szn

  nfl_plot_data <- qb_stats %>%
    filter(!is.na(headshot)) %>%
    arrange(-cum_epa, conference, division) %>%
    group_by(conference, division)

  qb_table <- nfl_plot_data |>
    select(
      headshot,
      player,
      team_wordmark,
      attempts,
      yards = passing_yards,
      tds = passing_tds,
      interceptions,
      cpoe,
      rating,
      conference:division,
      epa = cum_epa
    ) |>
    gt() |>
    tab_header(
      title = glue("QB Summary: {current_szn} Season"),
      subtitle = md("Starting QB by Division")
    ) |>
    cols_label(
      headshot = "",
      player = "QB1",
      team_wordmark = "",
      attempts = "Att",
      yards = "Yds",
      tds = "TD",
      interceptions = "INT",
      cpoe = "CPOE",
      rating = "Rating",
      epa = "EPA"
    ) |>
    gt_img_rows(headshot) |>
    gt_img_rows(team_wordmark) |>
    data_color(
      columns = vars(epa),
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(palette = "RColorBrewer::PRGn",
                                         direction = 1) |> as.character(),
        domain = NULL,
        na.color = "#0492c2"
      )
    ) |>
    fmt_number(columns = cpoe,
               decimals = 1) |>
    fmt_number(columns = rating,
               decimals = 1) |>
    fmt_number(columns = epa,
               decimals = 1) |>
    tab_options(
      table.background.color = "white",
      heading.title.font.size = 30,
      heading.title.font.weight = 'bold',
      heading.subtitle.font.size = 20,
      table.font.names = "Chivo",
      table.font.color = 'black',
      table.border.top.color = "transparent",
      footnotes.font.size = 14,
      source_notes.font.size = 14,
      data_row.padding = px(4),
      footnotes.padding = px(1)
    )

  # Save the plot
  gtsave(qb_table,
         glue("figures/{current_szn}_QB_Summary_func.png"))
}

