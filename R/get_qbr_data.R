#' Get QBR Data
#'
#' @param year is the season to call
#'
#' @import tidyverse
#' @import espnscrapeR
#' @import nflverse
#' @import dplyr
#' @import glue
#' @return a dataframe of weekly qbr
#' @export
#'
#' @examples
#' year <- 2022
#' weekly_qbr_data <- get_weekly_qbr(year)
get_weekly_qbr <- function(year) {

  # Determine the number of weeks in the regular season based on the input year
  if (year %in% 1999:2020) {
    weeks_regular <- 1:17
  } else if (year %in% c(2021, 2022)) {
    weeks_regular <- 1:18
  } else {
    stop("Invalid year. The function only supports years from 1999 to 2022.")
  }

  weeks_playoffs <- 1:4
  season_type <-
    c(rep("Regular", length(weeks_regular)), rep("Playoffs", length(weeks_playoffs)))

  espn_qbr <-
    data.frame()  # Initialize an empty dataframe to store the combined data

  for (i in seq_along(weeks_regular)) {
    week_data <-
      get_nfl_qbr(season = year,
                  season_type = season_type[i],
                  week = weeks_regular[i])
    week_data$game_week <-
      weeks_regular[i]  # Add the game week column
    espn_qbr <- rbind(espn_qbr, week_data)
  }

  for (i in seq_along(weeks_playoffs)) {
    week_data <-
      get_nfl_qbr(season = year,
                  season_type = season_type[length(weeks_regular) + i],
                  week = weeks_playoffs[i])
    week_data$game_week <-
      length(weeks_regular) + i  # Add the game week column with adjusted week number for playoffs
    espn_qbr <- rbind(espn_qbr, week_data)
  }

  # Remove redundant week-specific dataframes
  rm(week_data)

  # Reset row names
  rownames(espn_qbr) <- NULL

  return(espn_qbr)
}

get_weekly_qbr_all_years <- function(years) {

  espn_qbr_all_years <- data.frame()  # Initialize an empty dataframe to store the combined data for all years

  for (year in years) {
    espn_qbr_year <- get_weekly_qbr(year)  # Get the data for this year
    espn_qbr_all_years <- rbind(espn_qbr_all_years, espn_qbr_year)  # Combine the data
  }

  return(espn_qbr_all_years)

}


#' Plot QBR Data
#'
#' @param year is the season to call
#'
#' @import tidyverse
#' @import espnscrapeR
#' @import nflverse
#' @import dplyr
#' @import glue
#' @return a dataframe of weekly qbr
#' @export
#'
#' @examples
#' szn <- 2022
#' plot_weekly_qbr(year = szn, QB = "P. Mahomes", all_qbr_file = qbr) See func above for qbr
plot_weekly_qbr <- function(year, QB, all_qbr_file) {

  # Filter the QB we want to look at, modify team names and week numbers
  # to fit nflfastR convention and add the team color of the QB
  single_qbr <- all_qbr_file %>%
    dplyr::filter(season == year & name_short == QB) %>%
    dplyr::left_join(
      nflfastR::teams_colors_logos %>% select(team_nick, team_abbr, team_color),
      by = c("team" = "team_nick"))

  team <- unique(single_qbr$team_abb)
  current_szn <- year
  # Search for the QBs opponents using the schedule function of nflfastR
  # and add logos of the opponents
  opponents <- nflfastR::fast_scraper_schedules(year) %>%
    dplyr::filter(home_team == team | away_team == team) %>%
    dplyr::mutate(
      opp = dplyr::if_else(home_team == team, away_team, home_team)
    ) %>%
    dplyr::left_join(
      nflfastR::teams_colors_logos %>% select(team_abbr, team_logo_espn),
      by = c("opp" = "team_abbr")
    ) %>%
    dplyr::mutate(
      grob = purrr::map(seq_along(team_logo_espn), function(x) {
        grid::rasterGrob(magick::image_read(team_logo_espn[[x]]))
      })
    )

  # Combine the QBR data of the chosen QB with the game data
  chart <- single_qbr %>%
    dplyr::left_join(opponents, by = c("game_week" = "week"))

  # Set title string for later usage
  if (max(chart$game_week) > 17) {
    title_string <- glue::glue("{QB} Weekly Total QBR {year}")
  } else {
    title_string <- glue::glue("{QB} Weekly Total QBR {year} Regular Season")
  }

  # going to draw some quantile lines and combine them here
  quantiles <- c(
    quantile(all_qbr_file$qbr_total, 0.10),
    quantile(all_qbr_file$qbr_total, 0.25),
    quantile(all_qbr_file$qbr_total, 0.75),
    quantile(all_qbr_file$qbr_total, 0.90),
    quantile(all_qbr_file$qbr_total, 0.98)
  )

  chart %>%
    ggplot(aes(x = game_week, y = qbr_total)) +
    geom_hline(yintercept = quantiles, color = "black", linetype = "dashed", alpha = 0.7) +
    geom_hline(yintercept = quantile(all_qbr_file$qbr_total, 0.50), color = "black", linetype = "solid", alpha = 0.7) +
    geom_text(x = 0, y = 2 + quantile(all_qbr_file$qbr_total, 0.10), label = "10th Percentile", hjust = 1, size = 2) +
    geom_text(x = 0, y = 2 + quantile(all_qbr_file$qbr_total, 0.25), label = "25th Percentile", hjust = 1, size = 2) +
    geom_text(x = 0, y = 2 + quantile(all_qbr_file$qbr_total, 0.50), label = "50th Percentile", hjust = 1, size = 2) +
    geom_text(x = 0, y = 2 + quantile(all_qbr_file$qbr_total, 0.75), label = "75th Percentile", hjust = 1, size = 2) +
    geom_text(x = 0, y = 2 + quantile(all_qbr_file$qbr_total, 0.90), label = "90th Percentile", hjust = 1, size = 2) +
    geom_text(x = 0, y = 2 + quantile(all_qbr_file$qbr_total, 0.98), label = "98th Percentile", hjust = 1, size = 2) +
    geom_line(colour = chart$team_color) +
    scale_x_continuous(
      limits = c(-1.4, NA),
      breaks = scales::breaks_pretty()
    ) +
    geom_grob(aes(x = game_week, y = qbr_total, label = grob), vp.width = 0.05) +
    labs(
      x = "Game Week",
      y = "ESPN QBR",
      caption = "Figure: @tom | Data: espnscrapeR by @thomas_mock",
      title = title_string,
      subtitle = "Percentiles of the whole NFL 2006 - 2022 are drawn for orientation\nTo qualify, a player must play a minimum of 20 action plays per team game"
    ) +
    ggthemes::theme_stata(scheme = "sj", base_size = 8) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 1),
      axis.text.y = element_text(angle = 0, vjust = 0.5),
      legend.title = element_text(size = 8, hjust = 0, vjust = 0.5, face = "bold"),
      legend.position = "top",
      aspect.ratio = 1 / 1.618
    )
}

