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
  setwd("~/Desktop/nfl_plots")
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
