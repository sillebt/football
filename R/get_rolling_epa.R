#' Get Rolling EPA
#'
#' @param season is the numerical season to generate
#' @param roll_number is the number of lagging plays to create mean
#'
#' @import nflverse
#' @import tidyverse
#' @import dplyr
#' @import zoo
#' @import ggthemes
#' @return dataframe
#' @export
#'
#' @examples
#' epa <- get_rolling_epa(season = 2022, roll_number = 50)
get_rolling_epa <- function(season, roll_number) {
  pbp <- load_pbp(season)

  rolling_epa <- data.frame()

  rolling_epa <- pbp |>
    filter(qb_dropback == 1 & penalty == 0 & !is.na(epa)) |>
    filter(qb_spike == 0 | qb_kneel == 0) |>
    mutate(
      passer = ifelse(
        is.na(passer_player_name),
        rusher_player_name,
        passer_player_name
      ),
      team_abbr = posteam,
      passer_id = ifelse(
        is.na(passer_player_id),
        rusher_player_id,
        passer_player_id
      ),
      team_abbr = posteam
    ) |>
    filter(!is.na(passer)) |>
    select(passer_id, passer, posteam, week, epa) |>
    group_by(passer) |>
    mutate(moving_avg = rollmean(
      epa,
      k = roll_number,
      fill = 0,
      align = 'right'
    )) |>
    ungroup() |>
    left_join(
      nflreadr::load_rosters() %>%

        select(gsis_id, headshot_url),
      by = c("passer_id" = "gsis_id")
    ) |>
    select(passer, posteam, headshot_url, week, epa, moving_avg)

  rolling_epa$plays <-
    ave(rolling_epa$epa, rolling_epa$passer, FUN = seq_along)

  return(rolling_epa)
}



