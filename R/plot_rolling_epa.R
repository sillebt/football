#' Plot Rolling EPA
#'
#' @param season is the numerical season to generate
#' @param team is the team to call out on the graph in color
#' @param roll_number is the number of lagging plays to create mean
#'
#' @import nflverse
#' @import tidyverse
#' @import dplyr
#' @import zoo
#' @import ggthemes
#' @import ggplot2
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' epa_p <- plot_rolling_epa(season = 2022, team = "KC", roll_number = 50)
#' ggsave(plot = epa_p, filename = 'figures/rollingepa.png', width = 24, height = 15)
plot_rolling_epa <- function(season, team, roll_number) {
  pbp <- load_pbp(season)

  pbp_roll <- pbp |>
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
    select(passer_id, passer, posteam, epa) |>
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
    select(passer, posteam, headshot_url, epa, moving_avg)

  pbp_roll$plays <-
    ave(pbp_roll$epa, pbp_roll$passer, FUN = seq_along)

  # Filter out the first 50 passes
  pbp_roll <- pbp_roll %>%
    filter(plays > 50)

  # Filter data to include only the 'passer' with the most records for each team
  pbp_roll <- pbp_roll %>%
    group_by(posteam) %>%
    filter(passer == passer[which.max(plays)]) %>%
    ungroup()

  # Split data into three data frames: one for each selected team and one for all other teams
  pbp_roll_team <- pbp_roll %>%
    filter(posteam == team)

  pbp_roll_other <- pbp_roll %>%
    filter(posteam != team)

  ggplot() +
    geom_line(
      data = pbp_roll_other,
      aes(x = plays, y = moving_avg, color = "Other"),
      size = 0.5,
      alpha = 0.5,
      show.legend = F
    ) +
    geom_line(
      data = pbp_roll_team,
      aes(x = plays, y = moving_avg, color = team),
      size = 1.25,
      show.legend = F
    ) +
    ggimage::geom_image(
      data = pbp_roll_team,
      aes(
        max(plays),
        moving_avg + 0.15,
        image = ifelse(plays == max(plays), headshot_url, NA)
      ),
      asp = 1.618,
      by = "height",
      size = 0.075,
      inherit.aes = F
    ) +
    scale_x_continuous() +
    scale_y_continuous() +
    scale_color_nfl(type = "primary") +
    theme_clean() +
    theme(
      aspect.ratio = 9 / 16,
      plot.title = element_text(
        face = "bold",
        size = 56 / .pt,
        hjust = 0
      ),
      plot.subtitle = element_text(face = "italic", size = 48 / .pt),
      strip.background = element_rect(
        color = "black",
        fill = "white",
        size = 3.5,
        linetype = "blank"
      ),
      strip.text.x = element_text(face = "bold"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(size = 48 / .pt),
      axis.text.y = element_text(size = 48 / .pt,),
      axis.title = element_text(face = "bold", size = 56 / .pt),
      axis.title.y = element_text(angle = 90, vjust = 0.5),
      plot.caption = element_text(face = "italic", size = 40 / .pt)
    ) +
    labs(
      title = paste0("Rolling EPA Efficiency"),
      subtitle = "Avg EPA/Play Over 50 Plays",
      x = "Dropbacks",
      y = "Rolling EPA",
      caption =  "Chart: @tom | Data: @nflfastR"
    )

}

