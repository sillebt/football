library(dplyr)
library(extrafont)
library(ggplot2)
library(ggrepel)
library(ggimage)
library(ggridges)
library(ggtext)
library(ggfx)
library(geomtextpath)
library(cropcircles)
library(magick)
library(glue)
library(showtext)
library(nflplotR)

plot_weekly_qbepa <- function(player, szn) {

  pbp <- load_pbp(szn)

  team_info <- load_teams() %>%
    select(team = team_abbr, team_logo_wikipedia)

  qbs <- load_players() %>%
    filter(status == "ACT" & position == "QB")

  df <- pbp |>
    filter(qb_dropback == 1) %>%
    filter(!is.na(epa)) %>%
    filter(penalty == 0) %>%
    filter(qb_spike == 0 | qb_kneel == 0) %>%
    mutate(player_name = ifelse(!is.na(passer_player_name),passer_player_name,rusher_player_name))

  mahomes_epa <- df %>%
    select(player_name, week, team_abbr = posteam, opponent = defteam, epa) %>%
    filter(player_name %in% qbs$short_name) %>%
    group_by(player_name, week, team_abbr, opponent) |>
    summarise(mean_qb_epa = mean(epa, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(player_name == player) |>
    left_join(team_info, by = c("opponent" = "team"))

  ggplot(data = mahomes_epa, aes(x = week, y = mean_qb_epa)) +
    geom_area(fill = "#FFB612", alpha = 0.4)

  ggplot(data = mahomes_epa, aes(x = week, y = mean_qb_epa)) +
    geom_smooth(se = FALSE, color = "black", linetype = "dashed") +
    geom_area(fill = "#FFB612", alpha = 0.4) +
    geom_line(color = "#E31837", size = 1.5)

  ggplot(data = mahomes_epa, aes(x = week, y = mean_qb_epa)) +
    geom_smooth(se = FALSE, color = "black", linetype = "dashed") +
    geom_area(fill = "#FFB612", alpha = 0.4) +
    geom_line(color = "#E31837", size = 1.5) +
    geom_image(aes(image = team_logo_wikipedia), size = 0.045, asp = 16/9)

  ggplot(data = mahomes_epa, aes(x = week, y = mean_qb_epa)) +
    geom_smooth(se = FALSE, color = "black", linetype = "dashed") +
    geom_area(fill = "#FFB612", alpha = 0.4) +
    geom_line(color = "#E31837", size = 1.5) +
    geom_image(aes(image = team_logo_wikipedia), size = 0.045, asp = 16/9) +
    scale_x_continuous() +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    xlab("Week") +
    ylab("Average QB EPA") +
    labs(title = "Patrick Mahomes: Mean QB EPA per Week",
         subtitle = glue("{szn} NFL Season"),
         caption = "Mean EPA on Dropbacks")

  ggsave(glue('figures/{player}_weekly_epa_{szn}.png'))

}
