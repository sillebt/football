library(nflreadr)
library(dplyr)
library(ggplot2)
library(nflplotR)
library(ggh4x)
library(ggbeeswarm)

avg_time_to_throw <- function(szn) {
  tt <- nflreadr::load_nextgen_stats(stat_type = "passing",seasons = szn) %>%
    mutate(team_abbr = ifelse(team_abbr == "LAR", "LA", team_abbr))


  tt_qbs <- tt %>%
    filter(week == 0) %>%
    group_by(team_abbr) %>%
    arrange(desc(attempts)) %>%
    slice_head() %>%
    select(player_display_name, team_abbr, player_gsis_id) %>%
    ungroup()

  tt_top <- tt %>%
    filter(player_gsis_id %in% tt_qbs$player_gsis_id,
           (player_display_name != "Baker Mayfield" | team_abbr != "CAR")
    )

  tt_avg <- tt %>%
    summarise(avg = mean(avg_time_to_throw))

  tt_all <- nflreadr::load_nextgen_stats(stat_type = "passing", seasons = 2016:2022) %>%
    mutate(team_abbr = ifelse(team_abbr == "LAR", "LA", team_abbr))

  tt_all2 <- tt_all %>%
    filter(player_gsis_id %in% tt_top$player_gsis_id,
           week != 0) %>%
    group_by(player_gsis_id) %>%
    arrange(desc(season)) %>%
    mutate(team_abbr = ifelse(season == szn, team_abbr, NA)) %>%
    tidyr::fill(team_abbr) %>%
    filter(season != szn) %>%
    ungroup() %>%
    add_row(team_abbr = "PIT")


  tt_top$team_abbr2 <- nfl_team_factor(tt_top$team_abbr)
  tt_all2$team_abbr2 <- nfl_team_factor(tt_all2$team_abbr)

  # tt_top %>%
  #   filter(week == 0) %>%
  #   select(player_display_name, team_abbr) %>%
  #   clipr::write_clip()


  label_names <- c(
    "MIA" = "Tua Tagovailoa",
    "LV" = "Derek Carr",
    "NYJ" = "Zach Wilson",
    "NYG" = "Daniel Jones",
    "BUF" = "Josh Allen",
    "DAL" = "Dak Prescott",
    "SF" = "Jimmy Garoppolo",
    "PIT" = "Kenny Pickett",
    "TEN" = "Ryan Tannehill",
    "PHI" = "Jalen Hurts",
    "ARI" = "Kyler Murray",
    "CAR" = 'Sam Darnold',
    "HOU" = "Davis Mills",
    "NE" = "Mac Jones",
    "LAC" = "Justin Herbert",
    "BAL" = "Lamar Jackson",
    "IND" = "Matt Ryan",
    "MIN" = "Kirk Cousins",
    "JAX" = "Trevor Lawrence",
    "SEA" = "Geno Smith",
    "WAS" = "Carson Wentz",
    "LA" = "Baker Mayfield",
    "KC" = "Patrick Mahomes",
    "GB" = "Aaron Rodgers",
    "TB" = "Tom Brady",
    "CLE" = "Jacoby Brissett",
    "NO" = "Andy Dalton",
    "ATL" = "Marcus Mariota",
    "CHI" = "Justin Fields",
    "DEN" = "Russell Wilson",
    "DET" = "Jared Goff",
    "CIN" = "Joe Burrow"

  )

  # plot --------------------------------------------------------------------

  g <- tt_top %>%
    ggplot(aes(x = avg_time_to_throw, y = player_display_name))+
    nflplotR::geom_nfl_logos(aes(team_abbr = team_abbr2), width = 0.4,
                             stat = "unique",
                             alpha = 0.2,
                             x = 3.7, y = 1)+
    geom_beeswarm(data = tt_all2 %>% filter(week != 0),
                  size = 2.8,
                  alpha = 0.2,
                  shape = 16,
                  cex= 4,
                  priority='random',
                  color = "grey50")+
    geom_vline(data = tt_avg,
               aes(xintercept = avg),
               color = "#0570b0",
               linetype = "dashed")+
    geom_vline(data = . %>% filter(week == 0),
               aes(xintercept = avg_time_to_throw))+
    geom_beeswarm(data = . %>% filter(week != 0),
                  size = 2.5,
                  cex=4,
                  priority='random',
                  shape = 21,
                  aes(color = team_abbr2,
                      fill = team_abbr2))+
    geom_text(data = . %>% filter(week == 0),
              x = 1.9, y = 1.4,
              hjust = 0,
              family = "Open Sans",
              aes(label = paste0(scales::number(avg_time_to_throw,accuracy = 0.01)," s")))+
    scale_color_nfl(type = "secondary")+
    scale_fill_nfl(type = "primary")+
    facet_wrap2(team_abbr2~.,
                labeller = as_labeller(label_names),
                scales = "free_y",
                axes = "x",
                remove_labels = "x",
                ncol = 4)+
    labs(
      title = glue("Average time to throw for each QB - {2022}szn} Regular season"),
      subtitle = glue::glue("Current season highlighted - QB with most attempts per team. NFL 2022 season average ({round(tt_avg$avg,2)} sec) in blue."),
      caption = "@tom - Data: Next Gen Stats with nflreadr"
    )+
    theme_classic()+
    theme(
      text = element_text(family = "Open Sans"),
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(face = "bold"),
      axis.text.x = element_text(face = "bold"),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(color = "black", size = 0.8),
      axis.line.y = element_blank(),
      axis.line.x = element_line(size = 0.8),
      panel.grid.major.x = element_line(),
      plot.background = element_rect(fill = "#f0f0f0"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 10)
    )


}
