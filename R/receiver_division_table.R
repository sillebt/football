create_wr_summary_table <- function(szn) {

  nfl_stats <- load_player_stats(seasons = TRUE)
  nfl_stats_refined <- nfl_stats |>
    filter(position=="WR" | position=="TE") |>
    filter(season==szn)

  nfl_team<- teams_colors_logos

  current_szn <- max(nfl_stats_refined$season)

  #format data
  nfl_plot_data<-nfl_stats_refined|>
    select(headshot_url, player_name, player_display_name, team = recent_team, targets, receptions, receiving_yards, receiving_tds, receiving_first_downs, receiving_epa, receiving_fumbles_lost, special_teams_tds)|>
    group_by(headshot_url,player_name, player_display_name, team) |>
    summarize(
      targets = sum(targets),
      receptions = sum(receptions),
      rec_yards = sum(receiving_yards),
      touchdowns = sum(receiving_tds, special_teams_tds),
      fumbles = sum(receiving_fumbles_lost),
      first_downs = sum(receiving_first_downs),
      total_epa = sum(receiving_epa, na.rm = T)) |>
    inner_join(teams_colors_logos|>select(team_abbr, team_nick, team_division, team_wordmark), by=c("team"="team_abbr"))|>
    separate(team_division, into=c("conference","division"), sep=" ")|>
    mutate(yds_per_tgt = rec_yards/targets,
           yds_per_rec = rec_yards/receptions,
           rec_first_down = first_downs/receptions)|>
    arrange(conference, division, team, -total_epa, -rec_yards, -touchdowns)|>
    group_by(conference, division, team)|>
    mutate(rank = row_number()) |>
    ungroup() |>
    filter(rank==1) |>
    group_by(conference, division) |>
    arrange(-total_epa, conference, division, )


  # Create gt table
  table <- nfl_plot_data |>
    select(headshot_url, player_display_name, team_wordmark, targets:touchdowns, first_downs, yds_per_tgt, rec_first_down, conference:division,  total_epa) |>
    gt() |>
    tab_header(
      title = glue("Receiving Summary: {current_szn}"),
      subtitle = md("EPA Leader by Division"))  |>
    cols_label(headshot_url = "",
               player_display_name = "Player",
               team_wordmark = "",
               targets = "Tgt",
               receptions = "Rec",
               rec_yards = "Yards",
               touchdowns = "TDs",
               first_downs = "1D",
               yds_per_tgt = "Yd/Tgt",
               rec_first_down = "Rec/1D",
               total_epa = "EPA") |>
    gt_img_rows(headshot_url) |>
    gt_img_rows(team_wordmark) |>
    data_color(
      columns = vars(total_epa),
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "RColorBrewer::PRGn",
          direction  = 1
        ) |> as.character(),
        domain = NULL,
        na.color = "#00441BFF"
      )
    ) |>
    fmt_percent(
      columns = rec_first_down,
      decimals = 1
    ) |>
    fmt_number(
      columns = total_epa,
      decimals = 2
    ) |>
    fmt_number(
      columns = yds_per_tgt,
      decimals = 2
    ) |>
    tab_options(
      table.background.color = "white",
      heading.title.font.size  = 28,
      heading.title.font.weight = 'bold',
      heading.subtitle.font.size = 14,
      table.font.names = "Chivo",
      table.font.color = 'black',
      table.border.top.color = "transparent",
      footnotes.font.size = 12,
      source_notes.font.size = 12,
      data_row.padding = px(2),
      footnotes.padding = px(1),
    )

  gtsave(table, glue("figures/{current_szn}_Rec_Summary.png"))
}
