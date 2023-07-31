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
qb_summary_table <- function(szn) {
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

