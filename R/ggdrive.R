
#' Create Drive Chart from nflverse PBP
#'
#' @param pbp nflverse play-by-play data filtered down to one game
#' @param type One of "all", "away", "home". Decides what type of chart to build
#' @param with_score_plot Either `TRUE` or `FALSE`. Choose to add scores to the plot.
#' @param home_reverse Reverse direction of home team for chart `type` "home"
#'
#' @import ggplot2
#' @import dplyr
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' pbp <- nflreadr::load_pbp(2022)
#'
#' g <- dplyr::filter(pbp, game_id == "2022_05_LV_KC")
#'
#' ggdrive(g)
#' ggdrive(g, "away", with_score_plot = TRUE)
#' ggdrive(g, "home", with_score_plot = TRUE)
ggdrive <- function(pbp,
                    type = c("all", "away", "home"),
                    with_score_plot = FALSE,
                    home_reverse = TRUE) {
  type <- rlang::arg_match(type)

  if (length(unique(pbp$game_id)) > 1) {
    cli::cli_abort("Only 1 game allowed in argument {.arg pbp}")
  }

  final_score <- pbp |>
    dplyr::slice_tail(n = 1) |>
    dplyr::select(away_score = total_away_score, home_score = total_home_score)

  one_g <- pbp |>
    dplyr::group_by(drive_number = fixed_drive) |>
    dplyr::summarise(
      away_team = collapse::fmode(away_team),
      home_team = collapse::fmode(home_team),
      posteam = collapse::fmode(posteam),
      start_yard_line = collapse::fmode(drive_start_yard_line),
      end_yard_line = collapse::fmode(drive_end_yard_line),
      start_desc = collapse::fmode(drive_start_transition),
      end_desc = collapse::fmode(fixed_drive_result),
      end_desc_nfl = collapse::fmode(drive_end_transition),
      away_score = collapse::flast(total_away_score),
      home_score = collapse::flast(total_home_score),
      top = collapse::fmode(drive_time_of_possession),
      plays = collapse::fmode(drive_play_count),
      air_yards = collapse::flast(air_yards),
      yards_gained = collapse::flast(yards_gained),
      half = collapse::fmode(game_half),
      ret_yards = collapse::ffirst(return_yards) |> as.integer(),
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      start_side = stringr::str_extract(start_yard_line, "[:upper:]+"),
      start_yard = stringr::str_extract(start_yard_line, "[:digit:]+") |> as.integer(),
      end_side = stringr::str_extract(end_yard_line, "[:upper:]+"),
      end_yard = stringr::str_extract(end_yard_line, "[:digit:]+") |> as.integer(),
      start_abs = dplyr::case_when(
        start_desc == "KICKOFF" &
          end_desc_nfl == "TOUCHDOWN" &
          posteam == home_team & plays == 0 ~ 10L + ret_yards,
        start_desc == "KICKOFF" &
          end_desc_nfl == "TOUCHDOWN" &
          posteam == away_team & plays == 0 ~ 110L - ret_yards,
        start_side == away_team ~ start_yard + 10L,
        start_side == home_team ~ 110L - start_yard,
        is.na(start_side) ~ 60L,
        TRUE ~ start_yard
      ),
      end_abs = dplyr::case_when(
        posteam == away_team & end_desc == "Touchdown" ~ 110L,
        posteam == away_team & end_desc == "Opp touchdown" ~ 10L,
        posteam == home_team & end_desc == "Touchdown" ~ 10L,
        posteam == home_team & end_desc == "Opp touchdown" ~ 110L,
        end_side == away_team ~ end_yard + 10L,
        end_side == home_team ~ 110L - end_yard,
        is.na(end_side) ~ 60L,
        TRUE ~ end_yard
      ),
      yards = ifelse(posteam == away_team, end_abs - start_abs, start_abs - end_abs),
      y = 51.33 - (drive_number - 1) * (53.33 - 4) / (max(drive_number) - 1),
      scoring_drive = ifelse(
        stringr::str_detect(end_desc, "ouchdown") |
          stringr::str_detect(end_desc, "Field"),
        1,
        0
      ),
      end_desc = dplyr::case_when(
        end_desc == "Touchdown" ~ "TD",
        end_desc == "Opp touchdown" &
          end_desc_nfl == "PUNT" ~ "Punt Return TD",
        end_desc == "Opp touchdown" &
          end_desc_nfl == "INTERCEPTION" ~ "INT Return TD",
        end_desc == "Opp touchdown" ~ "Opp TD",
        end_desc == "Field goal" ~ "FG",
        end_desc == "Missed field goal" ~ "Missed FG",
        end_desc == "Turnover" &
          end_desc_nfl == "INTERCEPTION" ~ "INT",
        end_desc == "Turnover" & end_desc_nfl == "FUMBLE" ~ "FUM",
        end_desc == "Turnover on downs" ~ "DOWNS",
        is.na(end_desc) ~ "Ongoing",
        TRUE  ~ tools::toTitleCase(end_desc)
      ),
      to_location = dplyr::case_when(
        end_desc == "INT" &
          posteam == away_team ~ end_abs + as.integer(air_yards),
        end_desc == "INT" &
          posteam == home_team ~ end_abs - as.integer(air_yards),
        end_desc == "FUM" &
          posteam == away_team ~ end_abs + as.integer(yards_gained),
        end_desc == "FUM" &
          posteam == home_team ~ end_abs - as.integer(yards_gained),
        TRUE ~ NA_integer_
      ),
      label = ifelse(
        plays > 1,
        glue::glue(
          "{end_desc} / {plays} Plays {yards} Yds ({top}) / {away_score}-{home_score}"
        ),
        glue::glue(
          "{end_desc} / {plays} Play {yards} Yds ({top}) / {away_score}-{home_score}"
        )
      ),
      label_x = dplyr::case_when(
        !is.na(to_location) &
          posteam == away_team & end_abs <= to_location ~ to_location,!is.na(to_location) &
          posteam == home_team & end_abs >= to_location ~ to_location,
        TRUE ~ end_abs
      ),
      grob_x = dplyr::case_when(!is.na(to_location) ~ to_location,
                                TRUE ~ end_abs),
      i = abs(start_abs - end_abs) * 5,
      away_score_lab = ifelse(
        away_score != dplyr::lag(away_score) |
          drive_number == 1,
        away_score,
        ""
      ),
      home_score_lab = ifelse(
        home_score != dplyr::lag(home_score) |
          drive_number == 1,
        home_score,
        ""
      ),
    ) |>
    dplyr::left_join(
      nflreadr::load_teams() |> dplyr::filter(!team_abbr %in% c("LAR", "OAK", "STL", "SD")),
      by = c("posteam" = "team_abbr")
    )

  scores <- one_g
  half <- pbp |>
    dplyr::mutate(max_drive = max(fixed_drive),
                  max_qtr = max(qtr)) |>
    dplyr::filter(desc == "END QUARTER 2" |
                    (desc == "END QUARTER 4" & max_qtr > 4)) |>
    dplyr::mutate(
      y = 51.33 - (fixed_drive - 1) * (53.33 - 4) / (max_drive - 1) - (53.33 - 4) / (max_drive - 1) / 2,
      half_lab = ifelse(desc == "END QUARTER 2", "END\nHALF", "END\nREG")
    ) |>
    dplyr::select(y, half_lab)

  ko_ret_td <- one_g |>
    dplyr::filter(start_desc == "KICKOFF" &
                    end_desc_nfl == "TOUCHDOWN" & plays == 0) |>
    dplyr::mutate(
      lab = "Kickoff\nReturn",
      lab_hjust = dplyr::case_when(posteam == home_team &
                                     type == "all" ~ 0,
                                   TRUE ~ 1),
    )

  home_team <- unique(one_g$home_team)
  home_col <- one_g |>
    dplyr::filter(posteam == home_team) |>
    dplyr::distinct(team_color) |>
    dplyr::pull(team_color)
  away_team <- unique(one_g$away_team)
  away_col <- one_g |>
    dplyr::filter(posteam == away_team) |>
    dplyr::distinct(team_color) |>
    dplyr::pull(team_color)

  color_diff <- function(col1, col2) {
    rgb1 <- as.vector(col2rgb(col1))
    rgb2 <- as.vector(col2rgb(col2))

    diff <-
      (max(rgb1[1], rgb2[1]) - min(rgb1[1], rgb2[1])) +
      (max(rgb1[2], rgb2[2]) - min(rgb1[2], rgb2[2])) +
      (max(rgb1[3], rgb2[3]) - min(rgb1[3], rgb2[3]))

    return(diff)
  }

  if (color_diff(home_col, away_col) < 100) {
    one_g <- one_g |>
      dplyr::mutate(team_color = dplyr::case_when(posteam == away_team ~ team_color2,
                                                  TRUE ~ team_color))
    away_col <- one_g |>
      dplyr::filter(posteam == away_team) |>
      dplyr::distinct(team_color2) |>
      dplyr::pull(team_color2)
  }

  endzone <- data.frame(
    x = c(5, 115),
    y = 53.333 / 2,
    team_abbr = c(away_team, home_team),
    angle = c(90,-90)
  )

  final_drive <- one_g |> dplyr::slice_tail(n = 1)

  if (type == "away") {
    one_g <- one_g |> dplyr::filter(posteam == away_team)
    ko_ret_td <- ko_ret_td |> dplyr::filter(posteam == away_team)
  } else if (type == "home") {
    one_g <- one_g |> dplyr::filter(posteam == home_team)
    ko_ret_td <- ko_ret_td |> dplyr::filter(posteam == home_team)
  }

  plays <- one_g |>
    dplyr::left_join(
      pbp |>
        dplyr::filter(!is.na(down)) |>
        dplyr::select(drive_number = fixed_drive, home_team, away_team, posteam, yrdln, penalty) |>
        dplyr::mutate(
          side = stringr::str_extract(yrdln, "[:upper:]+"),
          yardline = stringr::str_extract(yrdln, "[:digit:]+") |> as.integer(),
          yard_abs = dplyr::case_when(
            side == away_team ~ yardline + 10L,
            side == home_team ~ 110L - yardline,
            side == "MID" ~ 60L,
            TRUE ~ NA_integer_
          ),
          count_color = dplyr::case_when(# penalty == 1 ~ "#F4B317",
            penalty == 1 ~ "#FCD32F",
            TRUE ~ "black"),
          count_bg = dplyr::case_when(penalty == 1 ~ "grey30",
                                      TRUE ~ NA_character_),
        ) |>
        dplyr::group_by(drive_number) |>
        dplyr::mutate(play_count = dplyr::row_number()) |>
        dplyr::ungroup() |>
        dplyr::select(
          drive_number,
          yard_abs,
          play_count,
          penalty,
          count_color,
          count_bg
        ),
      by = "drive_number"
    )

  if (type == "home" && isTRUE(home_reverse)) {
    rev_vars <-
      c("start_abs", "end_abs", "to_location", "label_x", "grob_x")
    one_g <-
      one_g |> dplyr::mutate_at(rev_vars, function(x)
        120L - x)
    ko_ret_td <-
      ko_ret_td |> dplyr::mutate_at(rev_vars, function(x)
        120L - x)
    plays <-
      plays |> dplyr::mutate_at(c("yard_abs"), function(x)
        120L - x)
    endzone$team_abbr <- rev(endzone$team_abbr)
  }

  pitch <- sportyR::geom_football(
    "NFL",
    color_updates = list(
      "offensive_half" = "#b6c6b3",
      "defensive_half" = "#b6c6b3",
      "sideline" = "#ffffff",
      "end_line" = "#ffffff",
      "goal_line" = "#ffffffe6",
      # assuming by yard_markings_color you want
      # yard line colors for all yard lines
      "minor_yard_line" = "#ffffffe6",
      "major_yard_line" = "#ffffffe6",
      "try_mark" = "#ffffffe6",
      "directional_arrow" = "#ffffffe6",
      "yardage_marker" = "#ffffffe6",
      "offensive_endzone" = "black",
      "defensive_endzone" = "#b6c6b3",
      "field_apron" = "#b6c6b3",
      "field_border" = "#b6c6b3",
      "team_bench_area" = NULL
    ),
    x_trans = 60,
    y_trans = 25 + 1 / 3
  )

  p <- pitch +
    # ENDZONE a bit brighter
    ggplot2::annotate(
      "tile",
      x = c(5, 115),
      y = 53.33 / 2,
      width = 10,
      height = 53.33,
      fill = "white",
      alpha = 0.3
    ) +
    ggplot2::annotate(
      ggplot2::GeomSegment,
      x = 10,
      xend = 110,
      y = half$y,
      yend = half$y,
      size = 0.5,
      alpha = 0.4,
      linetype = "dotted"
    ) +
    ggplot2::annotate(
      ggplot2::GeomText,
      x = 10,
      y = half$y,
      label = half$half_lab,
      # family = "Roboto Condensed",
      hjust = 1,
      vjust = 0.5,
      size = 2
    ) +
    ggplot2::annotate(
      ggplot2::GeomText,
      x = ko_ret_td$start_abs,
      y = ko_ret_td$y,
      label = ko_ret_td$lab,
      # family = "Roboto Condensed",
      hjust = ko_ret_td$lab_hjust,
      vjust = 0.5,
      size = 2
    ) +
    # WORDMARKS in ENDZONES
    nflplotR::geom_nfl_wordmarks(
      data = endzone,
      ggplot2::aes(
        x = x,
        y = y,
        team_abbr = team_abbr,
        angle = angle
      ),
      height = 0.1,
      alpha = 0.9
    ) +
    # add COMET TAILS. Since we are using alpha, each geom_link needs a separate
    # number of segments `n` which is why we are looping over all drives
    lapply(one_g$drive_number, function(x) {
      ggforce::geom_link(
        data = one_g |> dplyr::filter(drive_number == x),
        ggplot2::aes(
          x = start_abs,
          y = y,
          xend = end_abs,
          yend = y,
          size = ggplot2::after_stat(index),
          color = team_color,
          alpha = dplyr::case_when(scoring_drive == 1 ~ 1, TRUE ~ 0.1)# non-scoring drives with lower alpha
        ),
        # non-scoring drives with square line end
        lineend = dplyr::case_when(one_g$scoring_drive[one_g$drive_number == x] == 1 ~ "round", TRUE ~ "square"),
        n = one_g$i[one_g$drive_number == x]
      )
    }) +
    # add separate points for drives with no gain as they have length zero in geom_link
    ggplot2::geom_point(
      data = one_g |> dplyr::filter(scoring_drive == 0 & i < 1),
      ggplot2::aes(x = end_abs, y = y, color = team_color),
      shape = 15,
      size = 3,
      alpha = 0.6
    )

  if (type != "all") {
    p <- p +
      ggrepel::geom_text_repel(
        data = plays,
        ggplot2::aes(
          x = yard_abs,
          y = y,
          label = play_count,
          color = count_color,
          bg.color = count_bg
        ),
        size = 2,
        box.padding = 0,
        nudge_y = -1.75,
        direction = "x",
        bg.r = 0.10
      )
  }

  icon_from_desc <- function(desc) {
    dplyr::case_when(
      desc == "FG" ~             file.path("icons/fg.png"),
      desc == "TD" ~             file.path("icons/td.png"),
      desc == "Opp TD" ~         file.path("icons/td.png"),
      desc == "Punt Return TD" ~ file.path("icons/td.png"),
      desc == "INT Return TD" ~  file.path("icons/td.png"),
      desc == "INT" ~            file.path("icons/to.png"),
      desc == "FUM" ~            file.path("icons/to.png"),
      TRUE ~ NA_character_
    )
  }

  # add symbols for scoring drives
  p <- p + nflplotR::geom_from_path(
    data = one_g |>
      dplyr::filter(scoring_drive == 1 | !is.na(to_location)) |>
      dplyr::mutate(icon = icon_from_desc(end_desc)),
    ggplot2::aes(x = grob_x, y = y, path = icon),
    width = 0.02
  ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_size(range = c(.01, 3.5), guide = "none") +
    ggplot2::scale_alpha_identity() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(-1,-0.5,-0.5,-0.5, "cm"),
      plot.caption = ggplot2::element_blank(),
    ) +
    NULL

  # add labels with some drive stats
  if (type == "all") {
    p <- p +
      ggplot2::geom_label(
        data = one_g |> dplyr::filter(posteam == home_team),
        ggplot2::aes(
          x = label_x,
          y = y,
          hjust = 1,
          label = label,
          color = team_color,
          alpha = 0.9,
        ),
        size = 2,
        nudge_x = -1,
        label.padding = unit(0.10, "lines")
      ) +
      ggplot2::geom_label(
        data = one_g |> dplyr::filter(posteam == away_team),
        ggplot2::aes(
          x = label_x,
          y = y,
          hjust = 0,
          label = label,
          color = team_color,
          alpha = 0.9,
        ),
        size = 2,
        nudge_x = 1,
        label.padding = unit(0.10, "lines")
      )
  } else if (type == "home" && isFALSE(home_reverse)) {
    p <- p +
      ggplot2::geom_label(
        data = one_g |> dplyr::filter(posteam == home_team),
        ggplot2::aes(
          x = label_x,
          y = y,
          hjust = 1,
          label = label,
          color = team_color,
          alpha = 0.9,
        ),
        size = 2,
        nudge_x = -1,
        label.padding = unit(0.10, "lines")
      )
  } else if (type == "home" && isTRUE(home_reverse)) {
    p <- p +
      ggplot2::geom_label(
        data = one_g |> dplyr::filter(posteam == home_team),
        ggplot2::aes(
          x = label_x,
          y = y,
          hjust = 0,
          label = label,
          color = team_color,
          alpha = 0.9,
        ),
        size = 2,
        nudge_x = 1,
        label.padding = unit(0.10, "lines")
      )
  } else if (type == "away") {
    p <- p +
      ggplot2::geom_label(
        data = one_g |> dplyr::filter(posteam == away_team),
        ggplot2::aes(
          x = label_x,
          y = y,
          hjust = 0,
          label = label,
          color = team_color,
          alpha = 0.9,
        ),
        size = 2,
        nudge_x = 1,
        label.padding = unit(0.10, "lines")
      )
  }

  if (isTRUE(with_score_plot)) {
    home_alpha <- ifelse(type == "away", 0.1, 1)
    away_alpha <- ifelse(type == "home", 0.1, 1)
    p <- p +
      ggplot2::geom_path(
        data = scores,
        ggplot2::aes(x = away_score / 3 + 125, y = y),
        color = away_col,
        alpha = away_alpha
      ) +
      ggplot2::geom_text(
        data = scores,
        ggplot2::aes(
          x = away_score / 3 + 125,
          y = y,
          label = away_score_lab
        ),
        color = away_col,
        hjust = 0,
        size = 2,
        nudge_x = 1,
        alpha = away_alpha
      ) +
      ggplot2::geom_path(
        data = scores,
        ggplot2::aes(x = home_score / 3 + 125, y = y),
        color = home_col,
        alpha = home_alpha
      ) +
      ggplot2::geom_text(
        data = scores,
        ggplot2::aes(
          x = home_score / 3 + 125,
          y = y,
          label = home_score_lab
        ),
        color = home_col,
        hjust = 0,
        size = 2,
        nudge_x = 1,
        alpha = home_alpha
      )
  }

  p

}
