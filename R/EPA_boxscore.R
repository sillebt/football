library(nflreadr)
library(tidyverse)
install.packages("na.tools")
library(na.tools)
library(gt)
library(gtExtras)
library(htmltools)
library(glue)

#PICK THE GAME YOU WANT BY ENTERING THESE FIELDS
szn = 2022
game = "2022_22_KC_PHI"

create_epa_boxscore <- function(szn, game) {

  pbp_data <- load_pbp(seasons = szn) |>
    filter(!is_na(epa)) |>
    filter(!is_na(posteam)) |>
    filter(play_type=="pass" | play_type=="run")

  pbp <- pbp_data |>
    filter(game_id == game)

  home <- unique(pbp$home_team)

  away <- unique(pbp$away_team)

  #do stuff for the team summary table
  all <- pbp |> group_by(posteam) |> summarize(
    epa = mean(epa), success=mean(success), play=n(), ypp=mean(yards_gained)) |>
    mutate(rowname="All plays", type=1)

  early <- pbp |> filter(down == 1 | down ==2) |> group_by(posteam) |> summarize(
    epa = mean(epa), success=mean(success), play=n(), ypp=mean(yards_gained)) |>
    mutate(rowname="Early Downs", type=4)

  earlyr <- pbp |> filter(rush==1) |> group_by(posteam) |> summarize(
    epa = mean(epa), success=mean(success), play=n(), ypp=mean(yards_gained)) |>
    mutate(rowname="Rush", type=5)

  earlyp <- pbp |> filter((down == 1 | down ==2) & pass==1) |> group_by(posteam) |> summarize(
    epa = mean(epa), success=mean(success), play=n(), ypp=mean(yards_gained)) |>
    mutate(rowname="Early Down Pass", type=6)

  late <- pbp |> filter(down==3  | down == 4) |> group_by(posteam) |> summarize(
    epa = mean(epa), success=mean(success), play=n(), ypp=mean(yards_gained)) |>
    mutate(rowname="3rd/4th Down", type=7)

  first_half <- pbp |> filter(qtr==1  | qtr == 2) |> group_by(posteam) |> summarize(
    epa = mean(epa), success=mean(success), play=n(), ypp=mean(yards_gained)) |>
    mutate(rowname="1st Half", type=3)

  second_half <- pbp |> filter(qtr==3  | qtr == 4 | qtr == 5) |> group_by(posteam) |> summarize(
    epa = mean(epa), success=mean(success), play=n(), ypp=mean(yards_gained)) |>
    mutate(rowname="2nd Half", type=3)

  type <- pbp |> group_by(posteam, pass) |> summarize(
    epa = mean(epa), success=mean(success), play=n(), ypp=mean(yards_gained)) |>
    mutate(rowname=if_else(pass==1,"Pass","Rush"), type=2)

  bound <- bind_rows(all,first_half,second_half,early,earlyr,earlyp,late,type) |>
    mutate(epa=round(epa, digits=2), success=round(success,digits=2), ypp=round(ypp,digits=2)) |>
    arrange(type) |> select(-pass, -type)

  #team summary table
  table <- bound|>  select(posteam, rowname, epa, success, play, ypp) |> group_by(posteam) |>
    gt() |>
    cols_label(
      epa = md("**EPA/<br>play**"), success = md("**Success<br>Rate**"), play = md("**Plays**"), ypp = md("**Yds/<br>Play**")) |>
    cols_align(align = "center") |>
    opt_table_font(stack = "industrial") |>
    tab_source_note(
      source_note = "Table: @tom | Data: @nflreadr") |>
    tab_header(title = paste((glue("{home} v {away}: {szn} NFL Season")))) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")), locations = cells_group(groups=TRUE)) |>
    tab_style(
      style = list(
        cell_text(align="center")),
      locations = cells_stub(rows=c(2,3,9,10,5,6,12,13)))

  table


  ##### do stuff for player summary table

  rushers <- pbp |> filter(rush==1) |> group_by(rusher_player_name,posteam)|>
    summarize(
      tot_epa = sum(epa), epa = mean(epa),  success=mean(success), play=n(), ypp=mean(yards_gained))|>
    mutate(rowname="Rush attempts", type=1, p="Rush attempts", rowname=rusher_player_name) |>ungroup()

  receivers <- pbp |> filter(rush==0 & !is.na(receiver_player_name)) |> group_by(receiver_player_name,posteam)|>
    summarize(
      tot_epa = sum(epa), epa = mean(epa),  success=mean(success), play=n(), ypp=mean(yards_gained))|>
    mutate(rowname="Targets", type=2, p="Targets", rowname=receiver_player_name)|>ungroup()

  passers <- pbp |> filter(rush==0 & !is.na(passer_player_name)) |> group_by(passer_player_name,posteam)|>
    summarize(
      tot_epa = sum(epa), epa = mean(epa),  success=mean(success), play=n(), ypp=mean(yards_gained))|>
    mutate(rowname="Dropbacks", type=0, p="Dropbacks", rowname=passer_player_name)|>ungroup()


  rp <- bind_rows(passers,rushers,receivers) |>
    mutate(home=ifelse(posteam==home,1,0), epa=round(epa, digits=2), tot_epa=round(tot_epa,digits=1),success=round(success,digits=2),ypp=round(ypp,digits=2)) |>
    arrange(-tot_epa, type,-home,desc(play)) |> select(-type, -rusher_player_name, -receiver_player_name, -passer_player_name)

  #player summary as one big table
  t2 <- rp |> select(posteam,rowname,epa,tot_epa,success,play,ypp) |> gt() |>
    cols_label(
      posteam = md("**Team**"), success = md("**Success<br>rate**"), play = md("**Plays**"), ypp = md("**Yards/<br>Play**"), epa=md("**EPA/<br>Play**"),tot_epa=md("**Total<br>EPA**")) |>
    cols_align(align = "center") |>
    opt_table_font(stack = "industrial") |>
    tab_source_note(
      source_note = "Table: @tom | Data: @nflreadr") |>
    tab_header(title = paste(glue("{home} v {away}: {szn} NFL Season"))) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")), locations = cells_group(groups=TRUE))


  #player summary for away team
  t2a <- rp |> filter(posteam==away) |> select(posteam,rowname,epa,tot_epa,success,play,ypp) |> gt() |>
    cols_label(
      posteam = md("**Team**"), success = md("**Success<br>rate**"), play = md("**Plays**"), ypp = md("**Yards/<br>Play**"), epa=md("**EPA/<br>play**"),tot_epa=md("**Total<br>EPA**")) |>
    cols_align(align = "center") |>
    opt_table_font(stack = "industrial") |>
    tab_source_note(
      source_note = "Table: @tom | Data: @nflreadr") |>
    tab_header(title = paste(glue("{home} v {away}: {away} Players"))) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")), locations = cells_group(groups=TRUE))

  #player summary for home team
  t2h <- rp |> filter(posteam!=away) |> select(posteam,rowname,epa,tot_epa,success,play,ypp) |> gt() |>
    cols_label(
      posteam = md("**Team**"), success = md("**Success<br>rate**"), play = md("**Plays**"), ypp = md("**Yards/<br>Play**"),  epa=md("**EPA/<br>play**"),tot_epa=md("**Total<br>EPA**")) |>
    cols_align(align = "center") |>
    opt_table_font(stack = "industrial") |>
    tab_source_note(
      source_note = "Table: @tom | Data: @nflreadr") |>
    tab_header(title = paste(glue("{home} v {away}: {home} Players"))) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")), locations = cells_group(groups=TRUE))


  #export all the tables

  #team summary
  table |> gtsave(glue("~/Desktop/git_football/figures/BOXSCORE/0_game_summary_{game}.png"))

  #player summary
  t2 |> gtsave(glue("~/Desktop/git_football/figures//BOXSCORE/1_game_players_{game}.png"))

  #away player summary
  t2a |> gtsave(glue("~/Desktop/git_football/figures//BOXSCORE/1_game_players_{away}_{game}.png"))

  #home player summary
  t2h |> gtsave(glue("~/Desktop/git_football/figures//BOXSCORE/1_game_players_{home}_{game}.png"))


}
