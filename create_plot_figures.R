#Libraries --------------------------------------------------------------------
library(RColorBrewer)
library(dplyr)
library(espnscrapeR)
library(ggimage)
library(ggplot2)
library(ggtext)
library(ggthemes)
library(glue)
library(gt)
library(gtExtras)
library(nflverse)
library(showtext)
library(sysfonts)
library(tidyverse)
library(zoo)
library(ggpmisc)
library(ggpp)
library(Matrix)


#Define Inputs= --------------------------------------------------------------
szn <- 2022
player <- "P.Mahomes"
franchise <- "KC"
gamechart <- "2022_11_KC_LAC"  #input game_id


#QB Division Table -------------------------------------------------------------
stats <- get_combined_qb_stats(szn)
div_table_qb <- create_qb_summary_table(szn)


#WR Division Table -------------------------------------------------------------
div_table_wr <- create_wr_summary_table(szn)


#QBR ---------------------------------------------------------------------------
qbr <- get_weekly_qbr(year = szn)
qbr_plot <- plot_weekly_qbr(year = szn, QB = "P. Mahomes", all_qbr_file = qbr)
ggsave(glue("figures/qbr_{szn}.png"), plot = qbr_plot)


#Team Efficiency Trends --------------------------------------------------------
nfl_weekly_efficiency(franchise)


#Weekly QB EPA -----------------------------------------------------------------
plot_weekly_qbepa(player, szn)


#All Starters Cumulative QB EPA ------------------------------------------------
starting_qb_epa(szn)


#Plot Mahomes Scale ---------------------------------------------------------
mahomes_scale(szn)


#Plot Rolling EPA  ---------------------------------------------------------
rolling_epa <- get_rolling_epa(szn, roll_number = 35)
epa_p <- plot_rolling_epa(season = szn, team = franchise, roll_number = 35)
ggsave(plot = epa_p, filename = glue('figures/rollingepa_{szn}.png'), width = 24, height = 15)


#Plot NFL Drive Chart  ---------------------------------------------------------
g <- dplyr::filter(pbp, game_id == gamechart)

ggsave(plot = ggdrive(g, "all", with_score_plot = TRUE), glue('figures/gamechart_{gamechart}.png'))


#Time to Throw -----------------------------------------------------------------
plot <- avg_time_to_throw(szn)
ggsave(plot = plot, glue("figures/timetothrow_{szn}.png", width = 9, height = 9))
