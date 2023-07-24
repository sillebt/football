# packages ----------------------------------------------------------------

library(dplyr)
library(nflreadr)
library(nflplotR)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(elementalist)
library(stringr)
library(scales)

mahomes_scale <- function(szn) {
  pbp <- nflreadr::load_pbp(szn)

  QB_data <- pbp %>%
    dplyr::filter(
      !is.na(qb_epa),
      qb_dropback == 1,
      down <= 4
    ) %>%
    dplyr::mutate(down_var = ifelse(down <= 2,
                                    "early",
                                    "late")) %>%
    dplyr::group_by(name,
                    passer_id,
                    posteam,
                    down_var
    ) %>%
    dplyr::summarize(
      mean_epa = mean(qb_epa),
      n = n()
    ) %>%
    dplyr::filter(n >= 50) %>%
    tidyr::pivot_wider(
      names_from = down_var,
      values_from = c(mean_epa,n)
    ) %>%
    dplyr::filter(
      !is.na(mean_epa_early),
      !is.na(mean_epa_late)
    ) %>%
    dplyr::mutate(
      n_combined = n_early+n_late,
      name = stringr::str_extract(name, "(?<=\\.).*")
    )

  my_pal <- function(range = c(1,6)) {
    force(range)
    function(x) scales::rescale(x, to = range, from = c(0, 1))
  }

  QB_data %>%
    ggplot(aes(x = mean_epa_early, y = mean_epa_late))+
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey30")+
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey30")+
    geom_path(aes(x = c(0:0.1), y = c(0:0.2)),
              color = "blue")+
    geom_point(aes(fill = posteam, color = posteam,
                   size = n_combined),
               shape = 21)+
    continuous_scale(
      aesthetics = c("size", "point.size"), scale_name = "size",
      palette = my_pal(c(2,6)),
      guide = guide_legend(override.aes = list(label = "")) # hide "a" in legend
    ) +
    geom_text_repel(aes(label = name,
                        point.size = n_combined,
                        fontface = ifelse(name == "Mahomes", "bold", "plain")),max.time = 5,
                    max.overlaps = 7)+
    scale_x_continuous(breaks = seq(-0.5,0.5,0.1),
                       limits = c(-0.02,NA))+
    scale_y_continuous(breaks = seq(-0.5,0.5,0.1),
                       limits = c(-0.21,NA))+
    scale_fill_nfl("primary")+
    scale_color_nfl("secondary")+
    labs(
      title = "Introducing the Mahomes Scale",
      subtitle = "For players breaking records and charts",
      caption = "@tom - data: @nflfastR",
      x = "EPA/Play on early downs (1st&2nd)",
      y = "EPA/Play on late downs (3rd&4th)"
    )+
    theme_classic()+
    theme(
      text = element_text(family = "Open Sans"),
      axis.line = element_line(linewidth = 1),
      axis.line.x = element_line_multicolour(c("black", "black", "black","black","black","black", "#E31837"),
                                             size = 1),
      axis.line.y = element_line_multicolour(c("black", "black", "#E31837", "#E31837"),
                                             size = 1),
      axis.text = element_text(face = "bold", color = "black"),
      axis.text.y = element_text(color = c(rep("black",4),"#d02138","#e31d3b","#E31837")),
      axis.ticks.y = element_line(color = c(rep("black",4),"#d02138","#e31d3b","#E31837")),
      axis.ticks.x = element_line(color = "black"),
      axis.title = element_text(face = "bold"),
      panel.grid.major.x = element_line(color = "#dddddd"),
      panel.grid.major.y = element_line(color = c("#E3A3AD","#E38A98","#E36679","#dddddd","#dddddd","#dddddd", "#dddddd")),
      plot.title = element_text(face = "bold", size = 18),
      plot.title.position = "plot",
      plot.background = element_rect(fill = "#f1f1f1", color = NA),
      panel.background = element_rect(fill = NA, color = NA),
      legend.position = "none"
    )+
    coord_cartesian(xlim = c(-0.02,NA),
                    ylim = c(-0.2,NA))


  ggsave("figures/mahomes_scale.png", width = 8, height = 6.5)
}
