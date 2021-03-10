#### Describe panel of Chi neighborhoods --------------------------------------

#dependencies
library(tidyverse)
library(sf)
library(ggrepel)

#set working directory
setwd("R:/Project/chi-neighborhood-change/gang-nc-analysis")

#read in spatial data for tracts
panel <- st_read("./output/chi-tract-panel.geojson")


#### A. Descriptive analysis --------------------------------------------------

#reorder factor levels for gang presence change measure
panel$gang_present_cat <- factor(panel$gang_present_cat)
panel$gang_present_cat <- factor(panel$gang_present_cat, levels = levels(panel$gang_present_cat)[c(1, 3, 4, 2, 5, 6, 7)])

#reorder factor levels for largest gang in tract measure
panel$main_biggest[is.na(panel$main_biggest)] <- "NO GANG"
panel$main_biggest <- factor(panel$main_biggest)
panel$main_biggest <- factor(panel$main_biggest, levels = levels(panel$main_biggest)[c(1:5, 7:9, 6)])

## convert to better projection for print
panel <- st_transform(panel, "ESRI:102003")

## Map theme
theme_map <- function(...) {
  default_font_color = "Black"
  default_background_color = "White"
  
  theme_minimal() +
    theme(
      text = element_text(color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      
      legend.position = "bottom",
      legend.key.width = unit(.75, "inch"),
      legend.key.height = unit(.2, "inch"), 
      legend.title = element_text(hjust = -.75, vjust = .95),
      legend.text = element_text(angle = 0, hjust = 0, vjust = 1),
      
      ...
    )
}

#choropleth for gang presence in blkgrp at t
ggplot(panel, aes(fill = gang_present)) +
  facet_wrap(~ YEAR) +
  geom_sf(data = streets, fill = NA) +
  geom_sf(lwd = 0.025, color = NA, alpha = .8) +
  scale_fill_manual(values = c("grey80", "royalblue4")) +
  theme_map() +
  theme(strip.text = element_text(size = 20)) +
  labs(fill = "Gang Present at t") +
  ggsave(filename = "./output/choro_gang_present_tract.pdf",
         width = 22.5, height = 10, dpi = 300)

#choropleth for gang presence in blkgrp at t
ggplot(panel %>% filter(YEAR == 2017), aes(fill = dist_to_cbd_min)) +
  geom_sf(data = streets, fill = NA) +
  geom_sf(lwd = 0.025, color = NA, alpha = .8) +
  theme_map() +
  theme(strip.text = element_text(size = 20)) +
  labs(fill = "Distance to CBD in minutes") +
  ggsave(filename = "./output/choro_dist_to_cbd_tract.pdf",
         width = 22.5, height = 10, dpi = 300)

#choropleth for total gang presence (in periods) across CPD shapefile data
ggplot(panel %>% filter(YEAR == 2017), aes(fill = sum_present)) +
  geom_sf(data = streets, fill = NA) +
  geom_sf(lwd = 0.025, color = NA, alpha = .8) +
  #scale_fill_manual(values = c("grey80", "royalblue4")) +
  theme_map() +
  theme(strip.text = element_text(size = 20)) +
  labs(fill = "Count of CPD periods gang present in") +
  ggsave(filename = "./output/choro_gang_sum_present_tract.pdf",
         width = 8, height = 10, dpi = 300)

#choropleth of categories for change over time
ggplot(panel %>% filter(YEAR == 2017), aes(fill = gang_present_cat)) +
  geom_sf(data = streets, fill = NA) +
  geom_sf(lwd = 0.025, color = NA, alpha = .8) +
  #scale_fill_manual(values = c("grey90", "grey70", RColorBrewer::brewer.pal(6, "Paired")[-c(1:2)])) +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(3, "Reds"), RColorBrewer::brewer.pal(3, "Blues"), "grey80")) +
  theme_map() +
  labs(fill = "Gang Presence") +
  guides(fill = guide_legend(ncol = 3)) +
  ggsave(filename = "./output/choro_gang_present_cat_tract.pdf",
         width = 8, height = 10, dpi = 300)

#main gang at t
ggplot(panel, aes(fill = main_biggest)) +
  facet_wrap(~ YEAR) +
  geom_sf(data = streets, fill = NA) +
  geom_sf(lwd = 0.025, color = NA, alpha = .8) +
  scale_fill_manual(values = c(scales::hue_pal()(8), "grey80")) +
  theme_map() +
  theme(strip.text = element_text(size = 20)) +
  labs(fill = "Gang") +
  ggsave(filename = "./output/choro_main_gang_largest_tract.pdf",
         width = 22.5, height = 10, dpi = 300)

#main gang at t
ggplot(panel, aes(fill = new_tif)) +
  facet_wrap(~ YEAR) +
  geom_sf(data = streets, fill = NA) +
  geom_sf(lwd = 0.025, color = NA, alpha = .8) +
  scale_fill_manual(values = c(scales::hue_pal()(8), "grey80")) +
  theme_map() +
  theme(strip.text = element_text(size = 20)) +
  labs(fill = "Gang") +
  ggsave(filename = "./output/choro_chg_tif_tract.pdf",
         width = 22.5, height = 10, dpi = 300)


