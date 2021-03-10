
#dependencies
library(tidyverse)
library(lubridate)
library(readxl)
library(haven)
library(sf)
library(ggrepel)
library(snakecase)

#set working directory
setwd("R:/Project/chi-neighborhood-change/gang-nc-analysis")

#load table of unique gang names by race/eth
unique_gangs <- read_xlsx("./input/unique_gangs.xlsx")

#load 2010 place shapefile for US, filter to Chicago's boundaries
chi_poly <- read_sf(dsn = "./input/2010 Place Polygon/US_place_2010.shp", 
                 stringsAsFactors = FALSE) %>%
  filter(NAME10 == "Chicago") %>%
  select(geometry)

#load street network shapefile
chi_streets <- read_sf(dsn = "./input/Chicago Major Streets/Major_Streets.shp",
                   stringsAsFactors = FALSE)

#load chicago neighborhood shapefile
chi_neigh <- read_sf(dsn = "./input/Chicago Neighborhoods/chi-neigh.shp",
                 stringsAsFactors = FALSE)

#load gang shapefiles for each year of interest as list of sfs
cpd_files <- Sys.glob("./input/cpd/*.shp")
cpd <- map(cpd_files, ~ st_read(., stringsAsFactors = FALSE))

#theme for choropleth / spatial vis
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

#### A. process the spatial data -----------------------------------------------

cpd_tester <- function(cpd_sf, year){
  
  #correct names based on rules
  cpd_tmp <- cpd_sf %>%
    mutate(GANG_NAME = str_replace(GANG_NAME, "12TH STREET PLAYERS", "12TH ST PLAYERS"),
           GANG_NAME = str_replace(GANG_NAME, "GET DOWN BOYS", "GETDOWN BOYS"),
           GANG_NAME = str_replace(GANG_NAME, "TWO-SIX", "TWO SIX"),
           GANG_NAME = str_replace(GANG_NAME, "YOUNG LATIN ORGANIZATION", "YLO"),
           GANG_NAME = ifelse(GANG_NAME == "BLACK P STONE", "BLACK P STONES", GANG_NAME),
           GANG_NAME = str_replace(GANG_NAME, "YLO DISCIPLE[S]?", "YLO DISCIPLES"),
           GANG_NAME = str_replace(str_trim(GANG_NAME),"\\s+", " "),
           year = parse_number(year))

  #drop existing area/length in cpd
  cpd_tmp <- cpd_tmp %>% 
    select(-starts_with("SHAPE"), -OBJECTID) %>%
    rename_all(to_snake_case)
  
  cpd_tmp
}

#identify year for each cpd file
cpd_years <- str_extract(cpd_files, pattern = "[0-9]{4}")

#pass each cpd sf through the function with its respective year
cpd_gang <- map2(cpd, cpd_years, cpd_tester)

#now convert list to sf
cpd_gang <- reduce(cpd_gang, bind_rows)

#remove missing values for gang name
cpd_gang <- filter(cpd_gang, !is.na(gang_name), !str_detect(gang_name, "&"))

#join the codes that John wrote for gang race/ethnicity
cpd_gang <- left_join(cpd_gang, unique_gangs %>% select(gang_name = `Corrected Name`, 
                                                        race_eth = `Race/Ethnicity`))

#transform our supporting spatial data to ft-based IL projection used by CPD
chi_poly <- st_transform(chi_poly, st_crs(cpd_gang))
chi_neigh <- st_transform(chi_neigh, st_crs(cpd_gang))

#create raster grid
cpd_grid <- st_as_sf(st_make_grid(chi_poly, cellsize = 400, square = FALSE))
cpd_grid$raster_id <- 1:nrow(cpd_grid)
cpd_grid <- rename(cpd_grid, geometry = x)

#identify raster cells whose centroid is within city limits of Chicago
cpd_sample <- st_join(st_centroid(cpd_grid), chi_poly, left = FALSE) %>% 
  select(raster_id) %>% st_drop_geometry()

#filter grid to only these cells
cpd_grid <- cpd_grid %>% filter(raster_id %in% cpd_sample$raster_id)

#check work
ggplot() +
  geom_sf(data = cpd_grid) +
  geom_sf(data = cpd_gang) +
  theme_map() +
  ggsave(filename = "./output/raster_map_demo.png",
         width = 7, height = 10, dpi = 300)
  
cpd_gang <- st_join(cpd_grid, cpd_gang)
cpd_gang <- filter(cpd_gang, !is.na(gang_name))
cpd_gang <- cpd_gang %>%
  arrange(raster_id, year) %>%
  group_by(raster_id) %>%
  filter(gang_name == max(gang_name)) %>%
  mutate(last_year = max(year), 
         years = list(year),
         n_years = length(unique(year)),
         present_2018 = last_year == 2018) %>%
  ungroup()

most_recent_seq <- cpd_gang %>%
  st_drop_geometry() %>%
  arrange(raster_id, year) %>%
  group_by(raster_id) %>%
  # create an identifier for the start of the sequence
  mutate(seq_id = ifelse(row_number() == 1 | year - lag(year) > 1,
                         letters[row_number()], NA)) %>%
  fill(seq_id) %>%
  # add another grouping level (sequence identifier)
  group_by(raster_id) %>%
  # only keep most recent year of most recent sequence
  filter(seq_id == max(seq_id)) %>%
  mutate(seq_length = length(unique(year))) %>%
  filter(year == max(year)) %>%
  select(raster_id, seq_length, seq_end_year = year)


#### B. visualize the resulting raster data ------------------------------------

cpd_sum <- cpd_gang %>%
  st_drop_geometry %>%
  distinct(raster_id, last_year, n_years, present_2018)%>%
  right_join(cpd_grid) %>%
  st_as_sf() 
 
cpd_sum <- left_join(cpd_sum, most_recent_seq)
cpd_sum <- cpd_sum %>% 
  mutate(year_lost = ifelse(!present_2018 & !is.na(seq_end_year), seq_end_year, NA))


cpd_sum <- cpd_sum %>%
  mutate(gang_cat = case_when(
    is.na(last_year) ~ "Never",
    last_year < 2018 ~ "Not Gang Turf Anymore",
    last_year == 2018 ~ "Current Gang Turf"),
    n_years = ifelse(is.na(n_years), 0, n_years))

ggplot(cpd_sum, aes(fill = last_year == 2018)) +
  geom_sf(color = NA) +
  theme_map() +
  ggsave("./output/test_map_year.png", 
         width = 7, height = 10, dpi = 300)

ggplot(cpd_sum, aes(fill = n_years)) +
  geom_sf(color = NA) +
  theme_map() +
  scale_fill_viridis_c(breaks = c(0, 4, 7, 10, 14)) +
  labs(fill = "N Years Gang Observed") +
  ggsave("./output/test_map_n_years.png", 
         width = 7, height = 10, dpi = 300)

ggplot(cpd_sum, aes(fill = gang_cat)) +
  geom_sf(color = NA) +
  theme_map() +
  scale_fill_viridis_d() +
  ggsave("./output/test_map_gang_cat.png", 
         width = 7, height = 10, dpi = 300)

ggplot(cpd_sum, aes(fill = seq_end_year)) +
  geom_sf(color = NA) +
  theme_map() +
  scale_fill_viridis_c() +
  ggsave("./output/test_map_seq_end_year.png", 
         width = 7, height = 10, dpi = 300)

ggplot(cpd_sum, aes(fill = last_year)) +
  geom_sf(color = NA) +
  theme_map() +
  scale_fill_viridis_c() +
  ggsave("./output/test_map_last_year.png", 
         width = 7, height = 10, dpi = 300)

ggplot(cpd_sum, aes(fill = year_lost)) +
  geom_sf(color = NA) +
  theme_map() +
  scale_fill_viridis_c() +
  ggsave("./output/test_map_seq_year_lost.png", 
         width = 7, height = 10, dpi = 300)

#cpd_sum_nb <- poly2nb(cpd_sum, row.names = cpd_sum$raster_id)
#cpd_sum <- subset(cpd_sum, card(cpd_sum_nb) > 0)
#cpd_sum_nb <- subset(cpd_sum_nb, card(cpd_sum_nb) > 0)
#cpd_sum_listw <- nb2listw(cpd_sum_nb)
#local_moran <- localmoran(cpd_sum$year_lost, cpd_sum_listw)
#summary(local_moran)
#cpd_sum <- bind_cols(cpd_sum, as.data.frame(local_moran))

#ggplot(cpd_sum, aes(fill = Z.Ii)) +
#  geom_sf(color = NA) +
#  theme_map() +
#  scale_fill_viridis_c() +
#  ggsave("./output/test_map_local_moran.png", 
#         width = 7, height = 10, dpi = 300)



#number of unique gangs per year
cpd_gang %>%  
  st_drop_geometry() %>%
  filter(!is.na(race_eth)) %>%
  mutate(race_eth = to_snake_case(race_eth)) %>%
  group_by(year, race_eth) %>%
  summarize(n_gangs = length(unique(gang_name))) %>%
  pivot_wider(names_from = race_eth, values_from = n_gangs)

#list of gangs that no longer exist in 2018
cpd_gang %>% 
  group_by(gang_name, year) %>%
  mutate(tot_area = sum(st_area(geometry))) %>%
  ungroup() %>%
  group_by(gang_name) %>%
  mutate(avg_area_sq_mi = as.numeric(mean(tot_area)/5280^2)) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  filter(!gang_name %in% gang_name[year == 2018]) %>%
  distinct(gang_name, race_eth, avg_area_sq_mi) %>%
  arrange(race_eth, gang_name) %>%
  as.data.frame()

#list of gangs that exist across panel w/race_eth and avg size across years
cpd_gang %>%
  group_by(gang_name, year) %>%
  mutate(tot_area = sum(st_area(geometry))) %>%
  ungroup() %>%
  group_by(gang_name) %>%
  mutate(avg_area_sq_mi = as.numeric(mean(tot_area)/5280^2)) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  filter(n_years == 14) %>% 
  distinct(gang_name, race_eth, avg_area_sq_mi) %>%
  arrange(race_eth, gang_name) %>%
  as.data.frame() %>%
  print(row.names = FALSE) 



#### Models of possible outcomes beyond neighborhood change -------------------
cpd_gang_sum <- cpd_gang %>% 
  group_by(gang_name, race_eth, year) %>%
  summarize(tot_area = as.numeric(sum(st_area(geometry)))) %>%
  ungroup() %>%
  mutate(year_fct = factor(year))

cpd_gang_sum <- cpd_gang_sum %>%
  group_by(gang_name, race_eth, year) %>%
  mutate(tot_sets = length(geometry[[1]])) %>%
  ungroup()

#models for testing

test_n_sets <- cpd_gang_sum %>%
  st_drop_geometry() %>%
  filter(!is.na(race_eth)) %>%
  group_by(year, race_eth) %>%
  summarize(avg_n_sets = mean(tot_sets)) %>%
  group_by(race_eth) %>%
  mutate(chg_avg_n_sets = avg_n_sets - avg_n_sets[year==2004])

ggplot(test_n_sets, aes(x = year, y = chg_avg_n_sets, color = race_eth)) +
  facet_wrap(~ race_eth) +
  geom_line() +
  guides(color = FALSE) + 
  ggsave(filename = "./output/change_n_sets.png",
         width = 4, height = 3, dpi = 300)








sets_pred <- visreg::visreg(sets_model, xvar = "year", scale = "response", plot = FALSE)$fit
sets_pred$model <- "Number of Sets"
area_pred <- visreg::visreg(area_model, xvar = "year", scale = "response", plot = FALSE)$fit
area_pred$model <- "Total Area"

pred <- bind_rows(sets_pred, area_pred)
pred$year <-  type.convert(pred$year)

ggplot(pred, aes(x = year, y = visregFit,
                      ymin = visregLwr, ymax = visregUpr)) +
  facet_wrap(~ model, scales = "free") +
  geom_ribbon(color = NA, alpha = .25) +
  geom_line()



