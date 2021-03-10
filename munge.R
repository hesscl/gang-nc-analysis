#### Assemble panel of Chi neighborhoods --------------------------------------
 
#dependencies
library(tidyverse)
library(lubridate)
library(vroom)
library(readxl)
library(haven)
library(sf)
library(ggrepel)
library(lmtest)
library(sandwich)
library(snakecase)

#set working directory
setwd("R:/Project/chi-neighborhood-change/gang-nc-analysis")

#load table of unique gang names by race/eth
unique_gangs <- read_xlsx("./input/unique_gangs.xlsx")

#load neighborhood change database, immediately filter to Cook County, Illinois
ncdb <- read_dta("./input/NCDB_1970_2010.dta") %>%
  filter(STATE == 17, COUNTY == 31)

#load 2013-2017 ACS data
acs <- read_csv("./input/ACS Tract Tables/nhgis0126_ds233_20175_2017_tract.csv") %>%
  filter(STATEA == "17", COUNTYA == "031")

#load tract shapefile
tract <- read_sf(dsn = "./input/US_tract_2010/US_tract_2010.shp",
                 stringsAsFactors = FALSE)

#load street network shapefile
streets <- read_sf(dsn = "./input/Chicago Major Streets/Major_Streets.shp",
                   stringsAsFactors = FALSE)

#load chicago neighborhood shapefile
neigh <- read_sf(dsn = "./input/Chicago Neighborhoods/chi-neigh.shp",
                   stringsAsFactors = FALSE)

#load table of Chicago blockgroup's manhattan distances to CBD
man_dist <- vroom("./output/google_manhattan_distances.csv")

#load 2010 place shapefile for US, filter to Chicago's boundaries
place <- read_sf(dsn = "./input/2010 Place Polygon/US_place_2010.shp", 
             stringsAsFactors = FALSE) %>%
  filter(NAME10 == "Chicago") %>%
  select(geometry)

#load gang shapefiles for each year of interest as list of sfs
cpd_files <- Sys.glob("./input/cpd/*.shp")
cpd <- map(cpd_files, ~ st_read(., stringsAsFactors = FALSE))

#load and prepare TIF data
tif <- st_read(dsn = "./input/tif/boundaries.geojson") %>%
  rename(expiration_date = expirati_1, 
         approval_date = approval_1,
         repealed_date = repealed_d) %>%
  mutate_at(vars(ends_with("_date")), ~ as.Date(strptime(., "%m/%d/%Y"))) %>%
  st_transform(st_crs(tract))

#spatial inner join to filter blkgrps to Chicago
tract <- st_join(tract, place, left = FALSE)

#join manhattan distances to blkgrp sf
tract <- inner_join(tract, man_dist)

#load crime data
#crime <- vroom::vroom("./input/cpd/Crimes_-_2001_to_present_-_Map.csv") %>%
#  filter(!is.na(Longitude), !is.na(Latitude)) %>%
#  rename_all(to_snake_case) %>%
#  mutate(date = as.Date(strptime(date, "%m/%d/%Y"))) %>%
#  st_as_sf(coords = c("longitude", "latitude"), 
#           remove = FALSE) %>%
#  st_set_crs(4326) %>%
#  st_transform(st_crs(tract)) %>%
#  st_join(tract %>% select(GEOID10))


#### A. Munge sf objects to proper naming convention and structure for panel ----

#function to take cpd sf, reproject, figure out if tract intersected cpd and 
#adjudicate instances where tract has two+ zones based on area and counts
cpd_joiner <- function(cpd_sf){
  #grab the year of cpd data captured by the sf, store as str
  name <- str_remove(deparse(substitute(cpd_sf)), "cpd")
  
  #correct names based on rules
  cpd_tmp <- cpd_sf %>%
    mutate(GANG_NAME = str_replace(GANG_NAME, "12TH STREET PLAYERS", "12TH ST PLAYERS"),
           GANG_NAME = str_replace(GANG_NAME, "GET DOWN BOYS", "GETDOWN BOYS"),
           GANG_NAME = str_replace(GANG_NAME, "TWO-SIX", "TWO SIX"),
           GANG_NAME = str_replace(GANG_NAME, "YOUNG LATIN ORGANIZATION", "YLO"),
           GANG_NAME = ifelse(GANG_NAME == "BLACK P STONE", "BLACK P STONES", GANG_NAME))

  #reproject to same CRS as tract
  cpd_tmp <- st_transform(cpd_tmp, st_crs(tract))
  
  #drop existing area/length in cpd
  cpd_tmp <- cpd_tmp %>% select(-starts_with("SHAPE"))
  
  #spatial join
  cpd_tmp <- st_join(cpd_tmp, tract)
  
  #group by GEOID, create year column
  cpd_tmp <- cpd_tmp %>%
    filter(!is.na(GANG_NAME)) %>%
    mutate(YEAR = paste0("20", name),
           set_area = st_area(geometry)) %>%
    st_drop_geometry() %>%
    select(GEOID10, YEAR, GANG_NAME, set_area) %>%
    group_by(GEOID10, YEAR) %>%
    summarize(main = GANG_NAME[which(set_area == max(set_area))],
              list = list(GANG_NAME),
              count = length(unique(GANG_NAME))) %>%
    ungroup()
    
  cpd_tmp 
}

#run function on selected years of CPD data to compute measures of interest
sets <- map(cpd, cpd_joiner)
sets <- reduce(cpd, bind_rows)

#create a grid of unique tracts and years we have CPD data for
set_grid <- expand_grid(GEOID10 = unique(tract$GEOID10), 
                        YEAR = unique(sets$YEAR))

#append CPD data to the grid
sets <- left_join(set_grid, sets)

#process the grid some
sets <- sets %>% 
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  group_by(GEOID10) %>%
  mutate(sum_present = sum(count > 0),
         present_04 = count[YEAR == 2004] > 0,
         present_10 = count[YEAR == 2010] > 0,
         present_18 = count[YEAR == 2018] > 0,
         present_cat = paste(present_04, present_18),
         first_present = type.convert(first(YEAR[count > 0])),
         first_present = ifelse(is.na(first_present), Inf, first_present),
         last_present = type.convert(last(YEAR[count == 0])),
         last_present = ifelse(is.na(last_present), Inf, last_present),
         order_me = ifelse(present_04, last_present, first_present)) %>% 
  ungroup() %>%
  arrange(desc(first_present)) 

#look at whether there was a gang in the neighborhood at each CPD observation
ggplot(sets, 
       aes(x = YEAR, y = fct_reorder(GEOID10, sum_present), fill = count > 0)) +
  facet_wrap( ~ present_cat, scales = "free_y") +
  geom_tile() +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "bottom") +
  labs(x = "", fill = "Blockgroup in gang turf") +
  scale_fill_viridis_d(begin = .2, end = .8, option = "magma") +
  ggsave(filename = "./output/gang_presence_heatmap_tract.pdf",
         width = 10, height = 10, dpi = 300)

#relative frequency of gang change categories
table(sets$present_cat)/nrow(sets)


#### B. Recode NCDB and NHGIS data to proper structure and naming convention ----

#population composition
#-race/ethnicity (nh white, nh black, hsp, nh other)
#-poverty rate
#-highly educated (%25+ with college degree)

#housing composition
#-recently built (% built since 2000)
#-old housing (% built prior to 1960)
#-median rent
#-owner occupied

#spatial context
#-distance to Loop
#-distance to transit

#process 2000 measures
census_2000 <- ncdb %>%
  select(GEO2010, AREALAND, SHR0D, SHRNHW0, SHRNHB0, SHRNHA0, SHRHSP0, POVRAT0, EDUC160, EDUCPP0,
         TOTHSUN0, OCCHU0, VACHU0, OWNOCC0, RNTOCC0, TTUNIT50, TTUNT500, BLTYR000, BLTYR390, 
         BLTYR490, BLTYR590, PLMBT0, MDVALHS0, MDGRENT0, MDHHY0) %>%
  mutate(GEO2010 = as.character(GEO2010),
         tot_pop = SHR0D,
         pct_nh_wht = SHRNHW0,
         pct_nh_blk = SHRNHB0,
         pct_hsp = SHRHSP0,
         pct_oth = 1 - pct_nh_wht - pct_nh_blk - pct_hsp,
         pct_pov = POVRAT0,
         pct_col_grad = EDUC160/EDUCPP0,
         pct_vac_hu = VACHU0/TOTHSUN0,
         pct_own_occ = OWNOCC0/OCCHU0,
         pct_rnt_occ = RNTOCC0/OCCHU0,
         pct_5_plus = TTUNIT50/TOTHSUN0,
         pct_50_plus = TTUNT500/TOTHSUN0,
         pct_old_hu = (BLTYR390+BLTYR490+BLTYR590)/TOTHSUN0,
         pct_new_hu = (BLTYR000)/TOTHSUN0,
         pct_col_grad = EDUC160/EDUCPP0,
         med_rent = MDGRENT0,
         med_val = MDVALHS0,
         med_hh_inc = MDHHY0,
         pct_no_plumb = 1 - (PLMBT0/TOTHSUN0)) %>%
  select(GEO2010, tot_pop, starts_with("pct"), starts_with("med")) %>%
  rename(GEOID10 = GEO2010)

#process 2010 measures
census_2010 <- ncdb %>%
  select(GEO2010, AREALAND, SHR1D, SHRNHW1, SHRNHB1, SHRNHA1, SHRHSP1, POVRAT1A, EDUC161A, EDUCPP1A,
         TOTHSUN1, TOTHSUN1A, OCCHU1, VACHU1, OWNOCC1, RNTOCC1, TTUNIT51A, TTUNT501A, BLTYR041A, BLTYR051A, 
         BLTYR391A, BLTYR491A, BLTYR591A, PLMBT1A, MDVALHS1A, MDGRENT1A, MDHHY1A) %>%
  mutate(GEO2010 = as.character(GEO2010),
         tot_pop = SHR1D,
         pct_nh_wht = SHRNHW1,
         pct_nh_blk = SHRNHB1,
         pct_hsp = SHRHSP1,
         pct_oth = 1 - pct_nh_wht - pct_nh_blk - pct_hsp,
         pct_pov = POVRAT1A,
         pct_col_grad = EDUC161A/EDUCPP1A,
         pct_vac_hu = VACHU1/TOTHSUN1,
         pct_own_occ = OWNOCC1/OCCHU1,
         pct_rnt_occ = RNTOCC1/OCCHU1,
         pct_5_plus = TTUNIT51A/TOTHSUN1A,
         pct_50_plus = TTUNT501A/TOTHSUN1A,
         pct_old_hu = (BLTYR391A+BLTYR491A+BLTYR591A)/TOTHSUN1A,
         pct_new_hu = (BLTYR041A+BLTYR051A)/TOTHSUN1A,
         med_rent = MDGRENT1A,
         med_val = MDVALHS1A,
         med_hh_inc = MDHHY1A,
         pct_no_plumb = 1 - (PLMBT1A/TOTHSUN1A)) %>%
  select(GEO2010, tot_pop, starts_with("pct"), starts_with("med")) %>%
  rename(GEOID10 = GEO2010)

#process ACS measures
acs_2017 <- acs %>%
  mutate(GEO2010 = paste0(STATEA, COUNTYA, TRACTA),
         tot_pop = AHY1E001,
         pct_nh_wht = AHZAE003/AHZAE001,
         pct_nh_blk = AHZAE004/AHZAE001,
         pct_hsp = AHZAE012/AHZAE001,
         pct_oth = 1 - pct_nh_wht - pct_nh_blk - pct_hsp,
         pct_pov = (AH1JE002+AH1JE003)/AH1JE001,
         pct_col_grad = (AH04M022+AH04M023+AH04M024+AH04M025)/AH04M001,
         pct_vac_hu = AH36E003/AH36E001,
         pct_own_occ = AH37E002/AH37E001,
         pct_rnt_occ = AH37E003/AH37E001,
         pct_5_plus = (AH4WE006+AH4WE007+AH4WE008+AH4WE009)/AH4WE001,
         pct_50_plus = AH4WE009/AH4WE001,
         pct_old_hu = (AH4ZE009+AH4ZE010+AH4ZE011)/AH4ZE001,
         pct_new_hu = (AH4ZE002+AH4ZE003+AH4ZE004)/AH4ZE001,
         med_rent = AH5RE001,
         med_val = AH53E001,
         med_hh_inc = AH1PE001,
         pct_no_plumb = 1 - (AH5CE002/AH5CE001)) %>%
  select(GEO2010, tot_pop, starts_with("pct"), starts_with("med")) %>%
  rename(GEOID10 = GEO2010)
  

#### C. Assemble panel for analysis -------------------------------------------

#create a table for tracts whose centroids are in a TIF distirct
tif_tract <- tract %>%
  st_centroid() %>%
  st_join(tif) %>%
  mutate(tif_year = year(approval_date)) %>%
  select(GEOID10, tif_year)

#append gang set information to blkgrps for Cook County
t1 <- left_join(tract, sets04) 
t2 <- left_join(tract, sets10)
t3 <- left_join(tract, sets17)

#make time period indicator consistent across blkgrps regardless of prev
t1 <- t1 %>% mutate(YEAR = 2004)
t2 <- t2 %>% mutate(YEAR = 2010)
t3 <- t3 %>% mutate(YEAR = 2017)

#append sociodemographic indicators for each period
t1 <- inner_join(t1, census_2000)
t2 <- inner_join(t2, census_2010)
t3 <- inner_join(t3, acs_2017)

#assemble the period objects into a panel of blkgrp-periods
panel <- rbind(t1, t2, t3)

#save a table of across-CPD data presence count
sum_sets <- sets %>%
  distinct(GEOID10, sum_present)

#append to panel
panel <- left_join(panel, sum_sets)

#append TIF data to panel
panel <- left_join(panel, st_drop_geometry(tif_tract)) %>%
  mutate(tif = YEAR >= tif_year)

#code NA for count as 0
panel <- panel %>%
  mutate(YEAR = type.convert(YEAR),
         count = ifelse(is.na(count), 0, count),
         gang_present = count > 0) %>% 
  arrange(GISJOIN, YEAR) %>%
  select(GISJOIN, GEOID10, YEAR, everything())

#compute crosswalk of largest gangs by number of tracts held at t
biggest_sets <- panel %>%
  st_drop_geometry() %>%
  group_by(YEAR, main) %>%
  summarize(n = n()) %>%
  top_n(8, n) %>%
  ungroup() 

#create largest gang in tract and gang presence change measures
panel <- panel %>%
  mutate(main_biggest = ifelse(main %in% biggest_sets$main, main,
                               ifelse(!is.na(main), "SMALLER GANG", NA))) %>%
  group_by(GEOID10) %>%
  mutate(gang_present_cat = NA,
         gang_present_cat = ifelse(length(unique(gang_present)) == 1 && gang_present == TRUE, "Always Present", gang_present_cat),
         gang_present_cat = ifelse(length(unique(gang_present)) == 1  && gang_present == FALSE, "Never Present", gang_present_cat),
         gang_present_cat = ifelse(gang_present[YEAR == 2004] == FALSE & gang_present[YEAR == 2010] == TRUE & gang_present[YEAR == 2017] == TRUE, "Newly Present in 2010", gang_present_cat),
         gang_present_cat = ifelse(gang_present[YEAR == 2004] == FALSE & gang_present[YEAR == 2010] == FALSE & 
                                     gang_present[YEAR == 2017] == TRUE, "Newly Present in 2017", gang_present_cat),
         gang_present_cat = ifelse(gang_present[YEAR == 2004] == TRUE & gang_present[YEAR == 2010] == FALSE  & gang_present[YEAR == 2017] == FALSE, "Not Present as of 2010", gang_present_cat),
         gang_present_cat = ifelse(gang_present[YEAR == 2004] == TRUE & gang_present[YEAR == 2010] == TRUE & 
                                     gang_present[YEAR == 2017] == FALSE, "Not Present as of 2017", gang_present_cat),
         gang_present_cat = ifelse(is.na(gang_present_cat), "Temporary Change", gang_present_cat)) %>%
  ungroup()

#compute entropy
panel <- panel %>%
  mutate(entropy = ifelse(pct_nh_wht != 0, pct_nh_wht*log(1/pct_nh_wht), 0) +
                   ifelse(pct_nh_blk != 0, pct_nh_blk*log(1/pct_nh_blk), 0) +
                   ifelse(pct_hsp != 0, pct_hsp*log(1/pct_hsp), 0) +
                   ifelse(pct_oth != 0 & pct_oth > .00001, pct_oth*log(1/pct_oth), 0))

#compute change scores + population density
panel <- panel %>%
  group_by(GEOID10) %>%
  mutate(pop_dens = tot_pop/ALAND10,
         chg_pop_dens = pop_dens - lag(pop_dens),
         chg_tot_pop = tot_pop - lag(tot_pop),
         lag_present = lag(gang_present),
         lag_absent = !lag(gang_present),
         new_present = gang_present & !lag(gang_present),
         new_absent = !gang_present & lag(gang_present),
         new_tif = tif & !lag(tif),
         new_high_pov = pct_pov >= .20 & lag(pct_pov < .20),
         lag_pop_dens = lag(pop_dens),
         lag_pov = lag(pct_pov),
         lag_col_grad = lag(pct_col_grad),
         lag_med_hh_inc = lag(med_hh_inc),
         lag_med_rent = lag(med_rent),
         lag_med_val = lag(med_val),
         lag_new_hu = lag(pct_new_hu),
         lag_old_hu = lag(pct_old_hu),
         lag_own_occ = lag(pct_own_occ),
         lag_rnt_occ = lag(pct_rnt_occ),
         lag_vac_hu = lag(pct_vac_hu),
         lag_50_plus = lag(pct_50_plus),
         lag_no_plumb = lag(pct_no_plumb),
         lag_pct_blk = lag(pct_nh_blk),
         lag_pct_wht = lag(pct_nh_wht),
         lag_pct_hsp = lag(pct_hsp),
         lag_pct_oth = lag(pct_oth),
         chg_med_rent = med_rent - lag(med_rent),
         chg_pov = pct_pov - lag(pct_pov),
         chg_col_grad = pct_col_grad - lag(pct_col_grad),
         chg_own_occ = pct_own_occ - lag(pct_own_occ),
         chg_rnt_occ = pct_rnt_occ - lag(pct_rnt_occ),
         chg_vac_hu = pct_vac_hu - lag(pct_vac_hu),
         chg_med_hh_inc = med_hh_inc - lag(med_hh_inc),
         chg_med_val = med_val - lag(med_val),
         chg_new_hu = pct_new_hu - lag(pct_new_hu),
         chg_old_hu = pct_old_hu - lag(pct_old_hu),
         chg_50_plus = pct_50_plus - lag(pct_50_plus),
         chg_no_plumb = pct_no_plumb - lag(pct_no_plumb),
         chg_pct_blk = pct_nh_blk - lag(pct_nh_blk),
         chg_pct_wht = pct_nh_wht - lag(pct_nh_wht),
         chg_pct_hsp = pct_hsp - lag(pct_hsp),
         chg_pct_oth = pct_oth - lag(pct_oth),
         chg_entropy = entropy - lag(entropy)) %>%
  ungroup() %>%
  mutate(new_tif = ifelse(is.na(new_tif), FALSE, new_tif),
         dist_to_cbd_min = parse_number(duration_text),
         dist_to_cbd_km = distance_value / 1000)


#### D. Write to storage -------------------------------------------------------

#convert to WGs84 for storage
panel <- panel %>% st_transform(4326)

#remove prior panel file if present
file.remove("./output/chi-tract-panel.geojson")

#store data as geojson
write_sf(panel, "./output/chi-tract-panel.geojson")

