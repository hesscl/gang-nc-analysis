#### Assemble panel of Chi neighborhoods --------------------------------------

#dependencies
library(tidyverse)
library(vroom)
library(readxl)
library(sf)

#set working directory
setwd("R:/Project/chi-neighborhood-change/gang-nc-analysis")

#load table of unique gang names by race/eth
unique_gangs <- read_xlsx("./input/unique_gangs.xlsx")

#load time-series table of normalized blockgroup estimates for 2000-2010
dec_tbl <- vroom("./input/2000-2010 Blockgroup Tables/nhgis0120_ts_geog2010_blck_grp.csv") %>%
  filter(STATE == "Illinois", COUNTY == "Cook County",
         DATAYEAR %in% c(2000, 2010)) %>%
  rename(YEAR=DATAYEAR) %>%
  mutate(YEAR = ifelse(YEAR == 2000, 2004, YEAR))

#load table of 2013-2017 ACS blockgroup
acs_tbl <- vroom("./input/2017 Blockgroup Tables/nhgis0119_ds233_20175_2017_blck_grp.csv") %>%
  filter(COUNTY == "Cook County") %>%
  mutate(YEAR = 2017)
 
#load 2010 blkgrp shapefile for Illinois
blkgrp <- read_sf(dsn = "./input/2010 Blockgroup Polygon/IL_blck_grp_2010.shp", 
                 layer = "IL_blck_grp_2010",
                 stringsAsFactors = F)

#load 2010 place shapefile for US, filter to Chicago's boundaries
place <- read_sf(dsn = "./input/2010 Place Polygon/US_place_2010.shp", 
             layer = "US_place_2010",
             stringsAsFactors = F) %>%
  filter(NAME10 == "Chicago") %>%
  select(geometry)

#load gang shapefiles for each year of interest
cpd04 <- st_read(dsn = "./input/cpd/gang2004.shp", layer = "gang2004")
cpd10 <- st_read(dsn = "./input/cpd/gang2010.shp", layer = "gang2010")
cpd17 <- st_read(dsn = "./input/cpd/gang2017.shp", layer = "gang2017")

#spatial inner join to filter blkgrps to Chicago
blkgrp <- st_join(blkgrp, place, left = FALSE)


#### A. Munge sf objects to proper naming convention and structure for panel ----

#function to take cpd sf, reproject, figure out if blkgrp intersected cpd and 
#adjudicate instances where blkgrp has two+ zones based on area and counts
cpd_joiner <- function(cpd_sf){
  #grab the year of cpd data captured by the sf, store as str
  name <- str_remove(deparse(substitute(cpd_sf)), "cpd")
  
  #correct names based on rules
  cpd_tmp <- cpd_sf %>%
    mutate(GANG_NAME = str_replace(GANG_NAME, "12TH STREET PLAYERS", "12TH ST PLAYERS"),
           GANG_NAME = str_replace(GANG_NAME, "GET DOWN BOYS", "GETDOWN BOYS"),
           GANG_NAME = str_replace(GANG_NAME, "TWO-SIX", "TWO SIX"),
           GANG_NAME = str_replace(GANG_NAME, "YOUNG LATIN ORGANIZATION", "YLO"))

  #reproject to same CRS as blkgrp
  cpd_tmp <- st_transform(cpd_tmp, st_crs(blkgrp))
  
  #drop existing area/length in cpd
  cpd_tmp <- cpd_tmp %>% select(-starts_with("SHAPE"))
  
  #spatial join
  cpd_tmp <- st_join(cpd_tmp, blkgrp)
  
  #group by GEOID, create year column
  cpd_tmp <- cpd_tmp %>%
    filter(!is.na(GANG_NAME)) %>%
    mutate(YEAR = paste0("20", name),
           AREA = st_area(geometry)) %>%
    st_drop_geometry() %>%
    select(GEOID10, YEAR, GANG_NAME, AREA) %>%
    group_by(GEOID10, YEAR) %>%
    summarize(main = GANG_NAME[which(AREA == max(AREA))],
              list = list(GANG_NAME),
              count = length(unique(GANG_NAME)))
    
  cpd_tmp 
}

#run function on selected years of CPD data to compute measures of interest
sets04 <- cpd_joiner(cpd04)
sets10 <- cpd_joiner(cpd10)
sets17 <- cpd_joiner(cpd17)


#### B. Recode NHGIS data to proper structure and naming convention -----------

dec_tbl <- dec_tbl %>%
  mutate(tot_pop = CL8AA,
         tot_nh_wht = CW7AA,
         tot_nh_blk = CW7AB,
         tot_nh_aina = CW7AC,
         tot_nh_api = CW7AD,
         tot_nh_oth = CW7AE+CW7AF,
         tot_hsp = CW7AG+CW7AH+CW7AI+CW7AJ+CW7AK+CW7AL,
         tot_hu = CM7AA,
         tot_occ_hu = CM9AA,
         tot_vac_hu = CM9AB,
         tot_own_occ_hu = CN1AA,
         tot_rent_occ_hu = CN1AB) %>%
  select(GISJOIN, YEAR, starts_with("tot"))

acs_tbl <- acs_tbl %>%
  mutate(tot_pop = AHYQE001,
         tot_nh_wht = AHZAE003,
         tot_nh_blk = AHZAE004,
         tot_nh_aina = AHZAE005,
         tot_nh_api = AHZAE006+AHZAE007,
         tot_nh_oth = AHZAE008+AHZAE009,
         tot_hsp = AHZAE012,
         tot_hu = AH36E001,
         tot_occ_hu = AH36E002,
         tot_vac_hu = AH36E003,
         tot_own_occ_hu = AH37E002,
         tot_rent_occ_hu = AH37E003) %>%
  select(GISJOIN, YEAR, starts_with("tot"))


#### C. Assemble panel for analysis -------------------------------------------

#append gang set information to blkgrps for Cook County
t1 <- left_join(blkgrp, sets04) 
t2 <- left_join(blkgrp, sets10)
t3 <- left_join(blkgrp, sets17)

#make time period indicator consistent across blkgrps regardless of prev
t1 <- t1 %>% mutate(YEAR = 2004)
t2 <- t2 %>% mutate(YEAR = 2010)
t3 <- t3 %>% mutate(YEAR = 2017)

#append sociodemographic indicators for each period
t1 <- inner_join(t1, dec_tbl)
t2 <- inner_join(t2, dec_tbl)
t3 <- inner_join(t3, acs_tbl)

#assemble the period objects into a panel of blkgrp-periods
panel <- rbind(t1, t2, t3)

#code NA for count as 0
panel <- panel %>%
  mutate(YEAR = type.convert(YEAR),
         count = ifelse(is.na(count), 0, count),
         gang_present = count > 0) %>% 
  arrange(GISJOIN, YEAR) %>%
  group_by(GISJOIN) %>%
  mutate(newly_present = gang_present & !lag(gang_present),
         newly_absent = !gang_present & lag(gang_present),
         still_present = gang_present & lag(gang_present),
         still_absent = !gang_present & !lag(gang_present))

#add some composition measures
panel <- panel %>%
  mutate(pct_nh_wht = tot_nh_wht/tot_pop,
         pct_nh_blk = tot_nh_blk/tot_pop,
         pct_nh_api = tot_nh_api/tot_pop,
         pct_hsp = tot_hsp/tot_pop,
         pct_own_occ = tot_own_occ_hu/tot_hu)

#compute change score measures
panel <- panel %>%
  arrange(GISJOIN, YEAR) %>%
  group_by(GISJOIN) %>%
  mutate(chg_nh_wht = pct_nh_wht - lag(pct_nh_wht),
         chg_nh_blk = pct_nh_blk - lag(pct_nh_blk),
         chg_nh_api = pct_nh_api - lag(pct_nh_api),
         chg_hsp = pct_hsp - lag(pct_hsp)) %>%
  ungroup() %>%
  filter(YEAR %in% c(2010, 2017))


#### C. Descriptive analysis --------------------------------------------------

#correlation of change in race/eth comp with change to new gang presence
cor.test(as.numeric(panel$newly_present), panel$chg_nh_wht)
cor.test(as.numeric(panel$newly_present), panel$chg_nh_blk)
cor.test(as.numeric(panel$newly_present), panel$chg_hsp)

#correlation of change in race/eth comp with change to new gang absence
cor.test(as.numeric(panel$newly_absent), panel$chg_nh_wht)
cor.test(as.numeric(panel$newly_absent), panel$chg_nh_blk)
cor.test(as.numeric(panel$newly_absent), panel$chg_hsp)

#basic choropleths for gang presence in blkgrp; change in % white, % black, % latino
ggplot(panel, aes(fill = gang_present)) +
  facet_wrap(~ YEAR) +
  geom_sf(lwd = 0.025, color = "grey90")

ggplot(panel, aes(fill = chg_nh_wht)) +
  facet_wrap(~ YEAR) +
  geom_sf(lwd = 0.025, color = "grey90")

ggplot(panel, aes(fill = chg_nh_blk)) +
  facet_wrap(~ YEAR) +
  geom_sf(lwd = 0.025, color = "grey90")

ggplot(panel, aes(fill = chg_hsp)) +
  facet_wrap(~ YEAR) +
  geom_sf(lwd = 0.025, color = "grey90")


#blkgrps where gang presence changed between 2004 and 2018
panel %>% 
  group_by(GEOID10) %>%
  filter(gang_present[YEAR==2018] != gang_present[YEAR==2004],
         YEAR == 2018) %>%
  distinct(GEOID10) %>%
  plot()

#blkgrps where gang presence changed between 2004 and 2018, by current pres
panel %>% 
  group_by(GEOID10) %>%
  filter(gang_present[YEAR==2018] != gang_present[YEAR==2004],
         YEAR == 2018) %>%
  ungroup %>%
  select(gang_present) %>%
  plot()


#### D. Models ----------------------------------------------------------------

logit_1 <- glm(gang_present ~ tot_pop + pct_nh_wht + pct_nh_blk + pct_hsp +
                 chg_nh_wht + chg_nh_blk + chg_hsp + pct_own_occ, 
               panel, family = "binomial")
summary(logit_1)
