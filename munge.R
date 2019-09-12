#### Assemble panel of Chi neighborhoods --------------------------------------

#dependencies
library(tidyverse)
library(readxl)
library(sf)

#set working directory
setwd("R:/Project/chi-neighborhood-change/analysis")

#load table of unique gang names by race/eth
unique_gangs <- read_xlsx("./input/unique_gangs.xlsx")

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
cpd18 <- st_read(dsn = "./input/cpd/gang2018.shp", layer = "gang2018")

#spatial inner join to filter blkgrps to Chicago
blkgrp <- st_join(blkgrp, place, left = FALSE)


#### A. Munge objects to proper naming convention and structure for panel -----

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
sets18 <- cpd_joiner(cpd18)


#### B. Assemble panel for analysis -------------------------------------------

#append gang set information to blkgrps for Cook County
t1 <- left_join(blkgrp, sets04) 
t2 <- left_join(blkgrp, sets10)
t3 <- left_join(blkgrp, sets18)

#make time period indicator consistent across blkgrps regardless of prev
t1 <- t1 %>% mutate(YEAR = "2004")
t2 <- t2 %>% mutate(YEAR = "2010")
t3 <- t3 %>% mutate(YEAR = "2018")

#assemble the period objects into a panel of blkgrp-periods
panel <- rbind(t1, t2, t3)

#code NA for count as 0
panel <- panel %>%
  mutate(YEAR = type.convert(YEAR),
         count = ifelse(is.na(count), 0, count),
         gang_present = count > 0)


#### C. Descriptive analysis --------------------------------------------------

#basic choropleths for gang presence in blkgrp
ggplot(panel, aes(fill = gang_present)) +
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
