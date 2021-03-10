#### Compute manhattan distances to CBD --------------------------------------

#dependencies
library(tidyverse)
library(sf)
library(googleway)

#set working directory
setwd("R:/Project/chi-neighborhood-change/gang-nc-analysis")

#load and set 
set_key(readLines("H:/gmaps_key.txt"))


#load 2010 tract shapefile for Illinois
tract <- read_sf(dsn = "./input/US_tract_2010/US_tract_2010.shp",
                 stringsAsFactors = F)

#load 2010 place shapefile for US, filter to Chicago's boundaries
place <- read_sf(dsn = "./input/2010 Place Polygon/US_place_2010.shp", 
                 layer = "US_place_2010",
                 stringsAsFactors = F) %>%
  filter(NAME10 == "Chicago") %>%
  select(geometry)

#spatial inner join to filter tracts to Chicago
tract <- st_join(tract, place, left = FALSE)


#### A. Munge sf objects to proper naming convention and structure for panel ----

##compute manhattan distance of each centroid to Chicago city hall

#first take centroids and store coordinates
tract_cent <- st_centroid(tract$geometry)
tract_cent <- st_transform(tract_cent, crs = 4326)
tract_cent <- st_coordinates(tract_cent)

#we'll store the query results in a data.frame with n rows as tracts in chi
results <- data.frame(
  origin_coord_lat = rep(NA, nrow(tract_cent)),
  origin_coord_lng = rep(NA, nrow(tract_cent)),
  destination_addresses = rep(NA, nrow(tract_cent)),
  origin_addresses = rep(NA, nrow(tract_cent)),
  status = rep(NA, nrow(tract_cent)),
  distance_text = rep(NA, nrow(tract_cent)),
  distance_value = rep(NA, nrow(tract_cent)),
  duration_text = rep(NA, nrow(tract_cent)),
  duration_value = rep(NA, nrow(tract_cent))
)

#submit API queries to google to look up distances based on driving street network
for(i in 1:nrow(tract_cent)){
  result <- google_distance(origins = tract_cent[i, c(2, 1)],
                            destination = c(41.884023, -87.631475),
                            mode = "driving",
                            simplify = TRUE)
  
  results[i,] <- c(tract_cent[i, c(2)],
                   tract_cent[i, c(1)],
                   result$destination_addresses,
                   result$origin_addresses,
                   result$status,
                   result$rows$elements[[1]]$distance$text,
                   result$rows$elements[[1]]$distance$value,
                   result$rows$elements[[1]]$duration$text,
                   result$rows$elements[[1]]$duration$value)
}

#append the GISJOIN key for each blockgroup
results <- bind_cols(select(tract, GISJOIN), results)

#drop geometry
results <- st_drop_geometry(results)

#save to disk
write_csv(results, "./output/google_manhattan_distances.csv")
