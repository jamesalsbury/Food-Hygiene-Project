library(sf)
library(sp)
library(tidyverse)
library(mapview)
library(leaflet)
library(maptools)
library(rgeos)
library(rgdal)

full_postcode_dep_data <- readRDS("data/full_postcode_dep_data.rds")
my_establishment_data <- readRDS("data/my_establishment_data.rds")

establishment_dep_merged <- merge(my_establishment_data, full_postcode_dep_data, by.x = "postcode", by.y = "pcds")

establishment_dep_merged  <- establishment_dep_merged %>%
  filter(rating %in% 1:5)

establishment_dep_merged[,12] <- as.numeric(as.character(establishment_dep_merged[,12]))

for (i in 33:83){
  establishment_dep_merged[,i] <- as.numeric(as.character(establishment_dep_merged[,i]))
}

newcastle_box <- readRDS("data/Newcastle_points.rds")

newcastle_roads <- readRDS("data/Newcastle_roads.rds")

box_sp <- SpatialPoints(coords = c(newcastle_box[10], newcastle_box[11]))


new1 <- st_sfc(newcastle_roads$geometry, crs = "+proj=longlat +datum=WGS84")
new1 <- as(newcastle_roads, "Spatial")
combined <- snapPointsToLines(box_sp, new1, idField = "Fid")

road_count <- combined@data %>%
  count(nearest_line_id)


merged_snapped <- merge(new1@data, road_count, by.x = "Fid", by.y = "nearest_line_id")

n <- vector(length=nrow(newcastle_roads), mode="numeric")

newcastle_roads <- newcastle_roads %>%
  add_column(n)


merged_snapped <- st_join(newcastle_roads, merged_snapped)


