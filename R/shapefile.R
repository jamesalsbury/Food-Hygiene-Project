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
roads_as_spatial <- as(newcastle_roads, "Spatial")
combined <- snapPointsToLines(box_sp, roads_as_spatial, idField = "Fid")

road_count <- combined@data %>%
  count(nearest_line_id)

merged_snapped <- merge(roads_as_spatial@data, road_count, by.x = "Fid", by.y = "nearest_line_id")
merged_snapped <-  merged_snapped[,-(2:5)]
merged_snapped <- left_join(newcastle_roads, merged_snapped)
merged_snapped$n[is.na(merged_snapped$n)] <- 0



bins <- c(0,2, 4, 6, 8, 10, 100)
pal <- colorBin("YlOrRd", domain = merged_snapped$n, bins = bins)

leaflet(merged_snapped) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolylines(color = ~pal(merged_snapped$n)) %>%
  addLegend(pal = pal,
            values = merged_snapped$n,
            position = "bottomright",
            title = "Number of establishments on road")

mapview(newcastle_roads)




