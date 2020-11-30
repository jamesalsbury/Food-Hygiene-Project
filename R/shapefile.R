library(sf)
library(sp)
library(tidyverse)
library(mapview)
library(leaflet)
library(maptools)

full_postcode_dep_data <- readRDS("data/full_postcode_dep_data.rds")
my_establishment_data <- readRDS("data/my_establishment_data.rds")

establishment_dep_merged <- merge(my_establishment_data, full_postcode_dep_data, by.x = "postcode", by.y = "pcds")

establishment_dep_merged  <- establishment_dep_merged %>%
  filter(rating %in% 1:5)

establishment_dep_merged[,12] <- as.numeric(as.character(establishment_dep_merged[,12]))

for (i in 33:83){
  establishment_dep_merged[,i] <- as.numeric(as.character(establishment_dep_merged[,i]))
}


newcastle_boundary <- sf::read_sf("/Users/jamesalsbury/Downloads/Download_VectorTunnel_1626336/vml_3786818/nz/vml-nz26nw_Road_Centreline.shp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')


newcastle_boundary1 <- sf::read_sf("/Users/jamesalsbury/Downloads/Download_VectorTunnel_1626336/vml_3786818/nz/vml-nz26ne_Road_Centreline.shp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

newcastle_boundary2 <- sf::read_sf("/Users/jamesalsbury/Downloads/Download_VectorTunnel_1626336/vml_3786818/nz/vml-nz26se_Road_Centreline.shp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

newcastle_boundary3 <- sf::read_sf("/Users/jamesalsbury/Downloads/Download_VectorTunnel_1626336/vml_3786818/nz/vml-nz26sw_Road_Centreline.shp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

newcastle_box <- establishment_dep_merged %>%
  filter(long>-1.69321) %>%
  filter(long < -1.53239) %>%
  filter(lat > 54.93259) %>%
  filter(lat < 55.02451)

box_sp <- SpatialPoints(coords = c(newcastle_box[10], newcastle_box[11]))

new <- rbind(newcastle_boundary, newcastle_boundary1)
new <- rbind(new, newcastle_boundary2)
new <- rbind(new, newcastle_boundary3)
new1 <- st_sfc(new$geometry, crs = "+proj=longlat +datum=WGS84")
new1 <- as(new1, "Spatial")
combined <- snapPointsToLines(box_sp, new1)


mapview(new)
road_count <- combined@data %>%
  count(nearest_line_id)

merged_count <- merge(new, road_count, by.x = )

?merge()

road_count

new %>%
  filter(FeatCode=="ID18588")


mycount <- combined@data %>%
  count(nearest_line_id)


leaflet() %>%
  addTiles() %>%
  addMarkers(lng = combined@coords[,1], lat = combined@coords[,2]) %>%
  leaflet(new1) %>%
  addPolylines()

leaflet(new1) %>%
  addTiles() %>%
  addPolylines() %>%
  addMarkers(lng = combined@coords[,1], lat = combined@coords[,2])


class(new1@lines)


mycount$nearest_line_id %>%
  arrange()
