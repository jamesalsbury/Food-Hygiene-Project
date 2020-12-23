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



count=0
#withinhund <- vector(nrow(newcastle_box), mode="numeric")

for (i in 1834:nrow(newcastle_box)){
  for (j in 1:nrow(newcastle_box)){
    if (i!=j){
      calcdist <- sqrt((newcastle_box[i,]$lat - newcastle_box[j,]$lat)^2+(newcastle_box[i,]$long - newcastle_box[j,]$long)^2)
      if (calcdist<fivehundredmetres){
        count=count+1
      }
    }
  }
  withinhund[i] = count
  count=0
}
saveRDS(withinhund, file="data/Withinhund.rds")
savewithinhund

newcastle_box <- newcastle_box %>%
  add_column(withinhund)


bins <- c(0,10, 20, 30, 40, 50)
pal <- colorBin("YlOrRd", domain = newcastle_box$withinhund, bins = bins)
mytext <- paste0(
  "Name: ", newcastle_box$name,"<br/>",
  "Restuarants within 100m:", round(newcastle_box$withinhund, 2)) %>%
  lapply(htmltools::HTML)

leaflet(newcastle_box) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addCircles(color = ~pal(newcastle_box$withinhund), label = mytext) %>%
  addLegend(pal = pal,
            values = newcastle_box$withinhund,
            position = "bottomright",
            title = "Number of establishments on road")

newcastle_box %>%
  filter(withinhund<75)%>%
  arrange(desc(withinhund))

pac <- newcastle_box %>%
  filter(name == "Pacific Cafe Bar")

zap <- newcastle_box %>%
 filter(postcode=="NE1 8JW")

sqrt((pac$long - zap$long)^2+(pac$lat - zap$lat)^2)

hundredmetres

sqrt((54.97364 - 55.017754)^2+(-1.65090 - -1.539491)^2)/866

bedford <- establishment_dep_merged %>%
  filter(postcode=="BS7 0AB")
bedford <- my_establishment_data %>%
  filter(postcode=="BS7 9RE")
bedford$name


