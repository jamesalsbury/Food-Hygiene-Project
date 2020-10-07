library(tidyverse) ; library(httr) ; library(jsonlite) ; library(leaflet) ; library(geojsonR) ; library(geojsonio) ; library(sp)

Eng_Wal_NI_data <-  readRDS(file="/Users/jamesalsbury/Eng_Wal_NI_data.rds")
NE_data_only <- readRDS(file="/Users/jamesalsbury/NE_data_only.rds")
NE_data_only_rating <- readRDS(file="/Users/jamesalsbury/NE_data_only_rating.rds")
NE_postcodes <- geojson_read("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/NE.geojson",  what = "sp")

mean_ratings <- NE_data_only_rating %>% 
  group_by(Start_postcode) %>% 
  summarise(mean=mean(rating))


merged_data <- merge(NE_postcodes, mean_ratings, by.x="name", by.y="Start_postcode")
pal_sb <- colorNumeric("viridis", domain=merged_data$mean)


leaflet() %>%
  setView(lng = -2, lat = 55.25, zoom =8) %>% 
  addTiles()  %>% 
  addPolygons(data=merged_data,
              fillColor = ~pal_sb(merged_data$mean),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7) %>% 
  addLegend(pal = pal_sb, 
            values = merged_data$mean, 
            position = "bottomright", 
            title = "Mean hygiene rating")
