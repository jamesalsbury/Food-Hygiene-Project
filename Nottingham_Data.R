library(tidyverse) ; library(httr) ; library(jsonlite) ; library(leaflet) ; library(geojsonR) ; library(geojsonio) ; library(sp) ; library(magrittr) ; library(dplyr)


NG_data_only <- Eng_Wal_NI_data %>% 
  filter(substr(postcode, 1, 2)=="NG")

start_postcode <- vector(mode="character", length=nrow(NG_data_only))

NG_data_only <- NG_data_only %>% 
  add_column(start_postcode)

NG_data_only_rating <-  NG_data_only %>% 
  filter(rating==0 | rating==1 | rating==2 | rating==3 | rating==4 | rating==5)

for (i in 1:nrow(NG_data_only)) {
  if (!is.na(NG_data_only$postcode[i])) {
    if (nchar(NG_data_only$postcode[i])==7){
      NG_data_only$start_postcode[i] = substr(NG_data_only$postcode[i], 1, 3)
    }
    if (nchar(NG_data_only$postcode[i])==8){
      NG_data_only$start_postcode[i] = substr(NG_data_only$postcode[i], 1, 4)
    }
  }
}


NG_data_only_rating$rating = as.numeric(as.character(NG_data_only_rating$rating))

mean_ratings_NG <- NG_data_only_rating %>% 
  group_by(start_postcode) %>% 
  summarise(mean=mean(rating))



NG_postcodes <- geojson_read("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/NG.geojson",  what = "sp")


merged_data_NG <- merge(NG_postcodes, mean_ratings_NG, by.x="name", by.y="start_postcode")
pal_sb <- colorNumeric("viridis", domain=merged_data_NG$mean)

Eng_Wal_NI_data

leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom =8) %>% 
  addTiles()  %>% 
  addPolygons(data=merged_data_NG,
              fillColor = ~pal_sb(merged_data_NG$mean),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7) %>% 
  addLegend(pal = pal_sb, 
            values = merged_data_NG$mean, 
            position = "bottomright", 
            title = "Mean hygiene rating")


mytext <- paste(
  "Area: ", All_postcodes_merged@data$name,"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

All_UK_postcodes
leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom =8) %>% 
  addTiles()  %>% 
  addPolygons(data=all,
              weight = 5,
              opacity = 1,
              color = "blue",
              label=mytext,
              dashArray = "3",
              fillOpacity = 0.7)
x@data$name

saveRDS(NE_data_only, file = "/MyData/NE_data_only.rds")


saveRDS(NE_data_only_rating, file="NE_data_only_rating.rds")
saveRDS(NE_postcodes, file="NE_postcodes.rds")
saveRDS(Eng_Wal_NI_data, file="Eng_Wal_NI_data.rds")
saveRDS(NG_data_only, file="NG_data_only.rds")
saveRDS(NG_data_only_rating, file="NG_data_only_rating.rds")
saveRDS(NG_postcodes, file="NG_postcodes.rds")
saveRDS(All_data, file="All_data.rds")
