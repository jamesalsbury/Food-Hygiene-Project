library(dplyr)
library(tidyverse)
library(httr)
library(jsonlite)
library(leaflet)
library(geojsonio)
all_postcodes_deprivation <- readRDS(file = "data/all_postcodes_deprivation.rds")


postcodeAreas <- c("AL" ,"B", "BA" ,"BB" ,"BD" ,"BH" ,"BL" ,"BN" ,"BR" ,"BS", "CA", "CB",
                   "CF" ,"CH", "CM" ,"CO" ,"CR" ,"CT" ,"CV", "CW", "DA" ,"DE" ,"DH",
                   "DL", "DN", "DT", "DY", "E" ,"EC", "EN" ,"EX", "FY","GL" ,
                   "GU", "HA", "HD" ,"HG", "HP", "HR","HU", "HX", "IG", "IP",
                   "KT", "L", "LA", "LD", "LE", "LL" ,"LN", "LS", "LU" ,"M", "ME", "MK",
                   "N", "NE" ,"NG" ,"NN", "NP", "NR", "NW", "OL", "OX", "PE",
                   "PL", "PO", "PR", "RG", "RH", "RM", "S", "SA", "SE", "SG", "SK", "SL", "SM",
                   "SN", "SO", "SP", "SR", "SS", "ST", "SW", "SY", "TA", "TF", "TN", "TQ",
                   "TR", "TS", "TW", "UB", "W", "WA", "WC", "WD", "WF", "WN", "WR", "WS", "WV",
                   "YO")
bad_areas = c(10, 19, 31, 49)
goodPostcodeAreas = postcodeAreas[-bad_areas]
badPostcodeAreas = postcodeAreas[bad_areas]

sp_poly = list()


for (i in seq_along(goodPostcodeAreas)) {
  #Get the spatial data for the well-behaved postcode datasets
  path = paste0("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/",
                goodPostcodeAreas[i], ".geojson")
  sp_poly[[goodPostcodeAreas[i]]] = geojson_read(path, what = "sp")
  sp_poly[[goodPostcodeAreas[i]]]@data = sp_poly[[goodPostcodeAreas[i]]]@data[, c("name", "description")]
}

for (i in seq_along(badPostcodeAreas)) {
  #Get the spatial data for not so well-behaved postcode datasets
  path <- paste0("data/", badPostcodeAreas[i], ".json")
  sp_poly[[badPostcodeAreas[i]]] = geojson_read(path, what = "sp")
}


for (i in seq_along(postcodeAreas)) {
  if (i == 1) {
    All_postcodes_merged <- sp_poly[[postcodeAreas[1]]]
  } else {
    All_postcodes_merged <- rbind(All_postcodes_merged, sp_poly[[i]])
  }
}



postcode_dep_summary <- all_postcodes_deprivation %>%
  group_by(PostcodeDistrict)  %>%
  summarise(mean = mean(`Index of Multiple Deprivation Rank`), count=n())


merged_dep <- merge(All_postcodes_merged, postcode_dep_summary,  by.x = "name", by.y = "PostcodeDistrict")




pal_sb <- colorBin("BuGn", domain = merged_dep$mean)



mytext <- paste(
  "Area: ", merged_dep@data$name,"<br/>",
  "Count in area: ", merged_dep@data$count, "<br/>",
  "Mean deprivation rank: ", round(merged_dep@data$mean, 2),
  sep="") %>%
  lapply(htmltools::HTML)



leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom = 8) %>%
  addTiles() %>%
  addPolygons(data = merged_dep,
              fillColor = ~pal_sb(merged_dep$mean),
              weight = 2,
              opacity = 1,
              label = mytext,
              color = "yellow",
              dashArray = "3",
              fillOpacity = 0.7) %>%
  addLegend(pal = pal_sb,
            values = merged_dep$mean,
            position = "bottomright",
            title = "Mean deprivation rank")


