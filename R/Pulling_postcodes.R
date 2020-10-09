#Getting postcode data
#library(tidyverse)
library(httr)
library(jsonlite)
library(leaflet)
library(geojsonR)
library(geojsonio)
library(sp)
library(magrittr)
library(dplyr)
library(sf)
library(RColorBrewer)

postcodeAreas <- c("AB" ,"AL" ,"B", "BA" ,"BB" ,"BD" ,"BH" ,"BL" ,"BN" ,"BR" ,"BS", "CA", "CB",
                   "CF" ,"CH", "CM" ,"CO" ,"CR" ,"CT" ,"CV", "CW", "DA" ,"DD" ,"DE", "DG" ,"DH",
                   "DL", "DN", "DT", "DY", "E" ,"EC", "EH", "EN" ,"EX", "FK", "FY","GL" ,
                   "GU", "HA", "HD" ,"HG", "HP", "HR", "HS", "HU", "HX", "IG", "IP", "KA",
                   "KT", "KW" ,"KY", "L", "LA", "LD", "LE", "LL" ,"LN", "LS", "LU" ,"M", "ME", "MK",
                   "ML" ,"N", "NE" ,"NG" ,"NN", "NP", "NR", "NW", "OL", "OX", "PA", "PE",
                   "PL", "PO", "PR", "RG", "RH", "RM", "S", "SA", "SE", "SG", "SK", "SL", "SM",
                   "SN", "SO", "SP", "SR", "SS", "ST", "SW", "SY", "TA", "TD", "TF", "TN", "TQ",
                   "TR", "TS", "TW", "UB", "W", "WA", "WC", "WD", "WF", "WN", "WR", "WS", "WV",
                   "YO")
bad_areas = c(11, 20, 35, 58)
goodPostcodeAreas = postcodeAreas[-bad_areas]
badPostcodeAreas = postcodeAreas[bad_areas]
Eng_Wal_NI_data = readRDS(file="Data/Eng_Wal_NI_data.rds")

sp_poly = list()
postcode_data = list()
postcode_summary = list()
merged_sp_summary  = list()



for (i in seq_along(goodPostcodeAreas)) {
    #Get the spatial data for the well-behaved postcode datasets
    path = paste0("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/",
                  goodPostcodeAreas[i], ".geojson")
    sp_poly[[goodPostcodeAreas[i]]] = geojson_read(path, what = "sp")
    sp_poly[[goodPostcodeAreas[i]]]@data = sp_poly[[goodPostcodeAreas[i]]]@data[, c("name", "description")]
    
}

for (i in seq_along(badPostcodeAreas)) {
  #Get the spatial data for not so well-behaved postcode datasets
  path <- paste0("Data/", badPostcodeAreas[i], ".json")
  sp_poly[[badPostcodeAreas[i]]] = geojson_read(path, what = "sp")
  
  
}

for (i in seq_along(postcodeAreas)) {
    #Get the postcode data, only numeric values
    postcode_data[[postcodeAreas[i]]] <- Eng_Wal_NI_data %>%
      filter(postcodeArea==postcodeAreas[i])
    
    postcode_data[[postcodeAreas[i]]] <- postcode_data[[postcodeAreas[i]]] %>%
      filter(rating %in% 0:5)
    
    postcode_data[[postcodeAreas[i]]]$rating <- as.numeric(as.character(postcode_data[[postcodeAreas[i]]]$rating))
    
    #Get a summary of the postcode data
    postcode_summary[[postcodeAreas[i]]] <- postcode_data[[postcodeAreas[i]]] %>%
      group_by(postcodeDistrict) %>%
      summarise(mean=mean(rating), sd=sd(rating), count = n())
    
    #Merge the spatial and postcode summary data
    merged_sp_summary[[postcodeAreas[i]]] <- merge(sp_poly[[postcodeAreas[i]]], postcode_summary[[postcodeAreas[i]]], by.x = "name", by.y = "postcodeDistrict")
    
    #Merge all of the postcode data together
    if (i==1){
      All_postcodes_merged <- merged_sp_summary[[postcodeAreas[1]]]
    } else {
      All_postcodes_merged <- rbind(All_postcodes_merged, merged_sp_summary[[i]])
    }
}

pal_sb <- colorNumeric("BuGn", domain=All_postcodes_merged$mean)


leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom =8) %>%
  addTiles()  %>%
  addPolygons(data=All_postcodes_merged,
              fillColor = ~pal_sb(All_postcodes_merged$mean),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7) %>%
  addLegend(pal = pal_sb,
            values = All_postcodes_merged$mean,
            position = "bottomright",
            title = "Mean hygiene rating")

