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

postcodeAreas <- c("AB" ,"AL" ,"B", "BA" ,"BB" ,"BD" ,"BH" ,"BL" ,"BN" ,"BR" ,"BS", "CA", "CB",
                   "CF" ,"CH", "CM" ,"CO" ,"CR" ,"CT" ,"CV", "CW", "DA" ,"DD" ,"DE", "DG" ,"DH",
                   "DL", "DN", "DT", "DY", "E" ,"EC", "EH", "EN" ,"EX", "FK", "FY", "G","GL" ,
                   "GU", "HA", "HD" ,"HG", "HP", "HR", "HS", "HU", "HX", "IG", "IP", "IV", "KA",
                   "KT", "KW" ,"KY", "L", "LA", "LD", "LE", "LL" ,"LN", "LS", "LU" ,"M", "ME", "MK",
                   "ML" ,"N", "NE" ,"NG" ,"NN", "NP", "NR", "NW", "OL", "OX", "PA", "PE", "PH",
                   "PL", "PO", "PR", "RG", "RH", "RM", "S", "SA", "SE", "SG", "SK", "SL", "SM",
                   "SN", "SO", "SP", "SR", "SS", "ST", "SW", "SY", "TA", "TD", "TF", "TN", "TQ",
                   "TR", "TS", "TW", "UB", "W", "WA", "WC", "WD", "WF", "WN", "WR", "WS", "WV",
                   "YO", "ZE")
bad_areas = c(11, 20, 35, 38, 51, 60, 79, 120)
postcodeAreas = postcodeAreas[-bad_areas]
Eng_Wal_NI_data = readRDS(file="Data/Eng_Wal_NI_data.rds")

sp_poly = list()
postcode_data = list()
postcode_summary = list()
merged_sp_summary  = list()


for (i in seq_along(postcodeAreas)) {
    #Get the spatial data
    path = paste0("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/",
                  postcodeAreas[i], ".geojson")
    sp_poly[[postcodeAreas[i]]] = geojson_read(path, what = "sp")
    sp_poly[[postcodeAreas[i]]]@data = sp_poly[[postcodeAreas[i]]]@data[, c("name", "description")]
    
    
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
    
}




#Merge all of the postcode data together
All_postcodes_merged <- AB_merged_data
for(i in 2:length(postcodeAreas)){
  if (i==11 | i ==20 | i==35 | i==38 | i==51 | i==60 | i==79 | i==120){

  }
  else{
    tempmerge <- eval(parse(text = paste0(postcodeAreas[i], '_merged_data')))
    All_postcodes_merged <- rbind(All_postcodes_merged, tempmerge)
  }
}


all = d
for (i in seq_along(sp_poly)) {
  d = sp_poly[[i]]
  all = rbind(all, d)
}
all


pal_sb <- colorNumeric("YlOrRd", domain=All_postcodes_merged$mean)

# install.packages("RColorBrewer")
library(RColorBrewer)

display.brewer.all()

leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom =8) %>%
  addTiles()  %>%
  addPolygons(data=sp_poly$AB,
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


leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom =8) %>%
  addTiles()  %>%
  addPolygons(data=merged_sp_summary,
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7) 


path <- "/Users/jamesalsbury/Downloads/BS.json"
BS <- geojson_read(path, what="sp")

path <- "/Users/jamesalsbury/Downloads/CV.json"
CV <- geojson_read(path, what="sp")

path <- "/Users/jamesalsbury/Downloads/LL.json"
LL <- geojson_read(path, what="sp")

path <- "/Users/jamesalsbury/Downloads/EX.json"
EX <- geojson_read(path, what="sp")
