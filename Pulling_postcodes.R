#Getting postcode data
library(tidyverse) 
library(httr)
library(jsonlite) 
library(leaflet)
library(geojsonR)
library(geojsonio) 
library(sp) 
library(magrittr)
library(dplyr)
library(sf)

remotes::install_github("rCarto/osrm")

All_UK_postcodes <- vector(mode="list", length=130)
All_UK_postcodes <- vector(mode="character", length=130)
newdf <- vector(mode="character", length=1)
postcodeAreas <- c("AB" ,"AL" ,"B", "BA" ,"BB" ,"BD" ,"BH" ,"BL" ,"BN" ,"BR" ,"BS", "CA", "CB", "CF" ,"CH", "CM" ,"CO" ,"CR" ,"CT" ,"CV", "CW", "DA" ,"DD" ,"DE", "DG" ,"DH","DL", "DN", "DT", "DY", "E" ,"EC", "EH", "EN" ,"EX", "FK", "FY", "G","GL" ,"GU", "HA", "HD" ,"HG", "HP", "HR", "HS", "HU", "HX", "IG", "IP", "IV", "KA", "KT", "KW" ,"KY", "L", "LA", "LD", "LE", "LL" ,"LN", "LS", "LU" ,"M", "ME", "MK", "ML" ,"N", "NE" ,"NG" ,"NN", "NP", "NR", "NW", "OL", "OX", "PA", "PE", "PH", "PL", "PO", "PR", "RG", "RH", "RM", "S", "SA", "SE", "SG", "SK", "SL", "SM", "SN", "SO", "SP", "SR", "SS", "ST", "SW", "SY", "TA", "TD", "TF", "TN", "TQ", "TR", "TS", "TW", "UB", "W", "WA", "WC", "WD", "WF", "WN", "WR", "WS", "WV", "YO", "ZE")

for(i in 1:length(postcodeAreas)){
  if ( i ==20 | i==35 | i==38 | i==51 | i==60 | i==79 | i==120){
    
  }
   else{
     path <- paste0("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/", postcodeAreas[i], ".geojson")
     assign(paste0(postcodeAreas[i], "_spatial_data"),geojson_read(path, what="sp")) 
   } 
  }


for (i in 1:nrow(Eng_Wal_NI_data)){
  
}

postcodeArea <- vector(mode="character", length = nrow(Eng_Wal_NI_data))
postcodeDistrict <- vector(mode="character", length = nrow(Eng_Wal_NI_data))



Eng_Wal_NI_data <- Eng_Wal_NI_data %>% 
  add_column(postcodeArea)

Eng_Wal_NI_data <- Eng_Wal_NI_data %>% 
  add_column(postcodeDistrict)

for (i in 327160:nrow(Eng_Wal_NI_data)){
  Eng_Wal_NI_data$postcodeArea[i] = str_extract(Eng_Wal_NI_data$postcode[i], "[A-Z]*")
  Eng_Wal_NI_data$postcodeDistrict[i] = str_split(Eng_Wal_NI_data$postcode[i], " ")[[1]][[1]]
}

area <- Eng_Wal_NI_data %>% 
  count(postcodeArea)

Eng_Wal_NI_data  %>% 
  filter(postcodeDistrict == "LL18")

saveRDS(Eng_Wal_NI_data, file = )

pal_sb <- colorNumeric("viridis", domain=AB_spatial_data)

leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom =8) %>% 
  addTiles()  %>% 
  addPolygons(data=L_spatial_data,
              weight = 2,
              opacity = 1,
              color = "yellow",
              dashArray = "3",
              fillOpacity = 0.7)
  ab <- rbind(AB_spatial_data, AL_spatial_data)

