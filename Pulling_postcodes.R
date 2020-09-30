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


postcodeAreas <- c("AB" ,"AL" ,"B", "BA" ,"BB" ,"BD" ,"BH" ,"BL" ,"BN" ,"BR" ,"BS", "CA", "CB", "CF" ,"CH", "CM" ,"CO" ,"CR" ,"CT" ,"CV", "CW", "DA" ,"DD" ,"DE", "DG" ,"DH","DL", "DN", "DT", "DY", "E" ,"EC", "EH", "EN" ,"EX", "FK", "FY", "G","GL" ,"GU", "HA", "HD" ,"HG", "HP", "HR", "HS", "HU", "HX", "IG", "IP", "IV", "KA", "KT", "KW" ,"KY", "L", "LA", "LD", "LE", "LL" ,"LN", "LS", "LU" ,"M", "ME", "MK", "ML" ,"N", "NE" ,"NG" ,"NN", "NP", "NR", "NW", "OL", "OX", "PA", "PE", "PH", "PL", "PO", "PR", "RG", "RH", "RM", "S", "SA", "SE", "SG", "SK", "SL", "SM", "SN", "SO", "SP", "SR", "SS", "ST", "SW", "SY", "TA", "TD", "TF", "TN", "TQ", "TR", "TS", "TW", "UB", "W", "WA", "WC", "WD", "WF", "WN", "WR", "WS", "WV", "YO", "ZE")

for (i in 1:length(postcodeAreas)){
  path <- paste0("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/", postcodeAreas[i], ".geojson")
  assign(paste0(postcodeAreas[i], "_spatial_data"),geojson_read(path, what="sp"))
}

BS_postcodes <- geojson_read("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/BS.geojson",  what = "sp")
BS_postcodes <- read_sf("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/BS.geojson")


as_Spatial(BS_postcodes)
as(st_geometry(BS_postcodes), "Spatial")




pal_sb <- colorNumeric("viridis", domain=AB_spatial_data)

leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom =8) %>% 
  addTiles()  %>% 
  addPolygons(data=ab,
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7)
  ab <- rbind(AB_spatial_data, AL_spatial_data)

