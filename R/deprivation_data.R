library(dplyr)
library(tidyverse)
library(httr)
library(jsonlite)
library(leaflet)
library(geojsonio)
library(sf)
library(sp)

full_postcode_dep_data <- readRDS("data/full_postcode_dep_data.rds")


postcodeAreas <- c("AL" ,"B", "BA" ,"BB" ,"BD" ,"BH" ,"BL" ,"BN" ,"BR" ,"BS", "CA", "CB", "CF",
                   "CH", "CM" ,"CO" ,"CR" ,"CT" ,"CV", "CW", "DA" ,"DE" ,"DH",
                   "DL", "DN", "DT", "DY", "E" ,"EC", "EN" ,"EX", "FY","GL" ,
                   "GU", "HA", "HD" ,"HG", "HP", "HR","HU", "HX", "IG", "IP",
                   "KT", "L", "LA", "LD", "LE", "LL","LN", "LS", "LU" ,"M", "ME", "MK",
                   "N", "NE" ,"NG" ,"NN","NP", "NR", "NW", "OL", "OX", "PE",
                   "PL", "PO", "PR", "RG", "RH", "RM", "S","SA", "SE", "SG", "SK", "SL", "SM",
                   "SN", "SO", "SP", "SR", "SS", "ST", "SW","SY", "TA", "TF", "TN", "TQ",
                   "TR", "TS", "TW", "UB", "W", "WA", "WC", "WD", "WF", "WN", "WR", "WS", "WV",
                   "YO")

bad_areas = c(10, 19, 31, 49)
goodPostcodeAreas = postcodeAreas[-bad_areas]
badPostcodeAreas = postcodeAreas[bad_areas]


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
  path <- paste0("data/", badPostcodeAreas[i], ".json")
  sp_poly[[badPostcodeAreas[i]]] = geojson_read(path, what = "sp")
}

for (i in seq_along(postcodeAreas)) {
  #Get the postcode data, only numeric values
  postcode_data[[postcodeAreas[i]]] <- full_postcode_dep_data %>%
    filter(postcodeArea == postcodeAreas[i])

  #Get a summary of the postcode data
  postcode_summary[[postcodeAreas[i]]] <- postcode_data[[postcodeAreas[i]]] %>%
    group_by(postcodeDistrict) %>%
    summarise(meanrank = mean(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`),
              meanscore = mean(`Index of Multiple Deprivation (IMD) Score`), count=n())



  #Merge the spatial and postcode summary data
  merged_sp_summary[[postcodeAreas[i]]] <- merge(sp_poly[[postcodeAreas[i]]], postcode_summary[[postcodeAreas[i]]], by.x = "name", by.y = "postcodeDistrict")

}


#Merge all of the data together
for (i in seq_along(postcodeAreas)) {
  if (i == 1) {
    All_postcodes_merged <- merged_sp_summary[[postcodeAreas[1]]]
  } else {
    All_postcodes_merged <- rbind(All_postcodes_merged, merged_sp_summary[[i]])
  }
}


binrank = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 350000)
binscore = c(60,50,40,30,20,10,0)
pal_sb <- colorBin("RdYlGn", domain = All_postcodes_merged$meanscore, bins=binrank)


mytext <- paste(
  "Area: ", All_postcodes_merged@data$name,"<br/>",
  "Postcodes in area ", All_postcodes_merged@data$count, "<br/>",
  "Mean postcode Rank: ", round(All_postcodes_merged@data$meanrank, 2),
  sep="") %>%
  lapply(htmltools::HTML)



leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom = 8) %>%
  addTiles() %>%
  addPolygons(data = All_postcodes_merged,
              fillColor = ~pal_sb(All_postcodes_merged$meanrank),
              weight = 2,
              opacity = 0.3,
              label = mytext,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7) %>%
  addLegend(pal = pal_sb,
            values = All_postcodes_merged$meanrank,
            position = "bottomright",
            title = "Mean deprivation Rank")


