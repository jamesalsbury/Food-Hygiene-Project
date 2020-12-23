#Getting postcode data
#library(tidyverse)
# library(httr)
# library(jsonlite)
library(leaflet)
# library(geojsonR)
library(geojsonio)
library(sp)
# library(magrittr)
library(dplyr)
library(sf)
library(RColorBrewer)

postcodeAreas <- c("AL" ,"B", "BA" ,"BB" ,"BD" ,"BH" ,"BL" ,"BN" ,"BR" ,"BS", "CA", "CB",
                  "CH", "CM" ,"CO" ,"CR" ,"CT" ,"CV", "CW", "DA" ,"DE" ,"DH",
                   "DL", "DN", "DT", "DY", "E" ,"EC", "EN" ,"EX", "FY","GL" ,
                   "GU", "HA", "HD" ,"HG", "HP", "HR","HU", "HX", "IG", "IP",
                   "KT", "L", "LA", "LE" ,"LN", "LS", "LU" ,"M", "ME", "MK",
                   "N", "NE" ,"NG" ,"NN", "NR", "NW", "OL", "OX", "PE",
                   "PL", "PO", "PR", "RG", "RH", "RM", "S", "SE", "SG", "SK", "SL", "SM",
                   "SN", "SO", "SP", "SR", "SS", "ST", "SW", "TA", "TF", "TN", "TQ",
                   "TR", "TS", "TW", "UB", "W", "WA", "WC", "WD", "WF", "WN", "WR", "WS", "WV",
                   "YO")
bad_areas = c(10, 18, 30)
goodPostcodeAreas = postcodeAreas[-bad_areas]
badPostcodeAreas = postcodeAreas[bad_areas]
#Eng_Wal_NI_data = readRDS(file = "data/Eng_Wal_NI_data.rds")
#notRawNA <- Eng_Wal_NI_data %>%
  #filter(!is.na(rawScore))
establishment_dep_merged = readRDS(file = "data/establishment_dep_merged.rds")


sp_poly = list()
postcode_data = list()
postcode_summary = list()
merged_sp_summary  = list()
postcode_count = list()



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
  postcode_data[[postcodeAreas[i]]] <- establishment_dep_merged %>%
    filter(postcodeArea == postcodeAreas[i])

  postcode_data[[postcodeAreas[i]]] <- postcode_data[[postcodeAreas[i]]] %>%
    filter(rating %in% 0:5)

  postcode_data[[postcodeAreas[i]]]$rating <- as.numeric(as.character(postcode_data[[postcodeAreas[i]]]$rating))

  #Get a summary of the postcode data
  postcode_summary[[postcodeAreas[i]]] <- postcode_data[[postcodeAreas[i]]] %>%
    group_by(postcodeDistrict) %>%
    summarise(mean = mean(rating), sd = sd(rating), count = n(), median = median(rating))


  # for (j in 0:5){
  #   temp <- postcode_data[[postcodeAreas[i]]] %>%
  #     group_by(postcodeDistrict) %>%
  #     filter(rating==j) %>%
  #     count()
  #
  #   if (j==0){
  #     temp <- rename(temp, zero.count=n)
  #   }
  #   if (j==1){
  #     temp <- rename(temp, one.count=n)
  #   }
  #   if (j==2){
  #     temp <- rename(temp, two.count=n)
  #   }
  #   if (j==3){
  #     temp <- rename(temp, three.count=n)
  #   }
  #   if (j==4){
  #     temp <- rename(temp, four.count=n)
  #   }
  #   if (j==5){
  #     temp <- rename(temp, five.count=n)
  #   }
  #   if (nrow(temp)!=0){
  #     postcode_summary[[postcodeAreas[i]]] <-  full_join(postcode_summary[[postcodeAreas[i]]], temp)
  #   }
  # }


  #Merge the spatial and postcode summary data
  merged_sp_summary[[postcodeAreas[i]]] <- merge(sp_poly[[postcodeAreas[i]]], postcode_summary[[postcodeAreas[i]]], by.x = "name", by.y = "postcodeDistrict")

  merged_sp_summary[[postcodeAreas[i]]]@data[is.na(merged_sp_summary[[postcodeAreas[i]]]@data)] <- 0
}

#Make the non-connecting districts have the same data
badDistricts <- c(9, 22, 27)
badNames <- c(2, 7, 33)
for (i in seq_along(badPostcodeAreas)) {
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i],]$mean = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 2]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i] + 1,]$mean = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 2]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i],]$sd = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 3]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i] + 1,]$sd = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 3]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i],]$count = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 4]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i] + 1,]$count = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 4]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i],]$median = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 5]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i] + 1,]$median = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 5]]
}

# for (i in seq_along(postcodeAreas)) {
#   if (ncol(merged_sp_summary[[postcodeAreas[i]]]@data)==12){
#     merged_sp_summary[[postcodeAreas[i]]]@data <- merged_sp_summary[[postcodeAreas[i]]]@data %>%
#       mutate(zero.count = 0)
#   }
#
#   merged_sp_summary[[postcodeAreas[i]]]@data <- merged_sp_summary[[postcodeAreas[i]]]@data %>%
#     mutate(zero.percent = zero.count/count)
#
#   merged_sp_summary[[postcodeAreas[i]]]@data <- merged_sp_summary[[postcodeAreas[i]]]@data %>%
#     mutate(one.percent = one.count/count)
#
#   merged_sp_summary[[postcodeAreas[i]]]@data <- merged_sp_summary[[postcodeAreas[i]]]@data %>%
#     mutate(two.percent = two.count/count)
#
#   merged_sp_summary[[postcodeAreas[i]]]@data <- merged_sp_summary[[postcodeAreas[i]]]@data %>%
#     mutate(three.percent = three.count/count)
#
#   merged_sp_summary[[postcodeAreas[i]]]@data <- merged_sp_summary[[postcodeAreas[i]]]@data %>%
#     mutate(four.percent = four.count/count)
#
#   merged_sp_summary[[postcodeAreas[i]]]@data <- merged_sp_summary[[postcodeAreas[i]]]@data %>%
#     mutate(five.percent = five.count/count)
# }

#Merge all of the data together
for (i in seq_along(postcodeAreas)) {
  if (i == 1) {
    All_postcodes_merged <- merged_sp_summary[[postcodeAreas[1]]]
  } else {
    All_postcodes_merged <- rbind(All_postcodes_merged, merged_sp_summary[[i]])
  }
}



bins <- seq(3.5, 5, by = 0.25)
pal_sb <- colorBin("RdYlGn", domain = All_postcodes_merged$mean, bins=bins)
mytext <- paste0(
  "Area: ", All_postcodes_merged@data$name,"<br/>",
  "Count in area: ", All_postcodes_merged@data$count, "<br/>",
  "Mean  hygiene rating: ", round(All_postcodes_merged@data$mean, 2)) %>%
  lapply(htmltools::HTML)

leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom = 8) %>%
  addTiles() %>%
  addScaleBar() %>%
  addPolygons(data = All_postcodes_merged,
              fillColor = ~pal_sb(All_postcodes_merged$mean),
              weight = 2,
              opacity = 0.2,
              label = mytext,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.5) %>%
  addLegend(pal = pal_sb,
            values = All_postcodes_merged$mean,
            position = "bottomright",
            title = "Mean hygiene rating")


