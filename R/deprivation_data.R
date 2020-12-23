library(dplyr)
library(tidyverse)
library(httr)
library(jsonlite)
library(leaflet)
library(geojsonio)

full_postcode_dep_data <- readRDS("data/full_postcode_dep_data.rds")


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
#establishment_dep_merged = readRDS(file = "data/establishment_dep_merged.rds")
 for (i in 18:70){
   full_postcode_dep_data[,i] <- as.numeric(as.factor(full_postcode_dep_data[,i]))
 }

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

  # postcode_data[[postcodeAreas[i]]] <- postcode_data[[postcodeAreas[i]]] %>%
  #   filter(rating %in% 0:5)

  # postcode_data[[postcodeAreas[i]]]$rating <- as.numeric(as.character(postcode_data[[postcodeAreas[i]]]$rating))

  #Get a summary of the postcode data
  postcode_summary[[postcodeAreas[i]]] <- postcode_data[[postcodeAreas[i]]] %>%
    group_by(postcodeDistrict) %>%
    summarise(mean = mean(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`), count=n())



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


#Merge all of the data together
for (i in seq_along(postcodeAreas)) {
  if (i == 1) {
    All_postcodes_merged <- merged_sp_summary[[postcodeAreas[1]]]
  } else {
    All_postcodes_merged <- rbind(All_postcodes_merged, merged_sp_summary[[i]])
  }
}


bins = c(0, 5000, 10000, 15000, 20000, 25000, 30000)
pal_sb <- colorBin("RdYlGn", domain = All_postcodes_merged$mean, bins=bins)


mytext <- paste(
  "Area: ", All_postcodes_merged@data$name,"<br/>",
  "Count in area: ", All_postcodes_merged@data$count, "<br/>",
  "Mean deprivation Rank: ", round(All_postcodes_merged@data$mean, 2),
  sep="") %>%
  lapply(htmltools::HTML)



leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom = 8) %>%
  addTiles() %>%
  addPolygons(data = All_postcodes_merged,
              fillColor = ~pal_sb(All_postcodes_merged$mean),
              weight = 2,
              opacity = 0.3,
              label = mytext,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7) %>%
  addLegend(pal = pal_sb,
            values = All_postcodes_merged$mean,
            position = "bottomright",
            title = "Mean deprivation Rank")



All_postcodes_merged$mean %>%
  as_tibble() %>%
  arrange(desc(value))


#
#
# new <- full_postcode_dep_data$pcds %>%
#   as_tibble() %>%
#   mutate_if(is.factor, as.character)
#
# postcodeArea <- vector(length = nrow(new), mode="character")
# for (i in 1:nrow(new)){
#   postcodeArea[i] =   str_extract(new[i,],"[^0-9]+")
# }
#
# full_postcode_dep_data <- full_postcode_dep_data %>%
#   add_column(postcodeArea)
#
#
# Al <- postcode_data$AL



# full_postcode_dep_data$postcodeDistrict
#
#
# NE2 <- full_postcode_dep_data %>%
# filter(postcodeDistrict=="NE2")
#
# mean(L4$`Index of Multiple Deprivation (IMD) Score`)
# mean(NE2$`Index of Multiple Deprivation (IMD) Score`)
#
# sum1 <- All_postcodes_merged$mean %>%
#   as_tibble()
# mysum=0
# for (i in 1:nrow(sum1)){
#   mysum = mysum + sum1[i,]
# }
#
# mysum/2091
#
# sum %>%
#   filter(value>0) %>%
#   arrange(desc(value))


