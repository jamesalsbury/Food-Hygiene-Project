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


All_UK_postcodes <- vector(mode="list", length=130)
All_UK_postcodes <- vector(mode="character", length=130)
newdf <- vector(mode="character", length=1)
postcodeAreas <- c("AB" ,"AL" ,"B", "BA" ,"BB" ,"BD" ,"BH" ,"BL" ,"BN" ,"BR" ,"BS", "CA", "CB", "CF" ,"CH", "CM" ,"CO" ,"CR" ,"CT" ,"CV", "CW", "DA" ,"DD" ,"DE", "DG" ,"DH","DL", "DN", "DT", "DY", "E" ,"EC", "EH", "EN" ,"EX", "FK", "FY", "G","GL" ,"GU", "HA", "HD" ,"HG", "HP", "HR", "HS", "HU", "HX", "IG", "IP", "IV", "KA", "KT", "KW" ,"KY", "L", "LA", "LD", "LE", "LL" ,"LN", "LS", "LU" ,"M", "ME", "MK", "ML" ,"N", "NE" ,"NG" ,"NN", "NP", "NR", "NW", "OL", "OX", "PA", "PE", "PH", "PL", "PO", "PR", "RG", "RH", "RM", "S", "SA", "SE", "SG", "SK", "SL", "SM", "SN", "SO", "SP", "SR", "SS", "ST", "SW", "SY", "TA", "TD", "TF", "TN", "TQ", "TR", "TS", "TW", "UB", "W", "WA", "WC", "WD", "WF", "WN", "WR", "WS", "WV", "YO", "ZE")

for(i in 1:length(postcodeAreas)){
  if (i==11 | i ==20 | i==35 | i==38 | i==51 | i==60 | i==79 | i==120){
    
  }
   else{
     path <- paste0("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/", postcodeAreas[i], ".geojson")
     assign(paste0(postcodeAreas[i], "_spatial_data"),geojson_read(path, what="sp")) 
   } 
}


xxx <- E_spatial_data[,-(2:3)] 
E_spatial_data <- xxx[,-(3:4)] 

xxx <- EC_spatial_data[,-(2:3)] 
EC_spatial_data <- xxx[,-(3:4)]

xxx <- N_spatial_data[,-(2:3)] 
N_spatial_data <- xxx[,-(3:4)]

xxx <- NW_spatial_data[,-(2:3)] 
NW_spatial_data <- xxx[,-(3:4)]

xxx <- SE_spatial_data[,-(2:3)] 
SE_spatial_data <- xxx[,-(3:4)]

xxx <- SW_spatial_data[,-(2:3)] 
SW_spatial_data <- xxx[,-(3:4)]

xxx <- W_spatial_data[,-(2:3)] 
W_spatial_data <- xxx[,-(3:4)]

xxx <- WC_spatial_data[,-(2:3)] 
WC_spatial_data <- xxx[,-(3:4)]




postcodeArea <- vector(mode="character", length = nrow(Eng_Wal_NI_data))
postcodeDistrict <- vector(mode="character", length = nrow(Eng_Wal_NI_data))


Eng_Wal_NI_data <- Eng_Wal_NI_data %>% 
  add_column(postcodeArea)

Eng_Wal_NI_data <- Eng_Wal_NI_data %>% 
  add_column(postcodeDistrict)

for (i in 1:nrow(Eng_Wal_NI_data)){
  Eng_Wal_NI_data$postcodeArea[i] = str_extract(Eng_Wal_NI_data$postcode[i], "[A-Z]*")
  Eng_Wal_NI_data$postcodeDistrict[i] = str_split(Eng_Wal_NI_data$postcode[i], " ")[[1]][[1]]
}

saveRDS(Eng_Wal_NI_data, file = "MyData/Eng_Wal_NI_data.rds")

#Get postcode data
for(i in 1:length(postcodeAreas)){
  if (i==11 | i ==20 | i==35 | i==38 | i==51 | i==60 | i==79 | i==120){
    
  }
  else{
    temp <- Eng_Wal_NI_data %>% 
      filter(postcodeArea==postcodeAreas[i])
    assign(paste0(postcodeAreas[i], "_postcode_data"),temp) 
  } 
}

#Filter out exempt, awaiting publication etc
for(i in 1:length(postcodeAreas)){
  if (i==11 | i ==20 | i==35 | i==38 | i==51 | i==60 | i==79 | i==120){
    
  }
  else{
    temparea <- eval(parse(text = paste0(postcodeAreas[i], '_postcode_data')))
    temparea <- temparea %>% 
      filter(rating==0 | rating==1 | rating==2 | rating==3 | rating==4 | rating==5)
    assign(paste0(postcodeAreas[i], "_postcode_data_ratings"),temparea)
  } 
}

#Change ratings to numeric
for(i in 1:length(postcodeAreas)){
  if (i==11 | i ==20 | i==35 | i==38 | i==51 | i==60 | i==79 | i==120){
    
  }
  else{
    temparea <- eval(parse(text = paste0(postcodeAreas[i], '_postcode_data_ratings')))
  temparea$rating <- as.numeric(as.character(temparea$rating))
  assign(paste0(postcodeAreas[i], "_postcode_data_ratings"),temparea)
  } 
}

#Get a summary of all postcode areas
for(i in 1:length(postcodeAreas)){
  if (i==11 | i ==20 | i==35 | i==38 | i==51 | i==60 | i==79 | i==120){
    
  }
  else{
    temparea <- eval(parse(text = paste0(postcodeAreas[i], '_postcode_data_ratings')))
    temparea <- temparea %>% 
      group_by(postcodeDistrict) %>% 
      summarise(mean=mean(rating), sd=sd(rating), count = n()) 
    assign(paste0(postcodeAreas[i], "_postcode_summary"),temparea)
  } 
}

#Merge the spatial and postcode summary data
for(i in 1:length(postcodeAreas)){
  if (i==11 | i ==20 | i==35 | i==38 | i==51 | i==60 | i==79 | i==120){
    
  }
  else{
    tempspatial <- eval(parse(text = paste0(postcodeAreas[i], '_spatial_data')))
    tempsummary <- eval(parse(text = paste0(postcodeAreas[i], '_postcode_summary')))
    tempmerge <- merge(tempspatial, tempsummary, by.x="name", by.y="postcodeDistrict")
    assign(paste0(postcodeAreas[i], "_merged_data"),tempmerge)
  } 
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


  

pal_sb <- colorNumeric("YlOrRd", domain=All_postcodes_merged$mean)

install.packages("RColorBrewer")
  library(RColorBrewer)

display.brewer.all()

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

colnames(Eng_Wal_NI_data)
