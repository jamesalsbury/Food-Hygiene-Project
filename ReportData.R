library(tidyverse)

############################################################
#Getting the Scotland Data
############################################################

All_data_19_Oct <-readRDS("data/API_dated/All_data_19_Oct.rds")

NotScottish <- All_data_19_Oct %>%
  filter(Region!="Scotland")

############################################################
#Finding out the different types of establishments
############################################################

EstTypeCount <- NotScottish %>%
  count(type)

formatter1000 <- function(x){
  x/1000
}

TypeBarChart <- ggplot(data = EstTypeCount, aes(x = type, y=n)) +
  geom_bar(stat  = "identity", fill = "steelblue")  + ylab("Count") +ylim(c(0,145000)) +
  xlab("Type of establishment") + theme_classic() + geom_text(aes(label=n),hjust=-0.3) + coord_flip()

############################################################
#Finding out the different ratings of establishments
############################################################


RatingCount <- All_data_19_Oct %>%
  filter(rating %in% 0:5) %>%
  count(rating)


RatingBarChart <- ggplot(data = RatingCount, aes(x=rating, y=n)) + geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Rating") + theme_classic() + geom_text(aes(label=n),vjust=-0.3)


############################################################
#Finding out the different raw ratings of establishments
############################################################

OverallRawCount <- All_data_19_Oct %>%
  filter(rating %in% 0:5) %>%
  group_by(rating) %>%
  count(OverallRaw)

RawBarChart <- ggplot(data = OverallRawCount, aes(x = OverallRaw, y = n, fill=rating)) + geom_bar(stat="identity") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Overall score") + theme_classic() + labs(fill="Rating")



############################################################
#Finding out the breakdown of the raw ratings
############################################################


HygieneCount <- All_data_19_Oct %>%
  count(s_hygiene)

HygieneBarCount <- ggplot(data = HygieneCount, aes(x = s_hygiene, y=n)) +  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Hygiene score") + theme_classic() + geom_text(aes(label=n),vjust=-0.3)

StructuralCount <- All_data_19_Oct %>%
  count(s_structural)

StructuralBarCount <- ggplot(data = StructuralCount, aes(x = s_structural, y=n)) +  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Structural score") + theme_classic() + geom_text(aes(label=n),vjust=-0.3)

ManagementCount <- All_data_19_Oct %>%
  count(s_management)

ManagementBarCount <- ggplot(data = ManagementCount, aes(x = s_management, y=n)) +  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Management score") + theme_classic() + geom_text(aes(label=n),vjust=-0.3)

############################################################
############################################################
#Plotting the data set onto a map
############################################################
############################################################


library(leaflet)
library(geojsonio)
library(sp)
library(dplyr)
library(sf)
library(RColorBrewer)



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
  postcode_data[[postcodeAreas[i]]] <- All_data_19_Oct %>%
    filter(postcodeArea == postcodeAreas[i])

  postcode_data[[postcodeAreas[i]]] <- postcode_data[[postcodeAreas[i]]] %>%
    filter(rating %in% 0:5) %>%
    filter(OverallRaw %in% 0:80)

  postcode_data[[postcodeAreas[i]]]$rating <- as.numeric(as.character(postcode_data[[postcodeAreas[i]]]$rating))

  #Get a summary of the postcode data
  postcode_summary[[postcodeAreas[i]]] <- postcode_data[[postcodeAreas[i]]] %>%
    group_by(postcodeDistrict) %>%
    summarise(mean = mean(rating), sd = sd(rating), count = n(), rawmean = mean(OverallRaw))


  #Merge the spatial and postcode summary data
  merged_sp_summary[[postcodeAreas[i]]] <- merge(sp_poly[[postcodeAreas[i]]], postcode_summary[[postcodeAreas[i]]], by.x = "name", by.y = "postcodeDistrict")

  merged_sp_summary[[postcodeAreas[i]]]@data[is.na(merged_sp_summary[[postcodeAreas[i]]]@data)] <- 0
}

#Make the non-connecting districts have the same data
badDistricts <- c(9, 28, 27, 70)
badNames <- c(2, 7, 33, 57)
for (i in seq_along(badPostcodeAreas)) {
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i],]$mean = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 2]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i] + 1,]$mean = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 2]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i],]$sd = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 3]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i] + 1,]$sd = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 3]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i],]$count = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 4]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i] + 1,]$count = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 4]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i],]$rawmean = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 5]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i] + 1,]$rawmean = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 5]]
}


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


############################################################
############################################################
#Combining deprivation data and data set
############################################################
############################################################


DepData <- read_csv("data/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv")
PostcodeData <- read_csv("data/PCD_OA_LSOA_MSOA_LAD_AUG20_UK_LU.csv")
PostcodeDepMerged <- inner_join(PostcodeData, DepData, by = c("lsoa11cd" = "LSOA code (2011)"))
All_data_19_Oct <- readRDS("data/API_dated/All_data_19_Oct.rds")
EstDepMerged <- inner_join(All_data_19_Oct, PostcodeDepMerged, by = c("postcode" = "pcds"))


############################################################
############################################################
#Modelling of deprivation data
############################################################
############################################################

library(ordinal)
library(MASS)


#Identify chains
EstDepMerged <- EstDepMerged %>%
  mutate(chain = str_detect(name,"(?i)^Gregg", ) | str_detect(name,"(?i)^Domino", ) | str_detect(name,"(?i)^Burger King", ) | str_detect(name,"(?i)^Mcdonal", )
         | str_detect(name,"(?i)^KFC", ) | str_detect(name,"(?i)^Pizza Hut", ) | str_detect(name,"(?i)^Subway", ) | str_detect(name,"(?i)^Costa", )
         |str_detect(name,"(?i)^Toby Car", ) | str_detect(name,"(?i)^Bella Ital", ) | str_detect(name,"(?i)^PizzaE", ) | str_detect(name,"(?i)^Nando", )
         |str_detect(name,"(?i)^Harvester", ) | str_detect(name,"(?i)^TGI F", ) | str_detect(name,"(?i)^Papa J", ) | str_detect(name,"(?i)^Asda", )
         |str_detect(name,"(?i)^Tesco", ) | str_detect(name,"(?i)^Morrison", ) | str_detect(name,"(?i)^Sainsbury", ))

#Only consider ratings between 0-5:
EstDepMerged  <- EstDepMerged %>%
  filter(rating %in% 0:5)

#Ensure rating is a factor

EstDepMerged$rating <- as_factor(EstDepMerged$rating)

EstDepMerged$rating <- fct_rev(EstDepMerged$rating)


mymodel <- clm(formula = rating ~ chain, data = EstDepMerged, Hess = T)


summary(mymodel)

EstDepMerged %>%
  count(chain)


