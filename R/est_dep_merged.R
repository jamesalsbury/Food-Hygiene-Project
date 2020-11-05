#merge deprivation data witg establishment data
library(dplyr)
library(MASS)
library(leaflet)
full_postcode_dep_data <- readRDS("data/full_postcode_dep_data.rds")
Eng_Wal_NI_data <- readRDS("data/Eng_Wal_NI_data.rds")

establishment_dep_merged <- merge(Eng_Wal_NI_data, full_postcode_dep_data, by.x = "postcode", by.y = "pcds")

establishment_dep_merged  <- establishment_dep_merged %>%
  filter(rating %in% 1:5)

establishment_dep_merged[,12] <- as.numeric(as.character(establishment_dep_merged[,12]))

for (i in 33:85){
  establishment_dep_merged[,i] <- as.numeric(as.character(establishment_dep_merged[,i]))
}


#Ordinal regression
establishment_dep_merged[,12] <- as.factor(as.character(establishment_dep_merged[,12]))
ordinal <- polr(formula = rating~log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`)+type, data = establishment_dep_merged)
summary(ordinal)

#Pulling out chains
#KFC
KFC <- establishment_dep_merged %>%
  filter(name == "KFC")

KFC %>%
  count(type)

qpal <- colorFactor("YlOrRd", KFC$type)

leaflet(data = KFC) %>%
  setView(lng = -0.75, lat = 53, zoom = 8) %>%
  addTiles() %>%
  addCircleMarkers(color  = ~qpal(type)) %>%
  addLegend("bottomright", pal = qpal, values = ~type,
            title = "Type of KFC", opacity = 1)

#McDonalds

McDonalds <- establishment_dep_merged %>%
  filter(name == "McDonalds")

McDonalds %>%
  count(type)

qpal <- colorFactor("YlOrRd", McDonalds$type)

leaflet(data = McDonalds) %>%
  setView(lng = -0.75, lat = 53, zoom = 8) %>%
  addTiles() %>%
  addCircleMarkers(color  = ~qpal(type)) %>%
  addLegend("bottomright", pal = qpal, values = ~type,
            title = "Type of McDonalds", opacity = 1)
