#merge deprivation data with establishment data
library(dplyr)
library(MASS)
library(leaflet)
library(stringr)
library(ordinal)
full_postcode_dep_data <- readRDS("data/full_postcode_dep_data.rds")
Eng_Wal_NI_data <- readRDS("data/Eng_Wal_NI_data.rds")

establishment_dep_merged <- merge(Eng_Wal_NI_data, full_postcode_dep_data, by.x = "postcode", by.y = "pcds")

establishment_dep_merged  <- establishment_dep_merged %>%
  filter(rating %in% 1:5)

establishment_dep_merged[,12] <- as.numeric(as.character(establishment_dep_merged[,12]))

for (i in 33:85){
  establishment_dep_merged[,i] <- as.numeric(as.character(establishment_dep_merged[,i]))
}

#Create a new column called chain
establishment_dep_merged <- establishment_dep_merged %>%
  mutate(chain = str_detect(name,"(?i)^Gregg", ) | str_detect(name,"(?i)^Domino", ) | str_detect(name,"(?i)^Burger King", ) | str_detect(name,"(?i)^Mcdonal", )
         | str_detect(name,"(?i)^KFC", ) | str_detect(name,"(?i)^Pizza Hut", ) | str_detect(name,"(?i)^Subway", ) | str_detect(name,"(?i)^Costa", )
         |str_detect(name,"(?i)^Toby Car", ) | str_detect(name,"(?i)^Bella Ital", ) | str_detect(name,"(?i)^PizzaE", ) | str_detect(name,"(?i)^Nando", )
         |str_detect(name,"(?i)^Harvester", ) | str_detect(name,"(?i)^TGI F", ) | str_detect(name,"(?i)^Papa J", ) | str_detect(name,"(?i)^Asda", )
         |str_detect(name,"(?i)^Tesco", ) | str_detect(name,"(?i)^Morrison", ) | str_detect(name,"(?i)^Sainsbury", ))



#Ordinal regression
establishment_dep_merged[,12] <- as.factor(as.character(establishment_dep_merged[,12]))
ordinal <- polr(formula = rating~log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`)+type, data = establishment_dep_merged)
clm <- clm(formula = rating~log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`)+type, data = establishment_dep_merged)
summary(clm)
summary(ordinal)


#Pulling out chains
#KFC
KFC <- establishment_dep_merged %>%
  filter(str_detect(name,"(?i)^KFC", ))

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
  filter(str_detect(name,"(?i)^McDonald", ))

McDonalds %>%
  count(type)

qpal <- colorFactor("YlOrRd", McDonalds$type)

leaflet(data = McDonalds) %>%
  setView(lng = -0.75, lat = 53, zoom = 8) %>%
  addTiles() %>%
  addCircleMarkers(color  = ~qpal(type)) %>%
  addLegend("bottomright", pal = qpal, values = ~type,
            title = "Type of McDonalds", opacity = 1)


#Pick out caring premises, takeaways and schools etc
comparetypes <- establishment_dep_merged %>%
  filter(type=="Takeaway/sandwich shop"|type=="Caring Premises"|type=="School/college/university")


compareclm <- clm(formula = rating~log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`)+type, data = comparetypes)
summary(compareclm)
