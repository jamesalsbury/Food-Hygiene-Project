saveRDS(NE_data_only, file="NE_data_only.rds")
saveRDS(NE_data_only_rating, file="NE_data_only_rating.rds")
saveRDS(new_merge, file="new_merge.rds")



Eng_Wal_NI_data <-  readRDS(file="/Users/jamesalsbury/Eng_Wal_NI_data.rds")
NE_data_only <- readRDS(file="/Users/jamesalsbury/NE_data_only.rds")
NE_data_only_rating <- readRDS(file="/Users/jamesalsbury/NE_data_only_rating.rds")
# submit the request
#path <- "http://api.ratings.food.gov.uk/Authorities"
request <- GET(url = path,
               query = list(
                 localAuthorityId =1,
                 pageNumber = 1,
                 pageSize = 10000),
               add_headers("x-api-version" = "2"))

# parse the response and convert to a data frame
response <- content(request, as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE) %>%
  pluck("authorities") %>%
  as_tibble()

# tidy the data
library(tidyverse) ; library(httr) ; library(jsonlite)
df <- response %>%
  mutate_all(funs(replace(., . == '', NA))) %>%
  select(name = Name,
         region = RegionName,
         count = EstablishmentCount)
all_data <- readRDS(file="all_data.rds")
library(tidyverse) ; library(httr) ; library(jsonlite)

all_data <- all_data  %>%
  add_column(regioncol)
all_data
regioncol = vector(length=nrow(all_data))
all_data

for (i in 485617:548066){
  for (j in 1:381){
    if (all_data$authorityName[i]==df$name[j]){
      all_data$regioncol[i] = df$region[j]
      break
    }
  }
}


long <- all_data %>%
  count(regioncol)

sum(long$n)

Scottish_data <- all_data %>%
  filter(regioncol=="Scotland")

Eng_Wal_NI_data <- all_data %>%
  filter(regioncol!="Scotland")

saveRDS(Eng_Wal_NI_data, file="Eng_Wal_NI_data.rds")

  plot(Eng_Wal_NI_data$long, Eng_Wal_NI_data$lat)

Eng_Wal_NI_data %>%
  count(type)

random <- Eng_Wal_NI_data %>%
  filter(lat<(51.5)) %>%
  filter(lat>(51)) %>%
  filter(long>(-5)) %>%
  filter(long<(-4.6)) 

random

plot(random$long, random$lat)

Eng_Wal_NI_data <- Eng_Wal_NI_data %>%
  filter(address!="Marine Gardens,  Carrickfergus, Antrim")

Eng_Wal_NI_data %>%
  filter(address=="Marine Gardens,  Carrickfergus, Antrim")

Eng_Wal_NI_data %>%
  filter(regioncol=="North East")

North_East <- Eng_Wal_NI_data %>%
  filter(regioncol=="North East")


saveRDS(North_East, file="North_East.rds")

North_East <- readRDS(file="North_East.rds")

plot(North_East$long, North_East$lat)

long <- North_East %>%
  count(authorityName)

long$n

North_East %>%
  filter(lat<54)

install.packages("leaflet")
library(leaflet)
leaflet() %>%
  setView(lng = -1.5, lat = 55, zoom =8) %>% 
  addTiles()


data(ukgeom, package = "jrSpatial") 
leaflet(ukgeom) %>%
  addTiles() %>%
  addPolygons()

install.packages("geojsonR")
library(geojsonR)
NE_postcodes = FROM_GeoJson("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/NE.geojson")
leaflet(NE_postcodes) %>%
  addTiles()
head(NE_postcodes)
plot(NE_postcodes)

library(geojsonio)
NE_postcodes <- geojson_read("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/NE.geojson",  what = "sp")
plot(spdf)

CA_postcodes <- geojson_read("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/CA.geojson", what="sp")

class(NE_postcodes)
names(NE_postcodes)

leaflet() %>%
  setView(lng = -2, lat = 55.25, zoom =8) %>% 
  addTiles() %>%
  addPolygons(data = NE_postcodes)

postcodes <- North_East %>%
  count(postcode)

North_East  <- North_East %>%
  add_column(Start_postcode)

North_East

Start_postcode =vector(mode="character", length=nrow(North_East))

North_East$postcode[1]

substr(North_East$postcode[1], 1, 3)

nchar(North_East$postcode[1])



for (i in 1:19283) {
  if (!is.na(North_East$postcode[i])) {
    if (nchar(North_East$postcode[i])==7){
      North_East$Start_postcode[i] = substr(North_East$postcode[i], 1, 3)
    }
    if (nchar(North_East$postcode[i])==8){
      North_East$Start_postcode[i] = substr(North_East$postcode[i], 1, 4)
    }
  }
}


NE_data_only <- North_East

NE_data_only <- North_East %>%
  filter(substr(Start_postcode, 1, 2)=="NE")


long <- North_East %>%
  count(Start_postcode)

long

NE2 <- North_East %>%
  filter(Start_postcode=="NE2") 

North_East

plot(NE_data_only$long, NE_data_only$lat)

NE_data_only %>%
  count(Start_postcode)

NE_data_only %>%
  count(authorityName)

NE_data_only

NE_data_only %>% 
  group_by(Start_postcode) %>% 
  summarise(mean = mean(rating))

long <- NE_data_only %>% 
  count(rating)

long$n

NE_data_only_rating <-  NE_data_only %>% 
  filter(rating==0 | rating==1 | rating==2 | rating==3 | rating==4 | rating==5)

NE_data_only_rating %>% 
  summarise(group_by(Start_postcode), mean=mean(rating))

long1 <- NE_data_only_rating %>% 
  group_by(Start_postcode) %>% 
  summarise(mean=mean(rating))


new_merge <- merge(NE_postcodes, long1, by.x="name", by.y="Start_postcode")
new_merge


ggplot(new_merge)




library(leaflet)


pal_sb <- colorNumeric("viridis", domain=new_merge$mean)
leaflet() %>%
  setView(lng = -2, lat = 55.25, zoom =8) %>% 
  addTiles()  %>% 
  addPolygons(data=new_merge,
    fillColor = ~pal_sb(new_merge$mean),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7) %>% 
addLegend(pal = pal_sb, 
          values = new_merge$mean, 
          position = "bottomright", 
          title = "Mean hygiene rating")
install.packages("usethis")
library(usethis)
?use_github





