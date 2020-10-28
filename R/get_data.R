# Bunch of API commands
# to get data
#
# At the end, use saveRDS to save to data/

library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)


#Getting the establishment data
All_data = vector(mode="numeric",length=1)
for (i in 1:500){
  #Get establishment data
  path <- "http://api.ratings.food.gov.uk/Establishments"
  request <- GET(url = path,
                 query = list(
                   localAuthorityId =i,
                   pageNumber = 1,
                   pageSize = 10000),
                 add_headers("x-api-version" = "2"))

  # parse the response and convert to a data frame
  response <- content(request, as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    pluck("establishments") %>%
    as_tibble()

  # tidy the data

  if (nrow(response)==0) next

  df <- response %>%
    mutate_all(funs(replace(., . == '', NA))) %>%
    select(name = BusinessName,
           type = BusinessType,
           address1 = AddressLine1,
           address2 = AddressLine2,
           address3 = AddressLine3,
           address4 = AddressLine4,
           postcode = PostCode,
           authorityNum = LocalAuthorityCode,
           authorityName = LocalAuthorityName,
           s_hygiene = scores.Hygiene,
           s_structural = scores.Structural,
           s_management = scores.ConfidenceInManagement,
           long = geocode.longitude,
           lat = geocode.latitude,
           rating = RatingValue) %>%
    unite(address, address1, address2, address3, address4, remove = TRUE, sep = ", ") %>%
    mutate(address = str_replace_all(address, "NA,", ""),
           address = str_replace_all(address, ", NA", ""),
           long = as.numeric(long),
           lat = as.numeric(lat))

  All_data = rbind(All_data, df)
}

# #Get the authority data
# # submit the request
# path <- "http://api.ratings.food.gov.uk/Authorities"
# request <- GET(url = path,
#                query = list(
#                  localAuthorityId =1,
#                  pageNumber = 1,
#                  pageSize = 10000),
#                add_headers("x-api-version" = "2"))
#
# # parse the response and convert to a data frame
# response <- content(request, as = "text", encoding = "UTF-8") %>%
#   fromJSON(flatten = TRUE) %>%
#   pluck("authorities") %>%
#   as_tibble()
#
# # tidy the data
# df <- response %>%
#   mutate_all(funs(replace(., . == '', NA))) %>%
#   select(name = Name,
#          region = RegionName,
#          count = EstablishmentCount)
#
# AuthorityData <- df
#
# Region <- vector(mode="character", length = nrow(All_data))
#
# All_data <- All_data %>%
#   add_column(Region)

# for (i in i:nrow(All_data)){
#   for (j in 1:nrow(AuthorityData)){
#     if (All_data$authorityName[i]==AuthorityData$name[j]){
#       All_data$Region[i] = AuthorityData$region[j]
#       break
#     }
#   }
# }
#
#
# Eng_Wal_NI_data <- All_data %>%
#   filter(Region!="Scotland")

All_data <- All_data  %>%
  mutate(postcodeArea = str_extract(postcode, "[A-Z]+"))

All_data <- All_data  %>%
  mutate(postcodeDistrict = str_extract(postcode, "\\w+"))

#Change date!!

All_data_26_Oct <- All_data

saveRDS(All_data_26_Oct, file = "data/API_dated/All_data_26_Oct.rds")

All_data

