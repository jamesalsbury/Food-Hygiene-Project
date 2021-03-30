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
for (i in 1:700){
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
# #submit the request
# path <- "http://api.ratings.food.gov.uk/Authorities"
# request <- GET(url = path,
#                query = list(
#                  localAuthorityId =1,
#                  pageNumber = 1,
#                  pageSize = 10000),
#                add_headers("x-api-version" = "2"))
#
# # # parse the response and convert to a data frame
# response <- content(request, as = "text", encoding = "UTF-8") %>%
#   fromJSON(flatten = TRUE) %>%
#   pluck("authorities") %>%
#   as_tibble()
#
# # # tidy the data
# df <- response %>%
#   mutate_all(funs(replace(., . == '', NA))) %>%
#   select(name = Name,
#          region = RegionName,
#          count = EstablishmentCount)
#
# AuthorityData <- df

# Region <- vector(mode="character", length = nrow(All_data_19_Oct))
# #
# All_data_19_Oct <- All_data_19_Oct %>%
#  add_column(Region)
#
# for (i in 1:nrow(All_data_19_Oct)){
#   for (j in 1:nrow(AuthorityData)){
#     if (All_data_19_Oct$authorityName[i]==AuthorityData$name[j]){
#       All_data_19_Oct$Region[i] = AuthorityData$region[j]
#       break
#     }
#   }
# }



All_data <- All_data  %>%
  mutate(postcodeArea = str_extract(postcode, "[A-Z]+"))

All_data <- All_data  %>%
  mutate(postcodeDistrict = str_extract(postcode, "\\w+"))

#Change date!!

All_data_29_Mar <- All_data

saveRDS(All_data_29_Mar, file = "data/API_dated/All_data_29_Mar.rds")

#Might be needed!!
getwd()
setwd("/Users/jamesalsbury/Food-Hygiene-Project")

nrow(All_data) - nrow(All_data_19_Oct)




################################################
#Combining all data so far
################################################


library(dplyr)
library(tidyverse)
library(ggplot2)


names <- c("19_Oct", "26_Oct", "2_Nov", "9_Nov", "16_Nov", "23_Nov", "30_Nov", "7_Dec",
           "14_Dec", "21_Dec", "1_Feb", "8_Feb", "15_Feb", "22_Feb", "1_Mar", "22_Mar")

dayssincestartvec = c(seq(0, 63, 7), seq(105, 133, 7), 154)

for (i in 1:length(names)){
  assign(paste0("All_data_", names[i]), readRDS(paste0("data/API_dated/All_data_", names[i], ".rds")))
}

All_data_19_Oct = All_data_19_Oct[,1:14]


for (i in 1:length(names)){
  dayssincestart = vector(length = nrow(eval(parse(text = paste0("All_data_", names[i])))))
  for (j in 1:length(dayssincestart)){
    dayssincestart[j] = dayssincestartvec[i]
  }

 temp <- eval(parse(text = paste0("All_data_", names[i]))) %>%
    add_column(dayssincestart)

 assign(paste0("All_data_", names[i]), temp)
}

combined = rbind(All_data_19_Oct, All_data_26_Oct)


for (i in 3:length(names)){
  combined = rbind(combined, eval(parse(text = paste0("All_data_", names[i]))))
}

combined <- combined %>%
  filter(type!=0)

types <- combined %>%
  group_by(type, dayssincestart) %>%
  count(type)


ggplot(types) + geom_line(aes(x=dayssincestart, y = n, colour = type))


ratings <- combined %>%
  group_by(rating, dayssincestart) %>%
  filter(rating %in% 0:5) %>%
  count(rating)

ggplot(ratings) + geom_line(aes(x=dayssincestart, y = n, colour = rating))


mycount <- combined %>%
  group_by(dayssincestart) %>%
  count() %>%
  ungroup()



plot(mycount$dayssincestart, mycount$n)

