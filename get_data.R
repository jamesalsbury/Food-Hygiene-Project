# Bunch of API commands
# to get data
#
# At the end, use saveRDS to save to data/

library(tidyverse) 
library(httr)
library(jsonlite)


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

#Get the authority data
# submit the request
path <- "http://api.ratings.food.gov.uk/Authorities"
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
df <- response %>%
  mutate_all(funs(replace(., . == '', NA))) %>%
  select(name = Name,
         region = RegionName,
         count = EstablishmentCount)

AuthorityData <- df

saveRDS(AuthorityData, file="Authority_Data.rds")

Region <- vector(mode="character", length = nrow(All_data))

All_data <- All_data %>%
  add_column(Region)

for (i in 1:nrow(All_data)){
  for (j in 1:nrow(AuthorityData)){
    if (All_data$authorityName[i]==AuthorityData$name[j]){
      All_data$Region[i] = AuthorityData$region[j]
      break
    }
  }
}

All_data %>%
  filter(Region!="Scotland")

No_Scotland <- All_data %>%
  filter(Region!="Scotland")

Start_postcode <- vector(mode="character", length=nrow(No_Scotland))

No_Scotland <- No_Scotland %>%
  add_column(Start_postcode)

No_Scotland %>%
  count(postcode)

str_detect(No_Scotland, " ")

for (i in 473312:nrow(No_Scotland)){
  No_Scotland$Start_postcode[i] = str_split(No_Scotland$postcode[i], " ")[[1]][[1]]
}

i

str_split(x, " ")[[1]][[1]]

No_Scotland %>%
  count(Start_postcode)

postcodeNames <- c("AB","AL","B","BA", "BB","BD","BH","BL","BN","BR","BS","BT","CA","CB","CF","CH","CM","CO","CR","CT","CV","CW","DA","DD","DE","DG","DH","DL","DN","DT","DY","E","EC","EH","EN"		
EX		
FK		
FY	
G		
GL		
GU		
HA		
HD		
HG		
HP	 	
HR		
HS		
HU		
HX		
IG		
IP		
IV		
KA		
KT	  	
KW		
KY		
L		
LA		
LD	 	
LE		
LL		
LN		
LS		
LU		
M		
ME		
MK	 	
ML		
N		
NE	  	
NG		
NN		
NP		
NR		
NW	  	
OL		
OX		
PA		
PE		
PH		
PL		
PO		
PR		
RG		
RH		
RM		
S		
SA		
SE	  	
SG		
SK		
SL		
SM	
SN		
SO		
SP		 
SR		
SS		
ST		
SW	  	
SY		
TA		
TD		
TF		
TN	 	
TQ		
TR		
TS		
TW		
UB	
W	
WA		
WC		
WD	
WF	
WN		
WR		
WS		
WV	
YO		
ZE



