#merge deprivation data with establishment data
library(dplyr)
library(MASS)
library(leaflet)
library(stringr)
library(ordinal)
full_postcode_dep_data <- readRDS("data/full_postcode_dep_data.rds")
my_establishment_data <- readRDS("data/my_establishment_data.rds")

establishment_dep_merged <- merge(my_establishment_data, full_postcode_dep_data, by.x = "postcode", by.y = "pcds")

establishment_dep_merged  <- establishment_dep_merged %>%
  filter(rating %in% 0:5)

################################################
#Include a new column called chain, with 20 most popular chains
################################################
establishment_dep_merged <- establishment_dep_merged %>%
  mutate(chain = str_detect(name,"(?i)^Gregg", ) | str_detect(name,"(?i)^Domino", ) | str_detect(name,"(?i)^Burger King", ) | str_detect(name,"(?i)^Mcdonal", )
         | str_detect(name,"(?i)^KFC", ) | str_detect(name,"(?i)^Pizza Hut", ) | str_detect(name,"(?i)^Subway", ) | str_detect(name,"(?i)^Costa", )
         |str_detect(name,"(?i)^Toby Car", ) | str_detect(name,"(?i)^Bella Ital", ) | str_detect(name,"(?i)^PizzaE", ) | str_detect(name,"(?i)^Nando", )
         |str_detect(name,"(?i)^Harvester", ) | str_detect(name,"(?i)^TGI F", ) | str_detect(name,"(?i)^Papa J", ) | str_detect(name,"(?i)^Asda", )
         |str_detect(name,"(?i)^Tesco", ) | str_detect(name,"(?i)^Morrison", ) | str_detect(name,"(?i)^Sainsbury", ))

establishment_dep_merged[,12] <- as.factor(as.character(establishment_dep_merged[,12]))


################################################
#Ordinal regression
################################################
establishment_dep_merged[,12] <- as.factor(as.character(establishment_dep_merged[,12]))
ordinal <- polr(formula = rating~log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`), data = establishment_dep_merged)
clm <- clm(formula = rating~`Index of Multiple Deprivation (IMD) Score`, data = establishment_dep_merged)
summary(clm)
summary(ordinal)



################################################
#Linear regression
################################################
#Rating
################################################
#Rank

rank <- establishment_dep_merged$`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`
score <- establishment_dep_merged$`Index of Multiple Deprivation (IMD) Score`

RatingCount <- establishment_dep_merged %>%
  group_by(rating) %>%
  summarise(meanofdeprank = mean(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`))
plot(RatingCount)

mod1 <- lm(rating~`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`, data = establishment_dep_merged)
summary(mod1)

#When the area is most deprived
mod1$coefficients[1] + mod1$coefficients[2]*min(rank)
#When the area is least deprived
mod1$coefficients[1] + mod1$coefficients[2]*max(rank)

#Score
mod2 <- lm(rating~`Index of Multiple Deprivation (IMD) Score`, data = establishment_dep_merged)
summary(mod2)

#When the area is most deprived
mod2$coefficients[1] + mod2$coefficients[2]*max(score)
#When the area is least deprived
mod2$coefficients[1] + mod2$coefficients[2]*min(score)



################################################
#RawScore
################################################
establishment_dep_merged <- establishment_dep_merged %>%
  mutate(OverallRaw = s_hygiene + s_structural + s_management) %>%
  filter(OverallRaw %in% 0:80)

RawCount <- establishment_dep_merged %>%
  group_by(OverallRaw) %>%
  summarise(meanofdeprank = mean(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`))

plot(RawCount)

#Rank
mod3 <- lm(OverallRaw~`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`, data = establishment_dep_merged)
summary(mod3)

#When the area is least deprived
mod3$coefficients[1] + mod3$coefficients[2]*max(rank)
#When the area is most deprived
mod3$coefficients[1] + mod3$coefficients[2]*min(rank)

#Score
mod4 <- lm(OverallRaw~`Index of Multiple Deprivation (IMD) Score`, data = establishment_dep_merged)
summary(mod4)

#When the area is least deprived
mod4$coefficients[1] + mod4$coefficients[2]*min(score)
#When the area is most deprived
mod4$coefficients[1] + mod4$coefficients[2]*max(score)



################################################
#For getting the postcode deprivation data
################################################
# full_postcode_dep_data <- inner_join(PCD_OA_LSOA_MSOA_LAD_AUG20_UK_LU, File_7_All_IoD2019_Scores_Ranks_Deciles_and_Population_Denominators_3, by=c("lsoa11nm"="LSOA name (2011)"))
#
# full_postcode_dep_data <- full_postcode_dep_data  %>%
#   mutate(postcodeArea = str_extract(pcds, "[A-Z]+"))
#
# full_postcode_dep_data <- full_postcode_dep_data  %>%
#   mutate(postcodeDistrict = str_extract(pcds, "\\w+"))
#
# saveRDS(full_postcode_dep_data, "data/full_postcode_dep_data.rds")


##################################################################
#Pulling the top and bottom n postcode districts by deprivation
##################################################################
library(cowplot)
library(leaflet)
library(dplyr)
library(ggplot2)

All_data_19_Oct <- readRDS("data/API_dated/All_data_19_Oct.rds")

district_merged <- full_postcode_dep_data %>%
  group_by(postcodeDistrict) %>%
  summarise(meanrank = mean(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`),
            meanscore = mean(`Index of Multiple Deprivation (IMD) Score`))

myn <- 100
least_dep <- district_merged %>%
  slice_min(meanscore,  n= myn)

most_dep <- district_merged %>%
  slice_max(meanscore,  n= myn)



postcode_summary = list()
for (i in 1:myn){
  temp <- All_data_19_Oct %>%
    filter(rating %in% 0:5) %>%
    filter(OverallRaw %in% 0:80) %>%
    filter(postcodeDistrict==least_dep$postcodeDistrict[i])

  postcode_summary[[i]] <- list(least_dep[i,], temp)

}

least_dep_list <- rbind( postcode_summary[[1]][[2]],  postcode_summary[[2]][[2]])
for (i in 3:myn){
  least_dep_list <- rbind(least_dep_list, postcode_summary[[i]][[2]])
}


postcode_summary = list()
for (i in 1:myn){
  temp <- All_data_19_Oct %>%
    filter(rating %in% 0:5) %>%
    filter(OverallRaw %in% 0:80) %>%
    filter(postcodeDistrict==most_dep$postcodeDistrict[i])

  postcode_summary[[i]] <- list(most_dep[i,], temp)

}


most_dep_list <- rbind(postcode_summary[[1]][[2]],  postcode_summary[[2]][[2]])
for (i in 3:myn){
  most_dep_list <- rbind(most_dep_list, postcode_summary[[i]][[2]])
}

least_dep_data_rating <- least_dep_list %>%
    count(rating) %>%
    mutate(prop = n/sum(n))

least_dep_data_raw <- least_dep_list %>%
  count(OverallRaw) %>%
  mutate(prop = n/sum(n))

most_dep_data_rating <- most_dep_list %>%
  count(rating) %>%
  mutate(prop = n/sum(n))

most_dep_data_raw <- most_dep_list %>%
  count(OverallRaw) %>%
  mutate(prop = n/sum(n))

#Plotting the proportions of ratings in the 100 least deprived areas
plot1 <- ggplot(data = least_dep_data_rating, aes(rating, prop)) + geom_bar(stat = "identity") + ylim(0,0.8)
#Plotting the proportions of ratings in the 100 most deprived areas
plot2 <- ggplot(data = most_dep_data_rating, aes(rating, prop)) + geom_bar(stat = "identity") + ylim(0,0.8)

plot_grid(plot1, plot2)

#Distribution of the types of establishments in the top and bottom 100 areas

mosttype <- most_dep_list %>%
  group_by(rating) %>%
  count(type) %>%
  ungroup(rating) %>%
  mutate(prop = n/sum(n))

leasttype <- least_dep_list %>%
  group_by(rating) %>%
  count(type) %>%
  ungroup(rating) %>%
  mutate(prop = n/sum(n))

#Plotting the types of establishments in 100 least deprived areas, coloured by rating
plot3 <- ggplot(data = leasttype, aes(type, prop, fill = rating)) + geom_bar(stat = "identity") + coord_flip() + ylim(0,0.3)
#Plotting the types of establishments in 100 most deprived areas, coloured by rating
plot4 <- ggplot(data = mosttype, aes(type, prop, fill=rating)) + geom_bar(stat = "identity") + coord_flip() + ylim(0,0.3)

plot_grid(plot3, plot4)

#Given a type, what is the proportion of its ratings?

propleasttype <-  least_dep_list %>%
  group_by(type) %>%
  count(rating) %>%
  mutate(prop = n/sum(n))

propmosttype <-  most_dep_list %>%
  group_by(type) %>%
  count(rating) %>%
  mutate(prop = n/sum(n))

#100 least deprived areas, given type, what rating?
plot5 <- ggplot(data = propleasttype, aes(type, prop, fill = rating)) + geom_bar(stat="identity") + coord_flip()
#100 most deprived areas, given type, what rating?
plot6 <- ggplot(data = propmosttype, aes(type, prop, fill = rating)) + geom_bar(stat="identity") + coord_flip()

plot_grid(plot5, plot6)


#Given a number of different types from a postcode district, how would this correlate if 'moved' to an area in top 100?


OutputleastDF <- data.frame(Rating = 0:5, Count=0)
OutputmostDF <- data.frame(Rating = 0:5, Count=0)
clicked <- "NE6"
temp <- All_data_19_Oct %>%
  filter(postcodeDistrict==clicked) %>%
  filter(rating %in% 0:5) %>%
count(type)

for (i in 1:nrow(temp)){
  mytype <- temp[i,1]
  for (j in 1:nrow(propleasttype)){
    if (mytype==propleasttype[j,1]){
      currentRating = propleasttype[j,2]
     currentRating = as.numeric(currentRating)
     OutputleastDF[currentRating+1, 2] = OutputleastDF[currentRating+1, 2] + temp[i,2]*propleasttype[j,4]
    }
  }
}

for (i in 1:nrow(temp)){
  mytype <- temp[i,1]
  for (j in 1:nrow(propmosttype)){
    if (mytype==propmosttype[j,1]){
      currentRating = propmosttype[j,2]
      currentRating = as.numeric(currentRating)
      OutputmostDF[currentRating+1, 2] = OutputleastDF[currentRating+1, 2] + temp[i,2]*propmosttype[j,4]
    }
  }
}

barplot(OutputleastDF$Count, ylim=c(0,max(OutputleastDF$Count, OutputmostDF$Count)*1.2))
barplot(OutputmostDF$Count, ylim=c(0,max(OutputleastDF$Count, OutputmostDF$Count)*1.2))

barplot(All_data_19_Oct %>%
  filter(postcodeDistrict==clicked) %>%
  filter(rating %in% 0:5) %>%
  count(rating) %>%
  pull(2), ylim=c(0,max(OutputleastDF$Count, OutputmostDF$Count)*1.2))

par(mfrow=c(1,3))

OutputleastDF$Count
