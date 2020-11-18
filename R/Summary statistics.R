library(dplyr)
library(tidyverse)

establishment_dep_merged = readRDS(file = "data/establishment_dep_merged.rds")


ratings <- establishment_dep_merged %>%
  count(rating)
ratings <- ratings %>%
  filter(rating %in% 0:5)
hist(ratings)
ratings

#Standard data
establishment_dep_merged %>%
  count(rating)


ratings$rating <- as.numeric(as.character(ratings$rating))

SummaryCount <- RatingsOnly %>%
  count(rating)

sum(ratings$n)
establishment_dep_merged$postcodeArea
ratings %>%
  filter(rating==5)
#bar chart
ggplot(data=SummaryCount) + geom_bar(mapping=aes(x=rating, y=n), stat="identity")



#Raw data
notRawNA <- Eng_Wal_NI_Data %>%
  filter(!is.na(rawScore))


SummaryCount <- notRawNA %>%
  count(rawScore)

ggplot(data=SummaryCount) + geom_bar(mapping=aes(x=rawScore, y=n), stat="identity")

Eng_Wal_NI_Data %>%
  count(postcode)
