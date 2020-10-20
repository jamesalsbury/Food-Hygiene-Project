library(dplyr)
library(tidyverse)

Eng_Wal_NI_data = readRDS(file = "data/Eng_Wal_NI_data.rds")

#Standard data
RatingsOnly <-  Eng_Wal_NI_data %>%
  filter(rating %in% 0:5)

RatingsOnly$rating <- as.numeric(as.character(RatingsOnly$rating))

SummaryCount <- RatingsOnly %>%
  count(rating)


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
