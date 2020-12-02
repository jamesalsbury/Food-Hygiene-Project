library(dplyr)
library(tidyverse)

establishment_dep_merged = readRDS(file = "data/establishment_dep_merged.rds")

#Ratings
ratings <- establishment_dep_merged %>%
  count(rating)

ratings <- ratings %>%
  filter(rating %in% 0:5)

ratings$rating <- as.numeric(ratings$rating)

ggplot(ratings) + geom_bar(mapping=aes(x=rating, y=n), stat="identity")

#Raw
rawData <- establishment_dep_merged %>%
  mutate(raw = s_hygiene + s_management + s_structural)

rawcount <- rawData %>%
  count(raw)

rawcount <- rawcount %>%
  filter(raw %in% 0:80)

hygienerating <- c(5, 5, 5, 5, 4, 3, 3, 2, 2, 1, 1, 0, 0, 0, 0, 0, 0)

rawcount <- rawcount %>%
  add_column(hygienerating)


ggplot(rawcount) + geom_bar(mapping=aes(x=raw, y=n, fill=as.factor(hygienerating)), stat="identity") +
  labs(x = "Raw Score", y = "Number of establishments", legend = "")
