library(dplyr)
library(tidyverse)
library(hrbrthemes)

establishment_dep_merged = readRDS(file = "data/establishment_dep_merged.rds")

#Ratings
ratings <- establishment_dep_merged %>%
  count(rating)

ratings <- ratings %>%
  filter(rating %in% 0:5)

ratings$rating <- as.numeric(ratings$rating)

sum(ratings$n)


(ratings[5,2]+ratings[6,2])/sum(ratings$n)

ggplot(ratings) + geom_bar(mapping=aes(x=rating, y=n), stat="identity", fill="blue") + labs( y = "Number of establishments / 1000") +
  scale_x_discrete(breaks=c("0","1","2", "3", "4", "5"), labels=c("0 (bad)", "1", "2", "3", "4", "5"))  + scale_y_continuous(labels = formatter1000)


daily_plot <- ggplot(data=ratings, aes(x=factor(rating), y=n))
daily_plot <- daily_plot + geom_bar(stat = "identity", fill="")
daily_plot <- daily_plot + scale_x_discrete(breaks=0:5, labels=c("0 (bad)","1","2","3","4","5 (good)"))
daily_plot <- daily_plot + labs(x = "Food Hygiene Rating", y = "Number of establishments / 1000")
daily_plot <- daily_plot + scale_y_continuous(labels = formatter1000) + theme_minimal()
print(daily_plot)


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

formatter1000 <- function(x){
  x/1000
}

ggplot(rawcount) + geom_bar(mapping=aes(x=raw, y=n, fill=as.factor(hygienerating)), stat="identity") +
  labs(x = "Overall Score", y = "Number of establishments / 1000", fill = "Rating") + scale_y_continuous(labels = formatter1000) +
 scale_fill_discrete(name = "Rating", labels = c("0 (bad)", "1", "2", "3", "4", "5 (good)")) + theme_minimal()
