library(tidyverse)

############################################################
#Getting the Scotland Data
############################################################

All_data_19_Oct <-readRDS("data/API_dated/All_data_19_Oct.rds")

NotScottish <- All_data_19_Oct %>%
  filter(Region!="Scotland")

############################################################
#Finding out the different types of establishments
############################################################

EstTypeCount <- NotScottish %>%
  count(type)

formatter1000 <- function(x){
  x/1000
}

TypeBarChart <- ggplot(data = EstTypeCount, aes(x = type, y=n)) +
  geom_bar(stat  = "identity", fill = "steelblue")  + ylab("Count") +ylim(c(0,145000)) +
  xlab("Type of establishment") + theme_classic() + geom_text(aes(label=n),hjust=-0.3) + coord_flip()

############################################################
#Finding out the different ratings of establishments
############################################################


RatingCount <- All_data_19_Oct %>%
  filter(rating %in% 0:5) %>%
  count(rating)


RatingBarChart <- ggplot(data = RatingCount, aes(x=rating, y=n)) + geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Rating") + theme_classic() + geom_text(aes(label=n),vjust=-0.3)


############################################################
#Finding out the different raw ratings of establishments
############################################################

OverallRawCount <- All_data_19_Oct %>%
  filter(rating %in% 0:5) %>%
  group_by(rating) %>%
  count(OverallRaw)


RawBarChart <- ggplot(data = OverallRawCount, aes(x = OverallRaw, y = n, fill=rating)) + geom_bar(stat="identity") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Overall score") + theme_classic() + labs(fill="Rating")



############################################################
#Finding out the breakdown of the raw ratings
############################################################


HygieneCount <- All_data_19_Oct %>%
  count(s_hygiene)

HygieneBarCount <- ggplot(data = HygieneCount, aes(x = s_hygiene, y=n)) +  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Hygiene score") + theme_classic() + geom_text(aes(label=n),vjust=-0.3)

StructuralCount <- All_data_19_Oct %>%
  count(s_structural)

StructuralBarCount <- ggplot(data = StructuralCount, aes(x = s_structural, y=n)) +  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Structural score") + theme_classic() + geom_text(aes(label=n),vjust=-0.3)

ManagementCount <- All_data_19_Oct %>%
  count(s_management)

ManagementBarCount <- ggplot(data = ManagementCount, aes(x = s_management, y=n)) +  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Management score") + theme_classic() + geom_text(aes(label=n),vjust=-0.3)

############################################################
############################################################
#Plotting the data set onto a map
############################################################
############################################################





