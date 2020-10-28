#merge deprivation data witg establishment data
library(dplyr)
full_postcode_dep_data <- readRDS("data/full_postcode_dep_data.rds")
Eng_Wal_NI_data <- readRDS("data/Eng_Wal_NI_data.rds")

establishment_dep_merged <- merge(Eng_Wal_NI_data, full_postcode_dep_data, by.x = "postcode", by.y = "pcds")

establishment_dep_merged  <- establishment_dep_merged %>%
  filter(rating %in% 1:5)

establishment_dep_merged[,12] <- as.numeric(as.character(establishment_dep_merged[,12]))

for (i in 33:85){
  establishment_dep_merged[,i] <- as.numeric(as.character(establishment_dep_merged[,i]))
}

mod1 <- lm(log(rating) ~ log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`)*type, data = establishment_dep_merged)

mod1.res <- resid(mod1)


plot(log(establishment_dep_merged$`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`), mod1.res)

summary(mod1)


