library(dplyr)
Eng_Wal_NI_Data <- readRDS("data/Eng_Wal_NI_data.rds")


Eng_Wal_NI_Data <- Eng_Wal_NI_Data %>%
  mutate(rawScore = s_hygiene + s_structural + s_management)

saveRDS(Eng_Wal_NI_Data, file="data/Eng_Wal_NI_data.rds")


notNA <- Eng_Wal_NI_Data %>%
  filter(!is.na(rawScore))

