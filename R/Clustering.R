library(leaflet)
library(tidyverse)
'%ni%' <- Negate('%in%')

# #Clustering
#
# All_data_19_Oct <- readRDS("data/API_dated/All_data_19_Oct.rds")
#
# All_data_19_Oct$rating <- as.numeric(All_data_19_Oct$rating)
# All_data_19_Oct$s_hygiene <- as.numeric(All_data_19_Oct$s_hygiene)
# All_data_19_Oct$s_structural <- as.numeric(All_data_19_Oct$s_structural)
# All_data_19_Oct$s_management <- as.numeric(All_data_19_Oct$s_management)
#
# NewcCluster <- All_data_19_Oct %>%
#   filter(long > -1.657193) %>%
#   filter(long < -1.559175) %>%
#   filter(lat > 54.9656) %>%
#   filter(lat < 55.015456)
#
# Deli <- All_data_19_Oct %>%
#   filter(name=="Delicious Decadence")
#
# St_Sushi <- All_data_19_Oct %>%
#   filter(name=="St Sushi")
#
# dist <- sqrt((Deli$lat-St_Sushi$lat)^2+(Deli$long-St_Sushi$long)^2)
# fivehundredmetres <- (dist/3270)*500

#
# MeanRatingCluster <- vector(length=nrow(NewcCluster))
# MeanHygieneCluster <- vector(length=nrow(NewcCluster))
# MeanStructuralCluster <- vector(length=nrow(NewcCluster))
# MeanManagementCluster <- vector(length=nrow(NewcCluster))
#
# for (i in 1:nrow(NewcCluster)){
#   NumCluster <- 0
#   NumRating <- 0
#   NumHygiene <- 0
#   NumStructural <- 0
#   NumMangement <- 0
#   for (j in 1:nrow(NewcCluster)){
#     if (i!=j){
#       CalcDist <- sqrt((NewcCluster[i,]$lat - NewcCluster[j,]$lat)^2+(NewcCluster[i,]$long - NewcCluster[j,]$long)^2)
#       if (CalcDist < fivehundredmetres){
#         if (!is.na(NewcCluster[j,]$rating) && !is.na(NewcCluster[j,]$s_hygiene) && !is.na(NewcCluster[j,]$s_structural && !is.na(NewcCluster[j,]$s_management))){
#           NumCluster <- NumCluster + 1
#           NumRating <- NumRating + NewcCluster[j,]$rating
#           NumHygiene <- NumHygiene + NewcCluster[j,]$s_hygiene
#           NumStructural <- NumStructural + NewcCluster[j,]$s_structural
#           NumMangement <- NumMangement + NewcCluster[j,]$s_management
#         }
#       }
#     }
#   }
#   if (NumCluster!=0){
#     MeanRatingCluster[i] = NumRating/NumCluster
#     MeanHygieneCluster[i] = NumHygiene/NumCluster
#     MeanStructuralCluster[i] = NumStructural/NumCluster
#     MeanManagementCluster[i] = NumMangement/NumCluster
#   }
#   else{
#     MeanRatingCluster[i] = "None"
#     MeanHygieneCluster[i] = "None"
#     MeanStructuralCluster[i] = "None"
#     MeanManagementCluster[i] = "None"
#   }
# }
#
#
#
#
#
# NewMeanCluster <- as.numeric(MeanCluster)
#
# MeanData <- MeanData %>%
#   add_column(MeanManagementCluster)
#
# plot(MeanData$MeanRatingCluster,MeanData$rating)
#
# MeanData <- MeanData %>%
#   mutate(OverallRaw = s_hygiene + s_structural + s_management)

# MeanData <- readRDS("data/MeanData.rds")
#
# myrawsummary <- MeanData %>%
#   group_by(OverallRaw) %>%
#   summarise(rating = mean(MeanRatingCluster), hygiene = mean(MeanHygieneCluster), structural=mean(MeanStructuralCluster),management = mean(MeanManagementCluster))
#
# myrawsummary = myrawsummary[-12,]
#
# plot(myrawsummary$rating, myrawsummary$OverallRaw, ylab = "Raw score (lower the better)", xlab = "Mean rating of nearby establishments (higher the better)")
# abline(lm(myrawsummary$OverallRaw~myrawsummary$rating))
# plot(myrawsummary$hygiene, myrawsummary$OverallRaw, ylab = "Raw score (lower the better)", xlab = "Mean hygiene of nearby establishments (lower the better)")
# abline(lm(myrawsummary$OverallRaw~myrawsummary$hygiene))
# plot(myrawsummary$structural, myrawsummary$OverallRaw, ylab = "Raw score (lower the better)", xlab = "Mean structural of nearby establishments (lower the better)")
# abline(lm(myrawsummary$OverallRaw~myrawsummary$structural))
# plot(myrawsummary$management, myrawsummary$OverallRaw, ylab = "Raw score (lower the better)", xlab = "Mean management of nearby establishments (lower the better)")
# abline(lm(myrawsummary$OverallRaw~myrawsummary$management))
#
#
# #Try and do the same with Sheffield !
# library(tidyverse)
# library(leaflet)
# All_data_19_Oct <- readRDS("data/API_dated/All_data_19_Oct.rds")
#
# All_data_19_Oct$rating <- as.numeric(All_data_19_Oct$rating)
# All_data_19_Oct$s_hygiene <- as.numeric(All_data_19_Oct$s_hygiene)
# All_data_19_Oct$s_structural <- as.numeric(All_data_19_Oct$s_structural)
# All_data_19_Oct$s_management <- as.numeric(All_data_19_Oct$s_management)
#
# SheffCluster <- All_data_19_Oct %>%
#   filter(postcodeDistrict=="S1") %>%
#   filter(!is.na(long)) %>%
#   filter(!is.na(lat))
#
# MeanRatingCluster <- vector(length=nrow(NewcCluster))
# MeanHygieneCluster <- vector(length=nrow(NewcCluster))
# MeanStructuralCluster <- vector(length=nrow(NewcCluster))
# MeanManagementCluster <- vector(length=nrow(NewcCluster))
#
#
# for (i in 1:nrow(SheffCluster)){
#   NumCluster <- 0
#   NumRating <- 0
#   NumHygiene <- 0
#   NumStructural <- 0
#   NumMangement <- 0
#   for (j in 1:nrow(SheffCluster)){
#     if (i!=j){
#       CalcDist <- sqrt((SheffCluster[i,]$lat - SheffCluster[j,]$lat)^2+(SheffCluster[i,]$long - SheffCluster[j,]$long)^2)
#       if (CalcDist < fivehundredmetres){
#         if (!is.na(SheffCluster[j,]$rating) && !is.na(SheffCluster[j,]$s_hygiene) && !is.na(SheffCluster[j,]$s_structural && !is.na(SheffCluster[j,]$s_management))){
#           NumCluster <- NumCluster + 1
#           NumRating <- NumRating + SheffCluster[j,]$rating
#           NumHygiene <- NumHygiene + SheffCluster[j,]$s_hygiene
#           NumStructural <- NumStructural + SheffCluster[j,]$s_structural
#           NumMangement <- NumMangement + SheffCluster[j,]$s_management
#         }
#       }
#     }
#   }
#   if (NumCluster!=0){
#     MeanRatingCluster[i] = NumRating/NumCluster
#     MeanHygieneCluster[i] = NumHygiene/NumCluster
#     MeanStructuralCluster[i] = NumStructural/NumCluster
#     MeanManagementCluster[i] = NumMangement/NumCluster
#   }
#   else{
#     MeanRatingCluster[i] = "None"
#     MeanHygieneCluster[i] = "None"
#     MeanStructuralCluster[i] = "None"
#     MeanManagementCluster[i] = "None"
#   }
# }
#
# SheffMeanCluster <- SheffCluster %>%
#   add_column(MeanHygieneCluster)
#
# SheffMeanCluster <- SheffMeanCluster %>%
#   add_column(MeanManagementCluster)
#
# plot(SheffMeanCluster$rating, SheffMeanCluster$MeanRatingCluster)

SheffMeanCluster <- readRDS("data/SheffMeanCluster.rds")
SheffMeanCluster$OverallRaw = as.numeric(levels(SheffMeanCluster$OverallRaw))[SheffMeanCluster$OverallRaw]
myrawsummary <- SheffMeanCluster %>%
  group_by(OverallRaw) %>%
  summarise(meanrating = mean(MeanRatingCluster), hygiene = mean(MeanHygieneCluster), structural=mean(MeanStructuralCluster),management = mean(MeanManagementCluster), count=n())


plot(myrawsummary$meanrating, myrawsummary$OverallRaw, ylab = "Raw score (lower the better)", xlab = "Mean rating of nearby establishments (higher the better)")
abline(lm(myrawsummary$OverallRaw~myrawsummary$meanrating))
plot(myrawsummary$hygiene, myrawsummary$OverallRaw, ylab = "Raw score (lower the better)", xlab = "Mean hygiene of nearby establishments (lower the better)")
abline(lm(myrawsummary$OverallRaw~myrawsummary$hygiene))
plot(myrawsummary$structural, myrawsummary$OverallRaw, ylab = "Raw score (lower the better)", xlab = "Mean structural of nearby establishments (lower the better)")
abline(lm(myrawsummary$OverallRaw~myrawsummary$structural))
plot(myrawsummary$management, myrawsummary$OverallRaw, ylab = "Raw score (lower the better)", xlab = "Mean management of nearby establishments (lower the better)")
abline(lm(myrawsummary$OverallRaw~myrawsummary$management))
plot(m)



ggplot(SheffMeanCluster, aes(x = OverallRaw, y  = MeanRatingCluster)) + geom_boxplot()




####################################
#Doing the Sheffield data again
###################################
library(tidyverse)


S1Data <- All_data_19_Oct %>%
  filter(postcodeDistrict=="S1") %>%
  filter(!is.na(long)) %>%
   filter(!is.na(lat)) %>%
  filter(rating %in% 0:5) %>%
  filter(OverallRaw %in% 0:80)

S1Data$rating <- as.numeric(S1Data$rating)
MeanRatingCluster <- vector(length=nrow(S1Data))
MeanHygieneCluster <- vector(length=nrow(S1Data))
MeanStructuralCluster <- vector(length=nrow(S1Data))
MeanManagementCluster <- vector(length=nrow(S1Data))
MeanOverallRawCluster <- vector(length=nrow(S1Data))
fivehundredmetres = 0.004652264

for (i in 1:nrow(S1Data)){
  NumCluster <- 0
  NumRating <- 0
  NumHygiene <- 0
  NumStructural <- 0
  NumMangement <- 0
  NumRaw <- 0
  for (j in 1:nrow(S1Data)){
    if (i!=j){
      CalcDist <- sqrt((S1Data[i,]$lat - S1Data[j,]$lat)^2+(S1Data[i,]$long - S1Data[j,]$long)^2)
      if (CalcDist < fivehundredmetres){
          NumCluster <- NumCluster + 1
          NumRating <- NumRating + S1Data[j,]$rating
          NumHygiene <- NumHygiene + S1Data[j,]$s_hygiene
          NumStructural <- NumStructural + S1Data[j,]$s_structural
          NumMangement <- NumMangement + S1Data[j,]$s_management
          NumRaw <- NumRaw + S1Data[j,]$OverallRaw
      }
    }
  }
  if (NumCluster!=0){
    MeanRatingCluster[i] = NumRating/NumCluster
    MeanHygieneCluster[i] = NumHygiene/NumCluster
    MeanStructuralCluster[i] = NumStructural/NumCluster
    MeanManagementCluster[i] = NumMangement/NumCluster
    MeanOverallRawCluster[i] = NumRaw/NumCluster
  }
  else{
    MeanRatingCluster[i] = "None"
    MeanHygieneCluster[i] = "None"
    MeanStructuralCluster[i] = "None"
    MeanManagementCluster[i] = "None"
    MeanOverallRawCluster[i] = "None"
  }
}

S1Data <- S1Data %>%
  add_column(MeanOverallRawCluster)

mysum <- S1Data %>%
  group_by(OverallRaw) %>%
  summarise(meanRating = mean(MeanRatingCluster), meanHygiene = mean(MeanHygieneCluster),
            meanStructural = mean(MeanStructuralCluster), meanManagement = mean(MeanManagementCluster)) %>%
  gather(meanHygiene, meanStructural, meanManagement, key = "WhichRaw", value = "Mean")

ggplot(mysum) + geom_point(aes(x = Mean, y = OverallRaw,col = WhichRaw)) + ylim(0, 50) + theme_classic()

saveRDS(S1Data, "data/S1Data.rds")

