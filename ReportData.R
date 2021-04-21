library(tidyverse)
library(RColorBrewer)
############################################################
#Getting the England only Data
############################################################

All_data_19_Oct <-readRDS("data/API_dated/All_data_19_Oct.rds")

Eng_Only_data <- All_data_19_Oct %>%
  filter(Region!="Scotland") %>%
  filter(Region!="Wales") %>%
  filter(Region!="Northern Ireland")

############################################################
#Finding out the different types of establishments
############################################################

EstTypeCount <- Eng_Only_data %>%
  count(type) %>%
  arrange(desc(n))

formatter1000 <- function(x){
  x/1000
}

types = rev(EstTypeCount$type)
TypeBarChart <- ggplot(data = EstTypeCount, aes(x = type, y=n)) +
  geom_bar(stat  = "identity", fill = "steelblue")  + ylab("Count") +ylim(c(0,125000))  + scale_x_discrete(limits = types) +
  xlab("Type of establishment") + theme_classic() + geom_text(aes(label=n),hjust=-0.3, size=6) + coord_flip() + theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 40)))

TypeBarChart

png("BarChartType.png", width = 1200, height=400)
 TypeBarChart
 dev.off()

############################################################
#Finding out the different ratings of establishments
############################################################
myratingpalette <- rev(brewer.pal(n=6, name = "RdYlGn"))

RatingCount <- Eng_Only_data %>%
  filter(rating %in% 0:5) %>%
  count(rating)

Eng_Only_data %>%
  count(rating)

RatingCount

RatingBarChart <- ggplot(data = RatingCount, aes(x=rating, y=n)) + geom_bar(stat = "identity", fill = rev(myratingpalette))+
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Rating") + theme_classic() + geom_text(aes(label=n),vjust=-0.3, size=6) +  theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 40)))


png("BarChartRatings.png", width = 1200, height=400)
RatingBarChart
dev.off()


############################################################
#Finding out the different raw ratings of establishments
############################################################

OverallRawCount <- Eng_Only_data %>%
  filter(rating %in% 0:5) %>%
  group_by(rating) %>%
  count(OverallRaw)

RawBarChart <- ggplot(data = OverallRawCount, aes(x = OverallRaw, y = n, fill=rating)) + geom_bar(stat="identity") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Raw score") + theme_classic() + labs(fill="Rating") + scale_fill_manual(breaks=c('5', '4', '3', '2', '1', '0'), values = c("#1A9850", "#91CF60", "#D9EF8B", "#FEE08B" ,"#FC8D59", "#D73027")) +  theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 40)))

png("RawBarchart.png", width = 1200, height=400)
RawBarChart
dev.off()

############################################################
#Finding out the breakdown of the raw ratings
############################################################


library(fields)
library(pheatmap)


HygieneCount <- Eng_Only_data %>%
  count(s_hygiene)

HygieneBarCount <- ggplot(data = HygieneCount, aes(x = s_hygiene, y=n)) +  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Hygiene score") + theme_classic() + geom_text(aes(label=n),vjust=-0.3, size =5) + theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 20)))

StructuralCount <- Eng_Only_data %>%
  count(s_structural)

StructuralBarCount <- ggplot(data = StructuralCount, aes(x = s_structural, y=n)) +  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Structural score") + theme_classic() + geom_text(aes(label=n),vjust=-0.3, size=5) + theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 20)))

ManagementCount <- Eng_Only_data %>%
  count(s_management)

ManagementBarCount <- ggplot(data = ManagementCount, aes(x = s_management, y=n)) +  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels=formatter1000) + ylab("Count (in thousands)") +
  xlab("Management score") + theme_classic() + geom_text(aes(label=n),vjust=-0.3, size=5) + theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 20)))

png("3RawBarCharts.png", width = 1200, height=400)
require(gridExtra)
grid.arrange(HygieneBarCount, StructuralBarCount, ManagementBarCount, ncol=3)
dev.off()


Yes <- Eng_Only_data %>%
  filter(s_hygiene %in% 0:80)


Yes %>%
  filter(s_hygiene==s_structural) %>%
  filter(s_structural==s_management) %>%
  filter(s_hygiene==s_management)


Yes


Yes = Yes[,7:9]

cor(Yes$s_hygiene, Yes$s_structural)
cor(Yes$s_hygiene, Yes$s_management)
cor(Yes$s_management, Yes$s_structural)

plot(Yes$s_hygiene, Yes$s_structural)


library(ellipse)
library(RColorBrewer)

# Use of the mtcars data proposed by R
data <- cor(Yes)

# Build a Pannel of 100 colors with Rcolor Brewer
my_colors <- brewer.pal(5, "Spectral")
my_colors <- colorRampPalette(my_colors)(100)

# Order the correlation matrix
ord <- order(data[1, ])
data_ord <- data[ord, ord]
plotcorr(data_ord , col=my_colors[data_ord*50+50] , mar=c(1,1,1,1)  )



############################################################
############################################################
#Plotting the data set onto a map
############################################################
############################################################


library(leaflet)
library(geojsonio)
library(sp)
library(dplyr)
library(sf)
library(RColorBrewer)

Eng_Only_data <- Eng_Only_data %>%
  filter(rating %in% 0:5)



postcodeAreas <- c("AL" ,"B", "BA" ,"BB" ,"BD" ,"BH" ,"BL" ,"BN" ,"BR" ,"BS", "CA", "CB",
                   "CH", "CM" ,"CO" ,"CR" ,"CT" ,"CV", "CW", "DA" ,"DE", "DG","DH",
                   "DL", "DN", "DT", "DY", "E" ,"EC", "EN" ,"EX", "FY","GL" ,
                   "GU", "HA", "HD" ,"HG", "HP", "HR","HU", "HX", "IG", "IP",
                   "KT", "L", "LA", "LD", "LE", "LL","LN", "LS", "LU" ,"M", "ME", "MK",
                   "N", "NE" ,"NG" ,"NN","NP", "NR", "NW", "OL", "OX", "PE",
                   "PL", "PO", "PR", "RG", "RH", "RM", "S","SE", "SG", "SK", "SL", "SM",
                   "SN", "SO", "SP", "SR", "SS", "ST", "SW","SY", "TA", "TD", "TF", "TN", "TQ",
                   "TR", "TS", "TW", "UB", "W", "WA", "WC", "WD", "WF", "WN", "WR", "WS", "WV",
                   "YO")


bad_areas = c(10, 18, 31, 49)
goodPostcodeAreas = postcodeAreas[-bad_areas]
badPostcodeAreas = postcodeAreas[bad_areas]

sp_poly = list()
postcode_data = list()
postcode_summary = list()
merged_sp_summary  = list()

#Need geojsonio for this
for (i in seq_along(goodPostcodeAreas)) {
  #Get the spatial data for the well-behaved postcode datasets
  path = paste0("https://raw.githubusercontent.com/missinglink/uk-postcode-polygons/master/geojson/",
                goodPostcodeAreas[i], ".geojson")
  sp_poly[[goodPostcodeAreas[i]]] = geojson_read(path, what = "sp")
  sp_poly[[goodPostcodeAreas[i]]]@data = sp_poly[[goodPostcodeAreas[i]]]@data[, c("name", "description")]
}

for (i in seq_along(badPostcodeAreas)) {
  #Get the spatial data for not so well-behaved postcode datasets
  path <- paste0("data/", badPostcodeAreas[i], ".json")
  sp_poly[[badPostcodeAreas[i]]] = geojson_read(path, what = "sp")
}


for (i in seq_along(postcodeAreas)) {
  #Get the postcode data, only numeric values
  postcode_data[[postcodeAreas[i]]] <- Eng_Only_data %>%
    filter(postcodeArea == postcodeAreas[i])

  postcode_data[[postcodeAreas[i]]] <- postcode_data[[postcodeAreas[i]]] %>%
    filter(rating %in% 0:5) %>%
    filter(OverallRaw %in% 0:80)

  postcode_data[[postcodeAreas[i]]]$rating <- as.numeric(as.character(postcode_data[[postcodeAreas[i]]]$rating))

  #Get a summary of the postcode data
  postcode_summary[[postcodeAreas[i]]] <- postcode_data[[postcodeAreas[i]]] %>%
    group_by(postcodeDistrict) %>%
    summarise(mean = mean(rating), sd = sd(rating), count = n(), rawmean = mean(OverallRaw))


  #Merge the spatial and postcode summary data
  merged_sp_summary[[postcodeAreas[i]]] <- merge(sp_poly[[postcodeAreas[i]]], postcode_summary[[postcodeAreas[i]]], by.x = "name", by.y = "postcodeDistrict")

  merged_sp_summary[[postcodeAreas[i]]]@data[is.na(merged_sp_summary[[postcodeAreas[i]]]@data)] <- 0
}

#Make the non-connecting districts have the same data
badDistricts <- c(9, 28, 27, 70)
badNames <- c(2, 7, 33, 57)
for (i in seq_along(badPostcodeAreas)) {
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i],]$mean = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 2]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i] + 1,]$mean = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 2]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i],]$sd = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 3]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i] + 1,]$sd = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 3]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i],]$count = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 4]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i] + 1,]$count = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 4]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i],]$rawmean = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 5]]
  merged_sp_summary[[badPostcodeAreas[i]]]@data[badNames[i] + 1,]$rawmean = postcode_summary[[badPostcodeAreas[i]]][[badDistricts[i], 5]]
}


for (i in seq_along(postcodeAreas)) {
  if (i == 1) {
    All_postcodes_merged <- merged_sp_summary[[postcodeAreas[1]]]
  } else {
    All_postcodes_merged <- rbind(All_postcodes_merged, merged_sp_summary[[i]])
  }
}

#Just the postcodes plotted
leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom = 8) %>%
  addTiles() %>%
  addScaleBar() %>%
  addPolygons(data = All_postcodes_merged,
              fillColor = "red",
              weight = 2,
              opacity = 0.2,
              color = "red",
              dashArray = "2",
              fillOpacity = 0.2)

#Mean food hygiene ratings
bins <- seq(3.5, 5, by = 0.25)
pal_sb <- colorBin("RdYlGn", domain = All_postcodes_merged$mean, bins=bins)
mytext <- paste0(
  "Area: ", All_postcodes_merged@data$name,"<br/>",
  "Count in area: ", All_postcodes_merged@data$count, "<br/>",
  "Mean  hygiene rating: ", round(All_postcodes_merged@data$mean, 2)) %>%
  lapply(htmltools::HTML)

leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom = 8) %>%
  addTiles() %>%
  addScaleBar() %>%
  addPolygons(data = All_postcodes_merged,
              fillColor = ~pal_sb(All_postcodes_merged$mean),
              weight = 2,
              opacity = 0.2,
              label = mytext,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.5) %>%
addLegend(pal = pal_sb,
            values = All_postcodes_merged$mean,
            position = "bottomleft",
            title = "Mean hygiene rating")


hist(All_postcodes_merged@data$rawmean)

#Mean food hygiene ratings
bins <- seq(1, 21, by = 4)
pal_sb <- colorBin("RdYlGn", domain = All_postcodes_merged$rawmean, bins=bins, reverse = TRUE)
mytext <- paste0(
  "Area: ", All_postcodes_merged@data$name,"<br/>",
  "Count in area: ", All_postcodes_merged@data$count, "<br/>",
  "Mean  hygiene rating: ", round(All_postcodes_merged@data$rawmean, 2)) %>%
  lapply(htmltools::HTML)

leaflet() %>%
  setView(lng = -0.75, lat = 53, zoom = 8) %>%
  addTiles() %>%
  addScaleBar() %>%
  addPolygons(data = All_postcodes_merged,
              fillColor = ~pal_sb(All_postcodes_merged$rawmean),
              weight = 2,
              opacity = 0.2,
              label = mytext,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.5) %>%

  addLegend(pal = pal_sb,
            values = All_postcodes_merged$rawmean,
            position = "bottomleft",
            title = "Mean overall raw score")


############################################################
############################################################
#Combining deprivation data and data set
############################################################
############################################################
library(tidyverse)

DepData <- read_csv("data/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv")
PostcodeData <- read_csv("data/PCD_OA_LSOA_MSOA_LAD_AUG20_UK_LU.csv")
PostcodeDepMerged <- inner_join(PostcodeData, DepData, by = c("lsoa11cd" = "LSOA code (2011)"))
Eng_Only_data <- readRDS("data/Eng_Only_data.rds")
EstDepMerged <- inner_join(Eng_Only_data, PostcodeDepMerged, by = c("postcode" = "pcds"))

 rank <- DepData$`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`
 score <- DepData$`Index of Multiple Deprivation (IMD) Score`

 MyDepData = data.frame(name =DepData$`LSOA name (2011)`, rank <- DepData$`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`,
                        score <- DepData$`Index of Multiple Deprivation (IMD) Score` )

 MyDepDataPlot <- ggplot(data = MyDepData, aes(x = rank, y=score)) +  geom_point() +
     ylab("Score (where lower is better)") +
   xlab("Rank (where higher is better)") + theme_classic() + theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 20)))


MyDepDataPlot

png("DepScore.png", width = 1200, height=400)
MyDepDataPlot
dev.off()

############################################################
############################################################
#Modelling of deprivation data
############################################################
############################################################
library(tidyverse)
library(ordinal)
library(MASS)
library(RColorBrewer)
EstDepMerged <- readRDS("data/EstDepMerged.rds")
myratingpalette <- rev(brewer.pal(n=6, name = "RdYlGn"))


#Identify chains
EstDepMerged <- EstDepMerged %>%
  mutate(chain = str_detect(name,"(?i)^Gregg", ) | str_detect(name,"(?i)^Domino", ) | str_detect(name,"(?i)^Burger King", ) | str_detect(name,"(?i)^Mcdonal", )
         | str_detect(name,"(?i)^KFC", ) | str_detect(name,"(?i)^Pizza Hut", ) | str_detect(name,"(?i)^Subway", ) | str_detect(name,"(?i)^Costa", )
         |str_detect(name,"(?i)^Toby Car", ) | str_detect(name,"(?i)^Bella Ital", ) | str_detect(name,"(?i)^PizzaE", ) | str_detect(name,"(?i)^Nando", )
         |str_detect(name,"(?i)^Harvester", ) | str_detect(name,"(?i)^TGI F", ) | str_detect(name,"(?i)^Wether", ) | str_detect(name,"(?i)^KrispyK", ) |
       str_detect(name,"(?i)^Caffe Nero", ) | str_detect(name,"(?i)^Hard Rock Cafe", ) | str_detect(name,"(?i)^Frankie and B", )|
          str_detect(name,"(?i)^Harry Ramsdens", ) | str_detect(name,"(?i)^Asda", )
         |str_detect(name,"(?i)^Tesco", ) | str_detect(name,"(?i)^Morrison", ) | str_detect(name,"(?i)^Sainsbury", ))

#Only consider ratings between 0-5:
EstDepMerged  <- EstDepMerged %>%
  filter(rating %in% 0:5)

#Look at distributions of chains and not chains
chain = EstDepMerged %>%
  filter(chain==TRUE) %>%
  count(rating) %>%
  add_row(rating="0", n=0, .before = 1) %>%
  mutate(prop = round(n/sum(n),3))


ChainBC <- ggplot(data = chain, aes(x=rating, y=prop)) + geom_bar(stat = "identity", fill = rev(myratingpalette)) + ylim(0,1) +
  theme_classic() + xlab("Rating") + ylab("Proportion") + geom_text(aes(label=prop),vjust=-0.3, size=6) + theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 40)))

notchain = EstDepMerged %>%
  filter(chain==FALSE) %>%
  count(rating) %>%
  mutate(prop = round(n/sum(n),3))


NotChainBC <- ggplot(data = notchain, aes(x=rating, y=prop)) + geom_bar(stat = "identity", fill = rev(myratingpalette)) + ylim(0,1) +
  theme_classic() + xlab("Rating") + ylab("Proportion") + geom_text(aes(label=prop),vjust=-0.3, size=6) + theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 40)))


png("Chains.png", width = 1200, height=400)
require(gridExtra)
grid.arrange(ChainBC, NotChainBC, ncol=2)
dev.off()

EstDepMerged %>%
  count(chain)

most <- EstDepMerged %>%
  filter(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`%in% 1:100) %>%
  count(rating) %>%
  mutate(prop = round(n/sum(n),3))

sum(least$n)
MostBC <- ggplot(data = most, aes(x=rating, y=prop)) + geom_bar(stat = "identity", fill = rev(myratingpalette)) + ylim(0,1) +
  theme_classic() + xlab("Rating") + ylab("Proportion") + geom_text(aes(label=prop),vjust=-0.3, size=6) + theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 40)))


least <- EstDepMerged %>%
  filter(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`%in% 32744:32844) %>%
  count(rating)  %>%
  add_row(rating="0", n=0, .before = 1) %>%
  mutate(prop = round(n/sum(n),3))

LeastBC <- ggplot(data = least, aes(x=rating, y=prop)) + geom_bar(stat = "identity", fill = rev(myratingpalette)) + ylim(0,1) +
  theme_classic() + xlab("Rating") + ylab("Proportion") + geom_text(aes(label=prop),vjust=-0.3, size=6) + theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 40)))

png("DeprivationBC.png", width = 1200, height=400)
require(gridExtra)
grid.arrange(MostBC, LeastBC, ncol=2)
dev.off()


least <- EstDepMerged %>%
  filter(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`%in% 32744:32844) %>%
  count(chain)

most <- EstDepMerged %>%
  filter(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`%in% 1:100) %>%
  count(chain)
#Ordinal regression

EstDepMerged$rating <- as_factor(as.character(EstDepMerged$rating))

#Needs to be rescaled
clm <- clm(formula = fct_rev(rating)~`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`, data = EstDepMerged)
summary(clm)

#Works
 clmlog <- clm(formula = fct_rev(rating)~log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`), data = EstDepMerged)
summary(clmlog)

#Is a chain
clmchain <- clm(formula = fct_rev(rating)~chain, data = EstDepMerged)
summary(clmchain)


#Types of establishments
clmtype <- clm(formula = fct_rev(rating)~type, data = EstDepMerged)
summary(clmtype)

clminteraction <- clm(formula = fct_rev(rating)~log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`)*type, data = EstDepMerged)
summary(clminteraction)

clmregion<- clm(formula = fct_rev(rating)~log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`)+Region+type, data = EstDepMerged)
summary(clmregion)

clmregioninteract <- clm(formula = fct_rev(rating)~log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`)*Region, data = EstDepMerged)
summary(clmregioninteract)

allclm <- clm(formula = fct_rev(rating)~log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`)*Region*type, data = EstDepMerged)
summary(allclm)


fullclm <- clm(formula = fct_rev(rating)~log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`)+chain+type+local, data = EstDepMerged)
s = summary(fullclm)
coef =coef(s)
coef = coef[8:20,]
coef

EstDepMerged$local
names = rownames(coef)

coef <- coef%>%
  as_tibble() %>%
  add_column(names)
coef <- coef %>%
  mutate(types = substring(names, 5))
coef
types = rev(coef$types)
TypesRegression <- ggplot(data = coef, aes(x = types, y=Estimate)) +
  geom_point(colour="red")+scale_x_discrete(limits = types)  +theme_classic()  + coord_flip() + xlab("Type of establishment") + ylab("Regression Estimate") +
  geom_errorbar(aes(ymin = Estimate-2*`Std. Error`, ymax = Estimate+2*`Std. Error`)) +theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 40)))

png("TypesRegression.png", width = 1200, height=400)
TypesRegression
dev.off()


EstDepMerged %>%
  count(authorityName)

clmauth <- clm(formula = fct_rev(rating)~log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`)+type+authorityName, data=EstDepMerged)
summary(clmauth)

saveRDS(clmauth, "data/FullAuthorityModel.rds")



EstDepMerged %>%
  filter(authorityName=="Adur") %>%
  count(rating)

chainfull <- clm(formula = fct_rev(rating)~log(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`)+type+authorityName+chain, data=EstDepMerged)
summary(chainfull)

head(FinalCLM$Estimate)

LAPlot <- ggplot(coef) +
  geom_errorbar(aes(y = rank, xmin = lower, xmax = upper), alpha = 0.2) +
  geom_point(aes(y = rank, x = Estimate, colour = Region), size = 0.5) +
  theme_minimal() +
  geom_rug(aes(y = rank, x = Estimate, colour = Region), sides = "l") +
  theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 40)))

png("LAPlot.png", width = 1200, height=400)
LAPlot
dev.off()






FinalCLM


##############################
#Looking at regions too
##############################
library(MASS)
library(ordinal)
library(ggplot2)
library(tidyverse)

#Looking at full model; ratings ~ dep data + type + local authority
mycoef = readRDS("data/mycoef.rds")

ggplot(mycoef) + geom_point(aes(x = Estimate, y = rank, col=myRegion)) + geom_point(data = mycoef, aes(x = upper, y = rank, col=myRegion))+
  geom_point(data = mycoef, aes(x = lower, y = rank, col=myRegion))

ggplot(mycoef) +
  geom_errorbar(aes(y = rank, xmin = lower, xmax = upper), alpha = 0.2) +
  geom_point(aes(y = rank, x = Estimate, colour = myRegion), size = 0.5) +
  theme_minimal() +
  geom_rug(aes(y = rank, x = Estimate, colour = myRegion), sides = "l")


summarystatsfull <- coef %>%
  group_by(myRegion) %>%
  summarise(median = median(rank), mean = mean(rank))


#Looking at just local authorities; ratings ~ local authority

AuthorityCLM = readRDS("data/AuthorityCLM.rds")

ggplot(AuthorityCLM) + geom_point(aes(x = Coef, y = myRank, col=myRegion))+ geom_point(data = AuthorityCLM, aes(x = upper, y = myRank, col=myRegion))+
  geom_point(data = AuthorityCLM, aes(x = lower, y = myRank, col=myRegion))

summarystatsauth <- AuthorityCLM %>%
  group_by(myRegion) %>%
  summarise(median = median(myRank), mean = mean(myRank))

summary(clmfullchain)



############################################################
############################################################
#Clustering
############################################################
############################################################


#Sheffield
S1 <- Eng_Only_data %>%
  filter(postcodeDistrict=="S1") %>%
  filter(s_hygiene %in% 0:80) %>%
  filter(!is.na(long))

meanhygienevector = vector(length=396)
meanstructuralvector = vector(length=396)
meanmanagementvector = vector(length=396)
meanrawvector = vector(length=396)
meanratingvector = vector(length=396)
hygienesum = 0
structuralsum = 0
managementsum = 0
rawsum =  0
ratingsum = 0
count = 0

for (i in 1:396){
  for (j in 1:396){
    if (i!=j){
      dist = sqrt((S1[i,]$long - S1[j,]$long)^2+(S1[i,]$lat - S1[j,]$lat)^2)
      if (dist < fivehundred){
        count = count + 1
        hygienesum = hygienesum + S1[j,]$s_hygiene
        structuralsum = structuralsum + S1[j,]$s_structural
        managementsum = managementsum + S1[j,]$s_management
        rawsum = rawsum + S1[j,]$OverallRaw
        ratingsum = ratingsum + S1[j,]$rating
      }
    }
  }
  if (count!=0){
    meanhygienevector[i] = hygienesum/count
    meanstructuralvector[i] = structuralsum/count
    meanmanagementvector[i] = managementsum/count
    meanrawvector[i] = rawsum/count
    meanratingvector[i] = ratingsum/count
  } else{
    meanhygienevector[i] = NA
    meanstructuralvector[i] = NA
    meanmanagementvector[i] = NA
    meanrawvector[i] = NA
    meanratingvector[i] = NA
  }
  hygienesum = 0
  structuralsum = 0
  managementsum = 0
  rawsum =  0
  ratingsum = 0
  count = 0
}

S1Data = S1 %>%
  group_by(OverallRaw) %>%
  summarise(Rating = mean(meanratingvector), Hygiene = mean(meanhygienevector), Structural = mean(meanstructuralvector),
            Management = mean(meanmanagementvector), Raw = mean(meanrawvector))

plot(S1Data$OverallRaw, S1Data$Raw)



Sheff1 <- ggplot(data = S1Data, aes(x=OverallRaw, y=Rating)) + geom_point(colour="red", size=2) + xlab("Raw score (grouped)") +
  ylab("Mean rating (higher the better)") + theme_classic() + theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 40)))

Sheff2 <- ggplot(data = S1Data, aes(x=OverallRaw, y=Raw)) + geom_point(colour="red", size=2) + xlab("Raw score (grouped)") +
  ylab("Mean raw score (lower the better)") + theme_classic() + theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 40)))


png("SheffieldClustering.png", width = 1200, height=400)
require(gridExtra)
grid.arrange(Sheff1, Sheff2, ncol=2)
dev.off()

#Newcastle
NE1 <- Eng_Only_data %>%
  filter(postcodeDistrict=="NE1") %>%
  filter(s_hygiene %in% 0:80) %>%
  filter(!is.na(long))



meanhygienevector = vector(length=601)
meanstructuralvector = vector(length=601)
meanmanagementvector = vector(length=601)
meanrawvector = vector(length=601)
meanratingvector = vector(length=601)
hygienesum = 0
structuralsum = 0
managementsum = 0
rawsum =  0
ratingsum = 0
count = 0

for (i in 1:601){
  for (j in 1:601){
    if (i!=j){
      dist = sqrt((NE1[i,]$long - NE1[j,]$long)^2+(NE1[i,]$lat - NE1[j,]$lat)^2)
      if (dist < fivehundred){
        count = count + 1
        hygienesum = hygienesum + NE1[j,]$s_hygiene
        structuralsum = structuralsum + NE1[j,]$s_structural
        managementsum = managementsum + NE1[j,]$s_management
        rawsum = rawsum + NE1[j,]$OverallRaw
        ratingsum = ratingsum + NE1[j,]$rating
      }
    }
  }
  if (count!=0){
    meanhygienevector[i] = hygienesum/count
    meanstructuralvector[i] = structuralsum/count
    meanmanagementvector[i] = managementsum/count
    meanrawvector[i] = rawsum/count
    meanratingvector[i] = ratingsum/count
  } else{
    meanhygienevector[i] = NA
    meanstructuralvector[i] = NA
    meanmanagementvector[i] = NA
    meanrawvector[i] = NA
    meanratingvector[i] = NA
  }
  hygienesum = 0
  structuralsum = 0
  managementsum = 0
  rawsum =  0
  ratingsum = 0
  count = 0
}
NE1


NE1Data = NE1 %>%
  group_by(OverallRaw) %>%
  summarise(Rating = mean(meanratingvector), Hygiene = mean(meanhygienevector), Structural = mean(meanstructuralvector),
            Management = mean(meanmanagementvector), Raw = mean(meanrawvector))


Newc1 <-  ggplot(data = NE1Data, aes(x=OverallRaw, y=Rating)) + geom_point(colour="red", size=2) + xlab("Raw score (grouped)") +
  ylab("Mean rating (higher the better)") + theme_classic() + theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 40)))

Newc2 <- ggplot(data = NE1Data, aes(x=OverallRaw, y=Raw)) + geom_point(colour="red", size=2) + xlab("Raw score (grouped)") +
  ylab("Mean raw score (lower the better)") + theme_classic() + theme(text = element_text(size = 25), axis.title.y = element_text(margin = margin(r = 40)))

Newc2

png("NewcClustering.png", width = 1200, height=400)
require(gridExtra)
grid.arrange(Newc1, Newc2, ncol=2)
dev.off()

NE1Count <- NE1 %>%
  count(type) %>%
  mutate(prop = n/sum(n))

S1Count <- S1 %>%
  count(type)%>%
  mutate(prop = n/sum(n))

S1Count
NE1Count

NE1%>%
  count(chain)








S1 <- S1 %>%
  mutate(chain = str_detect(name,"(?i)^Gregg", ) | str_detect(name,"(?i)^Domino", ) | str_detect(name,"(?i)^Burger King", ) | str_detect(name,"(?i)^Mcdonal", )
         | str_detect(name,"(?i)^KFC", ) | str_detect(name,"(?i)^Pizza Hut", ) | str_detect(name,"(?i)^Subway", ) | str_detect(name,"(?i)^Costa", )
         |str_detect(name,"(?i)^Toby Car", ) | str_detect(name,"(?i)^Bella Ital", ) | str_detect(name,"(?i)^PizzaE", ) | str_detect(name,"(?i)^Nando", )
         |str_detect(name,"(?i)^Harvester", ) | str_detect(name,"(?i)^TGI F", ) | str_detect(name,"(?i)^Wether", ) | str_detect(name,"(?i)^KrispyK", ) |
           str_detect(name,"(?i)^Caffe Nero", ) | str_detect(name,"(?i)^Hard Rock Cafe", ) | str_detect(name,"(?i)^Frankie and B", )|
           str_detect(name,"(?i)^Harry Ramsdens", ) | str_detect(name,"(?i)^Asda", )
         |str_detect(name,"(?i)^Tesco", ) | str_detect(name,"(?i)^Morrison", ) | str_detect(name,"(?i)^Sainsbury", ))




library(ggplot2)
SheffMeanCluster <- readRDS("data/SheffMeanCluster.rds")
SheffMeanCluster$OverallRaw = as.numeric(levels(SheffMeanCluster$OverallRaw))[SheffMeanCluster$OverallRaw]
myrawsummary <- SheffMeanCluster %>%
  group_by(OverallRaw) %>%
  summarise(meanrating = mean(MeanRatingCluster), hygiene = mean(MeanHygieneCluster), structural=mean(MeanStructuralCluster),management = mean(MeanManagementCluster), count=n())


ggplot(myrawsummary, aes(x = meanrating, y = OverallRaw)) + geom_point(col = "blue") + ylim(0, 50) + theme_classic() +
  ylab("Overall Raw (Lower the better)") + xlab("Mean rating of nearby establishments (higher the better)")

ggplot(myrawsummary, aes(x = hygiene, y = OverallRaw)) + geom_point(col = "blue") + ylim(0, 50) + theme_classic() +
  ylab("Overall Raw (Lower the better)") + xlab("Hygiene rating (lower the better)")

ggplot(myrawsummary, aes(x = structural, y = OverallRaw)) + geom_point(col = "blue") + ylim(0, 50) + theme_classic() +
  ylab("Overall Raw (Lower the better)") + xlab("Structural rating (lower the better)")

ggplot(myrawsummary) + geom_point(aes(x = management, y = OverallRaw),col = "blue") + ylim(0, 50) + theme_classic() +
  ylab("Overall Raw (Lower the better)") + xlab("Management rating (lower the better)") + geom_point(data = myrawsummary, aes(x = hygiene, y = OverallRaw), col="green") +
  geom_point(data = myrawsummary, aes(x = structural, y = OverallRaw), col="red")

myrawsummary


summary <- All_data_19_Oct %>%
  filter(OverallRaw %in% 0:80) %>%
  summarise(meanrating = mean(rating), hygiene = mean(s_hygiene), structural=mean(s_structural),management = mean(s_management), count=n())
summary

plot(summary$OverallRaw, summary$management)

mean(All_data_19_Oct$s_hygiene)

myrawsummary1 <- MeanData %>%
  group_by(OverallRaw) %>%
  summarise(meanrating = mean(MeanRatingCluster), hygiene = mean(MeanHygieneCluster), structural=mean(MeanStructuralCluster),management = mean(MeanManagementCluster), count=n())
myrawsummary1

ggplot(myrawsummary1) + geom_point(aes(x = management, y = OverallRaw),col = "blue") + ylim(0, 50) + theme_classic() +
  ylab("Overall Raw (Lower the better)") + xlab("Management rating (lower the better)") + geom_point(data = myrawsummary1, aes(x = hygiene, y = OverallRaw), col="green") +
  geom_point(data = myrawsummary1, aes(x = structural, y = OverallRaw), col="red")



################################################
#Summary stats deprivation data
################################################

score <- EstDepMerged$`Index of Multiple Deprivation (IMD) Score`
score <- sort(score)
plot(score, xlab = "LSOA sorted by score", ylab="Overall deprivation score", ylim=c(0,100))
min(score)

rank <- EstDepMerged$`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`
plot(sort(rank))
max(rank)




