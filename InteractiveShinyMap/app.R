library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(sf)
library(RColorBrewer)
'%ni%' <- Negate('%in%')

All_postcode_areas_merged <- readRDS("data/All_postcode_areas_merged.rds")
ZoomReferenceTable <- readRDS("data/ZoomReferenceTable.rds")
merged_sp_summary <- readRDS("data/merged_sp_summary.rds")
postcodeAreas <- readRDS("data/postcodeAreas.rds")
Eng_Only_data <- readRDS("data/Eng_Only_data.rds")
Eng_Only_data <- Eng_Only_data %>%
  filter(rating %in% 0:5)
Eng_Only_data$rating <- as.numeric(Eng_Only_data$rating)
mycolourpalette <- c("grey", "salmon", "cornflowerblue")
myratingpalette <- rev(brewer.pal(n=6, name = "RdYlGn"))
my.options <- 5:0


my.fun <- function() {
  res <- list()
  for (o in my.options) {
    res[[length(res)+1]] <- tags$span(o,
                                      style = paste0('color: ', myratingpalette[which(my.options == o)],';'))
  }
  res
}

ui <- fluidPage(

  titlePanel("Food Hygiene Ratings"),

  sidebarLayout(

    sidebarPanel(
      checkboxInput(inputId = "esttoggle", label = "Establishments?"),
      conditionalPanel(condition = "input.esttoggle==1", checkboxGroupInput("ratingschosen",
                     label = c("Choose ratings to view"), choiceNames = my.fun(), choiceValues = myratingpalette)),
      uiOutput(outputId = "mytextoutput"),
      tableOutput(outputId = "mysummarystats1"),
      selectInput(inputId = "mychoices", label = "Plot?", choices = c("Ratings", "Overall ratings")),
      plotOutput(outputId = "ratingsplot")
    ),

    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Getting Started", uiOutput("helptext")),
                  tabPanel("Map", leafletOutput("map", height=800))
      )
    )
  )
)

# create the server
server <- function(input, output, session) {

ZoomedIn <<- FALSE
AreaClicked <<- FALSE
NoEst <<- FALSE


  output$helptext <- renderUI({
    # paste("To get started, click on a postcode area on the map.",
    #
    # HTML("<ul><li>...text...</li><li>...more text...</li></ul>"))

    helptext <- paste0("<font size=4>","<br/>",
      " At any time, if you hover your mouse over an area, the map will tell you what the area is before clicking on it.","<br/>","<br/>", "So to get started, go to the 'Map' tab and click on a postcode area on the map.","<br/>","<br/>",
   "Once you have clicked on a postcode area, you can either:", "<ul><li>Click on a postcode district inside that postcode area</li><li>Click on different postcode area</li></ul>", "<br/>",
   "When you click on a postcode district, the panel on the left-hand side will show some summary statistics associated with that postcode district. It will show:",
   "<ul><li>An establishment toggle which if clicked, shows the possible ratings of establishments (0 to 5). These individual ratings toggles can then be clicked, and then the map will show the establishments in the postcode district with the associated rating. These will be the same colour as shown in the plots and the text of the ratings. Multiple ratings can be shown simulatenously too.</li><li>The number of establishments in the chosen postcode district</li><li>The mean Food Hygiene rating (where higher is better) for: <ul><li> England</li><li>The postcode area in which the postcode district lies in</li><li>The chosen postcode district</li></ul><li>The mean overall raw rating (where lower is better) for:
   <ul><li>England</li><li>The postcode area in which the postcode district lies in</li><li>The chosen postcode district</li></ul><li>A bar chart of either:<ul><li>The count of ratings in postcode district</li><li>The raw ratings in postcode district, coloured by rating</li></ul>") %>%
      lapply(htmltools::HTML)
  })










  ###This loads the map when Shiny starts
  output$map <- renderLeaflet({

    mytext <- paste0(
       All_postcode_areas_merged@data$name,"<br/>") %>%
      lapply(htmltools::HTML)

    leaflet() %>%
      setView(lng = -0.75, lat = 53, zoom = 8) %>%
      addTiles() %>%
      addScaleBar() %>%
      addPolygons(data = All_postcode_areas_merged,
                  layerId = ~All_postcode_areas_merged$ID,
                  fillColor = mycolourpalette[1],
                  weight = 2,
                  opacity = 0.2,
                  label = mytext,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.5)

  })

    observeEvent(input$map_shape_click, {

      click <- input$map_shape_click


      if (ZoomedIn==FALSE){ #If nothing has been clicked on yet
        for (i in 1:nrow(ZoomReferenceTable)){ #We find out where to zoom to
          if (ZoomReferenceTable[i,]$IDPostcodeArea == click$id){
            this_shape <- ZoomReferenceTable[i,]
          }
        }


    intclick <- as.numeric(click$id)



    mytext1 <- paste0(
      merged_sp_summary[[intclick]]$name,"<br/>") %>%
      lapply(htmltools::HTML)


      #Clear the area that has been clicked on
      #Show the districts in the area that have been clicked on
        leafletProxy("map") %>%
          removeShape(layerId = click$id) %>%
          addPolygons(data = merged_sp_summary[[intclick]],
                      layerId = merged_sp_summary[[intclick]]$name,
                      fillColor = mycolourpalette[2],
                      weight = 2,
                      opacity = 0.2,
                      label = mytext1,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 0.5) %>%
        setView(
          lng = this_shape$lng,
          lat = this_shape$lat,
          zoom = 9.3)

        #We have now zoomed into the map
        ZoomedIn <<- TRUE
      } else { #If we have already zoomed into the map
        if (str_detect(click$id, "[A-Z]", negate = TRUE)){ #Work out whether we have clicked on a postcode area or district
          AreaClicked <<- TRUE #If we have clicked on an area then areaclicked = T
        }

        if (AreaClicked == TRUE){ #If we have clicked on another area


          for (i in 1:nrow(ZoomReferenceTable)){
            if (ZoomReferenceTable[i,]$IDPostcodeArea == click$id){
              this_shape <- ZoomReferenceTable[i,]
            }
          }
          mytext <- paste0(
            All_postcode_areas_merged@data$name,"<br/>") %>%
            lapply(htmltools::HTML)

          intclick <- as.numeric(click$id)

          mytext1 <- paste0(
            merged_sp_summary[[intclick]]$name,"<br/>") %>%
            lapply(htmltools::HTML)


          #We need to clear the map, then add all polygons again

          leafletProxy("map") %>%
            clearShapes() %>%
            clearMarkers() %>%
            addPolygons(data = All_postcode_areas_merged,
                        layerId = ~All_postcode_areas_merged$ID,
                        fillColor = mycolourpalette[1],
                        weight = 2,
                        opacity = 0.2,
                        label = mytext,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.5) %>%
            removeShape(layerId = click$id) %>% #Here we remove the area of the polygon we clicked on
            addPolygons(data = merged_sp_summary[[intclick]], #Here we add the district polygons of the area we clicked on
                        layerId = merged_sp_summary[[intclick]]$name,
                        fillColor = mycolourpalette[2],
                        weight = 2,
                        opacity = 0.2,
                        label = mytext1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.5) %>%
            setView(
              lng = this_shape$lng,
              lat = this_shape$lat,
              zoom = 9.3)

          AreaClicked <<- FALSE #Back to false in case we want to click a district next time

          MyMarkers = c()

        } else { #If we clicked on a district, not a different area


          print(click$id)

          #Work out which exact district we clicked on
          for (i in 1:length(postcodeAreas)){
            if (str_extract(click$id, "[A-Z]+") == postcodeAreas[i]){
              break
            }
          }

          for (j in 1:length(merged_sp_summary[[i]])){
            if (click$id == merged_sp_summary[[i]]@data$name[j]){
              break
            }
          }

          MyMarkers <- Eng_Only_data %>%
            filter(postcodeDistrict==click$id) %>%
            filter(rating %in% 0:5) %>%
            filter(OverallRaw %in% 0:80) %>%
            drop_na(long) %>%
            drop_na(lat)

          if (nrow(MyMarkers)==0){
            NoEst = TRUE
          } else{
            NoEst = FALSE
          }

          if (NoEst==FALSE){
            MyMarkers <- MyMarkers %>%
              st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
              st_jitter(factor = 0.001)
          }

        #We need to turn the polygon of the district we clicked blue


          mytext <- paste0(
            All_postcode_areas_merged@data$name,"<br/>") %>%
            lapply(htmltools::HTML)



          mytext1 <- paste0(
            merged_sp_summary[[i]]@data$name,"<br/>") %>%
            lapply(htmltools::HTML)

          mytext2 <- paste0(
            merged_sp_summary[[i]]@data$name[j],"<br/>") %>%
            lapply(htmltools::HTML)


          leafletProxy("map") %>%
            clearShapes() %>%
          clearMarkers() %>%
            addPolygons(data = All_postcode_areas_merged,
                        layerId = ~All_postcode_areas_merged$ID,
                        fillColor = mycolourpalette[1],
                        weight = 2,
                        opacity = 0.2,
                        label = mytext,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.5) %>%
            removeShape(layerId = postcodeAreas[i]) %>%
            addPolygons(data = merged_sp_summary[[i]], #Here we add the district polygons of the area we clicked on
                        layerId = merged_sp_summary[[i]]@data$name,
                        fillColor = mycolourpalette[2],
                        weight = 2,
                        opacity = 0.2,
                        label = mytext1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.5) %>%
            removeShape(layerId = merged_sp_summary[[i]]@data$name[j]) %>%
            addPolygons(data = merged_sp_summary[[i]]@polygons[[j]], #Here we add the district polygon we are interested in
                        layerId = merged_sp_summary[[i]]@data$name[j],
                        fillColor = mycolourpalette[3],
                        weight = 2,
                        opacity = 0.2,
                        label = mytext2,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.5)



          #We need to add establishments if toggle if true

          #Show the establishments if toggle = TRUE
          observe({

            if (NoEst==FALSE){
              proxy <- leafletProxy("map")
              proxy %>%
                clearMarkers()
              if (input$esttoggle) { #If TRUE
                if (length(input$ratingschosen)!=0){
                  chosenvec <- input$ratingschosen
                  proxy %>%
                    clearMarkers()
                  for (i in 1:length(chosenvec)){
                    ratingchosen <- my.options[which(chosenvec[i]== myratingpalette)]
                    temp <- MyMarkers %>%
                      filter(rating==ratingchosen)
                    mytext3 <- paste0(
                      "Name: ", temp$name,"<br/>",
                      "Rating: ", temp$rating, "<br/>",
                      "Raw rating: ", temp$OverallRaw) %>%
                      lapply(htmltools::HTML)
                    if (nrow(temp)!=0){
                      proxy %>%
                        addCircleMarkers(data = temp, label = mytext3, col = chosenvec[i])
                    }

                  }
                }
              }
            }

          })


          output$mytextoutput <- renderUI({
            for (i in 1:length(postcodeAreas)){
              if (str_extract(click$id, "[A-Z]+") == postcodeAreas[i]){
                break
              }
            }

            for (j in 1:length(merged_sp_summary[[i]])){
              if (click$id == merged_sp_summary[[i]]@data$name[j]){
                break
              }
            }



            HTML(paste("Chosen postcode area: ", click$id), ", Count in area: ", nrow(MyMarkers))
          })

           output$mysummarystats1 <- renderTable({

             if (NoEst==FALSE){
               for (i in 1:length(postcodeAreas)){
                 if (str_extract(click$id, "[A-Z]+") == postcodeAreas[i]){
                   break
                 }
               }

               for (j in 1:length(merged_sp_summary[[i]])){
                 if (click$id == merged_sp_summary[[i]]@data$name[j]){
                   break
                 }
               }

               Eng_rating_mean <- Eng_Only_data %>%
                 filter(rating %in% 0:5) %>%
                 summarise(mean = mean(rating))

               Chosen_area_rating_mean <- Eng_Only_data %>%
                 filter(postcodeArea==str_extract(click$id, "[A-Z]+")) %>%
                 filter(rating %in% 0:5) %>%
                 summarise(mean = mean(rating))


               Chosen_district_mean <- MyMarkers %>%
                 filter(rating %in% 0:5) %>%
                 summarise(mean = mean(rating))

               Eng_raw_mean <-Eng_Only_data %>%
                 filter(OverallRaw %in% 0:80) %>%
                 summarise(mean = mean(OverallRaw))

               Chosen_area_raw_mean <- Eng_Only_data %>%
                 filter(postcodeArea==str_extract(click$id, "[A-Z]+")) %>%
                 filter(OverallRaw %in% 0:80) %>%
                 summarise(mean = mean(OverallRaw))

               Chosen_district_raw_mean <- MyMarkers %>%
                 filter(rating %in% 0:5) %>%
                 summarise(mean = mean(OverallRaw))

               Country = c(Eng_rating_mean$mean, Eng_raw_mean$mean)
               Area = c(Chosen_area_rating_mean$mean, Chosen_area_raw_mean$mean)
               District = c(Chosen_district_mean$mean, Chosen_district_raw_mean$mean)


               df <- data.frame(Meanrating = c("Mean Food Hygiene Rating:", "Mean Overall Raw Rating:"),
                                CountryMean = Country,
                                AreaMean = Area,
                                DistrictMean = District)

               names(df) <- c("Area:", "England", paste("", str_extract(click$id, "[A-Z]+")), paste("", click$id))

               return(df)
             }


           })

           output$ratingsplot <- renderPlot({

             if (NoEst == FALSE){
               if (input$mychoices == "Ratings"){
                 myRatings <- MyMarkers %>%
                   count(rating)

                 n <- vector(length = 6)
                 rating = 0:5


                 for (i in 1:6){
                   if ((i-1) %in% myRatings$rating){
                     n[i] = myRatings[which(myRatings$rating==(i-1)),2]$n
                   }
                   else {
                     n[i] = 0
                   }
                 }


                 myRatings <- data.frame(rating = rating, count = as.numeric(n), colourvec = rev(myratingpalette))

                 ggplot(data = myRatings, aes(rating, count)) + geom_bar(stat = "identity", fill=rev(myratingpalette)) +
                   ylab("Count") +  xlab("Rating") + theme_classic() + geom_text(aes(label=n),vjust=-0.3)
               } else {

                 OverallRawCount <- MyMarkers %>%
                   group_by(rating) %>%
                   count(OverallRaw)

                 OverallRawCount$rating <- as_factor(OverallRawCount$rating)

                 ggplot(data = OverallRawCount, aes(x = OverallRaw, y = n, fill = rating)) +
                   geom_bar(stat="identity") + ylab("Count") +
                   xlab("Overall score") + theme_classic() + scale_fill_manual(breaks=5:0, values = myratingpalette)


               }
             }

           })

        }
      }


    })


}

shinyApp(ui = ui, server = server)

