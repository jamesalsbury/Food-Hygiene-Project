library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(sf)

All_postcode_areas_merged <- readRDS("data/All_postcode_areas_merged.rds")
ZoomReferenceTable <- readRDS("data/ZoomReferenceTable.rds")
merged_sp_summary <- readRDS("data/merged_sp_summary.rds")
postcodeAreas <- readRDS("data/postcodeAreas.rds")
All_data_19_Oct <- readRDS("data/All_data_19_Oct.rds")
mycolourpalette <- c("grey", "salmon", "cornflowerblue")
myratingpalette <- c("green", "lightgreen", "yellow", "orange", "red", "darkred")
my.options <- c('5', '4', '3', '2', '1', '0')


my.fun <- function() {
  res <- list()
  for (o in my.options) {
    res[[length(res)+1]] <- tags$span(o,
                                      style = paste0('color: ', myratingpalette[which(my.options == o)],';'))
  }
  res
}

ui <- fluidPage(

  titlePanel("MMathStat Project!"),

  sidebarLayout(

    sidebarPanel(
      checkboxInput(inputId = "esttoggle", label = "Establishments?"),
      conditionalPanel(condition = "input.esttoggle==1", checkboxGroupInput("ratingschosen",
                     label = c("Choose ratings to view"), choiceNames = my.fun(), choiceValues = myratingpalette)),
      uiOutput(outputId = "mytextoutput"),
      tableOutput(outputId = "mysummarystats"),
      selectInput(inputId = "mychoices", label = "Plot?", choices = c("Ratings", "Overall ratings")),
      plotOutput(outputId = "ratingsplot")
    ),

    mainPanel(
      leafletOutput("map", height=800)
    )
  )
)

# create the server
server <- function(input, output, session) {

ZoomedIn <<- FALSE
AreaClicked <<- FALSE

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

        } else { #If we clicked on a district, not a different area


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

          MyMarkers <- All_data_19_Oct %>%
            filter(postcodeDistrict==click$id) %>%
            filter(rating %in% 0:5) %>%
            filter(OverallRaw %in% 0:80) %>%
            drop_na(long) %>%
            drop_na(lat) %>%
            st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
            st_jitter(factor = 0.001)

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
                  proxy %>%
                    addCircleMarkers(data = temp, label = mytext3, col = chosenvec[i])
                }
              }
            }
          })


          output$mytextoutput <- renderUI({

            Eng_Wal_rating_mean <- All_data_19_Oct %>%
              filter(rating %in% 0:5) %>%
              summarise(mean = mean(rating))

            Eng_Wal_raw_mean <-All_data_19_Oct %>%
              filter(OverallRaw %in% 0:80) %>%
              summarise(mean = mean(OverallRaw))

            Chosen_area_rating_mean <- All_data_19_Oct %>%
              filter(postcodeArea==str_extract(click$id, "[A-Z]+")) %>%
              filter(rating %in% 0:5) %>%
              summarise(mean = mean(rating))

            Chosen_area_raw_mean <- All_data_19_Oct %>%
              filter(postcodeArea==str_extract(click$id, "[A-Z]+")) %>%
              filter(OverallRaw %in% 0:80) %>%
              summarise(mean = mean(OverallRaw))



            HTML(paste("Chosen postcode area: ", click$id, '<br>', "Mean rating for England and Wales:", round(Eng_Wal_rating_mean, 2),
                       '<br>', "Mean raw rating for England and Wales:", round(Eng_Wal_raw_mean, 2), '<br>',
                       "Mean rating for ", str_extract(click$id, "[A-Z]+"), ":", round(Chosen_area_rating_mean, 2),
                       '<br>', "Mean raw rating for ", str_extract(click$id, "[A-Z]+"), ":", round(Chosen_area_raw_mean,2)))
          })

           output$mysummarystats <- renderTable({
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

             data.frame(Mean = merged_sp_summary[[i]]@data[j,]$mean,
                        StandardDeviation = merged_sp_summary[[i]]@data[j,]$sd,
                        Count = merged_sp_summary[[i]]@data[j,]$count,
                        RawMean = merged_sp_summary[[i]]@data[j,]$rawmean)


           })
           output$ratingsplot <- renderPlot({
               if (input$mychoices == "Ratings"){
                 myRatings <- MyMarkers %>%
                   count(rating)

                 colourvec <- vector(length = nrow(myRatings))
                 for (i in 1:nrow(myRatings)){
                   colourvec[i] <- myratingpalette[which(myRatings$rating[i]==my.options)]

                 }

                 myRatings <- myRatings %>%
                   add_column(colourvec)

                 ggplot(data = myRatings, aes(rating, n)) + geom_bar(stat = "identity", fill=colourvec) +
                   ylab("Count") +  xlab("Rating") + theme_classic() + geom_text(aes(label=n),vjust=-0.3)
               } else {

                 OverallRawCount <- MyMarkers %>%
                   group_by(rating) %>%
                   count(OverallRaw)

                OverallRawCount$rating <- as_factor(OverallRawCount$rating)

                 ggplot(data = OverallRawCount, aes(x = OverallRaw, y = n, fill = rating)) +
                   geom_bar(stat="identity") + ylab("Count") +
                   xlab("Overall score") + theme_classic() + scale_fill_manual(breaks=c('5', '4', '3', '2', '1', '0'), values = c("green", "lightgreen", "yellow", "orange", "red", "darkred"))


             }
           })

        }
      }



    })


}

shinyApp(ui = ui, server = server)
