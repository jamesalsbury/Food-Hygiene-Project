library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)

getwd()
All_postcode_areas_merged <- readRDS("Shiny/All_postcode_areas_merged.rds")
ZoomReferenceTable <- readRDS("data/ShinyData/ZoomReferenceTable.rds")
merged_sp_summary <- readRDS("data/ShinyData/merged_sp_summary.rds")
postcodeAreas <- readRDS("data/ShinyData/postcodeAreas.rds")
All_data_19_Oct <- readRDS("data/ShinyData/All_data_19_Oct.rds")


ui <- fluidPage(

  titlePanel("MMathStat Project!"),

  sidebarLayout(

    sidebarPanel(
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

ZoomedIn <<- F
AreaClicked <<- F

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
                  fillColor = 2,
                  weight = 2,
                  opacity = 0.2,
                  label = mytext,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.5)

  })

    observeEvent(input$map_shape_click, {

      click <- input$map_shape_click


      if (ZoomedIn==F){ #If nothing has been clicked on yet
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
                      fillColor = "red",
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
        ZoomedIn <<- T
      } else { #If we have already zoomed into the map
        if (str_detect(click$id, "[A-Z]", negate = TRUE)){ #Work out whether we have clicked on a postcode area or district
          AreaClicked <<- T #If we have clicked on an area then areaclicked = T
        }

        if (AreaClicked == T){ #If we have clicked on another area


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
            addPolygons(data = All_postcode_areas_merged,
                        layerId = ~All_postcode_areas_merged$ID,
                        fillColor = 2,
                        weight = 2,
                        opacity = 0.2,
                        label = mytext,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.5) %>%
            removeShape(layerId = click$id) %>% #Here we remove the area of the polygon we clicked on
            addPolygons(data = merged_sp_summary[[intclick]], #Here we add the district polygons of the area we clicked on
                        layerId = merged_sp_summary[[intclick]]$name,
                        fillColor = "red",
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

          AreaClicked <<- F #Back to false in case we want to click a district next time

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
            addPolygons(data = All_postcode_areas_merged,
                        layerId = ~All_postcode_areas_merged$ID,
                        fillColor = 2,
                        weight = 2,
                        opacity = 0.2,
                        label = mytext,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.5) %>%
            removeShape(layerId = postcodeAreas[i]) %>%
            addPolygons(data = merged_sp_summary[[i]], #Here we add the district polygons of the area we clicked on
                        layerId = merged_sp_summary[[i]]@data$name,
                        fillColor = "red",
                        weight = 2,
                        opacity = 0.2,
                        label = mytext1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.5) %>%
            removeShape(layerId = merged_sp_summary[[i]]@data$name[j]) %>%
            addPolygons(data = merged_sp_summary[[i]]@polygons[[j]], #Here we add the district polygon we are interested in
                        layerId = merged_sp_summary[[i]]@data$name[j],
                        fillColor = "blue",
                        weight = 2,
                        opacity = 0.2,
                        label = mytext2,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.5)


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
                 myRatings <- All_data_19_Oct %>%
                   filter(postcodeDistrict==click$id) %>%
                   count(rating) %>%
                   filter(rating %in% 0:5)

                  ggplot(data = myRatings, aes(rating, n)) + geom_bar(stat = "identity", fill = "steelblue") +
                   ylab("Count") +  xlab("Rating") + theme_classic() + geom_text(aes(label=n),vjust=-0.3)
               } else {


                 OverallRawCount <- All_data_19_Oct %>%
                   filter(postcodeDistrict==click$id) %>%
                   filter(rating %in% 0:5) %>%
                   group_by(rating) %>%
                   count(OverallRaw)

                 ggplot(data = OverallRawCount, aes(x = OverallRaw, y = n, fill=rating)) +
                   geom_bar(stat="identity") + ylab("Count") +
                   xlab("Overall score") + theme_classic() + labs(fill="Rating")



             }
           })

        }
      }



    })


}


shinyApp(ui = ui, server = server)



