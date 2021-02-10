
library(shiny)
library(tidyverse)
library(leaflet)


All_postcodes_merged <- readRDS("data/All_postcodes_merged.rds")



ui <- fluidPage(selectInput(inputId = "myView", label = "Select parameter:",
                            choices = c("Mean rating", "Number of establishments")),
                leafletOutput(outputId = "myMap", width = "1400px",
                              height = "800px"))




server <-  function(input, output){

  #Assign output$distPlot with renderLeaflet object
  output$myMap <- renderLeaflet({

    basemap = leaflet() %>%
      addTiles()

    Parameter <- input$myView

    if (Parameter=="Mean rating"){
      bins <- seq(3.5, 5, by = 0.25)
      pal_sb <- colorBin("RdYlGn", domain = All_postcodes_merged$mean, bins=bins)
      mytext <- paste0(
        "Area: ", All_postcodes_merged@data$name,"<br/>",
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
                  position = "bottomright",
                  title = "Mean hygiene rating")
    }

     else if (Parameter=="Number of establishments"){
      bins <- seq(0, 1000, by = 200)
      pal_sb <- colorBin("RdYlGn", domain = All_postcodes_merged$count, bins=bins)
      mytext <- paste0(
        "Area: ", All_postcodes_merged@data$name,"<br/>",
        "Count in area: ", All_postcodes_merged@data$count, "<br/>") %>%
        lapply(htmltools::HTML)

      leaflet() %>%
        setView(lng = -0.75, lat = 53, zoom = 8) %>%
        addTiles() %>%
        addScaleBar() %>%
        addPolygons(data = All_postcodes_merged,
                    fillColor = ~pal_sb(All_postcodes_merged$count),
                    weight = 2,
                    opacity = 0.2,
                    label = mytext,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.5) %>%
        addLegend(pal = pal_sb,
                  values = All_postcodes_merged$count,
                  position = "bottomright",
                  title = "Mean hygiene rating")
    }


})

}

shinyApp(ui, server)
