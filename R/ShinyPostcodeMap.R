
library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)


All_postcodes_merged <- readRDS("data/All_postcodes_merged.rds")

All_data_19_Oct <- readRDS("data/API_dated/All_data_19_Oct.rds")


myPostcodes <- All_postcodes_merged$name %>%
  as_tibble() %>%
  mutate(postcodeArea = str_extract(value, "[A-Z]+"))



ui <- fluidPage(selectInput(inputId = "myPostcodeArea", label = "Select postcode area:",
                            choices = {myPostcodes %>%
                                count(postcodeArea) %>%
                                pull(1)}), selectInput(inputId = "myPostcodeDistrict", label = "Select postcode district", choices = ""),
                plotOutput(outputId = "ratingsplot"),
                leafletOutput(outputId = "myMap", width = "1400px",
                              height = "800px"))




server <-  function(input, output, session){



    output$ratingsplot <- renderPlot({

      myRatings <- All_data_19_Oct %>%
        filter(postcodeDistrict==input$myPostcodeDistrict) %>%
        count(rating) %>%
        filter(rating %in% 0:5)


      ggplot(data = myRatings, aes(rating, n)) + geom_bar(stat = "identity", fill = "steelblue") +
         ylab("Count") +  xlab("Rating") + theme_classic() + geom_text(aes(label=n),vjust=-0.3)
    })



  observe({updateSelectInput(session, "myPostcodeDistrict",
                             choices = myPostcodes %>%
                               filter(postcodeArea==input$myPostcodeArea) %>%
                               pull(1)
  )})

  #Assign output$distPlot with renderLeaflet object
  output$myMap <- renderLeaflet({

    basemap = leaflet() %>%
      addTiles()



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



  })

}

shinyApp(ui, server)
