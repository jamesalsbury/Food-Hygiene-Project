
library(shiny)
library(tidyverse)

All_data_19_Oct <- readRDS("data/API_dated/All_data_19_Oct.rds")

AuthorityData <- readRDS("data/Authority_Data.rds")

mysample <- All_data_19_Oct[sample(nrow(All_data_19_Oct), nrow(All_data_19_Oct)/10), ]


Regions <- mysample %>%
  count(Region)

AuthorityData %>%
  filter(region=="Scotland")


ui <- fluidPage(selectInput(inputId = "myRegion", label = "Select region:", choices = Regions$Region),
                selectInput(inputId = "myLocalArea", label = "Select local authority", choices = ""), dataTableOutput("table"))



server <- function(input, output, session) {

    observe({updateSelectInput(session, "myLocalArea",
                      choices = AuthorityData %>%
                        filter(region==input$myRegion) %>%
                        pull(1)
    )})
  #yes <- c(1,3,5)
  output$table <- renderDataTable({new <- mysample %>%
      filter(authorityName == input$myLocalArea)
    return(new[,])})


}

shinyApp(ui, server)



