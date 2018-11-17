library(shiny)
library(leaflet)
library(shinydashboard)
data = read.csv("./data.csv", header = T)
data1  = data[1:1000,]

shinyServer(function(input, output, session){
  filteredData <- reactive({
    df = data1[data1$nkill <= input$nkills,]
    print(nrow(df))
    df
  })
  output$map <- renderLeaflet({
    leaflet() %>% addTiles()
    })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
        addCircleMarkers(color = "red", radius = ~(sqrt(nkill)+1)*5,
                         popup = ~paste(nkill),
                         clusterOptions = markerClusterOptions()
        )
  })
  observe({
    output$totDeaths <- renderValueBox({
      deaths <- sum(filteredData()$nkill, na.rm = T)
      print(deaths)
      valueBox(deaths,"Total Fatalities",icon = icon("user"), color = 'blue') })
  })
})
