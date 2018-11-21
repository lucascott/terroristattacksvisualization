library(shiny)
library(leaflet)
library(shinydashboard)
library(dplyr)
library(plotly)

#to remove the rows with null values in specific columns
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#data = read.csv("./data.csv", header = T)
#data1  = completeFun(data, c("latitude","longitude"))[1:10000,]

shinyServer(function(input, output, session){
  # OVERVIEW
  leafIcons <- icons(
    iconUrl = "shockwave.png",
    iconWidth = 20, iconHeight = 20,
    iconAnchorX = 20, iconAnchorY = 20
  )
  filteredData <- reactive({
    data1 %>% filter(nkill >= input$nkills)
  })
  output$map <- renderLeaflet({
    leaflet(data = filteredData()) %>% addTiles() %>%
      addMarkers(~longitude, ~latitude, icon = leafIcons)
    })
  
  observeEvent(input$nkills, {
    leafletProxy("map") %>%
    clearShapes() %>%
    clearMarkers() %>%
    addMarkers(data = filteredData(),
                     popup = ~paste("Kills: ",nkill),
                     clusterOptions = markerClusterOptions(),
                     icon = leafIcons
    )

    output$totDeaths <- renderValueBox({
      deaths <- sum(filteredData()$nkill, na.rm = T)
      valueBox(deaths,"Total Fatalities",icon = icon("user"), color = 'blue') })
    output$totAttacks <- renderValueBox({
      attacks <- nrow(filteredData())
      valueBox(attacks,"Total Attacks",icon = icon("bomb"), color = 'red') })
    output$attType <- renderPlotly({
      filteredData() %>%
      group_by(attack_type) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~attack_type, values = ~count) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      })
    output$attCoutries <- renderPlotly({
      filteredData() %>%
        group_by(country) %>%
        summarize(count = n()) %>%
        slice(c(1:10)) %>%
        plot_ly(labels = ~country, values = ~count) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })

  })
    
})
