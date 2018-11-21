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

intToStr <- function(num){
  if(num / 1000000000 >= 1)
    return(paste(as.character(round(num / 1000000000, 2)), "B")) # 1.83 B
  else if (num / 100000000 >= 1)
    return(paste(substr(as.character(round(num / 1000000, 0)),1,3), "M")) # 188 M
  else if (num / 10000000 >= 1)
    return(paste(substr(as.character(round(num / 1000000, 1)),1,4), "M")) # 17.5 M 
  else if(num / 1000000 >= 1)
    return(paste(as.character(round(num / 1000000, 2)), "M")) # 1.53 M
  else if (num / 100000 >= 1)
    return(paste(substr(as.character(round(num / 1000, 2)),1,3), "k")) # 168 k
  else if (num / 10000 >= 1)
    return(paste(substr(as.character(round(num / 1000, 2)),1,4), "k")) # 11.4 k
  else if (num / 1000 >= 1)
    return(paste(as.character(round(num / 1000, 2)), "k")) # 1.38 k
  else
    return(as.character(num))
}

#data = read.csv("./data.csv", header = T)
data1  = completeFun(data[1:1000,], c("nkill", "latitude", "longitude"))
data1$country = as.character(data1$country)
data1$region = as.character(data1$region)

shinyServer(function(input, output, session){
  # OVERVIEW
  
  
  filteredData <- reactive({
    d = data1 %>% filter(nkill >= input$nkills[1] & nkill <= input$nkills[2])
    if(input$selRegion != "All"){
      d = d %>% filter(region == input$selRegion)
    }
    d
  })
  leafIcons <- reactive({
    icons(
      iconUrl = if_else(filteredData()$nkill == 0, "0.png",
                        if_else(filteredData()$nkill < 5, "1.png",
                                if_else(filteredData()$nkill < 10, "2.png",
                                        if_else(filteredData()$nkill < 20, "3.png", "4.png")))),
      iconWidth = 20, iconHeight = 20,
      iconAnchorX = 0, iconAnchorY = 0,
      popupAnchorX = 0, popupAnchorY = 0
      
    )
  })
  output$map <- renderLeaflet({
    leaflet(data = filteredData()) %>% addTiles() %>%
      addMarkers(~longitude, ~latitude, icon = leafIcons())
    })
  
  observeEvent(input$nkills, {
    leafletProxy("map") %>%
    clearShapes() %>%
    clearMarkers() %>%
    addMarkers(data = filteredData(),
                     popup = ~paste("Kills: ",nkill),
                     clusterOptions = markerClusterOptions(),
                     icon = leafIcons()
    )
  })
  observeEvent(input$selRegion, {
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addMarkers(data = filteredData(),
                 popup = ~paste("Kills: ",nkill),
                 clusterOptions = markerClusterOptions(),
                 icon = leafIcons()
      )
  })
  
  
  
  output$totDeaths <- renderValueBox({
    deaths <- sum(filteredData()$nkill, na.rm = T)
    valueBox(deaths,"Total Fatalities",icon = icon("user"), color = 'blue') })
  output$totAttacks <- renderValueBox({
    attacks <- nrow(filteredData())
    valueBox(attacks,"Total Attacks",icon = icon("bomb"), color = 'red') })
  output$totRansom <- renderValueBox({
    ransom <- intToStr(sum(filteredData()$ransomamt, na.rm = T))
    valueBox(ransom,"Total Ransom in $",icon = icon("hand-holding-usd"), color = 'green') })
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
      arrange(desc(count)) %>%
      group_by(country = ifelse(row_number() > 4, "Others", country)) %>%
      summarize(count = sum(count)) %>%
      plot_ly(labels = ~country, values = ~count) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  output$fatCountries <- renderPlotly({
    filteredData() %>%
      group_by(country) %>%
      summarize(count = sum(nkill, na.rm = T)) %>%
      arrange(desc(count)) %>%
      slice(1:10) %>%
      plot_ly(x = ~country, y = ~count, type = 'bar',
        marker = list(color = 'rgb(158,202,225)',
                      line = list(color = 'rgb(8,48,107)',
                                  width = 1.5))) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
})
