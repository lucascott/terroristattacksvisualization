library(shiny)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(plotly)
library(matlab)

# To remove the rows with null values in specific columns
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# To filter data by region
selRegion <- function(data, desiredRegion){
    return(data[which(data$region == desiredRegion),])
}

# Read data
data = read.csv("clean_data.csv", header = T)
data <- data[1:70000,]
data1 <- completeFun(data, c("latitude", "longitude", "nkill"))
data1$country = as.character(data1$country)
data1$region = as.character(data1$region)

regionList <- c("Australasia & Oceania", "Central America & Caribbean", "Central Asia", "East Asia", "Eastern Europe", "Middle East & North Africa", "North America", "South America", "South Asia", "Southeast Asia", "Sub-Saharan Africa", "Western Europe")
categoryList <- c("Target", "Type of attack", "Weapon")
impactList <- c("Number of attacks", "Number of fatalities", "Number of injured", "Economic Impact")

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Global Terrorism Visulization"),
                    dashboardSidebar(
                      sidebarMenu(id = "sbm", 
                                  menuItem("Terrorism evolution", tabName = "globalattack2", icon = icon("calendar"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "globalattack2",
                                fluidPage(fluidRow(id = "filtersRow",
                                                   box(title = HTML("<b>Filters</b> <i class='fa fa-filter text-small'></i>"),  width = 12, collapsible = T,
                                                       column(width = 6,
                                                              sliderInput("yearSlider",
                                                                          label = h4("Span of years:"),
                                                                          min = min(data1$iyear, na.rm = T),
                                                                          max = max(data1$iyear, na.rm = T),
                                                                          step = 1,
                                                                          value = c(min(data1$iyear, na.rm = T), max(data1$iyear, na.rm = T)),
                                                                          width = "100%"
                                                              )
                                                       ),
                                                       column(width = 6,
                                                              selectInput("selBox0", 
                                                                          h4("Region"), 
                                                                          choices = c(All = "All", regionList), 
                                                                          selected = "All")
                                                       )
                                                   )
                                          ),
                                          fluidRow(id = "yearRegionBoxRow",
                                                   box(title = textOutput("title1"),
                                                       solidHeader = T,
                                                       status = "primary",
                                                       width = 12,
                                                       fluidRow(id = "mapTimeRow",
                                                                column(width = 6,
                                                                       box(title = textOutput("title2"),
                                                                           collapsible = T,
                                                                           solidHeader = F,
                                                                           status = "info",
                                                                           width = 12,
                                                                       leafletOutput("yearMap", width = "100%")
                                                                        )
                                                                ),
                                                                column(width = 6,
                                                                       box(title = textOutput("title3"),
                                                                           collapsible = T,
                                                                           solidHeader = F,
                                                                           status = "info",
                                                                           width = 12,
                                                                           plotlyOutput("timePlot")
                                                                       ))
                                                       ),
                                                       fluidRow(id = "selRow",
                                                                column(width = 6,
                                                                       selectInput("selBox1", 
                                                                                   h4("Category"), 
                                                                                   choices = categoryList, 
                                                                                   selected = "Target")
                                                                ),
                                                                column(width = 6,
                                                                       selectInput("selBox2", 
                                                                                   h4("Attacks Impact"), 
                                                                                   choices = impactList, 
                                                                                   selected = "Number of attacks")
                                                                )
                                                       ),
                                                       fluidRow(id = "pieRankRow",
                                                                column(width = 6,
                                                                       box(title = textOutput("title4"),
                                                                           collapsible = T,
                                                                           solidHeader = F,
                                                                           status = "info",
                                                                           width = 12,
                                                                           plotlyOutput("plotPie")
                                                                           )
                                                                       ),
                                                                column(width = 6,
                                                                       box(title = textOutput("title5"),
                                                                           collapsible = T,
                                                                           solidHeader = F,
                                                                           status = "info",
                                                                           width = 12,
                                                                           plotlyOutput("plotBarChart")
                                                                       )
                                                                )
                                                       )
                                                   )
                                          )
                                )
                        )
                      )
                    )
)

server <- shinyServer(function(input, output, session){

  box1 <- reactive({switch(input$selBox1, 
                           "Target" = "target_type" , 
                           "Type of attack" = "attack_type",
                           "Weapon" = "weapon_type")})
  
  box2 <- reactive({switch(input$selBox2, 
                           "Number of attacks" = "nattacks", 
                           "Number of fatalities" = "nkill",
                           "Number of injured" = "nwound", 
                           "Economic Impact" = "propvalue")})
  
  
  
  yearsFilteredData <- reactive({
    d = data1 %>% filter(iyear >= input$yearSlider[1] & iyear <= input$yearSlider[2])
    if(input$selBox0 != "All"){
      d = d %>% filter(region == input$selBox0)
    }
    d
  })
  
  leafIcons <- reactive({
    icons(
      iconUrl = if_else(yearsFilteredData()$nkill == 0, "../0.png",
                        if_else(yearsFilteredData()$nkill < 5, "../1.png",
                                if_else(yearsFilteredData()$nkill < 10, "../2.png",
                                        if_else(yearsFilteredData()$nkill < 20, "../3.png", "../4.png")))),
      iconWidth = 20, iconHeight = 20,
      iconAnchorX = 0, iconAnchorY = 0,
      popupAnchorX = 0, popupAnchorY = 0
    )
  })
  
  popUpCreate <- function(nkill, attack_type, date){
    paste("<b>Kills:</b> ", nkill,"<br/><b>Type:</b> ", attack_type,"<br/><b>Date:</b> ", date)
  }
  
  output$title1 <- renderText({
    title1 = paste(input$selBox0, " (", input$yearSlider[1], " - ", input$yearSlider[2], ")", sep = "")
    title1
  })
  
  output$title2 <- renderText({
    title2 = paste("Cluster Map")
    title2
  })
  
  output$title3 <- renderText({
    title3 = paste("Time series of", input$selBox2, "vs", input$selBox1)
    title3
  })
  
  output$title4 <- renderText({
    title4 = paste("Pie plot of number of attacks grouped by", input$selBox1)
    title4
  })
  
  output$title5 <- renderText({
    if(input$selBox0 == "All"){
      title5 = paste("Regions ranking by ", input$selBox2)
      title5
    }
    else{
      title5 = paste("Top 5 countries by ", input$selBox2)
      title5
    }
  })
  
  # Map representing cluster of attacks for a selected region and date range
  # Problem does not show cluster for all regions, and then show them when changing date range
  output$yearMap <- renderLeaflet({

    leaflet(data = yearsFilteredData()) %>% addTiles() %>%
      addMarkers(~longitude, ~latitude,
                 icon = leafIcons(),
                 popup = ~popUpCreate(nkill, attack_type, date),
                 clusterOptions = markerClusterOptions()
      )
  })
  
  observeEvent(input$nkills, {
    leafletProxy("yearMap") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addMarkers(data = yearsFilteredData(),
                 popup = ~popUpCreate(nkill, attack_type, date),
                 clusterOptions = markerClusterOptions(),
                 icon = leafIcons()
      )
  })
  
  # Time series plot, sensible to selBox0 (region), selBox1(classification of attacks-color), selBox2(attack consequences). Also sensible to date range
  output$timePlot <- renderPlotly({
    
    time <- aggregate(switch(box2(),
                            "nattacks" = ones,
                            "nkill" = nkill,
                            "nwound" = nwound, 
                            "propvalue" = propvalue) ~ 
                            iyear + switch(box1(),
                                           "target_type" = target_type,
                                           "attack_type" = attack_type,
                                           "weapon_type" = weapon_type), data = yearsFilteredData(), FUN = sum)
                            
                            timePlot1 <- plot_ly(time) %>%
                                          add_trace(x = ~time[,1], y = ~log(time[,3], base = exp(1)), color = ~time[,2], type = 'scatter', 
                                                    mode = 'lines+markers',line = list(width = 2), hoverinfo = "text", 
                                                    text = ~paste(paste("Total ", input$selBox2, ":", time[,3]), time[,2],
                                                                  sep = "<br />"), colors = c("red","blue","green","orange")) %>%
                                          layout(
                                                    xaxis = list(zeroline = TRUE, title = "Year"),
                                                    yaxis = list(side = 'left', rangemode = "tozero", overlaying = "y", 
                                                                 title = paste("ln(", input$selBox2,")"),showgrid = TRUE, 
                                                                 zeroline = TRUE,showticklabels = TRUE),
                                                    legend = list(x = 0.06, y = 0.98)) %>%
                                                    config(displayModeBar = F)
                            timePlot1
  })

  output$plotPie <- renderPlotly({
      pie <- aggregate(ones ~ switch(box1(),
                              "target_type" = target_type,
                              "attack_type" = attack_type,
                              "weapon_type" = weapon_type), data = yearsFilteredData(), FUN = sum)
          colnames(pie) <- c("category", "nattacks")
          pie <- pie[order(pie$nattacks, decreasing = TRUE),]
             
          m <- list(l = 0, r = 0, b = 0, t = 0, pad = 0, autoexpand = TRUE)
             
    plotPie <- plot_ly(data = pie, labels = ~category, values = ~nattacks, type = "pie",
                       textposition = 'inside', textinfo = 'label',
                       insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                       text = ~paste('Numb. of attacks:', nattacks),showlegend = FALSE) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = F)
    plotPie
    })
  
  # Bar chart Rank showing ranking of the regions/countries order by attack consequences
  output$plotBarChart <- renderPlotly({
    
    if(input$selBox0 == "All"){
      bar <- aggregate(switch(box2(),
                              "nattacks" = ones,
                              "nkill" = nkill,
                              "nwound" = nwound, 
                              "propvalue" = propvalue) ~ region, data = yearsFilteredData(), FUN = sum)
      colnames(bar) <- c("region", "fatalities")
      bar <- bar[order(bar[,2], decreasing = TRUE),]
      
      plotBarChart <- plot_ly(x = bar[,2], y = reorder(bar[,1], bar[,2]), type = 'bar', orientation = "h")
      plotBarChart
      
    }else{
      bar <- aggregate(switch(box2(),
                              "nattacks" = ones,
                              "nkill" = nkill,
                              "nwound" = nwound, 
                              "propvalue" = propvalue) ~ country, data = yearsFilteredData(), FUN = sum)
      colnames(bar) <- c("country","fatalities")
      bar <- bar[order(bar$fatalities, decreasing = TRUE),]
      bar <- bar[1:5,]
      
      plotBarChart <- plot_ly(x = bar[,2], y = reorder(bar[,1], bar[,2]), type = 'bar', orientation = "h")
      plotBarChart
    }
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

