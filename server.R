library(shiny)
library(leaflet)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)
library(RecordLinkage)
library(stringr)
library(shinyWidgets)

Sys.setlocale('LC_ALL','C')

# To remove the rows with null values in specific columns
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# To filter data by region
selRegion <- function(data, desiredRegion){
    return(data[which(data$region == desiredRegion),])
}

# To apply string similiarity on the search page
myLevSim = function (str1, str2) {
  fast = T
  innerFunc = function(strArr,str2){
    return(max(levenshteinSim(strArr, str2)))
  }
  fastInnerFunc = function(strArr,str2){
    return(ifelse(str2 %in% strArr, 1, 0))
  }
  srt1Arr = strsplit(tolower(str1),"\\s+")
  str2 = tolower(trimws(str2))
  if(fast == T){
    d = lapply(srt1Arr, fastInnerFunc, str2 = str2)
  }
  else{
    d = lapply(srt1Arr, innerFunc, str2 = str2)
  }
  return (d)
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
# PREPROCESSING
# gtd = read.csv("./gtd.csv", header = T)
# gtd = gtd[c("iyear","imonth", "iday", "extended","country_txt", "region_txt", "provstate", "latitude", "longitude", "multiple", "success", "suicide", "attacktype1_txt", "targtype1_txt", "natlty1_txt", "gname", "guncertain1", "nperps", "nperpcap", "weaptype1_txt", "nkill", "nkillter","nwound", "property", "ishostkid", "ransom", "INT_LOG", "INT_IDEO", "INT_MISC", "scite1","scite2", "scite3")]
# gtd$scite1 = str_replace_all(gtd$scite1, "[[:punct:]]", "")
# gtd$scite1[gtd$scite1 == "\"\"" | gtd$scite1 == "\"" | gtd$scite1 == ""] <- NA
# gtd  = completeFun(gtd, c("scite1", "scite2", "scite3"))
gtd$country_txt = as.character(gtd$country_txt)
# gtd$gname = as.character(gtd$gname)

#gtd$scite1 = tolower(gtd$scite1)
#gtd$scite1 = as.character(gtd$scite1)
#gtd$scite2 = as.character(gtd$scite2)
#gtd$scite3 = as.character(gtd$scite3)



#data = read.csv("./data.csv", header = T)
data1  = completeFun(data[1:1000,], c("nkill", "latitude", "longitude"))
data1$country = as.character(data1$country)
data1$region = as.character(data1$region)
data1$group_name = data1$group_name %>% replace_na("Unknown")
data1$group_name = as.character(data1$group_name)
data1$attack_type = as.character(data1$attack_type)

categoryList <- c("Target", "Type of attack", "Weapon")
impactList <- c("Number of attacks", "Number of fatalities", "Number of injured", "Economic Impact")
regionList = lapply(as.list(data1 %>% distinct(region)), as.character)[[1]]


shinyServer(function(input, output, session){
  # INTRODUCTION
  output$intro <- renderText({
    readLines("index.html")
    
  })
  # ATTACKS BY COUNTRY
  filteredData <- reactive({
    if(input$filterType == 1){
      d = data1 %>% filter(nkill >= input$nkills[1] & nkill <= input$nkills[2])
    }
    else if (input$filterType == 2){
      d = data1 %>% filter(ransomamt >= input$ransomamt[1] & ransomamt <= input$ransomamt[2])
    }
    
    if(input$selRegion != "All"){
      d = d %>% filter(region == input$selRegion)
    }
    if(nrow(d) == 0){
      sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "The filters are too strict.",
        type = "warning"
      )
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
  popUpCreate <- function(nkill, attack_type, date, group_name){
    x = paste("<b>Kills:</b> ",nkill,"<br/><b>Type:</b> ", attack_type, "<br/><b>Group: </b>", group_name,"<br/><b>Date:</b> ", date)
  }
  output$map <- renderLeaflet({
    leaflet(data = filteredData()) %>% addTiles() %>%
      addMarkers(~longitude, ~latitude,
        icon = leafIcons(),
        popup = ~popUpCreate(nkill, attack_type, date, group_name),
        clusterOptions = markerClusterOptions()
      )
  })
  
  observeEvent(input$nkills, {
    leafletProxy("map") %>%
    clearShapes() %>%
    clearMarkers() %>%
    addMarkers(data = filteredData(),
                     popup = ~popUpCreate(nkill, attack_type, date, group_name),
                     clusterOptions = markerClusterOptions(),
                     icon = leafIcons()
    )
  })
  observeEvent(input$selRegion, {
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addMarkers(data = filteredData(),
                 popup = ~popUpCreate(nkill, attack_type,date, group_name),
                 clusterOptions = markerClusterOptions(),
                 icon = leafIcons()
      )
  })
  output$regionMap <- renderPlotly({
    
    l <- list(color = toRGB("grey"), width = 0.6)
    
    g1 <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator'))
    
    m <- list(l = 0,r = 0,b = 0,t = 100, pad = 0, autoexpand = TRUE)
    
    p <- plot_geo(attack_freq_country) %>%
      add_trace(
        z = ~FREQ, color = ~FREQ, colors = 'Reds',
        text = ~paste(paste("Country:",COUNTRY),paste("Total Attacks:",FREQ),
                      paste("Total Fatalities:", DEATH),sep = "<br />"), locations = ~CODE,
        marker = list(line = l), hoverinfo = "text"
      ) %>%
      colorbar(title = '', tickprefix = '',xanchor = "left",thickness = "20",len = 0.3,
               tickfont = list(size = 15), nticks = 5) %>%
      layout(
        geo = g1, xaxis=list(fixedrange=TRUE), yaxis=list(fixedrange=TRUE), margin = m) %>%
      config(displayModeBar = F)
    
    p })
  output$totAttacks <- renderValueBox({
    attacks <- nrow(filteredData())
    valueBox(attacks,"Total Attacks",icon = icon("bomb"), color = 'red') })
  output$totSuccess <- renderValueBox({
    success <- sum(filteredData()$success, na.rm = T)
    valueBox(paste(success," (",round(success/nrow(filteredData())*100, 0), "%)", sep = ""),"Total Successes",icon = icon("frown"), color = 'orange') })
  output$totDeaths <- renderValueBox({
    deaths <- sum(filteredData()$nkill, na.rm = T)
    valueBox(deaths,"Total Fatalities",icon = icon("cross"), color = 'blue') })
  output$totRansom <- renderValueBox({
    ransom <- intToStr(sum(filteredData()$ransomamt, na.rm = T))
    valueBox(ransom,"Total Ransom in $",icon = icon("hand-holding-usd"), color = 'green') })
  output$attType <- renderPlotly({
    filteredData() %>%
      group_by(attack_type) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      group_by(attack_type = ifelse(row_number() > 5, "Others", attack_type)) %>%
      summarize(count = sum(count)) %>%
      plot_ly(labels = ~attack_type, values = ~count) %>%
      add_pie() %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  output$attCoutries <- renderPlotly({
    d = filteredData() %>%
      group_by(country) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      group_by(country = ifelse(row_number() > 5, "Others", country)) %>%
      summarize(count = sum(count)) %>%
      arrange(desc(count)) %>%
      plot_ly(labels = ~country, values = ~count) %>%
      add_pie() %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  output$fatCountries <- renderPlotly({
    filteredData() %>%
      group_by(country) %>%
      summarize(count = sum(nkill, na.rm = T)) %>%
      arrange(desc(count)) %>%
      slice(1:10) %>%
      arrange(count) %>%
      plot_ly(x = ~count, y = ~country, type = 'bar', orientation = 'h',
        marker = list(line = list(width = 1.5))) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "", categoryarray = ~country))
  })

  # TIME VISUALIZATION

  box1 <- reactive({switch(input$selBox1, 
                           "Target" = "target_type" , 
                           "Type of attack" = "attack_type",
                           "Weapon" = "weapon_type")})
  
  box2 <- reactive({switch(input$selBox2, 
                           "Number of attacks" = "nattacks", 
                           "Number of fatalities" = "nkill",
                           "Number of injured" = "nwound", 
                           "Economic Impact" = "propvalue")})

  output$title1 <- renderText({
    title1 = paste(input$selBox0, " (", input$yearSlider[1], " - ", input$yearSlider[2], ")", sep = "")
    title1
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

  yearsFilteredData <- reactive({
    d = data1 %>% filter(iyear >= input$yearSlider[1] & iyear <= input$yearSlider[2])
    if(input$selBox0 != "All"){
      d = d %>% filter(region == input$selBox0)
    }
    d
  })
  
  timeLeafIcons <- reactive({
    icons(
      iconUrl = if_else(yearsFilteredData()$nkill == 0, "0.png",
                        if_else(yearsFilteredData()$nkill < 5, "1.png",
                                if_else(yearsFilteredData()$nkill < 10, "2.png",
                                        if_else(yearsFilteredData()$nkill < 20, "3.png", "4.png")))),
      iconWidth = 20, iconHeight = 20,
      iconAnchorX = 0, iconAnchorY = 0,
      popupAnchorX = 0, popupAnchorY = 0
    )
  })


  # Map representing cluster of attacks for a selected region and date range
  # Problem does not show cluster for all regions, and then show them when changing date range
  output$yearMap <- renderLeaflet({

    leaflet(data = yearsFilteredData()) %>% addTiles() %>%
      addMarkers(~longitude, ~latitude,
                 icon = timeLeafIcons(),
                 popup = ~popUpCreate(nkill, attack_type, date, group_name),
                 clusterOptions = markerClusterOptions()
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

  # SEARCH PAGE
  searchData <- reactive({
    if(input$searchBox != ""){
      gtd %>% filter(myLevSim(scite1, input$searchBox) > 0.8)
    }
  })
  observeEvent(input$searchBox, {
    if(input$searchBox != ""){
      output$totResults <- renderText({paste("<h4>There are ",nrow(searchData())," matches!</h4>")})
      output$srcAttCoutries <- renderPlotly({
        searchData() %>%
          group_by(country_txt) %>%
          summarize(count = n()) %>%
          arrange(desc(count)) %>%
          group_by(country_txt = ifelse(row_number() > 7, "Others", country_txt)) %>%
          summarize(count = sum(count)) %>%
          plot_ly(labels = ~country_txt, values = ~count) %>%
          add_pie() %>%
          layout(showlegend = T,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      })
      output$srcTerrOrg <- renderPlotly({
        searchData() %>%
          group_by(gname) %>%
          summarize(count = n()) %>%
          arrange(desc(count)) %>%
          group_by(gname = ifelse(row_number() > 7, "Others", gname)) %>%
          summarize(count = sum(count)) %>%
          arrange(count) %>%
          plot_ly(x = ~count, y = ~gname, type = 'bar', orientation = 'h',
                  marker = list(line = list(width = 1.5))) %>%
          layout(xaxis = list(title = ""),
                 yaxis = list(title = "", categoryarray = ~gname))
      })
      output$searchTbl <- renderDataTable(searchData()[c("country_txt","nkill","scite1")])
    }
  })
})
