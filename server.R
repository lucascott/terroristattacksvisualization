
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
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 0, iconAnchorY = 0,
      popupAnchorX = 0, popupAnchorY = 0
    )
  })
  output$ga1title <- renderText({
    input$selRegion
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
    valueBox(paste("$",ransom),"Total Ransom",icon = icon("hand-holding-usd"), color = 'green') })
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
       d = gtd %>% filter(myLevSim(scite1, input$searchBox) > 0.8)
       if(nrow(d) == 0){
         sendSweetAlert(
           session = session,
           title = "Oops! No results found",
           text = "Try to search for a more general keyword...",
           type = "warning"
         )
       }
       d
    }
  })
  observeEvent(input$searchBox, {
    if(input$searchBox != ""){
      output$totResults <- renderText({paste("<h4>There are ",nrow(searchData())," matches!</h4>")})
      if (nrow(searchData()) != 0){
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
        output$yearSearch <- renderPlotly({
          searchData() %>%
            group_by(iyear) %>% 
            summarize(count = n(), nkill = sum(nkill)) %>% 
            plot_ly() %>%
            add_trace(name = "Attacks", x = ~iyear, y = ~count, type = 'scatter', 
                      mode = 'lines+markers',line = list(width = 2)) %>% 
            add_trace(name = "Total kills", x = ~iyear, y = ~nkill, type = 'scatter', 
                      mode = 'lines+markers',line = list(width = 2)) %>% 
            layout(
              xaxis = list(zeroline = TRUE, title = "Year"),
              yaxis = list(title = "Amount", side = 'left', rangemode = "tozero", overlaying = "y", 
                           showgrid = TRUE, 
                           zeroline = TRUE,showticklabels = TRUE),
              legend = list(x = 0.06, y = 0.98)) %>%
            config(displayModeBar = F)
          
        })
        output$searchTbl <- renderDataTable(searchData()[c("country_txt","nkill","scite1")])
      }
    }
  })
})
