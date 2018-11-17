library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        sliderInput("nkills",
                    "Number of kills:",
                    min = 0,
                    max = max(data1$nkill,na.rm = T),
                    value = max(data1$nkill,na.rm = T))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("map"),
        h4(textOutput("Kcount"), align = "right")
      )
    )
  )
)