library(shiny)
library(shinydashboard)
library(leaflet)



dashboardPage(skin = "yellow",
  dashboardHeader(title = "Global Terrorism Visulization"),
  dashboardSidebar(
    sidebarMenu(id = "sbm", 
      menuItem("Data Overview", tabName = "globalattack1", icon = icon("area-chart")),
      menuItem("Attacks per Country", tabName = "globalattack2", icon = icon("area-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "globalattack1",
        fluidPage(
          title = "Data Overview",
          fluidRow(
            column(width = 12,
             valueBoxOutput("totCountry", width = 3),
             valueBoxOutput("totAttacks", width = 3),
             valueBoxOutput("totDeaths", width = 3),
             valueBoxOutput("totLoss", width = 3))),
          fluidRow(
            sliderInput("nkills",
              "Max number of attack's kills:",
              min = 0,
              max = max(data1$nkill,na.rm = T),
              value = 0
              )
            ),
          fluidRow(
            #plotlyOutput("plot1",height='auto', width = 'auto')),
            leafletOutput("map")
            )

        )
      ),
      tabItem(tabName = "globalattack2",
        fluidPage(
          title = "Attacks 2",
          fluidRow(
            column(width = 12,
              selectInput("select", 
                h3("Select box"), 
                choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), selected = 1
              )
            )
          )
        )
      )
    )
  )
)
