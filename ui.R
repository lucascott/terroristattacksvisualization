library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)



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
            box(
              title = "Map of the attacks:", status = "primary", solidHeader = TRUE,
              width = 8,
              collapsible = T,
              leafletOutput("map", width = "100%")
              
            ),
            box(title = "Attack types:", status = "primary",  solidHeader = TRUE,
                width = 4,
                collapsible = T,
                plotlyOutput("attType", width = "100%", height = 200)
            ),
            box(title = "Attack countries:", status = "primary",  solidHeader = TRUE,
                width = 4,
                collapsible = T,
                plotlyOutput("attCoutries", width = "100%", height = 200)
            )
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
