library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)


dashboardPage(skin = "yellow", #“blue”, “black”, “purple”, “green”, “red”, “yellow”
  
  dashboardHeader(title = "Global Terrorism Visulization"),
  
  dashboardSidebar(
    sidebarMenu(id = "sbm", 
      menuItem("Data Overview", tabName = "globalattack1", icon = icon("area-chart")),
      menuItem("Attacks per Country", tabName = "globalattack2", icon = icon("area-chart"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    ),
    tabItems(
      tabItem(tabName = "globalattack1",
        fluidPage(
          fluidRow(
            column(width = 12,
             valueBoxOutput("totCountry", width = 3),
             valueBoxOutput("totAttacks", width = 3),
             valueBoxOutput("totDeaths", width = 3),
             valueBoxOutput("totLoss", width = 3))),
          fluidRow(id = "filters",
            box(title = HTML("<strong>Filters</strong> <i class='fa fa-filter text-small'></i>"),  width = 12, collapsible = T,
              column(width = 6,
                sliderInput("nkills",label = 
                  h4("Max number of attack's kills:"),
                  min = 0,
                  max = max(data1$nkill,na.rm = T),
                  value = 0,
                  width = "100%"
                )
              ),
              column(width = 6,
                selectInput("selRegion",
                  h4("Filter by region:"), 
                  choices = c(All='All', as.character(regionList)), selected = 'All',
                  width = "100%"
                )
              )
            )
          ),
          fluidRow(
            #plotlyOutput("plot1",height='auto', width = 'auto')),
            box(
              title = "Map of the attacks:", status = "primary",
              width = 8,
              collapsible = F,
              leafletOutput("map", width = "100%")
              
            ),
            box(title = "Attacks' countries:", status = "primary",  solidHeader = TRUE,
                width = 4,
                collapsible = T, 
                plotlyOutput("attCoutries", width = "100%", height = 200)
            ),
            box(title = "Attack types:", status = "primary",  solidHeader = TRUE,
                width = 4,
                collapsible = T,
                plotlyOutput("attType", width = "100%", height = 200)
            )
          )
        )
      ),
      tabItem(tabName = "globalattack2",
        fluidPage(
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
