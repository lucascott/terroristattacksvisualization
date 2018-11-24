library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)


dashboardPage(skin = "yellow", #“blue”, “black”, “purple”, “green”, “red”, “yellow”
  
  dashboardHeader(title = "RunAway"),
  
  dashboardSidebar(
    sidebarMenu(id = "sbm", 
      menuItem("Introduction", tabName = "introduction", icon = icon("area-chart")),
      menuItem("Attacks by Country", tabName = "globalattack1", icon = icon("area-chart")),
      menuItem("Word Search", tabName = "wordsearch", icon = icon("search"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "animate.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
      tags$script(src = "app.js")
    ),
    tabItems(
      tabItem(tabName = "introduction",
        fluidPage(
          fluidRow(
            box( width = 12,
            htmlOutput("intro"))
          )
        )
      ),
      tabItem(tabName = "globalattack1",
        fluidPage(
          fluidRow(
            column(width = 12,
              valueBoxOutput("totAttacks", width = 3),
              valueBoxOutput("totSuccess", width = 3),
              valueBoxOutput("totDeaths", width = 3),
              valueBoxOutput("totRansom", width = 3)
            )
          ),
          fluidRow(id = "filters",
            box(title = HTML("<b>Filters</b> <i class='fa fa-filter text-small'></i>"),  width = 12, collapsible = T,
              column(width = 6,
                sliderInput("nkills",label = 
                  h4("Number of kills per attack:"),
                  min = 0,
                  max = max(data1$nkill,na.rm = T),
                  value = c(0,max(data1$nkill,na.rm = T)),
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
              width = 12,
              collapsible = F,
              leafletOutput("map", width = "100%")
              
            )
          ),
          fluidRow(
            box(title = "Attacks' countries:", status = "primary",  solidHeader = TRUE,
                width = 6,
                collapsible = T, 
                plotlyOutput("attCoutries", width = "100%", height = 200)
            ),
            box(title = "Attack types:", status = "primary",  solidHeader = TRUE,
                width = 6,
                collapsible = T,
                plotlyOutput("attType", width = "100%", height = 200)
            )
          ),
          fluidRow(
            box(title = "First 10 countries per fatalities:", status = "primary",  solidHeader = TRUE,
                width = 12,
                collapsible = T,
                plotlyOutput("fatCountries", width = "100%")
            )
          )
        )
      ),
      tabItem(tabName = "wordsearch",
        fluidPage(
          fluidRow(
            box( width = 12,
              column( width = 8,
                textInput("searchBox", h2("Search for a word:"), placeholder = "e.g. Bomb")
              ),
              column( width = 4,
                actionButton("searchBtn", "Search", width = "100%")
              )
            )
          ),
          fluidRow(
            box( width = 12,
              column( width = 12,
                dataTableOutput('searchTbl')
              )
            )
          )
        )
      )
    )
  )
)
