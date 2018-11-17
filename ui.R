library(shiny)
library(shinydashboard)
library(leaflet)



dashboardPage(skin = "yellow",
              dashboardHeader(title = "Global Terrorism Visulization"),
              dashboardSidebar(
                sidebarMenu(id = "sbm",
                            menuItem("Data Overview", tabName = "globalattack1", icon = icon("area-chart"),
                                     menuSubItem("Global Attacks - I", icon = icon("check-circle"),tabName = "globalattack1")
                            )
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
                                          value = max(data1$nkill,na.rm = T)
                              )
                            ),
                            fluidRow(
                              #plotlyOutput("plot1",height='auto', width = 'auto')),
                              leafletOutput("map")
                            )
                            
                          )
                  )
                )
              )
)
