
dashboardPage(skin = "yellow", #“blue”, “black”, “purple”, “green”, “red”, “yellow”
  
  dashboardHeader(title = "RunAway"),
  
  dashboardSidebar(
    sidebarMenu(id = "sbm", 
      menuItem("Introduction", tabName = "introduction", icon = icon("book")),
      menuItem("Attacks by Country", tabName = "globalattack1", icon = icon("area-chart")),
      menuItem("Terrorism evolution", tabName = "globalattack2", icon = icon("calendar")),
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
          fluidRow(class = "filters",
            box(title = HTML("<b>Filters</b> <i class='fa fa-filter text-small'></i>"),  width = 12, collapsible = T,
              column(
               width = 6,
               radioButtons("filterType", label = h4("Attacks based on:"),
                 choices = list("Fatalties" = 1, "Ransom" = 2), 
                 selected = 1)
              ),
              conditionalPanel(
               condition = "input.filterType == 1",
               column(
                 width = 6,
                 sliderInput("nkills",label = h4("Number of kills per attack:"),
                    min = 0,
                    max = max(data1$nkill,na.rm = T),
                    value = c(0,max(data1$nkill,na.rm = T)),
                    width = "100%"
                  )
               )# end of column
              ),# end of conditional panel 
              conditionalPanel(
               condition = "input.filterType == 2",
               column(
                 width = 6,
                 sliderInput("ransomamt",label = h4("Attack's ransom:"),
                    min = 0,
                    max = max(data1$ransomamt,na.rm = T),
                    value = c(0,max(data1$ransomamt,na.rm = T)),
                    width = "100%"
                  )
               )# end of column
              ), # end of conditional panel
              column(width = 12, class = "col-sm-12 col-md-12",
                selectInput("selRegion",
                  h4("Filter by region:"), 
                  choices = c(All='All', as.character(regionList)), selected = 'All',
                  width = "100%"
                )
              )
            )
          ),
          fluidRow(
            box(
              title = textOutput("ga1title"),
              solidHeader = T,
              status = "primary",
              width = 12,
              column(width = 12, class = "col-sm-6 col-md-3",
                     valueBoxOutput("totAttacks", width = "100%")
              ),
              column(width = 12, class = "col-sm-6 col-md-3",
                     valueBoxOutput("totSuccess", width = "100%")
              ),
              column(width = 12, class = "col-sm-6 col-md-3",
                     valueBoxOutput("totDeaths", width = "100%")
              ),
              column(width = 12, class = "col-sm-6 col-md-3",
                     valueBoxOutput("totRansom", width = "100%")
              ),
              column(width = 12, class = "col-sm-12 col-md-6",
                box(
                  title = "Cluster Map:", 
                  collapsible = T,
                  solidHeader = F,
                  status = "info",
                  width = "100%",
                  leafletOutput("map", width = "100%")
                )
              ),
              # column(width = 12, class = "col-sm-12 col-md-6",
              #   box(
              #     title = "Map of the countries:", 
              #     collapsible = T,
              #     solidHeader = F,
              #     status = "info",
              #     width = "100%",
              #     plotlyOutput("regionMap")
              #   )
              # ),
              column(width = 12, class = "col-sm-12 col-md-6",
                box(title = "Attacks' countries:",
                    collapsible = T,
                    solidHeader = F,
                    status = "info",
                    width = "100%",
                    plotlyOutput("attCoutries", width = "100%", height = 200)
                )
              ),
              column(width = 12, class = "col-sm-12 col-md-6",
                box(title = "Attack types:", 
                    collapsible = T,
                    solidHeader = F,
                    status = "info",
                    width = "100%",
                    plotlyOutput("attType", width = "100%", height = 200)
                )
              ),        
              box(title = "First 10 countries ranked by number of fatalities:",
                  collapsible = T,
                  solidHeader = F,
                  status = "info",
                  width = 12,
                  plotlyOutput("fatCountries", width = "100%")
              )
            )
          )
        )
      ),
      tabItem(tabName = "globalattack2",
        fluidPage(
          fluidRow(class = "filters",
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
                  h4("Region:"), 
                  choices = c(All = "All", regionList), 
                  selected = "All"
                )
              )
            )
          ),
          fluidRow(id = "yearRegionBoxRow",
            box(title = textOutput("title1"),
              solidHeader = T,
              status = "primary",
              width = 12,
              fluidRow(id = "mapTimeRow",
                column(width = 12, class = "col-sm-12 col-md-6",
                  box(title = "Cluster Map",
                    collapsible = T,
                    solidHeader = F,
                    status = "info",
                    width = 12,
                    leafletOutput("yearMap", width = "100%")
                  )
                ),
                column(width = 12, class = "col-sm-12 col-md-6",
                  box(title = textOutput("title3"),
                    collapsible = T,
                    solidHeader = F,
                    status = "info",
                    width = 12,
                    plotlyOutput("timePlot")
                  )
                )
              ),
              fluidRow(id = "selRow",
                column(width = 6,
                  selectInput("selBox1", 
                    h4("Category"), 
                    choices = categoryList, 
                    selected = "Target"
                  )
                ),
                column(width = 6,
                  selectInput("selBox2", 
                    h4("Attacks Impact"), 
                    choices = impactList, 
                    selected = "Number of attacks"
                  )
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
      ),
      tabItem(tabName = "wordsearch",
        fluidPage(
          fluidRow(
            box( width = 12,
              column( width = 12,
                searchInput(
                  inputId = "searchBox", 
                  label = HTML("<h2>Search for a word: <small>(This query might take a while)</small></h2>"), 
                  placeholder = "e.g. Bomb", 
                  btnSearch = icon("search"), 
                  btnReset = icon("remove"), 
                  width = "100%"
                )#,


                #textInput("searchBox", h2("Search for a word:"), placeholder = "e.g. Bomb"),
                #actionButton("searchBtn", "Search", width = "100%")
              ),
              column( width = 12,
                htmlOutput("totResults")
              )
            )
          ),
          fluidRow(
            box(title = "Attacks' countries:", status = "primary", 
                width = 6,
                collapsible = T, 
                plotlyOutput("srcAttCoutries", width = "100%", height = 200)
            ),
            box(title = "Terrorist organizations:", status = "primary", 
                width = 6,
                collapsible = T, 
                plotlyOutput("srcTerrOrg", width = "100%", height = 200)
            )
          ),
          fluidRow(
            box(title = "Time based attacks:", status = "primary", 
                width = 12, 
                collapsible = T, 
                plotlyOutput("yearSearch", width = "100%", height = "300px")
            )
          ),
          fluidRow(
            box(title = "Results' table:", width = 12,
              collapsible = T,
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
