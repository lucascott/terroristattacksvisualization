library(shiny)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(ggplot2)

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Global Terrorism Visulization"),
                    dashboardSidebar(
                      sidebarMenu(id = "sbm", 
                                  menuItem("Terrorism evolution", tabName = "globalattack1", icon = icon("calendar"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "globalattack1",
                                fluidPage(
                                  title = "Terrorism evolution",
                                  fluidRow(
                                    column(3,
                                           selectInput("selBox1", 
                                                       h3("Categorical Variable"), 
                                                       choices = list("Region" , 
                                                                      "Terrorist group",
                                                                      "Attack Type", 
                                                                      "Success", 
                                                                      "Suicide"), 
                                                       selected = "Region")),
                                    column(3,
                                           selectInput("selBox2", 
                                                       h3("Quantitative Variable"), 
                                                       choices = list(
                                                         "Number of attacks", 
                                                         "Number of fatalities",
                                                         "Number of injured", 
                                                         "Economic Impact"), 
                                                       selected = "Number of fatalities"))
                                  ),
                                  plotOutput("timePlot")
                                )
                        )
                      )
                    )
)


data = read.csv("raw_data.csv", header = T)

# Select variables
sel_var <- c("eventid", "iyear", "imonth", "iday", "country_txt", "region_txt", "provstate", "city", "latitude", "longitude", "specificity", "vicinity", "summary", "crit1", "crit2", "crit3", "doubtterr", "multiple", "success", "suicide", "attacktype1_txt", "targtype1_txt", "targsubtype1_txt", "natlty1_txt", "gname", "guncertain1", "nperps", "nperpcap", "claimed", "weaptype1_txt", "weapsubtype1_txt", "nkill", "nkillter", "nwound", "nwoundte", "property", "propextent_txt", "ishostkid", "ransom", "scite1", "scite2", "scite3", "INT_LOG", "INT_IDEO", "INT_MISC", "INT_ANY")
data <- select(data, sel_var)

#to remove the rows with null values in specific columns
completeFun <- function(fdata, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


server <- shinyServer(function(input, output, session){
  
  data1 <- data
  
  box1 <- reactive({switch(input$selBox1, 
                           "Region" = "region_txt", 
                           "Terrorist group" = "gname",
                           "Attack Type" = "attacktype_txt", 
                           "Success" = "success", 
                           "Suicide" = "suicide")})
  
  box2 <- reactive({switch(input$selBox2, 
                           "Number of attacks" = "nattacks", 
                           "Number of fatalities" = "nkill",
                           "Number of injured" = "nwound", 
                           "Economic Impact" = "propextent_txt")})
  
  output$timePlot <- renderPlot({
    
    if(box2() == "nattacks"){
      data1 <- completeFun(data1, c("iyear", box1()))
      data1 <- data1 %>% group_by(iyear, data1[,which(colnames(data1) == box1())]) %>% tally(sort = TRUE)
      colnames(data1) <- c("year", box1(), box2())
    }
    
    else{
      data1 <- completeFun(data1, c("iyear", box2(), box1()))
      data1 <- aggregate(data1[,which(colnames(data1) == box2())], by=list(year = data1$iyear, c = data1[,which(colnames(data1) == box1())]), FUN=sum)
      colnames(data1) <- c("year", box1(), box2())
    }
    
    ggplot(data1, aes(x = unlist(data1[,1]), y = unlist(data1[,3]), colour = unlist(data1[,2]), group = interaction(unlist(data1[,2])))) + geom_point(size=3) + geom_line(size=1)
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

