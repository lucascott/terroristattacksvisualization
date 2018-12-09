library(shiny)
library(leaflet)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)
library(RecordLinkage)
library(stringr)
library(shinyWidgets)

# Import all the custom functions
source("functions.R")

Sys.setlocale('LC_ALL','C')

# PREPROCESSING
gtd = read.csv("./gtd.csv", header = T)
gtd = gtd[c("iyear","imonth", "iday", "extended","country_txt", "region_txt", "provstate", "latitude", "longitude", "multiple", "success", "suicide", "attacktype1_txt", "targtype1_txt", "natlty1_txt", "gname", "guncertain1", "nperps", "nperpcap", "weaptype1_txt", "nkill", "nkillter","nwound", "property", "ishostkid", "ransom", "INT_LOG", "INT_IDEO", "INT_MISC", "scite1")]
gtd$scite1 = str_replace_all(gtd$scite1, "[[:punct:]]", "")
gtd$scite1[gtd$scite1 == "\"\"" | gtd$scite1 == "\"" | gtd$scite1 == ""] <- NA
gtd = completeFun(gtd, c("scite1"))
gtd$country_txt = as.character(gtd$country_txt)
gtd$gname = as.character(gtd$gname)
gtd$scite1 = as.character(gtd$scite1)

data = read.csv("./data.csv", header = T)
set.seed(0)
data1 = slice(data, sample(1:n())) # shuffle
data1 = completeFun(data1[1:70000,], c("nkill", "latitude", "longitude"))
data1$country = as.character(data1$country)
data1$region = as.character(data1$region)
data1$group_name = data1$group_name %>% replace_na("Unknown")
data1$group_name = as.character(data1$group_name)
data1$attack_type = as.character(data1$attack_type)

categoryList = c("Target", "Type of attack", "Weapon")
impactList = c("Number of attacks", "Number of fatalities", "Number of injured", "Economic Impact")
regionList = lapply(as.list(data1 %>% distinct(region)), as.character)[[1]]

shiny::runApp()