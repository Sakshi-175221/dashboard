# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#####calling the libraries####
#install.packages("leaflet.extras")

library(shiny)
library(shinydashboard)
## 
## Attaching package: 'shinydashboard'
## The following object is masked from 'package:graphics':
## 
##     box
library(leaflet)
library(dplyr) 
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(readr)
library(tidyverse)
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
## ✔ purrr     1.0.2
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(leaflet.extras)



#### Reading the data file####
gtd <- read_csv("C:/Users/USER/Downloads/gtd_clean.csv")
## Rows: 144606 Columns: 13
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (8): country, region, provstate, city, attacktype_txt, target_type_txt, ...
## dbl (5): year, month, latitude, longitude, nkill
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## using dplyr to group the target type and then grouping them year wise 
gtd_top_targets <- gtd %>%
  filter(target_type_txt %in% c('Citizens', 'Military', 'Police',
                                'Government', 'Business', 'Religious Institutions', 'Airports','Journalists'))

gtd_19_17 <- gtd %>%
  filter(year %in% c(1970, 2017))


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Global Terrorism Dashboard")  


#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))))


#body if the dashboard 

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3"))


frow2 <- fluidRow(
  box(title = "Comparison of Attacks overtime"
      ,status = "primary"
      ,solidHeader = TRUE
      ,collapsible = TRUE
      ,leafletOutput("mymap", height = "300px")
      ,sliderInput("slider", "Years:", 1970, 2017, 2000))
  ,box(
    title = "Number of Kills by Attack Type"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("Killbyattacktype", height = "300px"),
    selectInput("selector", h3("Select Region"),
                choices = list("Australasia" = "Australasia & Oceania", "Central America" = "Central America & Caribbean",
                               "Central Asia" = "Central Asia", "East Asia" = "East Asia", "Eastern Europe" = "Eastern Europe",
                               "MENA" = "Middle East & North Africa", "North America" = "North America",
                               "South America" = "South America", "South Asia" = "South Asia", "Southeast Asia" = "Southeast Asia",
                               "Sub-Saharan Africa" = "Sub-Saharan Africa", "Western Europe" = "Western Europe"), selected = 1)))

frow3 <- fluidRow(
  box(
    title = "Who were the main targets"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("targetkilled", height = "300px")
    ,radioButtons("radio", h3("Select Year"),
                  choices = list("1970" = 1970, "1975" = 1975, "1980" = 1980, "1985" = 1985,
                                 "1990" = 1990, "1995" = 1995, "2000" = 2000, "2005" = 2005,
                                 "2010" = 2010,"2015" = 2015, "2017" = 2017),selected = 1970, inline = TRUE))
  ,box(
    title = "Number of kills per Region"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("killbyregion", height = "300px")
    ,radioButtons("yearsradio", h3("Select Year"), choices = list("1970" = 1970, "1975" = 1975,
                                                                  "1980" = 1980, "1985" = 1985, "1990" = 1990, "1995" = 1995, "2000" = 2000, "2005" = 2005,
                                                                  "2010" = 2010,"2015" = 2015, "2017" = 2017), selected = 1970, inline = TRUE)))





# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3)    


#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Global Terrorism Statistics Dashboard', header,sidebar, body, skin='yellow')    



# create the server functions for the dashboard  
server <- function(input, output) {
  
  #some data manipulation to derive the values
  total_kills <- sum(gtd$nkill)
  countries <- gtd %>% group_by(country) %>% summarise(value = sum(nkill)) %>% filter(value==max(value))
  weapon.type <- gtd %>% group_by(weapon_type) %>% summarise(value = sum(nkill)) %>% filter(value==max(value))
  attacker <- gtd %>% group_by(nationality) %>% summarise(value = sum(nkill)) %>% filter(value==max(value))
  
  
  
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(countries$value, format="d", big.mark=',')
      ,paste('Top Country:',countries$country)
      ,color = "purple")
    
    
  })
  
  
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(total_kills, format="d", big.mark=',')
      ,paste('Top nationality:',attacker$nationality)
      ,color = "green")
    
  })
  
  
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(weapon.type$value, format="d", big.mark=',')
      ,paste('Top Weapon Used:',weapon.type$weapon_type)
      ,color = "yellow")
    
  })
  
  
  #creating the plotOutput content
  datasetInput <- reactive({
    
    filter(gtd, gtd$year <= input$yearsradio) #%>% arrange(desc(Sales))
    
  })
  
  output$killbyregion <- renderPlot({
    data_filter <- datasetInput()
    
    ggplot(data = data_filter,
           aes(x=region, y=nkill)) +
      geom_bar(position = "dodge", stat = "identity", fill="#FF5733")+ ylab("Count of kills") +
      xlab("Region") + theme(legend.position="bottom"
                             ,plot.title = element_text(size=15, face="bold"), axis.text.x=element_text(angle=90)) +
      ggtitle("Kills per Region")
  })
  
  datasetregion <- reactive({
    
    filter(gtd, gtd$region <= input$selector)
  })
  
  output$Killbyattacktype <- renderPlot({
    data_region <- datasetregion()
    
    ggplot(data = data_region,
           aes(x=attacktype_txt, y=nkill)) +
      geom_bar(position = "dodge", stat = "identity", fill="#007BFF") + ylab("Count of Kills") +
      xlab("Attack Type") + theme(legend.position="bottom"
                                  ,plot.title = element_text(size=15, face="bold")) +
      ggtitle("Kills by Type of Attack")
  })
  
  datasettarget <- reactive({
    
    filter(gtd_top_targets, gtd_top_targets$year %in% input$radio)
  })
  
  output$targetkilled <- renderPlot({
    data_target <- datasettarget()
    
    ggplot(data = data_target,
           aes(y=target_type_txt, x=nkill,color=target_type_txt)) +
      geom_bar(position = "dodge", stat = "identity",fill="yellow") + xlab("Count of Kills") + ylab("Target Type") +
      theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) +
      ggtitle("Type of Target") + coord_polar(theta="y")
  })
  
  datasetyears <- reactive({
    
    filter(gtd, gtd$year %in% input$slider)
  })
  ?filter
  
  #create the map
  output$mymap <- renderLeaflet({
    data_years <- datasetyears()
    
    leaflet(data_years) %>%
      setView(lng = 43.6793, lat = 33.2232, zoom = 2) %>%
      addTiles() %>%
      addCircles(data = data_years, lat = ~ latitude, lng = ~ longitude, weight = 1,
                 radius = ~sqrt(nkill)*25000, popup = ~as.character(nkill),
                 label = ~as.character(paste0("Kills: ", sep = " ", nkill)), fillOpacity = 0.5)
  })
  
}


shinyApp(ui, server) #this helps te shiny app to run all the codes on the browser.
