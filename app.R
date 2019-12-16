
library(shiny)
library(shinydashboard)
pacman::p_load(tidyverse,readr,knitr,ggplot2,magrittr,dplyr,leaflet,leaflet.extras,maps,plotly)
crime_shiny <- read.csv("crime_shiny.csv")
p<-ggplot(crime_shiny) + 
    geom_bar(aes(x = Year,y = ..prop..,fill=Year)) + 
    facet_wrap(~ City, nrow = 4)
y <- ggplot(crime_shiny) + 
    geom_bar(aes(x = City, y = ..prop..,group=1)) + 
    facet_wrap(~ Year, nrow = 2)
pp <- ggplot(crime_shiny)+
    geom_bar(aes(x = City,fill = factor(Year)),stat = "count",position = "dodge")+
    coord_flip()
py <- ggplot(crime_shiny)+
    geom_bar(aes(x = Year,fill = factor(City)),stat = "count",position = "dodge")+
    coord_flip()
    
cities <- unique(crime_shiny$City)

# Define UI for application that draws a histogram

ui <- dashboardPage(
    dashboardHeader(title = "Crime Incident Reports"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Map",tabName = "map",icon = icon("map")),
            menuItem("City",tabName = "city",icon = icon("th"),
                     menuSubItem("City in General",tabName = "a"),
                     menuSubItem("City Detail",tabName = "b")),
            menuItem("Year",tabName = "year",icon = icon("th"),
                     menuSubItem("Year in General",tabName = "c"),
                     menuSubItem("Year Detail",tabName = "d"))
        )
    ),
    dashboardBody(
    tabItems(
        tabItem(tabName = "map",
                h2("Map of Crime Incident"),
                leafletOutput(outputId = "mymap")
                ),
        
        tabItem(tabName = "a",
                h2("Crime Differ by Cities"),
                wellPanel(plotlyOutput("CityCount"))
                ),
                
        tabItem(tabName = "b",
                h2("Cities in Detail"),
                selectInput("City", "Select a City:", 
                            choices = unique(crime_shiny$City)),
                            
                wellPanel(h3(textOutput("caption")),
                            plotOutput("CityPlot")
                            )
        
                 ),
        
        tabItem(tabName = "c",
                h2("Crime Differ by Years"),
                wellPanel(plotlyOutput("YearCount"))
        ),
        
        tabItem(tabName = "d",
                h2("Years in Detail"),
                selectInput("Year", "Select a Year:", 
                            choices = unique(crime_shiny$Year)),
                
                wellPanel(h3(textOutput("cap")),
                          plotOutput("YearPlot")
                          )
                )
    
)
)
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    pal <- colorFactor(c("#999999", "#E69F00", "#56B4E9","#B2182B",
                         "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0",
                         "#92C5DE", "#4393C3", "#2166AC","#FFCC33"), domain = cities)
    
    output$mymap <- renderLeaflet({
        leaflet(crime_shiny) %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>%
            addTiles() %>% 
            addAwesomeMarkers(data = crime_shiny,lng = ~Longtitude,lat = ~Latitude,clusterOptions = T)
    })
    
    output$CityCount <- renderPlotly({
        plotly_build(pp)
    })
    
    CityText <- reactive({
        paste("Selected City:", input$City)
    })
    
    output$caption <- renderText({
        CityText()
    })

    output$CityPlot <- renderPlot({
        # plot the bar chart by Cities
        p$data <- p$data %>% group_by(City) %>% filter(City == input$City)
        p
    })
    
    output$YearCount <- renderPlotly({
        plotly_build(py)
    })
    
    YearText <- reactive({
        paste("Selected Year:", input$Year)
    })
    
    output$cap <- renderText({
        YearText()
    })
    
    output$YearPlot <- renderPlot({
        # plot the bar chart by Years
        y$data <- y$data %>% group_by(Year) %>% filter(Year == input$Year)
        y
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
