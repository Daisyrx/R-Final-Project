
library(shiny)
library(shinydashboard)
pacman::p_load(tidyverse,readr,knitr,ggplot2,magrittr,dplyr)
crime_shiny <- read.csv("crime_shiny.csv")
p<-ggplot(crime_shiny) + 
    geom_bar(aes(x = Year,y = ..prop..,fill=Year)) + 
    facet_wrap(~ City, nrow = 4)
y <- ggplot(crime_shiny) + 
    geom_bar(aes(x = City, y = ..prop..,group=1)) + 
    facet_wrap(~ Year, nrow = 2)

# Define UI for application that draws a histogram

ui <- dashboardPage(
    dashboardHeader(title = "Crime Incident Reports"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Map",tabName = "map",icon = icon("dashboard")),
            menuItem("City",tabName = "city",icon = icon("th")),
            menuItem("Year",tabName = "year",icon = icon("th"))
        )
    ),
    dashboardBody(
    tabItems(
        tabItem(tabName = "map",
                h2("Map of Crime Incident")),
        tabItem(tabName = "city",
                h2("Crime Differ by Cities"),
                selectInput("City", "Select a City:", 
                            choices = unique(crime_shiny$City)),
                            
                wellPanel(h3(textOutput("caption")),
                            plotOutput("CityPlot")
                            )
        
                 ),
        tabItem(tabName = "year",
                h2("Crime Differ by Years"),
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
