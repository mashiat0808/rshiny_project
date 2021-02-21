library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(webshot) 
#dataframe manipulation
country.data <-read.csv(file.choose())
country.data <- country.data[,c(1,3,8)]

##country.data <- country.data %>% spread(country, daily_vaccinations)
country.data[is.na(country.data)]=0

x <-factor(country.data$country)
y <- levels(x)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Number of vaccinations daily based on country"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "region", label= "Country:", 
                        choices=y, selected = "Albania"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) ({
    
    data <- subset(country.data, country==inputId$region)
    
    
    output$distplot <- renderPlotly({ 
        mn <-plot_ly(data, x = data$date, y = data$daily_vaccinations, type= 'scatter', mode= 'lines+markers',line = list(color = 'rgb(205, 12, 24)'))
        mn
        })
})


# Run the application 
shinyApp(ui = ui, server = server)
