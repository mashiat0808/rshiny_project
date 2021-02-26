library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(forecast)
library(ggplot2)
library(dplyr)

library(plotly)
#dataframe manipulation
covid_country <-read.csv(file.choose())

df <- covid_country[,c(3,4)]
df <- df %>% select(date,total_vaccinations,)%>%group_by(date)%>%filter(!is.na(total_vaccinations))%>%filter(total_vaccinations==max(total_vaccinations))
df$date <-  as.Date(df$date, format = "%Y-%m-%d")
datats <-ts(df, frequency=29)
      
# Define UI for application that draws a histogram
ui <- fluidPage(
    #app title 
    titlePanel("Shiny - First Interactive Visualization Example"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
        ),
        
        mainPanel(
            plotOutput(outputId = "plot1")
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input,output)({
    
    
    output$plot1 <- renderPlot({
        
        model2 <- holt(df$total_vaccinations,46)
        
        mn <- data.frame(model2)
        mn$total_vaccinations <- mn$Point.Forecast
        
        mn <- data.frame(date = row.names(mn), mn); rownames(mn) <- NULL
        df2 <- mn[,c(1,2)]
        plot(model2)
        
    })
})

# Run the application 
shinyApp(ui = ui, server = server)
