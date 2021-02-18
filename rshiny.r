
library(maps)
library(mapproj)
library(tidyverse)
library(dplyr)
library(plotly)
countries <- read.csv(file.choose())

worldmap <-countries%>%select(country,total_vaccinations,vaccines,iso_code)%>%group_by(country)%>%filter(!is.na(total_vaccinations))%>%filter(total_vaccinations==max(total_vaccinations))
head(worldmap)
l <- list(color = toRGB("grey"), width = 0.5)
worldmapplot <-plot_ly(worldmap, type='choropleth', locations=worldmap$iso_code, z=worldmap$total_vaccinations, text=worldmap$vaccines,color=worldmap$total_vaccinations, colorscale="Blues", marker = list(line = l))
worldmapplot <- worldmapplot %>% colorbar(title = 'Total number of vaccinations')

worldmapplot

library(shiny)
# User interface ----
ui <- fluidPage(
  titlePanel("Covid-19 Vaccination"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Worldmap of covid 19 vaccinations"),
      
      selectInput("var"),
      
    ),
    
    mainPanel(plotOutput("map"))
  )
)

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
   
    worldmapplot
    
    })
}

# Run app ----
shinyApp(ui, server)