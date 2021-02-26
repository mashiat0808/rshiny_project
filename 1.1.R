library(maps)
library(mapproj)
library(tidyverse)
library(dplyr)
library(plotly)
library(shiny)
library(webshot)


#data prep
countries <- read.csv(file.choose())


worldmap <-countries%>%select(country,total_vaccinations,vaccines,iso_code)%>%group_by(country)%>%filter(!is.na(total_vaccinations))%>%filter(total_vaccinations==max(total_vaccinations))
head(worldmap)
worldmap.hover <- with(worldmap, paste(country, '<br>', "Total Number of Vaccination:", total_vaccinations,  '<br>',"Vaccine name: ", vaccines, "<br>"))
l <- list(color = toRGB("grey"), width = 0.5)



ui <- fluidPage(
    #app title 
    titlePanel("Shiny - First Interactive Visualization Example"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
        ),
        
        mainPanel(
            plotlyOutput(outputId = "plot1")
        )
    )
)


server <- function(input,output)({
    
    
    output$plot1 <- renderPlotly({
        worldmapplot <-plot_ly(worldmap, type='choropleth', locations=worldmap$iso_code,
                               z=worldmap$total_vaccinations, text=worldmap.hover,color=worldmap$total_vaccinations, colorscale="Teals", marker = list(line = l))
        
        worldmapplot
        
    })
})


# Run the application 
shinyApp(ui = ui, server = server)