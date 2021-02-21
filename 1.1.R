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
    titlePanel("Geographical distribution of the COVID 19 vaccines"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel("In the map we can see the country name presented with the amount of vaccines they have managed to distribute so far and the name of the company which produced the vaccine."
        ),
        
        mainPanel(
            plotlyOutput(outputId = "plot1")
        )
    )
)
