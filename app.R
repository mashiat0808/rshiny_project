library(maps)
library(mapproj)
library(tidyverse)
library(dplyr)
library(plotly)
library(shiny)
library(webshot)
library(ggplot2)
library(forecast)
library(ggplot2)

#data prep
countries <- read.csv(file.choose())

#for 1.1
worldmap <-countries%>%select(country,total_vaccinations,vaccines,iso_code)%>%group_by(country)%>%filter(!is.na(total_vaccinations))%>%filter(total_vaccinations==max(total_vaccinations))
head(worldmap)
worldmap.hover <- with(worldmap, paste(country, '<br>', "Total Number of Vaccination:", total_vaccinations,  '<br>',"Vaccine name: ", vaccines, "<br>"))
l <- list(color = toRGB("grey"), width = 0.5)

#for 1.2
country.data <- countries[,c(1,3,8)]

#for 1.3
pred <- countries[,c(3,4)]
pred <- pred %>% select(date,total_vaccinations,)%>%group_by(date)%>%filter(!is.na(total_vaccinations))%>%filter(total_vaccinations==max(total_vaccinations))
pred <-  as.Date(pred$date, format = "%Y-%m-%d")
datats <-ts(df, frequency=29)

ui <- fluidPage(
    #app title 
    titlePanel("Presenting data regarding total vaccinations so far of COVID-19 Vaccine"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "region", label= "Country:", 
                        choices= as.factor(country.data$country) %>% levels() %>% as.list, selected = "Albania"),
            
            
            
        ),
        
        mainPanel(
            tabsetPanel(type= "tab",
                        tabPanel("World Data", plotlyOutput(outputId = "plot1")),
                        tabPanel("Country wise data", plotlyOutput(outputId = "distPlot")),
                        tabPanel("Prediction", plotOutput(outputId = "plot1"))

        )
    )
)
)


server <- function(input,output)({
    
    ##plot for 1.1
    output$plot1 <- renderPlotly({
        worldmapplot <-plot_ly(worldmap, type='choropleth', locations=worldmap$iso_code,
                               z=worldmap$total_vaccinations, text=worldmap.hover,color=worldmap$total_vaccinations, colorscale="Teals", marker = list(line = l))
        
        return(worldmapplot)
        
    })
    
    #plot for 1.2
    output$distplot <- renderPlotly({ 
        data <- subset(country.data, country== inputId$region)
        
        
        country.plot <-plot_ly(data, x = data$date, y = data$number, type= 'scatter', mode= 'lines+markers',line = list(color = 'rgb(205, 12, 24)'))
        country.plot<- country.plot %>%  layout(title = "Number of Vaccinations done by Selected Country per day", xaxis = list(title = "Dates"), yaxis = list (title = "Numbers"))
        country.plot
        
        
    })
    
    
    #plot for 1.3
    output$plot2 <- renderPlot({
        
        model2 <- holt(df$total_vaccinations,46)
        plot(model2)
    })
})


# Run the application 
shinyApp(ui = ui, server = server)
