#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(jsonlite)
library(highcharter)
library(geojson)
library(geojsonio)
library(geojsonlint)
library(here)

nuclearWorld <- read_csv("nuclearPowerWorld.csv") 
x <- c("Year", "Power")
y <- sprintf(c("{point.%s:.0f}","{point.%s:.4f} TWh"), c("year", "electricity_TWh"))
tltip1 <- tooltip_table(x, y)

nuclearIndia <- read_csv("nuclearPowerIndia.csv") 
a <- c("Year", "Power")
b <- sprintf(c("{point.%s:.0f}","{point.%s:.4f} TWh"), c("year", "production"))
tltip2 <- tooltip_table(a, b)


nuclearLocation <- read_csv("nuclearPowerLocation.csv") |>
  geojson_json()

india <- geojson_read("India.geojson")







# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$nuclearPowerLocation <- renderHighchart(highchart(type = "map") |>
                                                   # hc_chart(backgroundColor = "#132240") |>
                                                   
                                                   hc_add_series(mapData = india, 
                                                                 showInLegend = FALSE,
                                                                 nullColor ="#0F6CB8",
                                                                 borderColor = "#FAFAFA",
                                                                 borderWidth = 0.4) |>
                                                   
                                                   hc_add_series(data = nuclearLocation,
                                                                 type = "mappoint",
                                                                 color = hex_to_rgba("#FA0D06", 0.8),
                                                                 geojson = TRUE,
                                                                 dataLabels = list(enabled = FALSE),
                                                                 marker = list(lineWidth = 2, lineColor = "#DaF7a6" , radius = 6),
                                                                 showInLegend = FALSE,
                                                                 name = "",
                                                                 tooltip = list(pointFormat = "{point.name}", 
                                                                                borderColor = "#FAFAFA",
                                                                                borderWidth = 2)
                                                   ) |>
                                                   hc_title(text = "Nuclear Power Plants", 
                                                            fontFamily = 'serif', 
                                                            fontWeight = "bold", 
                                                            style = list(color = "#00000D")) |>
                                                   hc_subtitle(text = "Scigram Technologies Foundation - Summer School", 
                                                               verticalAlign = "bottom",
                                                               align = 'right', 
                                                               style = list(color = "#00000D")) |>
                                                   hc_mapNavigation(enabled = TRUE))
  
  output$nuclearPowerWorld <- renderHighchart(nuclearWorld[8619:8675,]  |>
                                                hchart('scatter', 
                                                       hcaes(x = year, y = electricity_TWh),
                                                       color = "#6b007b") |>
                                                
                                                
                                                
                                                hc_chart(backgroundColor = "#fffff9") |>
                                                
                                                hc_xAxis(title = list(text = "Year")) |> 
                                                
                                                hc_yAxis(title = list(text = "Electricity Produced (TWh)")) |> 
                                                
                                                
                                                hc_tooltip(
                                                  useHTML = TRUE,
                                                  headerFormat = "",
                                                  pointFormat = tltip1 
                                                ) |> 
                                                
                                                hc_title(text = "Trend of World - Nuclear Power Production",
                                                         fontFamily = 'serif', 
                                                         fontWeight = "bold", 
                                                         style = list(color = "#1c2833")) |>
                                                
                                                hc_subtitle(text = "Scigram Technologies Foundation - Summer School",
                                                            verticalAlign = "bottom", 
                                                            align = 'right', 
                                                            style = list(color = "#1c2833")))
  
  output$nuclearPowerIndia <- renderHighchart(nuclearIndia  |>
                                                hchart('scatter', 
                                                       hcaes(x = year, y = production),
                                                       color = "#0050eb") |>
                                                
                                                hc_chart(backgroundColor = "#fffff9") |>
                                                
                                                hc_xAxis(title = list(text = "Year")) |> 
                                                
                                                hc_yAxis(title = list(text = "Electricity Produced (TWh)")) |> 
                                                
                                                hc_tooltip(
                                                  useHTML = TRUE,
                                                  headerFormat = "",
                                                  pointFormat = tltip2 
                                                ) |> 
                                                
                                                hc_title(text = "Trend of World - Nuclear Power Production", fontFamily = 'serif', fontWeight = "bold", style = list(color = "#1c2833")) |>
                                                
                                                hc_subtitle(text = "Scigram Technologies Foundation - Summer School", verticalAlign = "bottom", align = 'right', style = list(color = "#1c2833")))
  
 
}



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(" Scigram Technologies Foundation - Summer School 2023"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(1),
    
    column(8, 
           fluidRow(
           includeHTML("1.Rhtml")
                    ),
           fluidRow(
             column(1),
             column(7,
                    highchartOutput('nuclearPowerWorld'))
                    ),
           fluidRow(
           includeHTML("2.Rhtml")
                    ),
           fluidRow(
             column(1),
             column(7,
                    highchartOutput('nuclearPowerIndia'))
                    ),
           fluidRow(
           includeHTML("3.Rhtml")
                    ),
           fluidRow(
             column(1),
             column(7,
                    highchartOutput('nuclearPowerLocation'))
                    ),
           fluidRow(
             includeHTML("4.html")
           ),
          ),
    
    column(1),
  
    column(2, 
           includeHTML("Aditya_photo.html"),
           h3("Aditya Phadtare"),
           includeHTML("Aditya.Rhtml"))
    
             
  )## fluidrow end            
  
)## fluidpage end


# Run the application 
shinyApp(ui = ui, server = server)
