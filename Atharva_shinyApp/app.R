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

batteryCost <- read_csv("priceLithiumBattery.csv") 
a <- c("Year","Price (USD)")
b <- sprintf(c("{point.%s:.0f}","USD {point.%s:.2f}"), c("year","price"))
tltip3 <- tooltip_table(a, b)



world <- geojson_read("world.geojson")

india <- geojson_read("India.geojson")

feLocation <- read_csv("Fe_reserve.csv") |>
  geojson_json()

liLocation <- read_csv("Li_reserve.csv") |>
  geojson_json()

ironYield <- read_csv("countries_yieldIronReserve.csv") 
w <- c("Country", "Iron Reserve in billion Tonnes")
x <- sprintf(c("{point.%s:.0f}","{point.%s:.4f} billion Tonnes"), c("country", "reserve_billionTones"))
tltip1 <- tooltip_table(w, x)

lithiumYield <- read_csv("countries_yieldLithiumReserve.csv") 
y <- c("Country", "Lithium Reserve in Tonnes")
z <- sprintf(c("{point.%s:.0f}","{point.%s:.4f} Tonnes"), c("country","reserve_Tons"))
tltip2 <- tooltip_table(y, z)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$batteryCost <- renderHighchart(batteryCost  |>
                                          hchart('scatter', 
                                                 hcaes(x = year, y = production),
                                                 color = "#0050eb") |>
                                          
                                          hc_chart(backgroundColor = "#fffff9") |>
                                          
                                          hc_xAxis(title = list(text = "Year")) |> 
                                          
                                          hc_yAxis(title = list(text = "Price (USD)")) |> 
                                          
                                          hc_tooltip(
                                            useHTML = TRUE,
                                            headerFormat = "",
                                            pointFormat = tltip3 
                                          ) |> 
                                          
                                          hc_title(text = "Decrease in Price of Lithium Batteries", 
                                                   fontFamily = 'serif', 
                                                   fontWeight = "bold", 
                                                   style = list(color = "#1c2833")) |>
                                          
                                          hc_subtitle(text = "Scigram Technologies Foundation - Summer School", 
                                                      verticalAlign = "bottom", 
                                                      align = 'right', 
                                                      style = list(color = "#1c2833")))
  
  
  output$ironYield <- renderHighchart(ironYield[8619:8675,]  |>
                                                hchart('column', 
                                                       hcaes(x = country, y = reserve_billionTones),
                                                       color = "#6b007b") |>
                                                
                                                
                                                
                                                hc_chart(backgroundColor = "#fffff9") |>
                                                
                                                hc_xAxis(title = list(text = "Country")) |> 
                                                
                                                hc_yAxis(title = list(text = "Iron Reserve in billion Tonnes")) |> 
                                                
                                                
                                                hc_tooltip(
                                                  useHTML = TRUE,
                                                  headerFormat = "",
                                                  pointFormat = tltip1 
                                                ) |> 
                                                
                                                hc_title(text = "Top ten countries with highest Iron yield",
                                                         fontFamily = 'serif', 
                                                         fontWeight = "bold", 
                                                         style = list(color = "#1c2833")) |>
                                                
                                                hc_subtitle(text = "Scigram Technologies Foundation - Summer School",
                                                            verticalAlign = "bottom", 
                                                            align = 'right', 
                                                            style = list(color = "#1c2833")))
  
  
  
  output$feLocation <- renderHighchart(highchart(type = "map") |>
                                         # hc_chart(backgroundColor = "#132240") |>
                                         
                                         hc_add_series(mapData = india, 
                                                       showInLegend = FALSE,
                                                       nullColor ="#0F6CB8",
                                                       borderColor = "#FAFAFA",
                                                       borderWidth = 0.4) |>
                                         
                                         hc_add_series(data = feLocation,
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
                                         hc_title(text = "Indian Iron Reserves", 
                                                  fontFamily = 'serif', 
                                                  fontWeight = "bold", 
                                                  style = list(color = "#00000D")) |>
                                         hc_subtitle(text = "Scigram Technologies Foundation - Summer School", 
                                                     verticalAlign = "bottom",
                                                     align = 'right', 
                                                     style = list(color = "#00000D")) |>
                                         hc_mapNavigation(enabled = TRUE))
  
  
  
  output$lithiumYield <- renderHighchart(lithiumYield[8619:8675,]  |>
                                           hchart('column', 
                                                  hcaes(x = country, y = reserve_Tons),
                                                  color = "#6b007b") |>
                                           
                                           hc_chart(backgroundColor = "#fffff9") |>
                                           
                                           hc_xAxis(title = list(text = "Country")) |> 
                                           
                                           hc_yAxis(title = list(text = "Lithium Reserve in Tonnes")) |> 
                                           
                                           
                                           hc_tooltip(
                                             useHTML = TRUE,
                                             headerFormat = "",
                                             pointFormat = tltip1 
                                           ) |> 
                                           
                                           hc_title(text = "Top ten countries with highest Lithium yield",
                                                    fontFamily = 'serif', 
                                                    fontWeight = "bold", 
                                                    style = list(color = "#1c2833")) |>
                                           
                                           hc_subtitle(text = "Scigram Technologies Foundation - Summer School",
                                                       verticalAlign = "bottom", 
                                                       align = 'right', 
                                                       style = list(color = "#1c2833")))
  
  output$liLocation <- renderHighchart(highchart(type = "map") |>
                                         # hc_chart(backgroundColor = "#132240") |>
                                         
                                         hc_add_series(mapData = world, 
                                                       showInLegend = FALSE,
                                                       nullColor ="#0F6CB8",
                                                       borderColor = "#FAFAFA",
                                                       borderWidth = 0.4) |>
                                         
                                         hc_add_series(data = liLocation,
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
                                         hc_title(text = "World Lithium Reserves", 
                                                  fontFamily = 'serif', 
                                                  fontWeight = "bold", 
                                                  style = list(color = "#00000D")) |>
                                         hc_subtitle(text = "Scigram Technologies Foundation - Summer School", 
                                                     verticalAlign = "bottom",
                                                     align = 'right', 
                                                     style = list(color = "#00000D")) |>
                                         hc_mapNavigation(enabled = TRUE))
  
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
             includeHTML("1.html")
           ),
           
           fluidRow(
             column(1),
             column(7,
                    highchartOutput('batteryCost'))
           ),
           fluidRow(
             includeHTML("2.html")
           ),
           
           fluidRow(
             column(1),
             column(7,
                    highchartOutput('ironYield'))
           ),
           
           fluidRow(
             includeHTML("3.html")
           ),
           
           fluidRow(
             column(1),
             column(7,
                    highchartOutput('feLocation'))
           ),
           
           fluidRow(
             includeHTML("4.html")
           ),
           
           fluidRow(
             column(1),
             column(7,
                    highchartOutput('lithiumYield'))
           ),
           
           fluidRow(
             includeHTML("5.html")
           ),
           
           fluidRow(
             column(1),
             column(7,
                    highchartOutput('liLocation'))
           ),
           
           fluidRow(
             includeHTML("6.html")
           ),
    ),
    
    column(1),
    
    column(2, 
           includeHTML("Atharva_photo.html"),
           h3("Atharva Phadtare"),
           includeHTML("Atharva.Rhtml"))
    
    
  )## fluidrow end            
  
)## fluidpage end


# Run the application 
shinyApp(ui = ui, server = server)
