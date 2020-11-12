library(readr)
library(tidyverse)
library(magrittr)
library(leaflet)
library(rgdal)
library(geojsonio)
library(tigris)
library(shiny)

PAF_NEW <- read_csv("PAF_NEW.csv")
display <- read_csv("display.csv")

xy <- geojsonio::geojson_read("gz_2010_us_050_00_5m.json", what = "sp")

xy$GEO_ID %<>% substr(start = 10, stop = 14)

ui <- navbarPage("Mapping with more data",
        tabPanel("Interactive Map",
        leafletOutput("map", width="100%", height="100%"),
        selectInput("year1", "Year", c("All", unique(PAF_NEW$year))),
        leafletOutput("Leafletmap"),
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 330, height = "auto"
        )),
        
        conditionalPanel(
                'input.dataset === "PAF_NEW"'),
        
        
        
        tabPanel("Data explorer",
                 id = 'dataset',
                 fluidRow(
                    column(3,
                     selectInput("year2", "Year", c("All", unique(PAF_NEW$year))
                     )),
                 DT::dataTableOutput("table")
                 )))

server <- function(input, output) {
        output$table <- DT::renderDataTable(DT::datatable({
            data <- PAF_NEW
            if (input$year2 != "All") {
                data <- data[data$year == input$year2,]
            }
            data
        }))

        output$Leafletmap <- renderLeaflet({
          data1 <- PAF_NEW
          data2 <- display
          if(input$year1 == "All"){
              data <- data2
          }
          else{
              data <- data1[data1$year == input$year1,] 
          }
          
          data <- data %>% group_by(GEO_ID)
          
          leafmap <- geo_join(xy, data, by = "GEO_ID", how = "inner")
          
          pal <- colorNumeric(palette = "RdPu", domain = leafmap$Amount)
          
          Encoding( x = leafmap$NAME ) <- "UTF-8"
          
          leafmap$NAME <-iconv( x = leafmap$NAME, from = "UTF-8", to = "UTF-8", sub = "" )
          
          
          popup <- paste0("CountyID: ", leafmap$GEO_ID, "<br>", 
                          "County: ", as.character(leafmap$NAME), "<br>", 
                          "Mean obligated amount: ", round(leafmap$Amount, 2))
          
          leaflet() %>%
              addProviderTiles("CartoDB.Positron") %>%
              addProviderTiles(providers$Stamen.TonerLines,
                               options = providerTileOptions(opacity = 0.75)) %>%
              setView(-89.275673, 37.098, zoom = 4) %>%
              addPolygons(data = leafmap, 
                          fillColor = ~pal(Amount), 
                          color = "#BDBDC3",
                          fillOpacity = 1, 
                          weight = 1, 
                          smoothFactor = 0.2,
                          popup = popup) %>%
              addLegend(pal = pal, 
                        values = leafmap$Amount, 
                        position = "bottomright", 
                        title = "Mean obligated amount") 
          })
}

# Run the application 
shinyApp(ui = ui, server = server)
