library(readr)
library(tidyverse)
library(leaflet)
library(usmap)
library(rgdal)
library(maps)
library(geojsonio)
library(tigris)
library(magrittr)

PAF_hurricane <- read_csv("PAF_hurricane.csv")
temp <- group_by(PAF_hurricane, fips)
display <- summarise(temp, state = unique(state),
                           Amount = mean(totalObligated))
                      
display %<>% rename(GEO_ID = fips)   

write.csv(display, file = "display.csv", row.names = F)
                           
xy <- geojsonio::geojson_read("gz_2010_us_050_00_5m.json", what = "sp")


xy$GEO_ID %<>% substr(start = 10, stop = 14)

leafmap <- geo_join(xy, display, by = "GEO_ID", how = "inner")

pal <- colorNumeric(palette = "RdPu", domain = leafmap$Amount)

Encoding( x = leafmap$NAME ) <- "UTF-8"

leafmap$NAME <-iconv( x =leafmap$NAME, from = "UTF-8", to = "UTF-8", sub = "" )


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
