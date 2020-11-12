library(readr)
library(tidyverse)
library(magrittr)
library(lubridate)
library(leaflet)
library(usmap)
library(rgdal)
library(maps)
library(geojsonio)
library(tigris)
library(leaflet.minicharts)

PAF_hurricane <- read_csv("PAF_hurricane.csv")

PAF_hurricane %<>% rename(GEO_ID = fips)

PAF_2009 <- filter(PAF_hurricane, PAF_hurricane$declarationDate == 2009)
temp2009 <- group_by(PAF_2009, GEO_ID)
display2009 <- summarise(temp2009, state = unique(state),
                                   Amount = mean(totalObligated))
display2009 %<>% mutate(year = 2009)

PAF_2010 <- filter(PAF_hurricane, PAF_hurricane$declarationDate == 2010)
temp2010 <- group_by(PAF_2010, GEO_ID)
display2010 <- summarise(temp2010, state = unique(state),
                         Amount = mean(totalObligated))
display2010 %<>% mutate(year = 2010)

PAF_2011 <- filter(PAF_hurricane, PAF_hurricane$declarationDate == 2011)
temp2011 <- group_by(PAF_2011, GEO_ID)
display2011 <- summarise(temp2011, state = unique(state),
                         Amount = mean(totalObligated))
display2011 %<>% mutate(year = 2011)


PAF_2012 <- filter(PAF_hurricane, PAF_hurricane$declarationDate == 2012)
temp2012 <- group_by(PAF_2012, GEO_ID)
display2012 <- summarise(temp2012, state = unique(state),
                         Amount = mean(totalObligated))
display2012 %<>% mutate(year = 2012)

PAF_2013 <- filter(PAF_hurricane, PAF_hurricane$declarationDate == 2013)
temp2013 <- group_by(PAF_2013, GEO_ID)
display2013 <- summarise(temp2013, state = unique(state),
                         Amount = mean(totalObligated))
display2013 %<>% mutate(year = 2013)

PAF_2014 <- filter(PAF_hurricane, PAF_hurricane$declarationDate == 2014)

PAF_2015 <- filter(PAF_hurricane, PAF_hurricane$declarationDate == 2015)

PAF_2016 <- filter(PAF_hurricane, PAF_hurricane$declarationDate == 2016)
temp2016 <- group_by(PAF_2016, GEO_ID)
display2016 <- summarise(temp2016, state = unique(state),
                         Amount = mean(totalObligated))
display2016 %<>% mutate(year = 2016)

PAF_2017 <- filter(PAF_hurricane, PAF_hurricane$declarationDate == 2017)
temp2017 <- group_by(PAF_2017, GEO_ID)
display2017 <- summarise(temp2017, state = unique(state),
                         Amount = mean(totalObligated))
display2017 %<>% mutate(year = 2017)

PAF_2018 <- filter(PAF_hurricane, PAF_hurricane$declarationDate == 2018)
temp2018 <- group_by(PAF_2018, GEO_ID)
display2018 <- summarise(temp2018, state = unique(state),
                         Amount = mean(totalObligated))
display2018 %<>% mutate(year = 2018)


PAF_NEW <- rbind(display2009, display2010, display2011, display2012, display2013, display2016, 
                 display2017, display2018)

write.csv(PAF_NEW, file = "PAF_NEW.csv", row.names = F)

# test <- group_by(PAF_hurricane, GEO_ID)

leafmap2 <- geo_join(xy, PAF_NEW, by = "GEO_ID", how = "inner")

# PAF_hurricane$obligatedDate %<>% year()

pal <- colorNumeric(palette = "RdPu", domain = leafmap2$Amount)

Encoding( x = leafmap2$NAME ) <- "UTF-8"

leafmap2$NAME <-iconv( x =leafmap2$NAME, from = "UTF-8", to = "UTF-8", sub = "" )


popup <- paste0("CountyID: ", leafmap2$GEO_ID, "<br>", 
                "County: ", as.character(leafmap2$NAME), "<br>", 
                "Mean obligated amount: ", round(leafmap2$Amount, 2))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.75)) %>%
  setView(-89.275673, 37.098, zoom = 4) %>%
  addPolygons(data = leafmap2, 
              fillColor = ~pal(Amount),
              color = "#BDBDC3",
              fillOpacity = 1, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal,
            values = leafmap2$Amount,
            position = "bottomright",
            title = "Mean obligated amount")
  # addMinicharts(
  #   leafmap2$lng, leafmap2$lat, 
  #   chartdata = leafmap2$Amount,
  #   time = leafmap2$year,
  #   colorPalette = colors,
  #   width = 45, height = 45
  # )
