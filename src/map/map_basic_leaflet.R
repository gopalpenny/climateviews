library(leaflet)
library(tidyverse)

# The map is too slow!! Use regular plots for now.

data <- read_csv("results/output/incubator_data.csv")
us_counties_sf <- sf::read_sf("spatial/shp/us_counties/us_counties_wgs.shp") %>% 
  dplyr::left_join(data,by="FIPS")


ycomPC1_Pal <- colorNumeric(palette = "viridis",domain=us_counties_sf$PC1)
D3_Pal <- colorNumeric(palette = "magma",domain=us_counties_sf$D3_pct_mean)

leaflet() %>%
  # addTiles(options = providerTileOptions(noWrap = TRUE), group="Map") %>%
  # leafem::addMouseCoordinates() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addScaleBar(position = c("bottomright"), options = scaleBarOptions()) %>%
  setView(lng = -95, lat = 35, zoom=4) %>%
  addPolygons(data=us_counties_sf, fillColor=~ycomPC1_Pal(PC1), color=~ycomPC1_Pal(PC1), fillOpacity=0.8, group="YCOM") %>%
  addPolygons(data=us_counties_sf, fillColor=~D4_Pal(D4_pct_mean), color=~D3_Pal(D3_pct_mean), fillOpacity=0.8, group="Drought") %>%
  addLayersControl(#baseGroups = c("Map","Atlas"),#overlayGroups = c("Red","Blue") ,
                   overlayGroups = c("YCOM","Wildfire"),
                   options = layersControlOptions(collapsed = FALSE))


