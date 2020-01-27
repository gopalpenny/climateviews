# prep_us_counties.R

us_counties_prep <- usmap::us_map(regions="counties")

length(unique(us_counties_prep$group))

us_counties <- usmap::us_map(regions="counties") %>% 
  sf::st_as_sf(coords=c("x","y"),crs=usmap::usmap_crs()) %>% 
  mutate(fips=as.numeric(as.character(fips))) %>%
  group_by(group,fips) %>%
  summarize(do_union=FALSE) %>% sf::st_cast('LINESTRING') %>% sf::st_cast("POLYGON")

sf::st_write(us_counties,"spatial/shp/us_counties/us_counties_laea.shp",delete_layer=TRUE)
