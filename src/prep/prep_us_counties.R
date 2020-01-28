# prep_us_counties.R
library(tidyverse)

#### LAEA
us_counties_prep <- usmap::us_map(regions="counties")

length(unique(us_counties_prep$group))

us_counties <- usmap::us_map(regions="counties") %>% 
  sf::st_as_sf(coords=c("x","y"),crs=usmap::usmap_crs()) %>% 
  mutate(fips=as.numeric(as.character(fips))) %>%
  group_by(group,fips) %>%
  summarize(do_union=FALSE) %>% sf::st_cast('LINESTRING') %>% sf::st_cast("POLYGON")

sf::st_write(us_counties,"spatial/shp/us_counties/us_counties_laea.shp",delete_layer=TRUE)

# states
us_states <- usmap::us_map(regions="states") %>% 
  sf::st_as_sf(coords=c("x","y"),crs=usmap::usmap_crs()) %>% 
  mutate(fips=as.numeric(as.character(fips))) %>%
  group_by(group,fips) %>%
  summarize(do_union=FALSE) %>% sf::st_cast('LINESTRING') %>% sf::st_cast("POLYGON")

sf::st_write(us_states,"spatial/shp/us_states/us_states_laea.shp",delete_layer=TRUE)

### SHP
counties_shp <- sf::read_sf("~/Downloads/tl_2013_us_county/tl_2013_us_county.shp")
# counties_simplify <- counties_shp %>% rmapshaper::ms_simplify(keep=0.1)
counties_simplify <- counties_shp %>% rmapshaper::ms_simplify(keep=0.07)
counties_simplify_wgs <- counties_simplify %>% 
  sf::st_transform(crs=4326) %>% 
  select(FIPS=GEOID,Name=NAMELSAD)

sf::st_write(counties_simplify_wgs,"spatial/shp/us_counties/us_counties_wgs.shp")

# ggp::obj_size(us_counties)
# ggp::obj_size(counties_simplify)
# 
# ggplot() + geom_sf(data=counties_shp,aes())
