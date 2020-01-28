# prep_incubator_data.R

# All data cleaned, summarized, and prepped in other files. See:
# src/prep/prep_Boustan_supersevere.R
# src/prep/prep_drought.R
# src/prep/prep_fima_nfip.R
# src/prep/prep_mit_election_data.R
# src/prep/prep_pew_surveys_incomplete.R
# src/prep/prep_us_counties.R
# src/prep/prep_wildfires_ncdp.R
# src/prep/prep_ycom_pca.R
# outdated: src/prep/prep_acs.R

# spatial data
us_counties <- sf::st_read("spatial/shp/us_counties/us_counties_laea.shp")

# perceptions prep
perceptions_pca <- read_csv("data/format/climate_perceptions_ycom_pca.csv") %>% select(-County)
drought <- read_csv("data/format/drought_usdm_2010s.csv") %>% mutate(fips=as.numeric(FIPS))
floods <- read_csv("data/format/floods_nfip_county_decades.csv") %>% mutate(fips=as.numeric(countycode)) %>% select(-countycode,-state)
longterm <- read_csv("data/format/longterm_disaster_count.csv") %>% 
  select(FIPS=`fips-new`,lt_nodrought=disaster_nodrought,lt_severe=severe,lt_supersevere=supersevere)
wildfires <- read_csv("data/format/wildfires_ncdp.csv") %>% select(fips=FIPS,wildfire_risk=Wildfire)
demographics_pct <- read_csv("data/format/mit_election_18.csv") %>% 
  select(fips,female_pct, age29andunder_pct, age65andolder_pct,
         clinton16_pct,obama12_pct,white_pct, black_pct, hispanic_pct, nonwhite_pct, rural_pct) %>%
  setNames(gsub("_pct$","",names(.)))


us_counties_ycom <- us_counties %>% left_join(perceptions_pca,by="fips")

ggplot() + geom_sf(data=us_counties_ycom,aes(fill=belief_PC1r),color=NA) +
  viridis::scale_fill_viridis()
ggplot() + geom_sf(data=us_counties_ycom,aes(fill=perception_PC1r),color=NA) +
  viridis::scale_fill_viridis()

