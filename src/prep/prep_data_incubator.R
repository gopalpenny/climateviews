# prep_incubator_data.R

# All data cleaned, summarized, and prepped in other files. See github.com/gopalpenny/climateviews:
# src/scrape/scrape_county_fips.R
# src/prep/prep_Boustan_supersevere.R
# src/prep/prep_drought.R
# src/prep/prep_fima_nfip.R
# src/prep/prep_mit_election_data.R
# src/prep/prep_pew_surveys_incomplete.R
# src/prep/prep_us_counties.R
# src/prep/prep_wildfires_ncdp.R
# src/prep/prep_ycom_pca.R
# outdated: src/prep/prep_acs.R

# counties data -- codes and spatial
us_counties_fips <- read_csv("data/format/usa_county_fips_codes.csv") %>% select(-county_lower)
us_counties_sf <- sf::st_read("spatial/shp/us_counties/us_counties_laea.shp")

# perceptions prep
perceptions <- read_csv("data/format/climate_perceptions_ycom_pca_with_clusters.csv") #%>% select(-GeoName,-GeoType)

# exposure prep
drought <- read_csv("data/format/drought_usdm_2010s.csv") %>% select(-State,-County) #%>% mutate(fips=as.numeric(FIPS))
floods <- read_csv("data/format/floods_nfip_county_decades.csv") %>% mutate(fips=as.numeric(countycode)) %>% select(-countycode,-state)
longterm <- read_csv("data/format/longterm_disaster_count.csv") %>% 
  select(FIPS=`fips-new`,lt_nodrought=disaster_nodrought,lt_severe=severe,lt_supersevere=supersevere)
wildfires <- read_csv("data/format/wildfires_ncdp.csv") %>% select(fips=FIPS,wildfire_risk=Wildfire)

exposure <- us_counties_fips %>%
  left_join(drought,by="FIPS") %>% 
  left_join(floods,by="fips") %>% 
  left_join(longterm,by="FIPS") %>% 
  left_join(wildfires,by="fips") %>%
  select(-fips)

# demographics prep
demographics_pct <- read_csv("data/format/mit_election_18.csv") %>% 
  select(fips,female_pct, age29andunder_pct, age65andolder_pct,
         clinton16_pct,obama12_pct,white_pct, black_pct, hispanic_pct, nonwhite_pct, rural_pct) %>%
  setNames(gsub("_pct$","",names(.)))

# DATASET PREP
data <- us_counties_fips %>%
  left_join(exposure,by=c("FIPS","Name","State")) %>% 
  left_join(perceptions,by="fips") %>%
  left_join(demographics_pct,by="fips") %>%
  select(FIPS,fips,everything())

write_csv(data,"results/output/incubator_data.csv")
