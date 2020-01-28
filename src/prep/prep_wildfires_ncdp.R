# prep_wildfires_ncdp.R

# 419 counties with high risk
wf_high <- read_delim("data/orig/NCDP_Natural_Hazards_index/natural_hazards_wildfires_high.txt",delim = "\t",
           col_names = c("County","State","Hazards","Wildfire","Volcano","Tornado","Snowfall",
                         "Landslide","Hurricane","Earthquake","Drought",
                         "Heatwave","Avalanche","Flood"))
# 425 counties with medium risk
wf_medium <- read_delim("data/orig/NCDP_Natural_Hazards_index/natural_hazards_wildfire_medium.txt",delim = "\t",
           col_names = c("County","State","Hazards","Wildfire","Volcano","Tornado","Snowfall",
                         "Landslide","Hurricane","Earthquake","Drought",
                         "Heatwave","Avalanche","Flood"))

# 2264 counties with low risk (not copied)

fips <- read_csv("data/format/usa_county_fips_codes.csv")

wf <- bind_rows(wf_medium,wf_high) %>% rename(State_name=State) %>%
  mutate(county_lower=tolower(gsub("[\\. ]","",County)),
         county_lower=gsub("miami-dade","dade",county_lower),
         State=state.abb[match(State_name,state.name)])

wildfires <- fips %>% left_join(wf,by=c("county_lower","State")) %>%
  select(FIPS,State,County,Wildfire) %>%
  mutate(Wildfire=if_else(is.na(Wildfire) & ! State %in% c("AK","HI"),"Low",Wildfire))

write_csv(wildfires,"data/format/wildfires_ncdp.csv")


