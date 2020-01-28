# prep_wildfires.R

# 419 counties with high risk
read_delim("~/natural_hazards_wildfires_high.txt",delim = "\t",
           col_names = c("County","State","Hazards","Wildfire","Volcano","Tornado","Snowfall",
                         "Landslide","Hurricane","Earthquake","Drought",
                         "Heatwave","Avalanche","Flood"))
# 425 counties with medium risk
read_delim("~/natural_hazards_wildfire_medium.txt",delim = "\t",
           col_names = c("County","State","Hazards","Wildfire","Volcano","Tornado","Snowfall",
                         "Landslide","Hurricane","Earthquake","Drought",
                         "Heatwave","Avalanche","Flood"))
