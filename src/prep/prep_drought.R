# prep_drought.R
# explanation of D0-D4: https://www.ncdc.noaa.gov/news/drought-degrees-drought-reveal-true-picture
# note that in *nearly all* cases, the area for D[i+1] > 0 is contained within D[i], meaning that sum(D0:D4) will
# often be greater than 100. In other words, the value for D0 indicates all drought D0 and greater.

# # failed code for USDM API
# library(httr)
# r <- GET("https://usdmdataservices.unl.edu/api/USStatistics/GetBasicStatisticsByAreaPercent?aoi=TOTAL&dx=1&DxLevelThresholdFrom=10&DxLevelThresholdTo=70&startdate=1/1/2012&enddate=1/1/2013&statisticsType=1",
#          add_headers(request="CSV"))
# headers(r)
# a <- content(r, "parsed")
# b <- lapply(a,function(x) as.data.frame(x))
# d <- do.call(rbind,b) %>% as_tibble()

library(tidyverse)

dm <- read_csv("data/orig/DroughtMonitor/dm_export_19800101_20200127.csv")

dm_prep <- dm %>% mutate(year=as.numeric(strftime(ValidStart,"%Y")),
              end_year=as.numeric(strftime(ValidEnd,"%Y")),
              ndays=as.numeric(ValidEnd-ValidStart+1)) %>%
  select(-ValidStart,-ValidEnd,-StatisticFormatID)

dm_prep_long <- dm_prep %>%
  gather(Drought,pct_area,None:D4) %>%
  mutate(pct_days=pct_area*ndays)

dm_county_year <- dm_prep_long %>%
  group_by(FIPS,County,State,year,Drought) %>%
  summarize(pct_year=sum(pct_days)/365)

dm_county_2010s <- dm_county_year %>% filter(year >= 2010) %>%
  group_by(FIPS,County,State,Drought) %>%
  summarize(pct_mean=round(mean(pct_year),1)) %>%
  mutate(Drought=paste0(Drought,"_pct_mean")) %>%
  spread(Drought,pct_mean)

write_csv(dm_county_2010s,"data/format/drought_usdm_2010s.csv")

# checks on data
# dm_prep %>% filter(D0 < D1)
# dm_prep_sameyear <- dm_prep %>% filter(year==end_year)
# dm_prep_multiyear <- dm_prep %>% filter(year!=end_year)

lapply(split(dm_prep_multiyear,1:nrow(dm_prep_multiyear)))