# prep_fima_nfip



fima <- read_csv("data/orig/FIMA_NFIP_Redacted_Claims_Data_Set/openFEMA_claims20190831.csv",guess_max = 100000)

fima_county_ts <- fima %>% group_by(countycode,yearofloss,state) %>%
  summarize(amountpaidonbuildingclaim=sum(amountpaidonbuildingclaim,na.rm=TRUE),
            amountpaidoncontentsclaim=sum(amountpaidoncontentsclaim,na.rm=TRUE),
            amountpaidonincreasedcostofcomplianceclaim=sum(amountpaidonincreasedcostofcomplianceclaim,na.rm=TRUE),
            amount_paid=amountpaidonbuildingclaim + amountpaidoncontentsclaim + amountpaidonincreasedcostofcomplianceclaim)

write_csv(fima_county_ts %>% select(countycode,yearofloss,state,amount_paid),
          "data/format/floods_nfip_county_timeseries.csv")

fima_county_decade <- fima_county_ts %>%
  mutate(decade=floor(yearofloss/10)*10) %>%
  group_by(countycode,state,decade) %>%
  summarize(amount_paid=sum(amount_paid)) %>%
  group_by() %>%
  filter(decade >= 1990) %>% 
  mutate(decade=paste0("amount_",decade,"s")) %>% 
  spread(decade,amount_paid,fill=0)

write_csv(fima_county_decade,"data/format/floods_nfip_county_decades.csv")

# exploratory state data
fima_state_ts <- fima_county_ts %>% group_by(state,yearofloss) %>%
  summarize(amount_paid=sum(amount_paid)) %>% arrange(yearofloss) %>%
  group_by(state) %>%
  mutate(cum_paid = cumsum(amount_paid))

write_csv(fima_state_ts,"data/format/floods_nfip_state_timeseries.csv")

# max(fima_state_ts$cum_paid)
# ggplot(fima_state_ts) + geom_line(aes(yearofloss,cum_paid,color=state)) + 
#   scale_y_log10() + annotation_logticks() + coord_cartesian(ylim=c(1e6,2e10))
