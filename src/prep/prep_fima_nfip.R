# prep_fima_nfip



fima <- read_csv("data/orig/FIMA_NFIP_Redacted_Claims_Data_Set/openFEMA_claims20190831.csv",guess_max = 100000)



fima_county_ts <- fima %>% group_by(countycode,occupancytype,yearofloss,state) %>%
  summarize(amountpaidonbuildingclaim=sum(amountpaidonbuildingclaim,na.rm=TRUE),
            amountpaidoncontentsclaim=sum(amountpaidoncontentsclaim,na.rm=TRUE),
            amountpaidonincreasedcostofcomplianceclaim=sum(amountpaidonincreasedcostofcomplianceclaim,na.rm=TRUE),
            amount_paid=amountpaidonbuildingclaim + amountpaidoncontentsclaim + amountpaidonincreasedcostofcomplianceclaim)

fima_state_ts <- fima_county_ts %>% group_by(state,yearofloss) %>%
  summarize(amount_paid=sum(amount_paid)) %>% arrange(yearofloss) %>%
  group_by(state) %>%
  mutate(cum_paid = cumsum(amount_paid))

# max(fima_state_ts$cum_paid)
ggplot(fima_state_ts) + geom_line(aes(yearofloss,cum_paid,color=state))# + 
  #scale_y_log10() + annotation_logticks() + coord_cartesian(ylim=c(1e6,2e10))

names(fima)
