# https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf
# vignette: https://walkerke.github.io/tidycensus/articles/basic-usage.html

v18 <- tidycensus::load_variables(2018, "acs5", cache = TRUE)
v18 %>% filter(grepl("TOTAL POPULATION COVERAGE RATE",concept))
v18_female <- v18 %>% filter(grepl("[Ff]emale",label),grepl("EDUCATION",concept))
v18_female$label

# tidycensus::get_acs(geography="county",variables = c(total_race="B98013_001"),

fips <- read_csv("data/format/usa_county_fips_codes.csv") %>%
  mutate(county_name=gsub("[\\. ]","",tolower(Name)),
         county_name=ifelse(State=="VA",gsub("city$","",county_name),county_name))
# fips %>% filter(grepl("western",county_name))

fema <- read_csv("data/orig/FEMA/DisasterDeclarationsSummaries.csv") %>%
  arrange(declarationDate) %>% group_by(incidentType) %>%
  mutate(cum_number=row_number()) %>%
  mutate(county_name = gsub("[\\. ]","",tolower(gsub("\\s?(\\(.*)","",declaredCountyArea)))) %>% 
  filter(!is.na(declaredCountyArea),state!="PR",!grepl("indianreservation",county_name)) %>%
  select(county_name,everything()) %>% left_join(fips %>% select(fips=FIPS,county_name),by="county_name")
# fema_missing <- fema %>% filter(!(county_name %in% fips$name))

fema_county_summary <- fema %>% group_by(incidentType,fips) %>%
  summarize(n=n()) %>% spread(incidentType,n,fill=0)


# fema_hist <- readxl::read_excel("data/orig/FEMA/FEMA_Historical_Declarations_1964_2013.xlsx") %>%
#   arrange(`Declaration Date`) %>% group_by(`Incident Type`) %>%
#   mutate(cum_number=row_number())
# 
# fema_hist_county_summary <- fema_hist %>% group_by(`Incident Type`,StCounty_FIPS) %>%
#   summarize(n=n()) %>% mutate(fips=as.numeric(StCounty_FIPS)) %>% spread(`Incident Type`,n,fill=0)
# 
# fema_hist_county_summary %>% filter(fips %in% us_counties$fips)

us_counties_fema <- us_counties %>% left_join(fema_county_summary,by="fips")# %>%

ggplot() + geom_sf(data=us_counties_fema,aes(fill=Fire),color=NA) #+
  facet_wrap(~incidentType)


table(fema$incidentType)
table(fema_hist$`Incident Type`)

ggplot(fema) + geom_line(aes(declarationDate,cum_number,color=incidentType))
ggplot(fema_hist) + geom_line(aes(`Declaration Date`,cum_number,color=`Incident Type`))
