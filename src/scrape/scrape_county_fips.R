library(rvest)

us_fips_website <- "https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697"

us_fips <- xml2::read_html(us_fips_website)

us_fips_codes <- us_fips %>%
  rvest::html_node(xpath="//*[@id=\"detail\"]/table") %>%
  rvest::html_table()

us_fips_codes_format <- us_fips_codes %>% rename(fips=FIPS) %>%
  mutate(FIPS=ifelse(str_length(fips)==4,paste0("0",fips),paste0(fips)),
         county_lower=gsub("[\\. ]","",tolower(Name)),
         county_lower=ifelse(State=="VA",gsub("city$","",county_lower),county_lower)) %>%
  select(FIPS,everything()) %>% as_tibble()

write_csv(us_fips_codes_format,"data/format/usa_county_fips_codes.csv")
