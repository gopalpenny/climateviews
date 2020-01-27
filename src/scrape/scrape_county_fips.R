library(rvest)

us_fips_website <- "https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697"

us_fips <- xml2::read_html(us_fips_website)

us_fips_codes <- us_fips %>%
  rvest::html_node(xpath="//*[@id=\"detail\"]/table") %>%
  rvest::html_table()

write_csv(us_fips_codes,"data/format/usa_county_fips_codes.csv")
