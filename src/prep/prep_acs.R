# https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf
# vignette: https://walkerke.github.io/tidycensus/articles/basic-usage.html

v18 <- tidycensus::load_variables(2018, "acs5", cache = TRUE)
v18 %>% filter(grepl("TOTAL POPULATION COVERAGE RATE",concept))
v18_female <- v18 %>% filter(grepl("[Ff]emale",label),grepl("EDUCATION",concept))
v18_female$label

# tidycensus::get_acs(geography="county",variables = c(total_race="B98013_001"),

