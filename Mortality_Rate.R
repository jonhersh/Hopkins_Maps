library('tidyverse')

dat <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-18-2020.csv")

head(dat)

dat <- dat %>% group_by(`Country/Region`) %>% 
               summarize(totDeaths = sum(Deaths), 
                         totConfirmed = sum(Confirmed)) %>% 
               mutate(DR = totDeaths/totConfirmed) %>% 
               mutate(DR = round(DR,3))

mapdat <- dat %>% arrange(-DR) %>% 
  select(`Country/Region`,DR) %>% 
  rename(region = 1, value = DR) %>% 
  mutate(region = tolower(region)) %>% 
  mutate(region = recode(region,
                         "us" = "united states of america",
                         "taiwan*" = "taiwan",
                         "korea, south" = "south korea",
                         "congo (brazzaville)" = "republic of congo",
                         "congo (kinshasa)" = "democratic republic of the congo",
                         "eswatini" = "lesotho",
                         "gambia, the" = "gambia",
                         "serbia" = "republic of serbia",
                         "tanzania" = "united republic of tanzania",
                         )) 
exclude_list <- c("san marino","cruise ship","martinique","bahrain","andorra",
                  "antigua and barbuda","aruba","barbados","cote d'ivoire",
                  "czechia","french guiana","greenland","guadeloupe","holy see",
                  "liechtenstein","maldives","malta","mauritius","mayotte","monaco",
                  "north macedonia","reunion","saint lucia", "saint vincent and the grenadines",
                  "seychelles","singapore","guam","guernsey","jersey", "puerto rico",
                  "republic of the congo", "the gambia")
  mapdat <- filter(mapdat, !region %in% exclude_list)

data(country.map, package = "choroplethrMaps")
unique(country.map$region)

library(choroplethr)
country_choropleth(mapdat, 
                   title = "Crude Mortality Rate (Deaths / Cases), 3/18/20",
                   legend = "Deaths/Cases",
                   num_colors = 8) + 
  scale_fill_brewer(na.value = "grey", palette = 3)
