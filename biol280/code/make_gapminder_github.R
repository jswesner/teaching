library(tidyverse)

# load final product (recreate with code below)
all_long <- read_csv("biol280/data/gapminder/all_data_together/all_gapminder_data.csv")

all_long_curated <- all_long %>% filter(!is.na(value)) %>% 
  filter(!grepl("aged_", name)) %>% 
  filter(!grepl("data_", name)) %>% 
  select(-geo)


# load identifiers for later
ddf_entities_geo_country <- read_csv("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/gapminder/gapminder/ddf--entities--geo--country.csv") %>% 
  rename(geo = country,
         country = name)

# read all files for gapminder data csv's
files <- list.files(path = "C:/Users/Jeff.Wesner/Documents/GitHub Projects/gapminder/gapminder/countries-etc-datapoints",
             pattern = ".*csv",
             full.names = T) 

tbl <- sapply(files, read_csv, simplify=FALSE)

all_gapminder <- as.list(tbl) %>% 
  reduce(left_join)

# pivot all csv's together
all_long <- all_gapminder %>% 
  filter(time <= 2021) %>% 
  rename(year = time) %>% 
  mutate_if(is.numeric,as.character, is.factor, as.character) %>% 
  pivot_longer(cols = c(-geo, -year)) %>% 
  mutate(year = as.integer(year),
         value = as.numeric(value)) %>% 
  left_join(ddf_entities_geo_country %>% distinct(country, geo)) %>% 
  select(country, year, everything())



# write csv's for every group
all_long_curated %>%
  group_by(name) %>%
  group_walk(~ saveRDS(.x, file = paste0("biol280/data/gapminder/all_data_curated_rds/", .y$name, ".rds")))
