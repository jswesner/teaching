library(tidyverse)
library(gapminder)

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
files <- list.files(path = "C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/gapminder-offline/ddf--gapminder--systema_globalis",
             pattern = ".*csv",
             full.names = T) 

tbl <- sapply(files[-1], read_csv, simplify=FALSE)

tbl_temp = NULL

country_continent = gapminder %>% ungroup %>% distinct(country, continent)

for (i in 1:length(tbl)) {
  tryCatch({
    tbl_temp[[i]] = tbl[[i]] %>% 
      left_join(ddf_entities_geo_country %>% select(geo, country, income_3groups, latitude, longitude, main_religion_2008,
                                                    un_sdg_ldc, world_4region),
                by = c("geo")) %>% 
      rename(value = 3,
             year = time) %>% 
      left_join(country_continent) %>% 
      select(-geo)  %>% 
      select(country, year, value, continent, everything()) %>% 
      rename_with(~ names(tbl[[i]][3]), value)
    
  }, error = function(e) {
    # Print or handle the error if needed
    cat("Error in iteration", i, ":", conditionMessage(e), "\n")
  })
}

for(i in 1:length(tbl_temp)){
  saveRDS(tbl_temp[[i]], file = paste0("biol280/data/gapminder/all_data_curated_rds/", names(tbl_temp[[i]][3]), ".rds"))

}

