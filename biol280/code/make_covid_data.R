library(tidyverse)
library(janitor)
library(gapminder)

cases_deaths <- readRDS("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/COVID-19_Unified-Dataset/COVID-19.rds")
vaccines <- readRDS("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/COVID-19_Unified-Dataset/Vaccine.rds")
ids <- read_csv("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/COVID-19_Unified-Dataset/COVID-19_LUT.csv") %>% 
  rename(country = Admin0,
         US_state = Admin1,
         US_county = Admin2)
state_populations <- readRDS("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/teaching/biol280/data/state_populations.rds") %>% 
  rename(us_state = Province_State) %>% clean_names()


covid_cases <- cases_deaths %>%
  filter(Age == "Total",
         Sex == "Total",
         Type == "Active") %>% 
  select(ID, Date, Cases, Type) %>% 
  distinct(ID, Date, Cases, Type) %>% 
  left_join(ids %>% select(ID, country, US_state, US_county)) %>% 
  rename(cumulative_cases = Cases) %>% 
  select(-Type, -country, -US_state, -US_county) %>% 
  group_by(ID) %>% 
  arrange(ID, Date) %>% 
  mutate(new_cases = cumulative_cases - lag(cumulative_cases,1, default = 0))


covid_hosp <- cases_deaths %>%
  filter(Age == "Total",
         Sex == "Total",
         Type == "Hospitalized") %>% 
  select(ID, Date, Cases, Cases_New, Type) %>% 
  distinct(ID, Date, Cases, Type) %>% 
  left_join(ids %>% select(ID, country, US_state, US_county)) %>% 
  rename(cumulative_hospitalizations = Cases) %>% 
  select(-Type, -country, -US_state, -US_county) %>% 
  group_by(ID) %>% 
  arrange(ID, Date) %>% 
  mutate(new_hospitalizations = cumulative_hospitalizations - lag(cumulative_hospitalizations,1, default = 0))

covid_death <- cases_deaths %>%
  filter(Age == "Total",
         Sex == "Total",
         Type == "Deaths") %>% 
  select(ID, Date, Cases, Cases_New, Type) %>% 
  distinct(ID, Date, Cases, Type, Cases_New) %>%
  left_join(ids %>% select(ID, country, US_state, US_county)) %>% 
  rename(cumulative_deaths = Cases) %>% 
  select(-Type, -country, -US_state, -US_county) %>% 
  group_by(ID) %>% 
  arrange(ID, Date) %>% 
  mutate(new_deaths = cumulative_deaths - lag(cumulative_deaths,1, default = 0))



covid_data_all <- left_join(covid_cases, covid_hosp) %>% 
  left_join(covid_hosp) %>% 
  left_join(vaccines %>% select(ID, Date, Vax_Full, Vax_Partial)) %>% 
  left_join(ids %>% select(ID, country, US_state, US_county, Population, Latitude, Longitude)) %>% 
  clean_names()


covid_by_country <- covid_data_all %>%  
  group_by(date, country, population) %>% 
  summarize(cumulative_cases = sum(cumulative_cases),
            new_cases = sum(new_cases),
            cumulative_hospitalizations = sum(cumulative_hospitalizations),
            new_hospitalizations = sum(new_hospitalizations),
            vax_full = sum(vax_full),
            vax_partial = sum(vax_partial)) %>% 
  left_join(gapminder %>% distinct(country, continent))
  
covid_by_USstate <- covid_data_all %>%  
  filter(country == "United States") %>% 
  filter(!is.na(us_state)) %>% 
  group_by(date, us_state) %>% 
  summarize(cumulative_cases = sum(cumulative_cases),
            new_cases = sum(new_cases),
            cumulative_hospitalizations = sum(cumulative_hospitalizations),
            new_hospitalizations = sum(new_hospitalizations),
            vax_full = sum(vax_full),
            vax_partial = sum(vax_partial)) %>% 
  left_join(state_populations)


covid_by_USstate %>% 
  filter(us_state == "South Dakota") %>%
  ggplot(aes(x = date, y = cumulative_cases)) + 
  geom_point()




