library(tidyverse)
library(janitor)
library(lubridate)


death_data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") 

population <- death_data %>% 
  ungroup() %>% 
  group_by(Province_State) %>% 
  summarize(population = sum(Population))

total_deaths <- death_data %>% pivot_longer(cols = c(-iso2, -iso3, -code3, -FIPS, -Admin2, -Province_State, -Country_Region,
                                     -Lat, -Long_, -Combined_Key, -Population, -UID)) %>% 
  filter(Province_State != "Diamond Princess" & Province_State != "Grand Princess") %>% 
  group_by(Province_State) %>% 
  summarize(deaths = max(value)) %>% 
  left_join(population) %>% 
  mutate(deaths_per_capita = deaths/population)


unsorted_covid_deaths <- total_deaths %>% 
  ggplot(aes(x = Province_State, y = deaths)) + 
  geom_point() +
  labs(x = "US State",
       y = "Total Covid Deaths (as of 2021-10-11)") +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 8))


sorted_covid_deaths <- total_deaths %>% 
  ggplot(aes(y = reorder(Province_State, deaths), x = deaths)) + 
  geom_point() +
  labs(y = "US State",
       x = "Total Covid Deaths (as of 2021-10-11)") +
  theme(text = element_text(size = 14),
        axis.text.y = element_text(size = 8))

library(cowplot)
plot_grid(unsorted_covid_deaths, sorted_covid_deaths, ncol = 1)
