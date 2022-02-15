library(tidyverse)

COVID_daily_cases_deaths_US <- readRDS("biol280/data/gapminder/all_data_curated_rds/COVID_daily_cases_deaths_US.rds")


covid_data_state_means <- COVID_daily_cases_deaths_US %>%
  filter(date == max(date)| date == "2021-02-08") %>% 
  ungroup() %>% 
  select(cases, deaths, cases_per_day, deaths_per_day, state, population, date) %>% 
  mutate(date = as.factor(date))

covid_data_state_means %>% 
  ggplot(aes(x = date, y = deaths_per_day)) + 
  geom_jitter(width = 0.05)

covid_data_state_means %>% 
  group_by(date) %>% 
  summarize(mean = mean(deaths_per_day),
            sd = sd(deaths_per_day),
            median = median(deaths_per_day))





# mental health data
male = tibble(counseling = rnorm(566, 1.13, 0.45))
female = rnorm(1207, 1.17, 0.5)
asian