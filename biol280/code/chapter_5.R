library(tidyverse)
library(readr)

# downloaded from d2l externally
continents <- read_csv("biol280/data/continents.csv")
population <- read_csv("biol280/data/population.csv")

# download data from gapminder externally and pivoted
life_expectancy_years <- read_csv("biol280/data/life_expectancy_years.csv") %>% 
  pivot_longer(cols = -country, names_to = "year", values_to = "life_exp") %>% 
  mutate(year = as.numeric(year)) 

# add continents and population to life_expectancy_years
life_expectancy_withpop <- life_expectancy_years %>% 
  left_join(population) %>% 
  left_join(continents)


  
my_plot <- life_expectancy_withpop %>% 
  ggplot(aes(x = year, y = life_exp, color = continent)) + 
  geom_point() +
  facet_wrap(~continent)

ggsave(my_plot, file = "biol280/my_plot.jpg", dpi = 400, width = 6, height = 6)
