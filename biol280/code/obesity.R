library(tidyverse)
library(gapminder)

obesity <- read_csv("biol280/data/obesity.csv") %>% 
  mutate(year = 2007)

life_exp_obesity <- gapminder %>% left_join(obesity) %>% filter(year == 2007)

write_csv(life_exp_obesity %>% select(country, lifeExp, obesity_rate),
          file = "biol280/data/life_exp_obesity.csv")

life_exp_obesity %>% 
  ggplot(aes(x = obesity_rate, y = lifeExp)) + 
  geom_point() +
  geom_smooth(method = "lm")

