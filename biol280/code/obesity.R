library(tidyverse)
library(gapminder)
library(ggrepel)
library(datasauRus)

state_convert <- tibble(state.abb = state.abb,
                        state.name = state.name)



# load data
obesity <- read_csv("biol280/data/obesity.csv") %>% 
  mutate(year = 2007)
state_obesity <- read_csv("biol280/data/state_obesity.csv") %>% 
  mutate(state.name = str_sub(state, 2, 30)) %>% 
  select(-state) %>% 
  left_join(state_convert) %>% 
  mutate(obesity_rate = parse_number(obesity_rate))
state_life_exp <- read_csv("biol280/data/state_life_exp.csv") %>% 
  rename(state.abb = state_ab) %>% 
  left_join(state_convert)

life_exp_obesity <- gapminder %>% left_join(obesity) %>% filter(year == 2007)
state_life_exp_obesity <- state_obesity %>% left_join(state_life_exp)


write_csv(life_exp_obesity %>% select(country, lifeExp, obesity_rate),
          file = "biol280/data/life_exp_obesity.csv")

write_csv(state_life_exp_obesity, file = "biol280/data/state_life_exp_obesity.csv")


life_exp_obesity <- read_csv("https://raw.githubusercontent.com/jswesner/teaching/master/biol280/data/life_exp_obesity.csv")
state_life_exp_obesity <- read_csv("https://raw.githubusercontent.com/jswesner/teaching/master/biol280/data/state_life_exp_obesity.csv")


life_exp_obesity %>% 
  mutate(name = case_when(country == "United States" ~ "US",
                          country == "China" ~ "China",
                          country == "Zambia" ~ "Zambia")) %>% 
  ggplot(aes(x = obesity_rate, y = lifeExp)) + 
  geom_point() +
  geom_text_repel(aes(label = name),
                  nudge_x = c(0, -5, -2),
                  nudge_y = c(4, 6, 4)) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim= c(0, 40),
                  ylim = c(40, 85)) +
  labs(y = "Life Expectancy (years)",
       x = "Obesity Rate (% of population)",
       title = "Life Expectancy and Obesity Rates per Country")


state_life_exp_obesity %>% 
  ggplot(aes(x = obesity_rate, y = life_exp)) +
  # geom_text(aes(label = state.abb),
  #           nudge_x = 0.5,
  #           nudge_y = 0.1,
  #           alpha = 0.5) +
  geom_point(size = 2) +
  geom_smooth(method = "lm") +
  # coord_cartesian(xlim= c(0, 40),
  #                 ylim = c(40, 85)) +
  labs(y = "Life Expectancy (years)",
       x = "Obesity Rate (% of population)",
       title = "Life Expectancy and Obesity Rates per US State")


life_exp_obesity %>% 
  mutate(name = case_when(country == "United States" ~ "US",
                          country == "China" ~ "China",
                          country == "Zambia" ~ "Zambia"),
         us = case_when(country == "United States" ~ "US")) %>% 
  ggplot(aes(x = obesity_rate, y = lifeExp)) + 
  geom_point(alpha = 0.1) +
  geom_point(data = life_exp_obesity %>% filter(country == "United States"),
             size = 5, color = "red") +
  geom_text_repel(aes(label = name),
                  nudge_x = c(0, -5, -2),
                  nudge_y = c(4, 6, 4)) +
  coord_cartesian(xlim= c(0, 40),
                  ylim = c(40, 85)) +
  labs(y = "Life Expectancy (years)",
       x = "Obesity Rate (% of population)",
       title = "Life Expectancy and Obesity Rates per Country") +
  # geom_point(data = state_life_exp_obesity, aes(y = life_exp)) +
  NULL


life_exp_obesity %>% 
  mutate(name = case_when(country == "United States" ~ "US",
                          country == "China" ~ "China",
                          country == "Zambia" ~ "Zambia"),
         us = case_when(country == "United States" ~ "US")) %>% 
  ggplot(aes(x = obesity_rate, y = lifeExp)) + 
  geom_point(alpha = 0.1) +
  geom_point(data = life_exp_obesity %>% filter(country == "United States"),
             size = 5, color = "red") +
  geom_text_repel(aes(label = name),
                  nudge_x = c(0, -5, -2),
                  nudge_y = c(4, 6, 4)) +
  coord_cartesian(xlim= c(0, 40),
                  ylim = c(40, 85)) +
  labs(y = "Life Expectancy (years)",
       x = "Obesity Rate (% of population)",
       title = "Life Expectancy and Obesity Rates per Country") +
  geom_point(data = state_life_exp_obesity, aes(y = life_exp))


life_exp_obesity %>% 
  mutate(name = case_when(country == "United States" ~ "US",
                          country == "China" ~ "China",
                          country == "Zambia" ~ "Zambia"),
         us = case_when(country == "United States" ~ "US")) %>% 
  ggplot(aes(x = obesity_rate, y = lifeExp)) + 
  geom_point(alpha = 0.1) +
  geom_point(data = life_exp_obesity %>% filter(country == "United States"),
             size = 5, color = "red") +
  geom_text_repel(aes(label = name),
                  nudge_x = c(0, -5, -2),
                  nudge_y = c(4, 6, 4)) +
  coord_cartesian(xlim= c(0, 40),
                  ylim = c(40, 85)) +
  labs(y = "Life Expectancy (years)",
       x = "Obesity Rate (% of population)",
       title = "Life Expectancy and Obesity Rates per Country") +
  geom_smooth(method = "lm") +
  geom_point(data = state_life_exp_obesity, aes(y = life_exp)) +
  geom_smooth(method = "lm", 
              data = state_life_exp_obesity, aes(y = life_exp))

simpsons_paradox %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point()

simpsons_paradox %>% 
  mutate(data= case_when(dataset == "simpson_1" ~ "Aggregated",
                         TRUE ~ "Disaggregated")) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point(aes(color = data))

