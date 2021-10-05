library(neonDivData)
library(tidyverse)
library(lubridate)
library(sars)



# fish rank abundance -----------------------------------------------------


data_fish <- data_fish


fish_ranked <- data_fish %>% 
  mutate(year = year(observation_datetime)) %>% 
  group_by(siteID, taxon_name, year) %>% 
  summarize(mean_catch = mean(value)) %>% 
  group_by(siteID, year) %>% 
  mutate(rank = rank(-mean_catch))


fish_ranked %>% 
  filter(year == 2020) %>% 
  ggplot(aes(x = rank, y = mean_catch, color = year)) + 
  geom_point() +
  geom_line(aes(group = as.factor(year))) +
  facet_wrap(~siteID) +
  labs(y = "Number of fish caught",
       x = "Species Rank") +
  # ylim(0, 2000) +
  # scale_y_log10() +
  NULL


fish_2020 <- fish_ranked %>% 
  group_by(siteID, taxon_name) %>% 
  filter(year == 2020) %>% 
  summarize(number_of_fish = mean(mean_catch)) %>% 
  mutate(rank = rank(-number_of_fish)) %>% 
  arrange(siteID, rank)

write_csv(fish_2020, file = "environmental_science/data/fish_2020.csv")


# species area curves -----------------------------------------------------

sars::aegean %>% as_tibble() %>% 
  ggplot(aes(x = a, y = s)) + 
  geom_point() + 
  labs(y = "number of species",
       x = "island area km2",
       subtitle = "Plant richness on islands",
  caption = "Sfenthourakis, S. & Triantis K.A. (2009). Habitat diversity, ecological requirements of species and
the Small Island Effect. Diversity Distrib.,15, 131–140.") + 
  theme(text = element_text(size = 13))


sars::galap %>% as_tibble() %>% 
  ggplot(aes(x = a, y = s)) + 
  geom_point() + 
  labs(y = "number of species",
       x = "island area km2",
       subtitle = "Plant richness on islands",
       caption = "Preston FW 1962. The Canonical Distribution of Commonness and Rarity: Part I. – Ecology
43:185-215.")+ 
  theme(text = element_text(size = 13))


sars::niering %>% as_tibble() %>% 
  ggplot(aes(x = a, y = s)) + 
  geom_point() + 
  labs(y = "number of species",
       x = "island area km2",
       subtitle = "Plant richness on islands",
       caption = "Niering, W.A. (1963). Terrestrial ecology of Kapingamarangi Atoll, Caroline Islands. Ecol. Monogr.,
33, 131–160.")+ 
  theme(text = element_text(size = 13))


fit <- sar_power(aegean2)
print(fit)
plot(fit)
plot(fit, allCurves = FALSE, ModTitle = "", lcol = "black")

plot(fit, ModTitle = "A)", lcol = "blue")


        


