library(brms)
library(janitor)
library(tidyverse)
library(lubridate)
library(tidybayes)

d <- read_csv("applied_bayesian_modeling/data/uobs_diversity_data/dry_mass_cleaned.csv") %>% 
  clean_names() 


d %>% 
  select(trt, richness, meantot_jul, contains("total")) %>% 
  mutate(total11jun2008 = parse_number(total11jun2008)) %>% 
  mutate(ln_biomass_july = log(meantot_jul)) %>% 
  group_by(trt) %>% 
  mutate(mean = mean(ln_biomass_july, na.rm = T),
         sd = sd(ln_biomass_july, na.rm = T),
         se = sd/sqrt(4)) %>% 
  ggplot(aes(x = richness, y = ln_biomass_july, color = trt)) + 
  geom_point(position = position_dodge(width = 0.3), size = 1, shape = 21) + 
  geom_pointrange(aes(y = mean, ymin = mean - se, ymax = mean + se),
                  position = position_dodge(width = 0.3)) +
  theme_default() 


# set up data for a glmm repeated measures
d_long <- d %>% 
  select(trt, richness, meantot_jul, contains("total")) %>% 
  mutate(total11jun2008 = parse_number(total11jun2008),
         tank = 1:nrow(.)) %>% 
  pivot_longer(cols = c(-trt, -richness, -meantot_jul, -tank)) %>% 
  mutate(day = str_sub(name, 6,7),
         month = str_sub(name, 8,10),
         year = str_sub(name, 11, 14),
         date = paste0(year, "-", month, "_", day),
         date = ymd(date),
         date = as.factor(date),
         ln_value = log(value + 1)) %>% 
  select(-meantot_jul, -name, -day, -month, -year)

# fit model
#original
brm_uobs <- brm(ln_value ~ trt*date + (1|tank), 
                family = gaussian(),
                data = d_long,
                file = "applied_bayesian_modeling/models/brm_uobs.rds")

plot(conditional_effects(brm_uobs, effects = "date:trt"), points = T)


# recreate Figure 1a
#extract posteriors
posts_uobs <- brm_uobs$data %>% 
  distinct(date, trt) %>% 
  add_epred_draws(brm_uobs, re_formula = NA)

#mean of emergence after start of experiment
posts_uobs_means <- posts_uobs %>% 
  filter(date != "2008-06-11") %>% 
  ungroup() %>% 
  select(trt, date, .draw, .epred) %>% 
  group_by(trt, .draw) %>% 
  summarize(mean_draw = mean(.epred)) %>% 
  left_join(d_long %>% distinct(trt, richness)) # add richness for plotting

#plot
posts_uobs_means %>% 
  group_by(trt, richness) %>% 
  summarize(mean = mean(mean_draw),
            sd = sd(mean_draw),
            se = sd/sqrt(4)) %>% 
  ggplot(aes(x = richness, y = mean, color = trt)) + 
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se),
                  position = position_dodge(width = 0.3)) +
  ylim(0, 4) +
  theme_default()

posts_uobs_means %>% 
  mutate(trt_group = case_when(richness == 0 ~ "Control",
                               TRUE ~ "Fish")) %>% 
  group_by(trt_group, .draw) %>% 
  summarize(group_mean = mean(mean_draw)) %>% 
  pivot_wider(names_from = trt_group, values_from = group_mean) %>% 
  mutate(diff = Fish-Control) %>% 
  summarize(prob = sum(diff<0)/4000)

posts_uobs_means %>% 
  group_by(richness, .draw) %>% 
  summarize(mean_rich = mean(mean_draw)) %>% 
  pivot_wider(names_from = richness, values_from = mean_rich) %>% 
  mutate(diff = (`3`/`0`)) %>%
  # summarize(prob = sum(diff>1)/4000)
  median_qi(diff)


# fit model again, but this time as a glmm with gamma likelihood
#as a glmm
d_long2 <- d_long %>% mutate(g_dm = (value/1000) + 0.1) %>%  # convert mg to grams to make fitting easier
  mutate(richness = as.factor(richness))

# run the model
brm_uobs_gamma <- brm(g_dm ~ trt*date + (1|tank),
                      family = Gamma(link = "log"),
                      data = d_long2,
                      file = "applied_bayesian_modeling/models/brm_uobs_gamma.rds")


posts_gamma <- brm_uobs_gamma$data %>% 
  distinct(trt, date) %>% 
  add_epred_draws(brm_uobs_gamma, re_formula = NA) %>% 
  left_join(d_long2 %>% distinct(trt, richness))


posts_gamma %>%
  mutate(.epred = 1000*.epred) %>% 
  filter(date != "2008-06-11") %>% 
  ungroup() %>% 
  select(-.row, -.chain, -.iteration) %>% 
  group_by(trt, .draw, richness) %>% 
  summarize(mean_draw = mean(.epred)) %>% 
  group_by(trt, richness) %>% 
  summarize(mean_trt = mean(mean_draw),
            sd = sd(mean_draw),
            se = sd/sqrt(4)) %>% 
  ggplot(aes(x = richness, y = mean_trt)) + 
  geom_pointrange(aes(ymin = mean_trt - se, ymax = mean_trt + se,
                      color = trt),
                  position = position_dodge(width = 0.3)) + 
  theme_default() + 
  NULL
  

