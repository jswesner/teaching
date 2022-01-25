# Annotate each part of this code (i.e., using a pen/pencil, briefly describe what 
# each part of the code)

# load packages
library(tidyverse)
library(brms)
library(janitor)

# simulate data
set.seed(234234)

data_exp <- tibble(a = rpois(20, 6),
                   b = rpois(20, 1)) %>% 
  pivot_longer(cols = everything())

# fit model
exp_brm_gaus <- brm(value ~ name, 
                    family = gaussian(),
                    data = data_exp, 
                    chains = 4, 
                    iter = 1000)
# check chains
plot(exp_brm_gaus)

# check model fit
pp_check(exp_brm_gaus, type = "hist")
pp_check(exp_brm_gaus, type = "boxplot")


#### BONUS #### (pp_check manually)
# prepare the posteriors and data
#1) extract posterior
post <- as_draws_df(exp_brm_gaus) %>% 
  mutate(iter = 1:nrow(.))

#2) simulate n data points from the posterior
y_rep <- post %>% 
  mutate(a = b_Intercept,
         b = b_Intercept + b_nameb) %>% 
  select(iter, a, b, sigma) %>% 
  pivot_longer(cols = c(-iter, -sigma), 
               names_to = "name", 
               values_to = "mean") %>% 
  rowwise(iter, name) %>%  # prepare R to summarize by these groups (iter and name)
  summarize(value = rnorm(20, mean, sigma)) # for each iteration and group, simulate 20 data points

#3) Add the raw data (so they can be plotted together)
y_rep_raw <- y_rep %>% 
  mutate(source = "y_rep") %>% #add an identifying column
  bind_rows(data_exp %>%
              mutate(source = "raw_data",   #add an identifier to raw data then combine
                     iter = 0)) 

# pp_check(type = "hist)
y_rep_raw %>% 
  filter(iter <= 10) %>% # only the first n iterations
  group_by(iter, name) %>% 
  ggplot(aes(fill = source)) + # different color for y_rep versus raw data
  geom_histogram(aes(x = value, group = name)) + 
  facet_wrap(~iter) 

# pp_check(type = "boxplot")
y_rep_raw %>% 
  filter(iter <= 10) %>% # only the first n iterations
  group_by(iter, name) %>% 
  ggplot(aes(fill = source)) + # different color for y_rep versus raw data
  geom_boxplot(aes(x = iter, y = value, group = iter)) 
