# Model checking. 
# load packages
library(tidyverse)
library(rethinking)
library(brms)
library(janitor)
library(here)

# create directory if it doesn't exist
ifelse(!dir.exists(file.path(here(), "applied_bayesian_modeling/models")), 
                   dir.create(file.path(here, "applied_bayesian_modeling/models")), FALSE)

# simulate data
set.seed(234234)

data_exp <- tibble(a = rpois(20, 6),
       b = rpois(20, 1)) %>% 
  pivot_longer(cols = everything())

# plot raw data 
data_exp %>% 
  ggplot(aes(x = name, y = value)) + 
  geom_jitter(width = 0.05, height = 0)

# fit models with different likelihoods
exp_brm_gaus <- brm(value ~ name, 
                    family = gaussian(),
                    data = data_exp, 
                    chains = 1, iter = 1000,
                    file_refit = "on_change",
                    file = "applied_bayesian_modeling/models/exp_brm_gaus.rds")

exp_brm_poisson <- update(exp_brm_gaus, 
                      family = poisson(link = "log"),
                      newdata = data_exp,
                      file = "applied_bayesian_modeling/models/exp_brm_poisson.rds")

pp_check(exp_brm_gaus, type = "stat_grouped", group = "name")
pp_check(exp_brm_poisson, type = "stat_grouped", group = "name") + scale_x_log10()



# pp_check by hand

# prepare the posteriors and data
#1) extract posterior
post <- as_draws_df(exp_brm_poisson) %>% 
  mutate(iter = 1:nrow(.))

#2) simulate n datapoints from the posterior
y_rep <- post %>% 
  mutate(a = b_Intercept,
         b = b_Intercept + b_nameb) %>% 
  select(iter, a, b) %>% 
  pivot_longer(cols = c(-iter), names_to = "name", values_to = "mean") %>% 
  rowwise(iter, name) %>%  # prepare R to summarize by these groups (iter and group)
  summarize(value = rpois(20, exp(mean))) # for each iteration and group, simulate 20 data points

#3) Add the raw data (so they can be plotted together)
y_rep_raw <- y_rep %>% mutate(source = "y_rep") %>% #add an identifying column
  bind_rows(data_exp %>%
              mutate(source = "raw_data",   #add an identifier to raw data then combine
                     iter = 0)) 


# now plot
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

#pp_check()  ... the default pp_check plot
y_rep_raw %>% 
  filter(iter <= 10) %>% # only the first n iterations
  group_by(iter, name) %>% 
  ggplot(aes(color = source)) + # different color for y_rep versus raw data
  geom_density(aes(x = value, group = iter)) 

# pp_check(type = "stat")
y_rep_raw %>% 
  group_by(iter, source) %>% 
  summarize(stat = mean(value)) %>%  
  ggplot() + 
  geom_histogram(data = . %>% filter(source != "raw"),
                                     aes(x = stat)) +
  geom_vline(data = . %>% filter(source == "raw_data"),
             aes(xintercept = stat), color = "dodgerblue")
  
# pp_check(type = "stat_grouped", group = "name")
y_rep_raw %>% 
  group_by(iter, source, name) %>% 
  summarize(stat = mean(value)) %>%  
  ggplot() + 
  geom_histogram(data = . %>% filter(source != "raw"),
                 aes(x = stat)) +
  geom_vline(data = . %>% filter(source == "raw_data"),
             aes(xintercept = stat), color = "dodgerblue") +
  facet_wrap(~name)


# Instead of the mean, check a quartile
y_rep_raw %>% 
  group_by(iter, source, name) %>% 
  summarize(stat = quantile(value, 0.5)) %>%  ###### This is the only line that changes
  ggplot() + 
  geom_histogram(data = . %>% filter(source != "raw"),
                 aes(x = stat)) +
  geom_vline(data = . %>% filter(source == "raw_data"),
             aes(xintercept = stat), color = "dodgerblue") +
  facet_wrap(~name)




# bayesian p-value (This is not the same thing as a freq p-value (i.e., not for significant/non-significant parameters))

# 1) compute a stat from the raw data
raw_stats <- data_exp %>% summarize(stat = quantile(value, 0.5))

# 2) compute the same stats from the posterior. Add the observed, then calculate the difference for each iteration
pred_obs <- y_rep_raw %>% 
  group_by(iter, source) %>% 
  summarize(stat = quantile(value, 0.5)) %>% # estimate the predicted values
  filter(source != "raw_data") %>% # delete the raw data here...adding it in the next step
  mutate(observed = raw_stats$stat) %>%  # add the observed value
  mutate(pred_minus_obs = stat - observed)


# 3) calculate the p-value (what proportion of predicted are above/below the observed?) should be close to 50 percent
sum(pred_obs$pred_minus_obs > 0)/length(pred_obs)



