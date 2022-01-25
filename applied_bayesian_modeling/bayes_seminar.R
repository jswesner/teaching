library(tidyverse)
library(rethinking)
library(brms)
library(janitor)
library(tidybayes)
library(gapminder)
library(ggridges)
library(ggthemes)
library(ggrepel)


# bayes citations ---------------------------------------------------------


bayes_n <- read_delim("bayes_citations_neuroscience.txt", 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE) %>% 
  mutate(search = "Bayesian AND Neuroscience")

bayes_p <- read_delim("bayes_citations_physiology.txt", 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE) %>% 
  mutate(search = "Bayesian AND Physiology")

bayes_m <- read_delim("bayes_citations_molecular.txt", 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE) %>% 
  mutate(search = "Bayesian AND Molecular Biology")

bayes_ce <- read_delim("bayes_citations_ecology_conservation.txt", 
                     delim = "\t", escape_double = FALSE, 
                     trim_ws = TRUE) %>% 
  mutate(search = "Bayesian AND Conservation/Ecology")


bayes_all <- bind_rows(bayes_n, bayes_p, bayes_m, bayes_ce)

citations <- bayes_all %>% 
  filter(year != 2022) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~search, scales = "free_y") +
  labs(y = "Number of Publications",
       x = "Year of Publication",
       caption = "Source: Clarivate Analytics 2022-01-19") + 
  theme_classic() +
  theme(text = element_text(size = 19))

ggsave(citations, file = "plots/citations.jpg", width = 9, height = 9)





# Life expectancy ---------------------------------------------------------


gapminder

# Sample subset of countries
set.seed(rpois(1, 20000))
gap_subset <- gapminder %>% 
  group_by(year) %>%
  do(sample_n(., 10)) %>% 
  mutate(data = "subsample")

life_exp_global <- gapminder %>% mutate(data = "all data") %>% 
  bind_rows(gap_subset) %>% 
  filter(year == max(year) | year == min(year)) %>% 
  filter(data != "subsample") %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x = year, y = lifeExp)) + 
  geom_point(alpha = 0.5, shape = 21) +
  # geom_line(aes(group = country), alpha = 0.2) +
  labs(subtitle = "Life Expectancy (each dot is a country average)",
       caption = "Source: https://gapminder.com",
       y = "Life Expectancy (mean years)") +
  # geom_boxplot(aes(group = interaction(data, year)), width = 0.2) +
  ylim(30, 85) +
  theme_default() +
  theme(text = element_text(size = 20))

ggsave(life_exp_global, file = "plots/life_exp_global.jpg", width = 7, height = 5)




le_diff <- gapminder %>% mutate(data = "all data") %>% 
  bind_rows(gap_subset) %>% 
  filter(year == max(year) | year == min(year)) %>% 
  filter(data != "subsample") %>% 
  mutate(year = as.factor(year),
         country_int = as.integer(as.factor(country))) %>% 
  # filter(country_int >= 110 & country_int<= 125) %>%
  mutate(year = as.factor(year)) %>%
  select(year, country, lifeExp) %>% 
  distinct(year, country, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp) %>% 
  mutate(diff = `2007` - `1952`) %>% 
  ggplot(aes(y = diff)) + 
  geom_point(alpha = 1, shape = 21, aes(x = 0),
             size = 3) +
  labs(caption = "Source: https://gapminder.com",
       y = "Change (years)",
       x = "1952 to 2007") +
  geom_hline(yintercept = 0) +
  ylim(-10, 40) +
  xlim(-1, 1) +
  theme_default() +
  theme(text = element_text(size = 20),
        axis.text.x = element_blank())

le_diff

ggsave(le_diff, file = "plots/le_diff.jpg", width = 5, height = 5)




# Life expectancy models --------------------------------------------------


diff_gap <- gapminder %>% mutate(data = "all data") %>% 
  # bind_rows(gap_subset) %>% 
  filter(year == max(year) | year == min(year)) %>% 
  # filter(data != "subsample") %>% 
  mutate(year = as.factor(year),
         country_int = as.integer(as.factor(country))) %>% 
  # filter(country_int >= 110 & country_int<= 125) %>%
  mutate(year = as.factor(year)) %>%
  select(year, country, lifeExp) %>% 
  distinct(year, country, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp) %>% 
  mutate(diff = `2007` - `1952`)


freq_diff <- lm(diff ~ 1, data = diff_gap)
sum_freq_diff <- summary(freq_diff)

summary(freq_diff)
confint(freq_diff)

get_prior(diff ~ 1, data = diff_gap, family = gaussian())

bayes_diff <- brm(diff ~ 1, data = diff_gap, family = gaussian(),
                  prior = c(prior(normal(0, 1000), class = "Intercept"),
                            prior(exponential(0.1), class = "sigma")),
                  file_refit = "on_change",
                  file = "applied_bayesian_modeling/models/bayes_diff.rds")

sum_bayes_diff <- summary(bayes_diff)

bayes_diff_posts <- as_draws_df(bayes_diff)



freq_bayes_means <- tibble(frequentist_mean = freq_diff$coefficients[1],
                           frequentist_lower = confint(freq_diff)[1],
                           frequentist_upper = confint(freq_diff)[2],
       bayesian_mean = sum_bayes_diff$fixed$Estimate,
       bayesian_lower = sum_bayes_diff$fixed$`l-95% CI`,
       bayesian_upper = sum_bayes_diff$fixed$`u-95% CI`) %>% 
  pivot_longer(cols = everything()) %>% 
  separate(name, c("method", "metric")) %>% 
  pivot_wider(names_from = metric, values_from = value) %>% 
  mutate(method = fct_relevel(method, "frequentist"),
         x = c(-0.2, 0.2))

library(ggrepel)
freq_bayes_plot <- le_diff +
  geom_pointrange(data = freq_bayes_means, 
                  aes(ymin = lower, ymax = upper, y = mean, x = x, color = method),
                  position = position_dodge(width = 0.4),
                  size = 1.5) +
  geom_text(data = freq_bayes_means, aes(x = x*2,
                                         y = 22, label = method),
            size = 5) +
  scale_color_colorblind() +
  guides(color = "none")
  
ggsave(freq_bayes_plot, file = "plots/freq_bayes_plot.jpg", width = 5, height = 5)



# toy priors --------------------------------------------------------------
prior_toys <- tibble(norm = rnorm(n = 1000),
                     unif = runif(n = 1000),
                     exp = rexp(n = 1000)) %>% 
  pivot_longer(cols = everything())


prior_toys %>% 
  ggplot(aes(x = value)) + 
  stat_density() +
  facet_wrap(~name) +
  theme_void()


# Plot prior, likelihood, posterior ---------------------------------------

bayes_diff <- readRDS("applied_bayesian_modeling/models/bayes_diff.rds")
post_bayesdiff <- as_draws_df(bayes_diff)

mean_prior = 0
sd_prior = 1000

post_like <- post_bayesdiff %>% 
  rename(posterior_mean = b_Intercept) %>% 
  mutate(prior_mean = rnorm(nrow(.), mean = mean_prior, sd = sd_prior),
         likelihood_mean = rnorm(nrow(.), mean = freq_diff$coefficients[[1]], 
                                 sd = sum_freq_diff$coefficients[[2]])) %>% 
  pivot_longer(cols = contains("mean")) %>% 
  separate(name, c("name", "measure")) 

colorblind_hex <- ggplot_build(ggplot(mtcars, aes(x = as.factor(cyl), y = hp, color = as.factor(cyl))) +
                                 scale_color_colorblind())
colorblind_hex_codes <- colorblind_hex$data %>% bind_rows() %>% distinct(colour) %>% pull()

prior_data <- tibble(diff = rnorm(25, 0, 10)) %>% 
  mutate(source = "previous study") %>% 
  bind_rows(diff_gap %>% mutate(source = "this study"))

prior_names <- tibble(name = c("likelihood", "prior", "posterior"),
                      x = c(15, -10, 12),
                      y = c(0.8, 0.20, 0.8)) 

prior_le <- post_like %>% 
  ggplot(aes(x = value)) + 
  geom_density(data = . %>% filter(name == "likelihood"), 
               aes(y = ..scaled..), color = "white") +
  scale_fill_viridis_d() +
  theme_default() +
  # geom_hline(yintercept = 0.25) +
  coord_cartesian(xlim = c(-40, 40)) +
  geom_point(data = prior_data %>% filter(source != "previous study"), 
             aes(x = diff, y = 0),
             shape = 21) +
  theme_default() +
  theme(text = element_text(size = 25),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Change (years)") +
  scale_color_colorblind()  +
  guides(color = "none")


prior_le2 <- prior_le + 
  geom_density(data = . %>% filter(name == "likelihood"), 
               aes(y = ..scaled..)) +
  geom_text_repel(data = prior_names %>% filter(name == "likelihood"), aes(x = x + 2, y = y, label = name, 
                                          color = name),
                  nudge_y = 0.06,
                  nudge_x = 10) 

prior_le3 <- prior_le + 
  geom_density(data = . %>% filter(name == "likelihood"), 
               aes(y = ..scaled..)) +
  geom_hline(yintercept = 0.2, color = colorblind_hex_codes[3]) +
  geom_text_repel(data = prior_names %>% filter(name != "posterior"), aes(x = x + 2, y = y, label = name, 
                                                                           color = name),
                  nudge_y = 0.06,
                  nudge_x = c(10, 0)) +
  scale_color_manual(values = c(colorblind_hex_codes[2], colorblind_hex_codes[3]))


prior_le4 <- prior_le + 
  geom_density(data = . %>% filter(name != "prior"), 
               aes(y = ..scaled.., color = name)) +
  geom_hline(yintercept = 0.2, color = colorblind_hex_codes[3]) +
  geom_text_repel(data = prior_names,
                  aes(x = x + 2, y = y, label = name,color = name),
                  nudge_y = 0.06,
                  nudge_x = c(10, 0, -10))

prior_le4 +
  # coord_cartesian(xlim = c(-100e1, 100e1)) +
  geom_text_repel(data = prior_names, aes(x = x + 2, y = y, label = name, 
                                          color = name),
                  nudge_y = 0.06,
                  nudge_x = c(-10, 0, 10)) +
  guides(color = "none")


ggsave(prior_le, file = "plots/prior_le.jpg", width = 7, height = 5)
ggsave(prior_le2, file = "plots/prior_le2.jpg", width = 7, height = 5)
ggsave(prior_le3, file = "plots/prior_le3.jpg", width = 7, height = 5)
ggsave(prior_le4, file = "plots/prior_le4.jpg", width = 7, height = 5)


# informed prior ----------------------------------------------------------
bayes_diff_inf <- update(readRDS("applied_bayesian_modeling/models/bayes_diff.rds"),
                     prior = c(prior(normal(-8, 5), class = "Intercept")))
post_bayesdiff_inf <- as_draws_df(bayes_diff_inf)

mean_prior = -8
sd_prior = 5

post_like_inf <- post_bayesdiff_inf %>% 
  rename(posterior_mean = b_Intercept) %>% 
  mutate(prior_mean = rnorm(nrow(.), mean = mean_prior, sd = sd_prior),
         likelihood_mean = rnorm(nrow(.), mean = freq_diff$coefficients[[1]], 
                                 sd = sum_freq_diff$coefficients[[2]])) %>% 
  pivot_longer(cols = contains("mean")) %>% 
  separate(name, c("name", "measure")) 

colorblind_hex <- ggplot_build(ggplot(mtcars, aes(x = as.factor(cyl), y = hp, color = as.factor(cyl))) +
                                 scale_color_colorblind())
colorblind_hex_codes <- colorblind_hex$data %>% bind_rows() %>% distinct(colour) %>% pull()

prior_data <- tibble(diff = rnorm(25, -8, 5)) %>% 
  mutate(source = "previous study") %>% 
  bind_rows(diff_gap %>% mutate(source = "this study"))

prior_names <- tibble(name = c("likelihood", "prior", "posterior"),
                      x = c(15, -8, 8),
                      y = c(0.8, 0.8, 0.8)) 

prior_le_inf <- post_like_inf %>% 
  ggplot(aes(x = value)) + 
  geom_density(data = . %>% filter(name == "likelihood"), 
               aes(y = ..scaled..), color = "white") +
  scale_fill_viridis_d() +
  theme_default() +
  # geom_hline(yintercept = 0.25) +
  coord_cartesian(xlim = c(-40, 40)) +
  geom_point(data = prior_data %>% filter(source != "previous study"), 
             aes(x = diff, y = 0),
             shape = 21) +
  theme_default() +
  theme(text = element_text(size = 25),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Change (years)") +
  scale_color_colorblind()  +
  guides(color = "none")


prior_le2_inf <- prior_le_inf + 
  geom_density(data = . %>% filter(name == "likelihood"), 
               aes(y = ..scaled..)) +
  geom_text_repel(data = prior_names %>% filter(name == "likelihood"), aes(x = x + 2, y = y, label = name, 
                                                                           color = name),
                  nudge_y = 0.06,
                  nudge_x = 10) +
  geom_point(data = prior_data %>% filter(source == "previous study"), 
             aes(x = diff, y = 0), color = colorblind_hex_codes[3]) 

prior_le3_inf <- prior_le_inf + 
  geom_density(data = . %>% filter(name != "posterior"), 
               aes(y = ..scaled.., color = name)) +
  geom_text_repel(data = prior_names %>% filter(name != "posterior"), aes(x = x + 2, y = y, label = name, 
                                                                          color = name),
                  nudge_y = 0.06,
                  nudge_x = c(10, 4)) +
  scale_color_manual(values = c(colorblind_hex_codes[2], colorblind_hex_codes[3])) +
  geom_point(data = prior_data %>% filter(source == "previous study"), 
             aes(x = diff, y = 0), color = colorblind_hex_codes[3]) 



prior_le4_inf <- prior_le_inf + 
  geom_density(aes(y = ..scaled.., color = name)) +
  geom_text_repel(data = prior_names,
                  aes(x = x + 2, y = y, label = name,color = name),
                  nudge_y = 0.06,
                  nudge_x = c(10, 0, -10)) +
  geom_point(data = prior_data %>% filter(source == "previous study"), 
             aes(x = diff, y = 0), color = colorblind_hex_codes[3]) 



ggsave(prior_le_inf, file = "plots/prior_le_inf.jpg", width = 7, height = 5)
ggsave(prior_le2_inf, file = "plots/prior_le2_inf.jpg", width = 7, height = 5)
ggsave(prior_le3_inf, file = "plots/prior_le3_inf.jpg", width = 7, height = 5)
ggsave(prior_le4_inf, file = "plots/prior_le4_inf.jpg", width = 7, height = 5)


# how to lie with a prior
# informed prior ----------------------------------------------------------
bayes_diff_inf <- update(readRDS("applied_bayesian_modeling/models/bayes_diff.rds"),
                         prior = c(prior(normal(-8, 0.0001), class = 'Intercept')))
post_bayesdiff_inf <- as_draws_df(bayes_diff_inf)

bayes_diff_inf

mean_prior = -8
sd_prior = 0.001

post_like_inf <- post_bayesdiff_inf %>% 
  rename(posterior_mean = b_Intercept) %>% 
  mutate(prior_mean = rnorm(nrow(.), mean = mean_prior, sd = sd_prior),
         likelihood_mean = rnorm(nrow(.), mean = freq_diff$coefficients[[1]], 
                                 sd = sum_freq_diff$coefficients[[2]])) %>% 
  pivot_longer(cols = contains("mean")) %>% 
  separate(name, c("name", "measure")) 

colorblind_hex <- ggplot_build(ggplot(mtcars, aes(x = as.factor(cyl), y = hp, color = as.factor(cyl))) +
                                 scale_color_colorblind())
colorblind_hex_codes <- colorblind_hex$data %>% bind_rows() %>% distinct(colour) %>% pull()

prior_data <- tibble(diff = rnorm(25, -8, 5)) %>% 
  mutate(source = "previous study") %>% 
  bind_rows(diff_gap %>% mutate(source = "this study"))

prior_names <- tibble(name = c("likelihood", "prior", "posterior"),
                      x = c(15, -8, 12),
                      y = c(0.8, 0.8, 0.8)) 

prior_le_inf <- post_like_inf %>% 
  ggplot(aes(x = value)) + 
  geom_density(data = . %>% filter(name == "likelihood"), 
               aes(y = ..scaled..), color = "white") +
  scale_fill_viridis_d() +
  theme_default() +
  # geom_hline(yintercept = 0.25) +
  coord_cartesian(xlim = c(-40, 40)) +
  geom_point(data = prior_data %>% filter(source != "previous study"), 
             aes(x = diff, y = 0),
             shape = 21) +
  theme_default() +
  theme(text = element_text(size = 25),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Change (years)") +
  scale_color_colorblind()  +
  guides(color = "none")


prior_le2_inf <- prior_le_inf + 
  geom_density(data = . %>% filter(name == "likelihood"), 
               aes(y = ..scaled..)) +
  geom_text_repel(data = prior_names %>% filter(name == "likelihood"), aes(x = x + 2, y = y, label = name, 
                                                                           color = name),
                  nudge_y = 0.06,
                  nudge_x = 10) +
  geom_point(data = prior_data %>% filter(source == "previous study"), 
             aes(x = diff, y = 0), color = colorblind_hex_codes[3]) 

prior_le3_inf <- prior_le_inf + 
  geom_density(data = . %>% filter(name != "posterior"), 
               aes(y = ..scaled.., color = name)) +
  geom_text_repel(data = prior_names %>% filter(name != "posterior"), aes(x = x + 2, y = y, label = name, 
                                                                          color = name),
                  nudge_y = 0.06,
                  nudge_x = c(10, 4)) +
  scale_color_manual(values = c(colorblind_hex_codes[2], colorblind_hex_codes[3])) +
  geom_point(data = prior_data %>% filter(source == "previous study"), 
             aes(x = diff, y = 0), color = colorblind_hex_codes[3]) 



prior_le4_inf <- prior_le_inf + 
  geom_density(aes(y = ..scaled.., color = name)) +
  geom_text_repel(data = prior_names,
                  aes(x = x + 2, y = y, label = name,color = name),
                  nudge_y = 0.06,
                  nudge_x = c(10, 0, -10)) +
  geom_point(data = prior_data %>% filter(source == "previous study"), 
             aes(x = diff, y = 0), color = colorblind_hex_codes[3]) 



ggsave(prior_le_inf, file = "plots/prior_le_inf.jpg", width = 7, height = 5)
ggsave(prior_le2_inf, file = "plots/prior_le2_inf.jpg", width = 7, height = 5)
ggsave(prior_le3_inf, file = "plots/prior_le3_inf.jpg", width = 7, height = 5)
ggsave(prior_le4_inf, file = "plots/prior_le4_inf.jpg", width = 7, height = 5)

#  t-test freq vs bayes ---------------------------------------------------

# data from : Figure 2: https://academic.oup.com/jxb/article/52/364/2105/423995
set.seed(23234)
koster_data_wide <- tibble(four_hr = rnorm(5, 60, 10),
                           one_week = rnorm(5, 2, 0.5))

koster_data <- koster_data_wide %>% 
  pivot_longer(cols = everything())

koster_data %>% 
  ggplot(aes(x = name, y = value)) +
  geom_point() +
  coord_cartesian(ylim = c(0, 100)) +
  labs(y = "Survival (%)",
       x = "Time Imbibed")


# t.test()
koster_ttest_raw <- t.test(koster_data_wide$four_hr, koster_data_wide$one_week)

# lm ttest
koster_ttest <- lm(value ~ 1 + name, data = koster_data)
summary(koster_ttest)
# hypothesis test: pvalue < 0.0001. P(d|H). The null hypothesis is that survival in the four_hr and one_week treatment are
# exactly the same. If we assume the null hypothesis is true, then the probability of obtaining a t-value at least 
# as large as the one we obtained is less than 0.0001. Given this low p-value, we can reject the null hypothesis.
# What is the probability of the null hypothesis? can't answer

# estimation: What is survival in the two groups? 
koster_confint <- confint(lm(value ~ 0 + name, data = koster_data))
# four_hr = 95% CI(52 to 66). one_week = 95% CI(-5 to 8) 

# estimation: What is the difference in survival between the groups? 
confint(lm(value ~ 1 + name, data = koster_data))
# CI of difference is -66 to -48. In words. If we were to repeat this experiment an infinite number of times, then the
# 95% of the confidence intervals of the procedure would contain the true mean difference. Our current confidence interval is one o
# of those infinite repetitions. Whether it is one of the 95% correct or 5% incorrect is not known.
# i.e., this confidence interval has a 95% chance or containing the true mean, but also a 5% chance of not containing it. This says
# nothing about the probability of the true mean itself. It is either inside of this interval or it isn't. We don't
# know if this interval contains it or doesn't.



koster_bayes_ttest <- brm(value ~ name, data = koster_data, chains = 1, iter = 1000,
                          priors = c(prior(normal(50, 20), class = "Intercept"),
                                     prior(normal(0, 50), class = "b")),
                          file_refit = "on_change",
                          file = "applied_bayesian_modeling/models/koster_bayes_ttest.rds")

# hypothesis test: The probability that survival is lower at one week versus four hours is >99.9%

# estimation: What is survival in the two groups? 
post_koster <- as_draws_df(koster_bayes_ttest)

post_koster_summary <- post_koster %>% mutate(one_week_mean = b_Intercept + b_nameone_week) %>% 
  pivot_longer(cols = c(b_Intercept, one_week_mean)) %>% 
  group_by(name) %>% 
  median_qi(value)
# four_hr = 95% CI(52 to 66). one_week = 95% CI(-4 to 8.5) 

# estimation: What is the difference in survival between the groups? 
summary(koster_bayes_ttest)
# CrI of difference is -66 to -48. There is a 95% probability that the difference between one_week and four_hr survival is
# between -48 and -66 percentage points, given the model and data.



# plot comparison
kost_noint <- lm(value ~ 0 + name, data = koster_data)


# compare to tight prior

koster_bayes_tight_prior <- brm(value ~ name, data = koster_data, chains = 1, iter = 1000,
                                prior = c(prior(normal(50, 20), class = "Intercept"),
                                          prior(normal(0, 5), class = "b")),
                                file_refit = "on_change",
                                file = "applied_bayesian_modeling/models/koster_bayes_tight_prior.rds")


koster_bayes_tight_prior



priors = tibble(weak_fourhr = rnorm(200, 50, 20),
                weak_oneweek = weak_fourhr + rnorm(200, 0, 50),
                strong_fourhr = rnorm(200, 50, 20),
                strong_oneweek = strong_fourhr + rnorm(200, 0, 5)) %>% 
  pivot_longer(cols = everything(), values_to = "mean") %>% 
  separate(name, c("prior_type", "trt")) %>% 
  mutate(trt = case_when(trt == "fourhr" ~ "four_hr", 
                         TRUE ~ "one_week"))

prior_means = priors %>% 
  group_by(prior_type, trt) %>% 
  median_qi()


freq_bayes_koster <- tibble(mean = c(kost_noint$coefficients[[1]], kost_noint$coefficients[[2]]),
                            trt = c("four_hr", "one_week"),
                            lower = c(koster_confint[[1]], koster_confint[[2]]),
                            upper = c(koster_confint[[3]], koster_confint[[4]])) %>% 
  mutate(model = "Frequentist") %>% 
  bind_rows(post_koster_summary %>% 
              mutate(trt = case_when(name == "b_Intercept" ~ "four_hr",
                                     TRUE ~ "one_week")) %>% 
              rename(mean = value, lower = .lower, upper = .upper) %>% 
              select(trt, mean,lower, upper) %>% 
              mutate(model = "Bayesian")) %>% 
  mutate(model = fct_relevel(model, "Frequentist"))


plot_comparison <- freq_bayes_koster %>% 
  ggplot(aes(x = trt, y = mean)) + 
  geom_pointinterval(aes(ymin = lower, ymax = upper, color = model),
                     position = position_dodge(width = 0.2), 
                     size = 6) + 
  geom_point(data = koster_data, aes(x = name, y = value)) +
  # ylim(-10, 100) +
  labs(y = "Survival (%)",
       x = "Time Imbibed") + 
  theme_classic()






plot_comparison +
  geom_pointinterval(data = prior_means %>% filter(prior_type == "weak"), 
                     aes(x = trt, y = mean, ymin = .lower, ymax = .upper),
                     position = position_nudge(x = 0.2)) + 
  # geom_violin(data = priors %>% filter(prior_type == "weak"), aes(x = trt, y = mean)) +
  coord_cartesian(ylim = c(NA, NA))

plot_comparison +
  geom_pointinterval(data = prior_means %>% filter(prior_type != "weak"), 
                     aes(x = trt, y = mean, ymin = .lower, ymax = .upper),
                     position = position_nudge(x = 0.2)) + 
  # geom_violin(data = priors %>% filter(prior_type == "weak"), aes(x = trt, y = mean)) +
  coord_cartesian(ylim = c(NA, NA))






