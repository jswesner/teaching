# Simulate prior means and prior data using the prior predictive distribution

# load packages
library(tidyverse)
library(rethinking)
library(brms)
library(janitor)

# load data
d <- as_tibble(mtcars)


# gaussian regression  -------

# y ~ dnorm(mu, sigma)
# mu  = a + bx
# a ~ ?
# b ~ ?
# sigma ~ ?

N = 1  # number of simulations (change as needed)

# simulate priors
priors <- tibble(a = rnorm(N, 0, 10),
                 b = rnorm(N, 0, 10),
                 sigma = rexp(N, 0.0001),
                 sim = 1:N)

# data (only the x values, since we're simulating y and mu and pretending we don't have them yet)
x <- d$hp

# combine and simulate
prior_and_x <- priors %>% expand_grid(x = x) %>%    # combine priors and x's
  mutate(mu = a + b*x,                              # simulate regressions
         y = rnorm(nrow(.), mu, sigma))             # simulate data (e.g., y_rep)

# plot
prior_and_x %>% 
  ggplot(aes(x = x, y = mu, group = sim)) + 
  geom_line() +
  geom_point(aes(y = y)) +
  labs(y = "sim")


# gamma regression  -------

# y ~ dgamma(scale, shape)
# scale = mu/shape
# log(mu)  = a + bx
# a ~ ?
# b ~ ?
# shape ~ ?

N = 10  # number of simulations

# simulate priors
priors <- tibble(a = rnorm(N, 2, 1),
                 b = rnorm(N, 0, 1),
                 shape = rexp(N, 1),
                 sim = 1:N)

# data (only the x values, since we're simulating y and mu and pretending we don't have them yet)
x <- (d$hp - mean(d$hp))/sd(d$hp)

# combine and simulate
prior_and_x <- priors %>% expand_grid(x = x) %>%    # combine priors and x's
  mutate(mu = exp(a + b*x),                         # simulate regressions
         y = rgamma(nrow(.), scale = mu/shape, shape))  # simulate data (e.g., y_rep)
          

# plot
prior_and_x %>% 
  ggplot(aes(x = x, y = mu, group = sim)) + 
  geom_line() +
  geom_point(aes(y = y)) +
  labs(y = "sim") 

# gaussian anova  -------

# y ~ dnorm(mu, sigma)
# mu  = a + bx
# a ~ ?
# b ~ ?
# sigma ~ ?

N = 10  # number of simulations

# simulate priors
priors <- tibble(a = rnorm(N, 0, 10),
                 b1 = rnorm(N, 0, 10),
                 b2 = rnorm(N, 0, 10),
                 sigma = rexp(N, 0.0001),
                 sim = 1:N)

# simulate
prior_and_x <- priors %>%  
  mutate(mu_3 = a + b1*0 + b2*0,                                       # simulate means
         mu_4 = a + b1*1 + b2*0,
         mu_5 = a + b1*0 + b2*1) %>%
  pivot_longer(cols = c(mu_3, mu_4, mu_5), values_to = "mu") %>% 
  separate(name, c("measure", "x")) %>% 
  mutate(x = as.numeric(x)) %>% 
  right_join(d %>% select(gear) %>% rename(x = gear)) %>% # repeat means for each row of gear in original data
  mutate(y = rnorm(nrow(.), mu, sigma))            # simulate data (e.g., y_rep)

# plot
prior_and_x %>% 
  ggplot(aes(x = x, y = mu, group = sim)) + 
  geom_line() +
  geom_point(aes(y = y)) +
  labs(y = "sim") 

# gamma anova  -------

# y ~ dgamma(scale, shape)
# scale = mu/shape
# log(mu)  = a + b1x1 + b2*x2
# a ~ ?
# b ~ ?
# shape ~ ?

N = 100  # number of simulations

# simulate priors
priors <- tibble(a = rnorm(N, 2, 1),
                 b1 = rnorm(N, 0, 1),
                 b2 = rnorm(N, 0, 1),
                 shape = rexp(N, 0.1),
                 sim = 1:N)

# simulate
prior_and_x <- priors %>%  
  mutate(mu_3 = exp(a + b1*0 + b2*0),                                 # simulate means
         mu_4 = exp(a + b1*1 + b2*0),
         mu_5 = exp(a + b2*0 + b2*1)) %>%
  pivot_longer(cols = c(mu_3, mu_4, mu_5), values_to = "mu") %>% 
  separate(name, c("measure", "x")) %>% 
  mutate(x = as.numeric(x)) %>% 
  right_join(d %>% select(gear) %>% rename(x = gear)) %>%             # repeat means for each row of gear in original data
  mutate(y = rgamma(nrow(.), scale = mu/shape, shape = shape))        # simulate data (e.g., y_rep)

# plot
prior_and_x %>% 
  ggplot(aes(x = x, y = mu, group = sim)) + 
  geom_line() +
  geom_point(aes(y = y)) +
  labs(y = "sim") 

# gaussian regression hierarchical------

# y ~ dnorm(mu, sigma)
# mu  = a + a[cyl] + bx
# a ~ ?
# a[cyl] ~ dnorm(0, sd_cyl)
# b ~ ?
# sigma ~ ?
# sd_cyl ~ ?

N = 10  # number of simulations

# simulate priors

varying_intercepts <- tibble(cyl = unique(d$cyl),
                             n_levels = length(cyl)) %>%   # get levels of intercept
  expand_grid(sim = 1:N) %>%                               # repeat levels sim times
  group_by(sim) %>%                                        # group by sims
  mutate(sd_cyl = rexp(1, 0.001),                          # simulate a standard deviation
         a_cyl = rnorm(n_levels, 0, sd_cyl))               # simulate an intercept offset

priors <- tibble(a = rnorm(N, 0, 10),
                 b = rnorm(N, 0, 10),
                 sigma = rexp(N, 0.001),
                 sim = 1:N) 

# data (only the x values, since we're simulating y and mu and pretending we don't have them yet)
x <- tibble(x = d$hp,
            cyl = d$cyl)

# combine and simulate
prior_and_x <- priors %>% expand_grid(x) %>%        # combine priors and x's
  left_join(varying_intercepts) %>%                 # add the varying intercepts
  mutate(mu = a + b*x,                              # simulate regressions - no varying intercept
         mu_cyl = a + a_cyl + b*x,                  # simulate regressions - with varying intercept
         y = rnorm(nrow(.), mu, sigma),             # simulate data (e.g., y_rep) - no varying intercept
         y_cyl = rnorm(nrow(.), mu_cyl, sigma))     # simulate data (e.g., y_rep) - with varying intercept

# plot
prior_and_x %>% 
  ggplot(aes(x = x, y = mu_cyl, group = interaction(cyl, sim), color = cyl)) + 
  geom_line() +
  geom_point(aes(y = y_cyl)) +
  labs(y = "sim") +
  facet_wrap(~sim)

