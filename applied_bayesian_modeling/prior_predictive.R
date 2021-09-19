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

N = 100  # number of simulations (change as needed)

# simulate priors
priors <- tibble(a = rnorm(N, 25, 5),
                 b = rnorm(N, 0, 0.1),
                 sigma = rexp(N, 1),
                 sim = 1:N)

# data (only the x values, since we're simulating y and mu and pretending we don't have them yet)
x <- d$hp - mean(d$hp)

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


# fit with rethinking
gaus_ret <- ulam(
  alist(
    mpg ~ dnorm(mu, sigma),
    mu <- a + b*hp_c,
    a ~ dnorm(25, 5),
    b ~ dnorm(0, 0.1),
    sigma ~ dexp(1)),
  data = list(mpg = d$mpg, hp_c = d$hp - mean(d$hp)),
  cores = 4)


# fit with brms
gaus_brm <- brm(mpg ~ hp_c, 
                data = d %>% mutate(hp_c = hp - mean(hp)),
                family = gaussian(),
                prior = c(prior(normal(25, 5), class = "Intercept"),
                          prior(normal(0, 0.1), class = "b"),
                          prior(exponential(1), class = "sigma")), 
                cores = 4)

# gamma regression  -------

# y ~ dgamma2(scale, shape)
# scale = mu/shape
# log(mu)  = a + bx
# a ~ ?
# b ~ ?
# shape ~ ?

N = 10  # number of simulations

# simulate priors
priors <- tibble(a = rnorm(N, 2, 1),
                 b = rnorm(N, 0, 1),
                 shape = rexp(N, 2), # for brms
                 scale = rexp(N, 2), # for rethinking
                 sim = 1:N)

# data (only the x values, since we're simulating y and mu and pretending we don't have them yet)
x <- (d$hp - mean(d$hp))/sd(d$hp)

# combine and simulate
prior_and_x <- priors %>% expand_grid(x = x) %>%    # combine priors and x's
  mutate(mu = exp(a + b*x),                         # simulate regressions
         y = rgamma(nrow(.), scale = mu/shape, shape),   # simulate data (e.g., y_rep) - brms parameterization
         y_scale = rgamma(nrow(.), shape = mu/scale, scale))  # simulate data (e.g., y_rep) - rethinking parameterization
# plot
prior_and_x %>% 
  ggplot(aes(x = x, y = mu, group = sim)) + 
  geom_line() +
  geom_point(aes(y = y)) +
  labs(y = "sim") 

# fit with rethinking (NOTE: rethinking and brms use different parameterizations for gamma)
gamma_ret <- ulam(
  alist(
    mpg ~ dgamma2(mu, scale),
    log(mu) <- a + b*hp_c,
    a ~ dnorm(3, 1),
    b ~ dnorm(0, 1),
    scale ~ dexp(0.1)),
  data = list(mpg = d$mpg, hp_c = (d$hp - mean(d$hp))/sd(d$hp)),
  cores = 4)

# fit with brms (NOTE: rethinking and brms use different parameterizations for gamma)
gamma_brm <- brm(mpg ~ hp_c, 
                 family = Gamma(link = "log"),
                 data = d %>% mutate(hp_c = (hp - mean(hp))/sd(hp)),
                 prior = c(prior(normal(3, 1), class = "Intercept"),
                           prior(normal(0, 1), class = "b"),
                           prior(exponential(0.1), class = "shape")),
                 cores = 4)

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

