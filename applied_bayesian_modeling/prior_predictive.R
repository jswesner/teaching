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
  # geom_point(aes(y = y)) +
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
  cores = 4, chains = 1, iter = 1000, 
  file = "applied_bayesian_modeling/models/gaus_ret.rds")


# fit with brms
gaus_brm <- brm(mpg ~ hp_c, 
                data = d %>% mutate(hp_c = hp - mean(hp)),
                family = gaussian(),
                prior = c(prior(normal(25, 5), class = "Intercept"),
                          prior(normal(0, 0.1), class = "b"),
                          prior(exponential(1), class = "sigma")), 
                cores = 4, chains = 1, iter = 1000,
                file = "applied_bayesian_modeling/models/gaus_brm.rds")

gaus_brm

posteriors <- tibble(a = rnorm(N, 20.21, 0.72),
                 b = rnorm(N, -0.07, 0.01),
                 sigma = rexp(N, 0.28),
                 sim = 1:N)

posterior_and_x <- posteriors %>% expand_grid(x = x) %>%    # combine priors and x's
  mutate(mu = a + b*x,                              # simulate regressions
         y = rnorm(nrow(.), mu, sigma))             # simulate data (e.g., y_rep)


posterior_and_x %>% 
  filter(sim <= 20) %>% 
  ggplot(aes(x = y)) + 
  geom_histogram() + 
  facet_wrap(~sim) +
  geom_histogram(data = d %>% mutate(sim = "raw"), aes(x = mpg), fill = "blue")

# gamma regression  -------

# y ~ dgamma(scale, shape)
# scale = mu/shape
# log(mu)  = a + bx
# a ~ ?
# b ~ ?
# shape ~ ?

N = 100  # number of simulations

# simulate priors
priors <- tibble(a = rnorm(N, 2, 1),
                 b = rnorm(N, 0, 0.01),
                 shape = rexp(N, 2), # for brms
                 scale = rexp(N, 2), # for rethinking
                 sim = 1:N)

# data (only the x values, since we're simulating y and mu and pretending we don't have them yet)
x <- mtcars$hp

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
  labs(y = "sim") +
  # scale_y_log10() +
  NULL

# fit with rethinking (NOTE: rethinking and brms use different parameterizations for gamma)
gamma_ret <- ulam(
  alist(
    mpg ~ dgamma2(mu, scale),
    log(mu) <- a + b*hp_c,
    a ~ dnorm(3, 1),
    b ~ dnorm(0, 1),
    scale ~ dexp(0.1)),
  data = list(mpg = d$mpg, hp_c = (d$hp - mean(d$hp))/sd(d$hp)),
  cores = 4, chains = 1, iter = 1000, 
  file = "applied_bayesian_modeling/models/gamma_ret.rds")

# fit with brms (NOTE: rethinking and brms use different parameterizations for gamma)
gamma_brm <- brm(mpg ~ hp_c, 
                 family = Gamma(link = "log"),
                 data = d %>% mutate(hp_c = (hp - mean(hp))/sd(hp)),
                 prior = c(prior(normal(3, 1), class = "Intercept"),
                           prior(normal(0, 1), class = "b"),
                           prior(exponential(0.1), class = "shape")),
                 cores = 4, chains = 1, iter = 1000, 
                 file = "applied_bayesian_modeling/models/gaus_brm.rds")

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
  right_join(d %>% select(gear) %>% rename(x = gear)) %>%              # repeat means for each row of gear in original data
  mutate(y = rnorm(nrow(.), mu, sigma))                                # simulate data (e.g., y_rep)

# plot
prior_and_x %>% 
  ggplot(aes(x = x, y = mu, group = sim)) + 
  geom_line() +
  geom_point(aes(y = y)) +
  labs(y = "sim") 

# fit with rethinking
# make the data matrix
d_mat <- d %>% mutate(gear4 = case_when(gear == 4 ~ 1, TRUE ~ 0),
                      gear5 = case_when(gear == 5 ~ 1, TRUE ~ 0))

gaus_anova_ret <- ulam(
  alist(mpg ~ dnorm(mu, sigma),
        mu <- a + b1*gear4 + b2*gear5,
        a ~ dnorm(0, 10),
        b1 ~ dnorm(0, 10),
        b2 ~ dnorm(0, 10),
        sigma ~ dexp(0.01)
        ),
  data = list(mpg = d_mat$mpg,
              gear4 = d_mat$gear4,
              gear5 = d_mat$gear5),
  cores = 4, chains = 1, iter = 1000, 
  file = "applied_bayesian_modeling/models/gaus_anova_ret.rds"
)

# fit with brms
d_brm <- d %>% mutate(gear_f = as.factor(gear)) # make gear a factor

gaus_anova_brm <- brm(mpg ~ gear_f,
                      family = gaussian(),
                      data = d_brm,
                      prior = c(prior(normal(0, 10), class = "Intercept"),
                                prior(normal(0, 10), class = "b"),
                                prior(exponential(0.01), class = "sigma")),
                      cores = 4, chains = 1, iter = 1000, 
                      file = "applied_bayesian_modeling/models/gaus_anova_brm.rds")


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

# fit with rethinking (NOTE: rethinking and brms use different parameterizations for gamma)
# make the data matrix
d_mat <- d %>% mutate(gear4 = case_when(gear == 4 ~ 1, TRUE ~ 0),
                      gear5 = case_when(gear == 5 ~ 1, TRUE ~ 0))

anova_gamma_ret <- ulam(
  alist(
    mpg ~ dgamma2(mu, scale),
    log(mu) <- a + b1*gear4 + b2*gear5,
    a ~ dnorm(0, 2),
    b1 ~ dnorm(0, 1),
    b2 ~ dnorm(0, 1),
    scale ~ dexp(0.1)
  ),
  data = list(mpg = d_mat$mpg,
              gear4 = d_mat$gear4,
              gear5 = d_mat$gear5),
  cores = 4, chains = 1, iter = 1000, 
  file = "applied_bayesian_modeling/models/anova_gamma_ret.rds")

# fit with brms (NOTE: rethinking and brms use different parameterizations for gamma)

d_brm <- d %>% mutate(gear_f = as.factor(gear)) # make gear a factor

anova_gamma_brm <- brm(mpg ~ gear_f,
    family = Gamma(link = "log"),
    data = d_brm,
    prior = c(prior(normal(0, 1), class = "Intercept"),
              prior(normal(0, 1), class = "b"),
              prior(exponential(0.1), class = "shape")),
    cores = 4, chains = 1, iter = 1000, 
    file = "applied_bayesian_modeling/models/anova_gamma_brm.rds")


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
  mutate(sd_cyl = rexp(1, 0.01),                          # simulate a standard deviation
         a_cyl = rnorm(n_levels, 0, sd_cyl))               # simulate an intercept offset

priors <- tibble(a = rnorm(N, 0, 10),
                 b = rnorm(N, 0, 10),
                 sigma = rexp(N, 0.01),
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

# fit in rethinking
d$cyl_int <- as.integer(as.factor(d$cyl)) # make cylinder an integer with lowest value = 1
d$hp_s <- (d$hp - mean(d$hp))/sd(d$hp)

gaus_h_ret <- ulam(
  alist(mpg ~ dnorm(mu, sigma),
        mu <- a + a_cyl[cyl_int] + b1*hp_s,
        a ~ dnorm(0, 10),
        b1 ~ dnorm(0, 10),
        sigma ~ dexp(0.1),
        a_cyl[cyl_int] ~ dnorm(0, sd_cyl),
        sd_cyl ~ dexp(0.01)),
  data = list(mpg = d$mpg,
              hp_s = d$hp_s,
              cyl_int = d$cyl_int), cores = 4, chains = 1, iter = 1000, 
  file = "applied_bayesian_modeling/models/gaus_h_ret.rds"
)

# fit in brms
d$cyl_fac <- as.factor(d$cyl) # make cylinder a factor
d$hp_s <- (d$hp - mean(d$hp))/sd(d$hp)

gaus_h_brm <- brm(mpg ~ hp_s + (1|cyl_fac),
                  data = d,
                  family = gaussian(),
                  prior = c(prior(normal(0, 10), class = "Intercept"),
                            prior(normal(0, 10), class = "b"),
                            prior(exponential(0.1), class = "sigma"),
                            prior(exponential(0.01), class = "sd")),
                  cores = 4, chains = 1, iter = 1000, 
                  file = "applied_bayesian_modeling/models/gaus_h_brm.rds"
                  )


