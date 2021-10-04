anova_gamma_brm

anova_gamma_post <- posterior_samples(anova_gamma_brm) %>% as_tibble()


derived<- anova_gamma_post %>% mutate(gear_3 = exp(b_Intercept),
                            gear_4 = exp(b_Intercept + b_gear_f4),
                            gear_5 = exp(b_Intercept + b_gear_f5)) %>% 
  select(gear_3, gear_4, gear_5)


derived %>% mutate(diff_45 = gear_4 - gear_5) %>%  
            summarize(prob_45 = sum(gear_3>0)/nrow(.),
                      mean = mean(gear_3),
                      sd = sd(gear_3))
# prob (>0.999)

plot_posts <- plot(conditional_effects(anova_gamma_brm), points = T)
plot_posts$gear_f + 
  theme_classic() +
  labs(x = "Gears")

derived %>% 
  pivot_longer(cols = everything(), names_to = "Gears", 
               values_to = "mpg") %>% 
  group_by(Gears) %>% 
  summarize(mean = mean(mpg),
            sd = sd(mpg)) %>% 
  ggplot(aes(x = Gears, y = mean, ymax = mean + sd, 
             ymin = mean - sd)) +
  geom_pointrange()


derived %>% 
  pivot_longer(cols = everything(), names_to = "Gears", 
               values_to = "mpg") %>% 
  ggplot(aes(x = Gears, y = mpg)) + 
  geom_violin(aes(group = Gears))

derived %>% 
  pivot_longer(cols = everything(), names_to = "Gears", 
               values_to = "mpg") %>% 
  ggplot(aes(x = mpg, fill = Gears)) + 
  geom_density()













