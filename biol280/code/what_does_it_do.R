library(tidyverse) # what does this do?



d <- mtcars # what does this do?




d <- mtcars %>% 
  mutate(mean = mean(hp)) # what does this do?



d <- mtcars %>% 
  filter(mpg <= 20) # what does this do?  # what would happen to this data set if we re-ran the code above?



d <- mtcars %>% 
  group_by(cyl) %>% 
  mutate(median = median(qsec)) # what does this do?



d <- mtcars %>% 
  group_by(cyl) %>% 
  mutate(median = median(qsec),
         mean = mean(qsec),
         sd = sd(qsec)) # what does this do?



mtcars %>% 
  ggplot(aes(x = am, y = disp)) +
  geom_point() # what does this do?



mtcars %>% 
  ggplot(aes(x = am, y = mpg)) + 
  geom_point(color = "green")



mtcars %>% 
  ggplot(aes(x = am, y = mpg)) + 
  geom_point(aes(color = cyl))


mtcars %>% 
  ggplot(aes(x = hp, y = mpg)) + 
  geom_point() + 
  geom_line()



