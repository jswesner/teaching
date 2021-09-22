# what does this do?
library(tidyverse) 


# what does this do?
d <- iris 


# what does this do?
d <- iris %>%  
  pivot_longer(cols = c(Petal.Length, Sepal.Length))


# what does this do?
iris %>% 
  ggplot(aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(shape = 21) +
  geom_line()



# what is wrong here?
iris %>% 
  ggplot(aes(x = Sepal.length, y = petal.Length)) +
  geom_point(shape = 21) +
  geom_line()
  