

# Slide 4 -- example of pipe usage
library(magrittr)
data(infert)
infert %>% subset(case==1) %>% nrow

nrow(infert[infert$case == 1,])

# Slide 5 -- pipe from raw data into regression model, where data source is not first argument
infert %>% subset(case==1) %>% lm(parity ~ age, data=.)


# Slide 6 -- less common forms of pipes
# %$% to expose names
infert %>% subset(case==1) %$% mean(age)


# %<>% for assignment
myinfert <- infert
nrow(myinfert)
myinfert %<>% subset(case==1) 
nrow(myinfert)


# Slide 10
library(dplyr)
data(mtcars)
mtcars %>%
  filter(am == 1) %>%
  group_by(cyl) %>%
  summarise(mean_mpg=mean(mpg))

