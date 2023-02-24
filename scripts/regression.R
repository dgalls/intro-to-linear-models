# SET UP ----

library(tidyverse)
library(rstatix)
library(performance)

janka <- read_csv(here("data", "janka.csv"))

## tidy ----

glimpse(janka)

head(janka)

# VISUALISATION ----

janka %>%
  ggplot(aes(x = dens,
             y = hardness)) +
  geom_point()+
  geom_smooth(method="lm")

## Pearson's R

janka %>% 
  cor_test(dens, 
           hardness)

janka_ls1 <- lm(hardness ~ dens, data = janka) 

## summary ----

janka_ls1 %>% 
  broom::tidy()                # tidyverse

#summary(janka_ls1)            # base R

# MEAN-CENTERED REGRESSION ----

dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))                   # 45.73333


janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()

# CONFIDENCE LEVELS ----

broom::tidy(janka_ls1, conf.int=T, conf.level=0.95)











