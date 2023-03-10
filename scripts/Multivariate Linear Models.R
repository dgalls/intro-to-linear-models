# SET UP ----

library(tidyverse)
library(rstatix)
library(performance)
library(patchwork)
library(here)
library(kableExtra)
library(GGally)
library(broom.helpers)
library(emmeans)
library(performance)

biomass <- read_csv(here("data", "biomass.csv"))

## tidy ----

glimpse(biomass)                             # check the structure of the data

head(biomass)                                # check data is in a tidy format

colnames(biomass)                            # check variable names

biomass %>%                                  # checking for duplicates
  duplicated() %>% 
  sum()

biomass %>%                                  # check typos by looking at impossible values
  summarise(min=min(Biomass.m2, na.rm=TRUE), 
            max=max(Biomass.m2, na.rm=TRUE))

biomass %>%                                  # check typos by looking at distinct characters/values
  distinct(Fert)

biomass %>% 
  distinct(Light)

biomass %>% 
  distinct(FL)

biomass %>%                                  # check for missing values
  is.na() %>% 
  sum()

summary(biomass)

# ONE-WAY ANOVA ----

ls_1 <- lm(Biomass.m2 ~ FL, data = biomass)

summary(ls_1)

## precise confidence intervals ----

broom::tidy(ls_1, conf.int = T)

## coefficient plot ----

GGally::ggcoef_model(ls_1,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)

### does A+B not equal A+B?

coef(ls_1)[2] + coef(ls_1)[3]                 # combine the average mean differences of the light effect and fertiliser effect

coef(ls_1)[4]                                 # compare this to the average difference of the combined treatment

# ADDITIVE MODEL ----

biomass %>% ggplot(aes(x=Fert, 
                       y=Biomass.m2, 
                       colour = Light, 
                       fill = Light, 
                       group = Light))+
  geom_jitter(width=0.1)+
  stat_summary(geom = "point", 
               fun = "mean", 
               size = 3, 
               shape = 23)+
  stat_summary(geom = "line", 
               fun = "mean", 
               size = 1, 
               linetype = "dashed")

## testing for an interactive term ----

ls_2 <- lm(Biomass.m2 ~ Fert +                # main effect
             Light +                          # main effect
             Fert:Light,                      # interaction term
           data = biomass)

summary(ls_2)

GGally::ggcoef_model(ls_2,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)

# Model Estimates and Confidence Intervals ----

coef(ls_1)[4]                                 # model 1

coef(ls_2)[2] + coef(ls_2)[3] + coef(ls_2)[4] # model 2

# ANOVA TABLES ----

## f statistic for interaction effect ----

### full model ----

drop1(ls_2, test = "F")

### reduced model ----

ls_3 <- lm(Biomass.m2 ~ Fert + Light, data = biomass)

drop1(ls_3, test = "F")                       # we have to remove the interaction term before we can keep using drop1()












































