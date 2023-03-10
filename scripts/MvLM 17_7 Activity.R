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

## data ----
# make three vectors and combine them into a new tibble

height <- c(50,57,91,94,102,110,57,71,85,105,120)
size <- c(rep("small", 2), rep("large", 4), rep("small", 3), rep("large", 2))
treatment <- c(rep("Control", 6), rep("Removal", 5))

unbalanced <- tibble(height, size, treatment)

unbalanced

# TESTING SIZE AND TREATMENT AGAINST HEIGHT ----

ls_4 <- lm(height ~ treatment + size, data = unbalanced)
anova(ls_4)

ls_5 <- lm(height ~ size + treatment, data = unbalanced)
anova(ls_5)

## comparing 2 models ----

drop1(ls_4)

drop1(ls_5)























