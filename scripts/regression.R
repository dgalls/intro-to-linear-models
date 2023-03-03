# SET UP ----

library(tidyverse)
library(rstatix)
library(performance)
library(patchwork)

janka <- read_csv(here("data", "janka.csv"))
darwin <- read_csv(here("data", "darwin.csv"))

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

#broom::tidy(janka_ls1, conf.int=T, conf.level=0.95)     # tidyverse

confint(janka_ls1)                                      # base R

# EFFECT SIZE ----

janka_ls1 %>% 
  broom::glance()

# CHECKING ASSUMPTIONS ----

# checking that the unexplained variation around the regression line (residuals)
# is (approx) normally distributed and has constant variance

janka_ls1 %>% 
  broom::augment() %>% 
  head()

augmented_ls1 <- janka_ls1 %>% 
  broom::augment()

augmented_ls1 %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), 
               linetype="dashed", colour="red")

# checking that (1) residuals are normally distributed (2) residuals are homogenous

model_plot <- function(data=augmented_ls1, 
                       x="dens", 
                       y="hardness", 
                       title="Full data"){
  ggplot(aes(x=.data[[x]], 
             y=.data[[y]]), 
         data=data)+
    geom_line()+
    theme_bw()+
    ggtitle(title)
}

p1 <- model_plot() # A line connecting all the data points in order 
p2 <- model_plot(y=".fitted", title="Linear prediction") # Plotting the fitted values against the independent e.g. our regression line
p3 <- model_plot(y=".resid", title="Remaining pattern") # Plotting the residuals against the fitted values e.g. remaining variance

## normality of residuals ----

performance::check_model(janka_ls1, check=c("normality","qq"))

## equal variance ----

performance::check_model(janka_ls1, check="homogeneity")

## outliers ----

performance::check_model(janka_ls1, check="outliers")

# PREDICTIONS ----

coef(janka_ls1)  # estimates intercept and slope

#predict(janka_ls1)

broom::augment(janka_ls1, 
               newdata=tibble(dens=c(22, 35, 65)),
               se = TRUE,                          # standard error
               interval = "confidence")            # confidence intervals               

## using emmeans ---

emmeans::emmeans(janka_ls1, 
                 specs = "dens", 
                 at = list(dens = c(22, 35, 65)))

## plotting predictions onto an existing figure ----

pred_newdata <- broom::augment(janka_ls1, 
                               newdata=tibble(dens=c(22, 35, 65)))

janka %>%                                             # piping in data
  ggplot(aes(x=dens, y=hardness))+                    # plot
  geom_point()+                                       
  geom_smooth(method="lm")+
  geom_point(data=pred_newdata, 
             aes(y=.fitted, x=dens), 
             colour="red")+
  geom_label(data=pred_newdata, 
             (aes(y=(.fitted+10), x=(dens+3), 
                  label=round(.fitted, digits=0))))+
  theme_bw()+
  labs(x="Density", y="Timber Hardness")+
  scale_x_continuous(limits=c(20,70), 
                     expand=expansion(add=c(0,5)))

# ANOVA ----

## model 1 ----

lsmodel1 <- lm(height ~ type, data = darwin)

anova(lsmodel1)

## model 2 ----


lsmodel2 <- lm(height ~ type + as.factor(pair), data = darwin)

anova(lsmodel2)


