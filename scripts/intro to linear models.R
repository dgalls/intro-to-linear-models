# SET UP ----

library(tidyverse)
library(here)
library(kableExtra)
library(GGally)
library(broom.helpers)
library(emmeans)
library(performance)

darwin <- read_csv(here("data", "darwin.csv"))

## data tidy ----

glimpse(darwin)                             # check the structure of the data

head(darwin)                                # check data is in a tidy format

colnames(darwin)                            # check variable names

darwin <- janitor::clean_names(darwin)      # clean up column names

darwin %>% 
  duplicated() %>% 
  sum()                                     # check for duplication

darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))    # check for typos - by looking at impossible values

darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)                            # check for typos by looking at distinct characters/values

darwin %>% 
  is.na() %>% 
  sum()                                     # missing values

summary(darwin)                             # quick summary



# VISUALISATION ----

# geom point ----

#darwin %>% 
#  ggplot(aes(x=type,
#             y=height))+
#  geom_point()

# geom boxplot ----

#darwin %>% 
#  ggplot(aes(x=type,
#             y=height))+
#  geom_boxplot()

# geom violin ----

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_violin()

# geom histogram ----

#darwin %>% 
#  ggplot(aes(
              #x=type,
#              y=height
#             ))+
#  geom_histogram()

# COMPARISON ----

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# SUMMARY STATISTICS ----

## summary object ----

darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

## summary plot ----

darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

## kable extra functions ----

# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

# DIFFERENCES ----

# pivots data to wide format then subtract Selfed plant heights from Crossed plant heights

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)

darwin_wide

difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())                      # average difference in height between these groups of plants 
                                        #    and the standard deviation of the difference
difference_summary

## standard error

difference_summary %>% 
  mutate(se= sd/sqrt(n))

# "the average difference in height was 2.62 ± 1.22 inches (mean ± SE)."

# CONFIDENCE INTERVALS ----

lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI

# "The maize plants that have been cross pollinated were 
#     taller on average than the self-pollinated plants, 
#     with a mean difference in height of 
#       2.62 [0.18, 5.06] inches (mean [95% CI])."

# LINEAR MODELS ----

## lsmodel0 ----

lsmodel0 <- lm(formula = height ~ 1, data = darwin)

lsmodel0

### broom ----

broom::tidy(lsmodel0) # overall mean

broom::glance(lsmodel0) 

broom::augment(lsmodel0) 

### comparing means ----

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))

lsmodel1 <- lm(height ~ type, data=darwin)

# note that the following is identical
#     lsmodel1 <- lm(height ~ 1 + type, data=darwin)

broom::tidy(lsmodel1)

## summary ----

summary(lsmodel1)

darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

## confidence intervals ----

broom::tidy(lsmodel1, conf.int=T)

GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

## for 'other' mean ----

darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()

### emmeans ----

means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

means

# CHECKING ASSUMPTIONS ----

## base R ----

performance::check_model(lsmodel1)

## tidyverse ----

#plot(lsmodel1)

## normality check ----

performance::check_model(lsmodel1, check=c("normality","qq"))

plot(lsmodel1, which=c(2,2))

## equal variance check ----

performance::check_model(lsmodel1, check="homogeneity")

plot(lsmodel1, which=c(1,3))

## outliers check ----

performance::check_model(lsmodel1, check="outliers")

plot(lsmodel1, which=c(4,4))

## summary ----

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

# STUDENT'S T-TEST

## base R ----

x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

## tidyverse ----

x <- seq(-4, 4, length=100)
z_dist <- dnorm(x)

values <- tibble(x,z_dist)

t <- map_dfc(degf, ~dt(x, .x))         # map_dfc combines values returned into a dataframe
colnames(t) <- degf

combined <- cbind(values,t)

combined %>% 
  pivot_longer(cols=!x, names_to="distribution") %>% 
  mutate(distribution=factor(distribution, levels=c("z_dist", "1", "3", "8", "30"))) %>%  
  mutate(distribution=fct_recode(distribution, "z distribution" = "z_dist", "df = 1" = "1", "df = 3" = "3", "df = 8" = "8", "df = 30" = "30")) %>% 
  ggplot(aes(x=x, y=value, colour=distribution))+
  geom_line(linetype="solid")+
  theme_classic()

# VALUES FOR CRTICAL AT DF = <30

df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

## summary ----

lsmodel1 <- lm(height ~ type, data = darwin)

summary(lsmodel1)     # base R

broom::tidy(lsmodel1) # tidyverse



tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]]






































