# SET UP ----

library(tidyverse)
library(here)
library(kableExtra)

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























