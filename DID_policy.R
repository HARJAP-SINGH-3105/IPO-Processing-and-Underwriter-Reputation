
library(tidyverse)
library(haven)  # help in import file
library(lfe) 

# Suppose a policy is implemented starting 1994,
#but only in countries 5,6,7
#we want to know what casual effect the policy  on generic outcome y.

panel_raw <- read_dta("http://dss.princeton.edu/training/Panel101.dta")
View(panel_raw)
panel <- panel_raw %>% mutate(country = as.factor(country),year = as.integer(year))
#Descriptive Stats 

count(panel,country)
count (panel,year)

#Graphical Summary

ggplot(panel,aes(x=year,y=y,group =country,color =country))+geom_line()+ geom_point()+scale_color_viridis_d()+geom_vline(xintercept = 1993.5 , linetype="dashed")

#DID 
# Two differeces : Time and Regions
modified_panel <-panel %>% mutate(is_post = (year>=1994),treated_group = (country %in% c(5,6,7)),interact =  is_post*treated_group)
View(modified_panel)
count(modified_panel,is_post,treated_group,interact)

# To Scale the y 
modified_panel<- modified_panel %>% mutate(y = y/1e9)

# Naive approach , just take in account interaction........

felm(y~interact ,modified_panel) %>%summary()

felm(y~interact+is_post + treated_group ,modified_panel) %>%summary()

# So we get a coefficients in Summary which tells the 
#impact on treatment group like here slope of "interact"
#comes out to be" -2.519" which show negative effects after treatment.
# 
