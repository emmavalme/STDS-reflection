library(readr)
library(tidyverse)
library(ggplot2)

covid <- read_csv("covid_owid.csv")
age_median <- read_csv("age_median.csv")

#clean dataset------
covid <- covid %>% 
    select(location, total_deaths) %>%
    group_by(location) %>%
    filter(total_deaths == max(total_deaths)) %>%
    arrange(location, total_deaths) %>%
    distinct_all(keep_all= TRUE) %>%
    rename(Place = location)
    
age_median <- age_median %>% 
    select(Place, Median)
    
# Merge the datasets, pick out the worst hit countries death wise
#the countries with more than 2000 deaths, 15 countries
theMerge <- merge(covid,age_median, by="Place")
theMerge <- theMerge %>% 
    subset(total_deaths > 2000)

#linear regression of total deaths and median age

scatter.smooth(x=theMerge$Median, y=theMerge$total_deaths, main="Total deaths covid ~ Median age", xlab = "Median age", ylab="Total Deaths")


    