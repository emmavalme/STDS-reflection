library(tidyverse)
library(skimr)
library(janitor)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(lubridate)
library(ggpubr)


# Read in datasets ---------
covidTimeline  <- read_excel("data/govern_file.xlsx")
covidData <- read_excel("data/covid-data.xlsx")

# Clean dataset ------- 

covidTimeline$date <- as.Date(covidTimeline$date_implemented) 
covidData$date <- as.Date(covidData$date) 


dfCovid <- covidData %>%
  filter(iso_code == "AUS",  total_cases > 0)

covidTimeline$date <- as.Date(covidTimeline$date) 
covidData$date <- as.Date(covidData$date) 


# data seperation --------
restriction = "Social distancing"

df1 <- covidTimeline %>%
  filter(iso_code == "AUS", category == restriction)

df2 <- covidTimeline %>%
  filter(iso_code == "FRA", category == restriction)

df3 <- covidTimeline %>%
  filter(iso_code == "QAT", category == restriction)

# plot total cases with timeline events--------
df = df3
title = df$country
ggplot() + 
  geom_vline(data = df, aes(xintercept = as.Date(date_implemented)), color = "grey", size = 0.5, linetype = "dashed") +
  geom_line(data = dfCovid, aes(as.Date(date), y = total_cases), color = "blue") +
  ggtitle(title) + 
  xlab('Dates') +
  ylab('cases') +
  geom_text_repel(data = df, aes(label= measure, x = as.Date(date_implemented), y = seq(1000,6000,length.out = nrow(df))),
                  direction    = "x",
                  angle        = 90,
                  segment.size = 0.2,
                  size = 4)

