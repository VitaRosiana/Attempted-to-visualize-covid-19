setwd("C:/Apply Jabar")
coronavirus <- read.csv("C:/Apply Jabar/coronavirus.csv")
coronavirus$date <- as.Date(coronavirus$date)

library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(tidyr)


## Daily num of confirmed cases across the world
worldsum <- coronavirus %>% 
  filter(type=="confirmed")%>%
  group_by(country)%>%
  summarise(total_cases=sum(cases))%>%
  arrange(-total_cases)


## Cumulative confirmed cases across time
dailydata <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(date)%>%
  summarise(total_cases = sum(cases)) 

dailycases <- dailydata %>%
  select(date, total_cases)%>%
  summarise(date, cum_cases=cumsum(total_cases))
dailycases$cum_cases <- as.double(dailycases$cum_cases)

ggplot(dailycases, aes(x=date, y=cum_cases)) + geom_line()

## Indonesia againts the countries around the world
sumInd <- coronavirus%>%
  filter(country=="Indonesia", type=="confirmed")%>%
  summarise(total_cases=sum(cases))

names(coronavirus) 
coronavirus$is_ind <- ifelse(coronavirus$country=="Indonesia", "Indonesia", "not Indonesia")

daily_ind_world <- coronavirus%>%
  select(is_ind, date, cases, country, type)%>%
  filter(type=="confirmed")%>%
  group_by(is_ind, date)%>%
  summarise(tot_cases=sum(cases))

tail(daily_ind_world, n=6)

daily_ind_vs_worlds <- daily_ind_world %>%
  select(is_ind, date, tot_cases)%>%
  summarise(is_ind, date, cum_cases=cumsum(tot_cases))

  
 

