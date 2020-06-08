setwd("C:/Apply Jabar")
install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(tidyr)

covid <-read.csv("C:/Apply Jabar/coronavirus.csv")
head(covid, n=6)
N <- length(covid[,1])
N
Ind <- covid %>%
  filter(country=="Indonesia")

Indconfirmed <- Ind %>%
      filter(type=="confirmed")%>%
      group_by(date)%>%
      summarise(total_cases = sum(cases))

confirmedInd <- sum(Indconfirmed[,7])

## Confirmed cases worldwide
summary_df <- covid %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

head(summary_df, n=20)

covid$date <- as.Date(covid$date)

## Confirmed cases for latest 24 hours
latest <- covid %>% 
  filter(date == max(date)) %>%
  select(country, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed)

head(latest, n=25)

##Group by date
by_date <- covid%>%
  filter(type=="confirmed")%>%
  group_by(date)%>%
  summarise(total_cases = sum(cases))
