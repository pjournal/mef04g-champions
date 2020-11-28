## Libraries
library(tidyverse)
library(lubridate)
library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2movies)
library(jsonlite)



setwd("C:/Users/ahmet/Desktop/MEF_BDA/BDA-503/Project")

## df_all <- readRDS("df_all.rds")
df_clean_all = readRDS("df_clean_2.1.rds")

df_fund = df_clean_all %>% 
  mutate(year=lubridate::year(date), month=lubridate::month(date)) %>%
  filter(ymd(date)>'2019-11-15') %>%
  select(date, year, month, code, fund_type, category, name, price, shares, people, total_value)

## GENERAL
fund_day = df_fund %>% group_by(date) %>%
  summarise(avg_price=mean(price), avg_total_value = mean(total_value)) %>%
  arrange(date) %>%
  transform(previousday_avg_price=as.numeric(lag(avg_price,1))) %>%
  transform(change_avg_price=replace_na((avg_price-previousday_avg_price)/previousday_avg_price,0)) %>%
  transform(change_per_avg_price=100*change_avg_price) %>%
  select(date,avg_price,previousday_avg_price,change_avg_price,change_per_avg_price,avg_total_value)

std_fund = sd(unlist(fund_day['change_avg_price']))
mean_fund = mean(unlist(fund_day['avg_price']))

## FUND TYPE
fund_type = df_fund %>% group_by(date, fund_type) %>%
  summarise(avg_price=mean(price), avg_total_value = mean(total_value)) %>%
  group_by(fund_type) %>%
  arrange(fund_type,date) %>%
  mutate(previousday_avg_price=as.numeric(lag(avg_price,n=1))) %>%
  relocate(date,fund_type, avg_price, previousday_avg_price) %>%
  transform(change_avg_price=replace_na((avg_price-previousday_avg_price)/previousday_avg_price,0)) %>%
  transform(change_per_avg_price=100*change_avg_price)

summarize_fund_type = fund_type %>% group_by(fund_type) %>%
  summarise(avg_fund_type=mean(avg_price) ,std_fund_type = sd(change_avg_price))


## CATEGORY
category_df = df_fund %>% group_by(date, fund_type, category) %>%
  summarise(avg_price=mean(price), avg_total_value = mean(total_value)) %>%
  group_by(fund_type, category) %>%
  arrange(fund_type,category, date) %>%
  mutate(previousday_avg_price=as.numeric(lag(avg_price,n=1))) %>%
  relocate(date,fund_type,category, avg_price, previousday_avg_price) %>%
  transform(change_avg_price=replace_na((avg_price-previousday_avg_price)/previousday_avg_price,0)) %>%
  transform(change_per_avg_price=100*change_avg_price)

summarize_category = category_df %>% group_by(category) %>%
  summarise(avg_category=mean(avg_price), std_category = sd(change_avg_price))


## COMPANY CODE
code_df = df_fund %>% group_by(date, code) %>%
  summarise(avg_price=mean(price), avg_total_value = mean(total_value)) %>%
  group_by(code) %>%
  arrange(code, date) %>%
  mutate(previousday_avg_price=as.numeric(lag(avg_price,n=1))) %>%
  relocate(date,code, avg_price, previousday_avg_price) %>%
  transform(change_avg_price=replace_na((avg_price-previousday_avg_price)/previousday_avg_price,0)) %>%
  transform(change_per_avg_price=100*change_avg_price)

summarize_code = code_df %>% group_by(code) %>%
  summarise(avg_code=mean(avg_price), std_code = sd(change_avg_price))





