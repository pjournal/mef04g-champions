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
  transform(change_avg_price=(avg_price-previousday_avg_price)/previousday_avg_price) %>%
  transform(change_per_avg_price=100*change_avg_price) %>%
  select(date,avg_price,previousday_avg_price,change_avg_price,change_per_avg_price,avg_total_value)

std_fund = sd(unlist(fund_day['change_avg_price']), na.rm = TRUE)
mean_fund = mean(unlist(fund_day['avg_price']))

## FUND TYPE
fund_type = df_fund %>% group_by(date, fund_type) %>%
  summarise(avg_price=mean(price), avg_total_value = mean(total_value)) %>%
  group_by(fund_type) %>%
  arrange(fund_type,date) %>%
  mutate(previousday_avg_price=as.numeric(lag(avg_price,n=1))) %>%
  relocate(date,fund_type, avg_price, previousday_avg_price) %>%
  transform(change_avg_price=(avg_price-previousday_avg_price)/previousday_avg_price) %>%
  transform(change_per_avg_price=100*change_avg_price)

summarize_fund_type = fund_type %>% group_by(fund_type) %>%
  summarise(avg_fund_type=mean(avg_price) ,std_fund_type = sd(change_avg_price, na.rm = TRUE))


## CATEGORY
category_df = df_fund %>% group_by(date, fund_type, category) %>%
  summarise(avg_price=mean(price), avg_total_value = mean(total_value)) %>%
  group_by(fund_type, category) %>%
  arrange(fund_type,category, date) %>%
  mutate(previousday_avg_price=as.numeric(lag(avg_price,n=1))) %>%
  relocate(date,fund_type,category, avg_price, previousday_avg_price) %>%
  transform(change_avg_price=(avg_price-previousday_avg_price)/previousday_avg_price) %>%
  transform(change_per_avg_price=100*change_avg_price)

summarize_category = category_df %>% group_by(category) %>%
  summarise(avg_category=mean(avg_price), std_category = sd(change_avg_price,na.rm = TRUE))


## COMPANY CODE
code_df = df_fund %>% group_by(date, code) %>%
  summarise(avg_price=mean(price), avg_total_value = mean(total_value)) %>%
  group_by(code) %>%
  arrange(code, date) %>%
  mutate(previousday_avg_price=as.numeric(lag(avg_price,n=1))) %>%
  relocate(date,code, avg_price, previousday_avg_price) %>%
  transform(change_avg_price=(avg_price-previousday_avg_price)/previousday_avg_price) %>%
  transform(change_per_avg_price=100*change_avg_price)

summarize_code = code_df %>% group_by(code) %>%
  summarise(avg_code=mean(avg_price), std_code = sd(change_avg_price, na.rm = TRUE))


## Mean and STD Graphics For Categories
scatter_category = ggplot(summarize_category, aes(x=avg_category, y=std_category)) +
  geom_point() +
  labs(title = "Avg Fund Price vs Avg Std of Price Change for Categories",x="Average Fund Price", y="Average Std of Daily Change")

scatter_category

# CHANGE IN 1 YEAR
df_2 = df_fund %>%
  filter(date == ymd("2019-11-18") | date == ymd("2020-11-16")) %>%
  select(date,fund_type, category, code, name, total_value, price) %>%
  arrange(code, name, date)


df_3 = df_2 %>%
  group_by(code, name) %>%
  mutate(previous_price=as.numeric(lag(price,n=1)), 
         change_price_percentage=100*(price-as.numeric(lag(price,n=1)))/ as.numeric(lag(price,n=1)),
         previous_total_value = as.numeric(lag(total_value,n=1)),
         change_total_value_percentage=100*(total_value-as.numeric(lag(total_value,n=1)))/ as.numeric(lag(total_value,n=1)))%>%
  filter(date==ymd('2020-11-16')) %>%
  select(date,fund_type, category, code, name, price, previous_price, change_price_percentage, total_value, previous_total_value,change_total_value_percentage )

## BEST 40
order_price_best = df_3 %>%
  arrange(desc(change_price_percentage)) %>%
  select(fund_type, category, code, name, previous_price, price, change_price_percentage)

best_price_funds=order_price_best[1:40, ]

best_fund_type = best_price_funds %>%
  group_by(fund_type) %>%
  count(fund_type)

best_category = best_price_funds %>%
  group_by(category) %>%
  count(category) %>%
  arrange(desc(n))

## WORST 40
order_price_worst = df_3 %>%
  arrange(change_price_percentage) %>%
  select(fund_type, category, code, name, previous_price, price, change_price_percentage)

worst_price_funds=order_price_worst[1:40, ]

worst_fund_type = worst_price_funds %>%
  group_by(fund_type) %>%
  count(fund_type)

worst_category = worst_price_funds %>%
  group_by(category) %>%
  count(category) %>%
  arrange(desc(n))



