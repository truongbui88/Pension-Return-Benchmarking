rm(list = ls())
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)


# Import pension data
pension_data_all <- read.csv("ppd-data-latest-7.csv")

# Fix plans' full names
pension_data_all <- pension_data_all %>% 
  mutate(PlanFullName = gsub("\x92", "'", PlanFullName),
         PlanName = gsub("\x92", "'", PlanName)) 

# Filter some key variables
pension_data <- pension_data_all %>% 
  filter(fy > 2000) %>% 
  select(PlanName, PlanFullName, fy, fye, ActLiabilities_GASB, InvestmentReturn_1yr) %>% 
  mutate(fye = ymd(fye),
         month = month(fye))


#Get index prices
symbols <- c("SWTSX", "VBMFX")

prices <- getSymbols(symbols,
                     src = "yahoo",
                     from = "1998-12-31",
                     to = "2020-12-31",
                     auto.assign = T,
                     warnings = F) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  `colnames<-`(symbols)

#Get yearly prices for months available in pension data only
prices_monthly <- to.monthly(prices, OHLC = F)
unique_month <- unique((pension_data$month))
unique_month <- unique_month[!is.na(unique_month)]
prices_yearly <- prices_monthly[month(index(prices_monthly)) %in% unique_month]


# Calculate annual returns for the individual securities
stock_returns <- prices_yearly %>%
  data.frame(date = index(.)) %>%
  remove_rownames() %>%
  mutate(fy = year(date), month = month(date)) %>% 
  pivot_longer(cols = 1:2, names_to = "security", values_to = "prices") %>%
  arrange(security, month, fy) %>% 
  group_by(security, month) %>%
  mutate(returns = prices/lag(prices) - 1) %>%
  select(-prices, -date) %>%
  pivot_wider(names_from = security, values_from = returns) %>% 
  ungroup()

#Join pension data with stock returns
pension_data <- pension_data %>% 
  left_join(stock_returns, by = c("fy", "month"))

write.csv(pension_data, "pension_data.csv")


