pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, xgboost)

train <- fread("train_sample.csv", drop = c("attributed_time"), showProgress=F)

train <- train %>%
  mutate(wday = Weekday(click_time), hour = hour(click_time)) %>% 
  select(-c(click_time)) %>%
  add_count(ip) %>% rename("ip_count" = n) %>%
  add_count(app) %>% rename("app_count" = n) %>%
  add_count(channel) %>% rename("chan_count" = n) %>%
  add_count(device) %>% rename("dev_count" = n) %>%
  add_count(os) %>% rename("os_count" = n) %>%
  add_count(ip, app) %>% rename("ip_app" = n) %>%
  add_count(ip, os) %>% rename("ip_os" = n) %>%
  add_count(ip, device) %>% rename("ip_dev" = n) %>%
  add_count(ip, channel) %>% rename("ip_chan" = n) %>%
  add_count(app, device) %>% rename("app_dev" = n) %>%
  add_count(app, channel) %>% rename("app_chan" = n) %>%
  add_count(app, hour) %>% rename("app_hour" = n) %>%
  add_count(ip, wday, hour) %>% rename("ip_wday_h" = n) %>%
  add_count(ip, hour, channel) %>% rename("ip_h_chan" = n) %>%
  add_count(ip, hour, os) %>% rename("ip_h_os" = n) %>%
  add_count(ip, hour, app) %>% rename("ip_h_app" = n) %>%
  add_count(ip, hour, device) %>% rename("ip_h_dev" = n) %>%
  add_count(ip, hour, device) %>% rename("ip_h_dev" = n) %>%
  add_count(ip, hour, device) %>% rename("ip_h_dev" = n) %>%
  add_count(ip, hour, device) %>% rename("ip_h_dev" = n) %>%
  select(-c(ip))