
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, xgboost)

train <- fread("train_sample.csv", drop = c("attributed_time"), showProgress=F)

train <- train %>%
  mutate(wday = Weekday(click_time), hour = hour(click_time)) %>% 
  select(-c(click_time)) %>%
  add_count(ip) %>% rename("ip_count" = n) %>%
  add_count(app) %>% rename("app_count" = n) %>%
  add_count(channel) %>% rename("channel_count" = n) %>%
  add_count(device) %>% rename("device_count" = n) %>%
  add_count(os) %>% rename("os_count" = n) %>%
  add_count(ip, app) %>% rename("ip_app" = n) %>%
  add_count(ip, os) %>% rename("ip_os" = n) %>%
  add_count(ip, device) %>% rename("ip_device" = n) %>%
  add_count(ip, channel) %>% rename("ip_channel" = n) %>%
  add_count(app, device) %>% rename("app_device" = n) %>%
  add_count(app, channel) %>% rename("app_channel" = n) %>%
  add_count(ip, wday, hour) %>% rename("nip_day_h" = n) %>%
  add_count(ip, hour, channel) %>% rename("nip_h_chan" = n) %>%
  add_count(ip, hour, os) %>% rename("nip_h_osr" = n) %>%
  add_count(ip, hour, app) %>% rename("nip_h_app" = n) %>%
  add_count(ip, hour, device) %>% rename("nip_h_dev" = n) %>%
  select(-c(ip))