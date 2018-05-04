pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools)

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


# Split half and half
# pos <- which(train$is_attributed==1)
# neg <- sample(which(train$is_attributed==0),length(pos))
# 
# train_sample <- train[c(pos,neg),]
# rm(neg, pos, train)

# InTrain
train_sample <- train

inTrain <- createDataPartition(train_sample$app, p=.7, list=F)
train_sample <- train_sample %>% lapply(as.numeric) %>% as_data_frame()
train_val <- train_sample[inTrain,]
valid_val <- train_sample[-inTrain,]
rm(inTrain, train_sample)

train_val$is_attributed <- as.factor(train_val$is_attributed)

rf_model <- randomForest(is_attributed~., data=train_val, method = "class")
rf_pred  <- predict(rf_model, newdata = valid_val, type="class")
confusionMatrix(rf_pred,valid_val$is_attributed)
varImpPlot(rf_model)
rm(rf_model,rf_pred)
