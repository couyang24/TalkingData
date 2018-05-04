library(data.table)
library(xgboost)
library(fasttime)
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools)

train <- fread("train_sample.csv", drop = c("attributed_time"), showProgress=F)

train <- train %>%
  mutate(wday = Weekday(click_time), hour = hour(click_time)) %>% 
  select(-c(click_time)) %>%
  add_count(ip, wday, hour) %>% rename("nip_day_h" = n) %>%
  add_count(ip, hour, channel) %>% rename("nip_h_chan" = n) %>%
  add_count(ip, hour, os) %>% rename("nip_h_osr" = n) %>%
  add_count(ip, hour, app) %>% rename("nip_h_app" = n) %>%
  add_count(ip, hour, device) %>% rename("nip_h_dev" = n) %>%
  select(-c(ip))

# Split half and half
pos <- which(train$is_attributed==1)
neg <- sample(which(train$is_attributed==0),length(pos))

train_sample <- train[c(pos,neg),]
rm(neg, pos, train)

# InTrain
inTrain <- createDataPartition(train_sample$app, p=.7, list=F)
train_sample <- train_sample %>% lapply(as.numeric) %>% as_data_frame()
y_train <- train_sample$is_attributed[inTrain]
y_valid <- train_sample$is_attributed[-inTrain]
# train_sample$is_attributed <- NULL
train_val <- train_sample[inTrain,]
valid_val <- train_sample[-inTrain,]

rm(inTrain, train_sample)

dtrain <- xgb.DMatrix(data = data.matrix(train_val), label = y_train)
dval <- xgb.DMatrix(data = data.matrix(valid_val), label = y_valid)

rm(); gc()

params <- list(objective = "binary:logistic",
               booster = "gbtree",
               eval_metric = "auc",
               nthread = 7,
               eta = 0.05,
               max_depth = 10,
               gamma = 0.9,
               subsample = 0.8,
               colsample_bytree = 0.8,
               scale_pos_weight = 50,
               nrounds = 100)

myxgb_model <- xgb.train(params, dtrain, params$nrounds, list(val = dval), print_every_n = 20, early_stopping_rounds = 50)


(xgb_pred <- predict(myxgb_model,newdata = dval))

xgb_pred1 <- if_else(xgb_pred>=.5,1,0)

confusionMatrix(y_valid,xgb_pred1)

(imp <- xgb.importance(colnames(train_val), model=myxgb_model))

xgb.plot.importance(imp, top_n = 30)






