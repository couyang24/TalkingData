require(pacman)
pacman::p_load(GGally, ggthemes,lubridate, caret, tidyverse,
               e1071, gridExtra, data.table, rpart.plot, 
               randomForest, rpart, ROSE, DMwR, xgboost)

train <-fread('train_sample.csv', stringsAsFactors = FALSE, data.table = FALSE, 
              na.strings=c("NA","NaN","?", ""))

train <- train %>% mutate(click_time = ymd_hms(click_time),
                          attributed_time = ymd_hms(attributed_time),
                          weekdays = as.factor(weekdays(click_time)),
                          hour    = hour(click_time)+1,
                          click_time = NULL,
                          attributed_time = NULL) %>% 
  lapply(as.numeric) %>% as_data_frame()

train %>% str()

pos <- which(train$is_attributed==1)
neg <- sample(which(train$is_attributed==0),length(pos))

train_sample <- train[c(pos,neg),]
rm(neg, pos, train)

# InTrain
inTrain <- createDataPartition(train_sample$is_attributed, p=.7, list=F)
train_val <- train_sample[inTrain,]
valid_val <- train_sample[-inTrain,]
rm(inTrain, train_sample)


# xgboost

(dtrain1 <- train_val[, colnames(train_val) != "is_attributed"])

dtrain <- xgb.DMatrix(as.matrix(dtrain1), 
                      label = train_val$is_attributed)

dvalid <- xgb.DMatrix(as.matrix(valid_val[, colnames(valid_val) != "is_attributed"]), 
                      label = valid_val$is_attributed)

params <- list( objective   = "binary:logistic", 
                grow_policy = "lossguide",
                tree_method = "hist",
                eval_metric = "auc", 
                max_leaves  = 5, 
                max_delta_step = 7,
                scale_pos_weight = 9.7,
                eta = 0.1, 
                max_depth = 3, 
                subsample = 0.9, 
                min_child_weight = 0,
                colsample_bytree = 0.7, 
                random_state = 84
)

xgb.model <- xgb.train(data = dtrain, params = params, 
                       silent = 1, watchlist = list(valid = dvalid), nthread = 4, 
                       nrounds = 2000, print_every_n = 100, early_stopping_rounds = 50)
