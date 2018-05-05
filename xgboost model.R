# # Split half and half
# pos <- which(train$is_attributed==1)
# neg <- sample(which(train$is_attributed==0),length(pos))
# 
# train_sample <- train[c(pos,neg),]
# rm(neg, pos, train)

# InTrain
train_sample <- train

inTrain <- createDataPartition(train_sample$app, p=.7, list=F)
train_sample <- train_sample %>% lapply(as.numeric) %>% as_data_frame()
y_train <- train_sample$is_attributed[inTrain]
y_valid <- train_sample$is_attributed[-inTrain]
train_sample$is_attributed <- NULL
train_val <- train_sample[inTrain,]
valid_val <- train_sample[-inTrain,]

rm(inTrain, train_sample)

dtrain <- xgb.DMatrix(data = data.matrix(train_val), label = y_train)
dval <- xgb.DMatrix(data = data.matrix(valid_val), label = y_valid)

invisible(gc())



# xgboost 1
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
               nrounds = 2000)

xgb_model <- xgb.train(params, dtrain, params$nrounds, list(val = dval), 
                       print_every_n = 100, early_stopping_rounds = 300)
rm(params)

xgb_pred <- predict(xgb_model,newdata = dval)

xgb_pred_result <- if_else(xgb_pred>=.5,1,0)

confusionMatrix(xgb_pred_result,y_valid)

imp <- xgb.importance(colnames(train_val), model=xgb_model)

imp %>% kable()

xgb.plot.importance(imp, top_n = 30)

# xgboost 2
params <- list( objective   = "binary:logistic", 
                grow_policy = "lossguide",
                # tree_method = "hist",
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

xgb_model <- xgb.train(data = dtrain, params = params, 
                       silent = 1, watchlist = list(valid = dval), nthread = 4, 
                       nrounds = 2000, print_every_n = 100, early_stopping_rounds = 300)

xgb_pred <- predict(xgb_model,newdata = dval)

xgb_pred_result <- if_else(xgb_pred>=.5,1,0)

confusionMatrix(xgb_pred_result,y_valid)

imp <- xgb.importance(colnames(train_val), model=xgb_model)

imp %>% kable()

xgb.plot.importance(imp, top_n = 30)


xgb_pred = predict(xgb_model, test)
sub <- fread("../input/sample_submission.csv")
sub[, is_attributed := xgb_pred]
fwrite(sub, "xgb.csv")



