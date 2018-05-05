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
