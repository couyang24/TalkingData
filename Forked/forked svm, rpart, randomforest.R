pacman::p_load(tidyverse, lubridate, caret, DMwR, ROSE, randomForest, rpart, 
               rpart.plot, data.table, e1071, gridExtra, ggthemes, xgboost)


train <-fread('train_sample.csv', stringsAsFactors = FALSE, data.table = FALSE, 
              na.strings=c("NA","NaN","?", ""))
# test <-fread('../input/test.csv', stringsAsFactors = FALSE, data.table = FALSE)

unique_acc <- train %>% sapply(function(x) length(unique(x))) %>% as.data.frame() %>% rownames_to_column()

colnames(unique_acc) <- c("var","num")

unique_acc %>% ggplot(aes(reorder(var,-num),log(num),fill=var)) + geom_col() + 
  labs(title='Unique Variable Analysis',subtitle="Unique Count in Log Value for Visualization", x = "Variables", 
       y = "unique count (log)", caption="Source: Kaggle TalkingData Challenge") + 
  theme_economist() +
  theme(legend.position="none") +
  geom_label(aes(label=num), col="white")

rm(unique_acc)

glimpse(train)

sapply(train, function(x) sum(is.na(x)))

train %>% select(is_attributed) %>% table()

train %>% filter(is.na(attributed_time)) %>% select(is_attributed) %>% summary()
train %>% filter(!is.na(attributed_time)) %>% select(is_attributed) %>% summary()

train %>% filter(!attributed_time>click_time)

train <- train %>% mutate(click_time = ymd_hms(click_time),
                          attributed_time = ymd_hms(attributed_time))

train %>% summary()

train <- train %>% mutate(weekdays = as.factor(weekdays(click_time)),
                          hour    = hour(click_time)+1)

train %>% head()

# Split half and half
pos <- which(train$is_attributed==1)
neg <- sample(which(train$is_attributed==0),length(pos))

train_sample <- train[c(pos,neg),]
rm(neg, pos)


train_sample$click_time <- NULL
train_sample$attributed_time <- NULL


# InTrain
inTrain <- createDataPartition(train_sample$ip, p=.7, list=F)
train_val <- train_sample[inTrain,]
valid_val <- train_sample[-inTrain,]
rm(inTrain)

train_val$attributed_time <- NULL
train_val$is_attributed <- as.factor(train_val$is_attributed)


# rpart
rpart_model <- rpart(is_attributed~., data=train_val, method = "class")
rpart.plot(rpart_model,extra =  3,fallen.leaves = T)
rpart_pred  <- predict(rpart_model, newdata = valid_val, type="class")
rpart_pred %>% table()

confusionMatrix(valid_val$is_attributed, rpart_pred)
rm(rpart_model,rpart_pred)


# randomForest
rf_model <- randomForest(is_attributed~., data=train_val, method = "class")
rf_pred  <- predict(rf_model, newdata = valid_val, type="class")
confusionMatrix(valid_val$is_attributed, rf_pred)
varImpPlot(rf_model)
rm(rf_model,rf_pred)

# xgboost
