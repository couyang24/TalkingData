pacman::p_load(lubridate,caret,tidyverse,e1071,gridExtra,data.table,rpart.plot,randomForest,rpart,ROSE,DMwR,xgboost)

train <-fread('train_sample.csv', stringsAsFactors = FALSE, data.table = FALSE, na.strings=c("NA","NaN","?", ""))

train %>% str()

sapply(train,function(x) sum(is.na(x)))
colSums(is.na(train))

train %>% filter(is.na(attributed_time)) %>% select(is_attributed) %>% summary()
train %>% filter(!is.na(attributed_time)) %>% select(is_attributed) %>% summary()

summary(train$click_time %>% as.Date())

train <- train %>% mutate(wday=weekdays(as.Date(click_time)),hour=hour(click_time))

head(train)

pos <- which(train$is_attributed==1)
neg <- sample(which(train$is_attributed==0),length(pos))

train_sample <- train[c(pos,neg),]

train_sample %>% ggplot(aes(hour,fill=as.factor(is_attributed)))+geom_bar()
train_sample %>% ggplot(aes(wday,fill=as.factor(is_attributed)))+geom_bar()

train_sample %>% group_by(hour) %>% summarise(avg_attr=mean(is_attributed),count=n()) %>% View()
train_sample %>% group_by(wday) %>% summarise(avg_attr=mean(is_attributed))
