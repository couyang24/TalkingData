
pacman::p_load(tidyverse, lubridate, caret, DMwR, ROSE, randomForest, rpart, 
               rpart.plot, data.table, e1071, gridExtra)

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
