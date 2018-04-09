library(pacman)
p_load(xgboost,tidyverse,data.table)

train_sample <- fread("train_sample.csv", 
               select =c("ip", "app", "device", "os", "channel", "click_time", "is_attributed"),
               colClasses=c("ip"="numeric","app"="numeric","device"="numeric",
                            "os"="numeric","channel"="numeric","click_time"="character",
                            "is_attributed"="numeric"))
test <- fread("test.csv", 
              select =c("ip", "app", "device", "os", "channel", "click_time"),
              showProgress=F,
              colClasses=c("ip"="numeric","app"="numeric","device"="numeric",
                           "os"="numeric","channel"="numeric",
                           "click_time"="character"))


train_sample %>% str()

train_sample$click_time <- train_sample$click_time %>% as.Date()

train_sample$click_time %>% summary()

train_sample %>% filter(is_attributed==1) %>% count()
train_sample %>% filter(is_attributed==0) %>% count()

val <- which(grepl("11-09", train_sample$click_time) == T)
val <- sample(val, 2 * 10 ^ 2)

pos <- which(train_sample$is_attributed == 1)
pos <- setdiff(pos, val)

neg <- which(train_sample$is_attributed == 0)
neg <- setdiff(neg, val)
neg <- sample(neg, length(pos))

under <- train_sample[c(pos, neg), ]
valid <- train_sample[val, ]

rm(train_sample)
gc(reset = T)

str(under)

model_train <- xgb.DMatrix(data=data.matrix(under[,-7]),label=under$is_attributed)
model_val <- xgb.DMatrix(data=data.matrix(valid[,-7]))

invisible(gc())

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

myxgb_model <- xgb.train(params, model_train, params$nrounds, list(val = model_val), print_every_n = 20, early_stopping_rounds = 50)



param <- list(max_depth = 2,
              eta = 1,
              objective = "binary:logistic",
              eval_metric = "auc")

bst <- xgb.train(param,
                 model_train,
                 nrounds = 100,
                 print_every_n = 10)

