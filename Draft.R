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