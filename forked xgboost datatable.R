library(data.table)
library(xgboost)
library(fasttime)

#---------------------------
cat("Loading data...\n")
train <- fread("train.csv", drop = c("attributed_time"), showProgress=F)[(.N - 50e6):.N] 
test <- fread("../input/test.csv", drop = c("click_id"), showProgress=F)

set.seed(0)
train <- train[sample(.N, 30e6), ]
train <-fread('train_sample.csv', stringsAsFactors = FALSE, drop = c("attributed_time"), data.table = T, 
              na.strings=c("NA","NaN","?", ""))


#---------------------------
cat("Preprocessing...\n")
y <- train$is_attributed
tri <- 1:nrow(train)
tr_te <- rbind(train, test, fill = T)

rm(train, test); gc()

train[, `:=`(hour = hour(click_time),
             min = minute(click_time),
             sec = second(click_time),
             click_time = fastPOSIXct(click_time))
      ][, next_clk := as.integer(click_time - shift(click_time))
        ][, click_time := NULL
          ][, ip_f := .N, by = "ip"
            ][, app_f := .N, by = "app"
              ][, channel_f := .N, by = "channel"
                ][, device_f := .N, by = "device"
                  ][, os_f := .N, by = "os"
                    ][, app_f := .N, by = "app"
                      ][, ip_app_f := .N, by = "ip,app"
                        ][, ip_dev_f := .N, by = "ip,device"
                          ][, ip_os_f := .N, by = "ip,os"
                            ][, ip_chan_f := .N, by = "ip,channel"
                              ][, c("ip", "is_attributed") := NULL]


train %>% summary()
train %>% dim()
train %>% glimpse()
train %>% group_by(device) %>% summarise(count=n())



