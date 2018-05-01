if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, pryr, caTools, tidyverse, caret, data.table, lubridate, tictoc, DescTools, xgboost)
set.seed(84)               
options(scipen = 9999, warn = -1, digits= 4)

train_path <- "../input/train.csv"
test_path  <- "../input/test.csv"

vars <- c("ip", "app", "device", "os", "channel", "click_time", "attributed_time", "is_attributed") 
most_freq_hours_in_test_data <- c("4","5","9","10","13","14")
least_freq_hours_in_test_data <- c("6","11","15")

cat("memory in use"); mem_used()
cat("--------------------------------", "\n")

#*****************************************************************
#Feature engineering function
add_features <- function(df) {
  cat("Addidng user history features..", "\n")
  df <- as.data.table(df)
  df[, UsrappRank:=1:.N, by=list(ip,app,device,os)]
  df[, UsrCount:=.N, by=list(ip,device,os)]
  cat("Addidng freq count features..", "\n")
  df <- df %>% 
    mutate(hour = hour(click_time),
           in_test_hh = ifelse(hour %in% most_freq_hours_in_test_data, 1,
                               ifelse(hour %in% least_freq_hours_in_test_data, 3, 2))) %>%
    add_count(ip, in_test_hh) %>% rename("nip_day_test_hh" = n) %>%
    select(-c(in_test_hh, click_time)) 
  cat("Addidng ip interaction + hour features..", "\n")
  df <- as.data.table(df)
  df[, nipos:=.N, by=list(ip, hour, os)]
  df[, nipapp:=.N, by=list(ip, hour, app)]
  df[, nipdev:=.N, by=list(ip, hour, device)]
  df[, napp:=.N, by=list(app, hour)]
  df <- df %>% select(-c(ip)) 
  return(as.data.frame(df))
}

#*****************************************************************
print ("reading training data...")

total_rows <- 184903890
chunk_rows <- 40000000
skiprows <- total_rows - chunk_rows 

train <- fread(train_path, skip=skiprows, nrows=chunk_rows, colClasses=list(numeric=1:5), 
               showProgress = FALSE, col.names = vars) %>% 
  select(-c(attributed_time))

#*****************************************************************
print("creating validation partition...")

# split
train.index <- createDataPartition(train$is_attributed, p = 0.9, list = FALSE)
dtrain <- train[ train.index,]
dvalid <- train[-train.index,]

rm(train, valid, train.index)
invisible(gc())

print("Table of class unbalance for scale_pos_weight...")
table(dtrain$is_attributed)
cat("--------------------------------", "\n")

print("data preparation...")
tic("Total processing time for train data --->")
dtrain <- add_features(dtrain)
dim(dtrain)
print(object.size(dtrain), units = "auto")
print("Converting train to xgb.DMatrix")
dtrain <- xgb.DMatrix(as.matrix(dtrain[, colnames(dtrain) != "is_attributed"]), 
                      label = dtrain$is_attributed)
toc()
invisible(gc())
cat("--------------------------------", "\n")

tic("Total processing time for validation data --->")
dvalid <- add_features(dvalid)
dim(dvalid)
print(object.size(dvalid), units = "auto")
print("Converting valid to xgb.DMatrix")
dvalid <- xgb.DMatrix(as.matrix(dvalid[, colnames(dvalid) != "is_attributed"]), 
                      label = dvalid$is_attributed)
toc()
invisible(gc())
cat("--------------------------------", "\n")

#*****************************************************************
# Set params and run XGBoost model
print("Modelling with XGBoost Histogram Opitimized")
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

tic("Total time for model training --->")
xgb.model <- xgb.train(data = dtrain, params = params, 
                       silent = 1, watchlist = list(valid = dvalid), nthread = 4, 
                       nrounds = 2000, print_every_n = 100, early_stopping_rounds = 50)

rm(dtrain, dvalid)
invisible(gc())
toc()

cat("Best score: ", xgb.model$best_score, "\n") 
cat("Best iteration: ", xgb.model$best_ntreelimit, "\n") 
cat("memory in use before predictions: "); mem_used()
cat("--------------------------------", "\n")
gc()
#*****************************************************************
print("read and process test data:")

tic("Total processing time for test data --->")
test <- fread(test_path, colClasses=list(numeric=2:6), showProgress = FALSE) 
sub <- data.table(click_id = test$click_id, is_attributed = NA) 
test$click_id <- NULL
invisible(gc())

test <- add_features(test) # add features
dim(test)
print("test file size: ")
print(object.size(test), units = "auto")
print("Converting to xgb.DMatrix")
test  <- xgb.DMatrix(as.matrix(test[, colnames(test)]))
toc()
invisible(gc())
cat("--------------------------------", "\n")

#*****************************************************************
print("Predictions")
preds <- predict(xgb.model, newdata = test, ntreelimit = xgb.model$best_ntreelimit)
rm(test)
invisible(gc())

preds <- as.data.frame(preds)
sub$is_attributed = round(preds, 5)
fwrite(sub, "sub_xgb_hist_R_40m.csv")
head(sub,15)
cat("--------------------------------", "\n")

#*****************************************************************
# Performance evaluation

print("Feature importance")
kable(xgb.importance(model = xgb.model))
rm(preds, xgb.model)
cat("--------------------------------", "\n")

#*****************************************************************
print("Closer look at predictions...")
Freq(sub$is_attributed)

print("finished...")