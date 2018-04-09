if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, data.table, lubridate, zoo, DescTools, lightgbm)
set.seed(84)               
options(scipen = 9999, warn = -1, digits= 4)


train_sample <- read_csv("C:/Users/oouyang/Projects/TalkingData/train_sample.csv")
train_sample %>% summary()

train_col_names %>% train_sample %>% colnames()
