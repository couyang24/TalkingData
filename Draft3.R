improve2 <- fread("improve2.csv")
wordbatch_fm_ftrl <- fread("wordbatch_fm_ftrl.csv")

library(tidyverse)
improve2 %>% str()

ensemble <- improve2 %>% left_join(wordbatch_fm_ftrl,by='click_id')

ensembling_submission <- ensemble %>% mutate(is_attributed=(is_attributed.x*.973+is_attributed.y*.9712)/(.9730+.9712)) %>% select(click_id,is_attributed)
write.csv(ensembling_submission,"ensembling_submission.csv",row.names = F)

library(data.table)
library(ggplot2)
library(DT)
library(magrittr)
library(corrplot)
library(Rmisc)
library(ggalluvial)
library(caret)
library(ModelMetrics)
require(scales)
library(irlba)
library(forcats)
library(forecast)
library(TSA)
library(zoo)

