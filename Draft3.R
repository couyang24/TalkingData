if (!require(pacman)) install.packages(pacman)
pacman::p_load(data.table, tidyverse)
result1 <- fread("submission_final.csv")
result2 <- fread("sub_it6.csv")
result3 <- fread("sub_it7.csv")


ensemble <- result1 %>% left_join(result2,by='click_id') %>% left_join(result3, by="click_id")


ensemble %>% str()
result1 %>% str()
result2 %>% str()
result3 %>% str()


ensembling_submission <- ensemble %>% mutate(is_attributed=(is_attributed.x*.34+is_attributed.y*.33+is_attributed*.33)) %>% select(click_id,is_attributed)
fwrite(ensembling_submission,"ensembling_submission.csv",row.names = F)

ensembling_submission %>% head()