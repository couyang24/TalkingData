if (!require(pacman)) install.packages(pacman)
pacman::p_load(data.table, tidyverse)
result1 <- fread("my_lgb_sub_0.9772.csv")
result2 <- fread("sub_it4 (1).csv")
result3 <- fread("lgb_Usrnewness.csv")


ensemble <- result1 %>% left_join(result2,by='click_id') %>% left_join(result3, by="click_id")


ensemble %>% str()
result1 %>% str()
result2 %>% str()
result3 %>% str()


ensembling_submission <- ensemble %>% mutate(is_attributed=(is_attributed.x*.25+is_attributed.y*.5+is_attributed*.25)) %>% select(click_id,is_attributed)
write.csv(ensembling_submission,"ensembling_submission.csv",row.names = F)

