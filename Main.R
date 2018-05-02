pacman::p_load(GGally,ggthemes,lubridate,caret,tidyverse,e1071,gridExtra,data.table,rpart.plot,randomForest,rpart,ROSE,DMwR,xgboost)

train <-fread('train_sample.csv', stringsAsFactors = FALSE, data.table = FALSE, na.strings=c("NA","NaN","?", ""))

train %>% str()

unique_acc <- train %>% sapply(function(x) length(unique(x))) %>% as.data.frame() %>% rownames_to_column()

colnames(unique_acc) <- c("var","num")

unique_acc %>% ggplot(aes(reorder(var,-num),log(num),fill=var)) + geom_col() + 
  labs(title='Unique Variable Analysis',subtitle="Unique Count in Log Value for Visualization", x = "Variables", 
       y = "unique count (log)", caption="Source: Kaggle TalkingData Challenge") + 
  theme_economist() +
  theme(legend.position="none") +
  geom_label(aes(label=num), col="white")

rm(unique_acc)

# sapply(train,function(x) sum(is.na(x)))
colSums(is.na(train))


train %>% select(is_attributed) %>% table()

train %>% filter(is.na(attributed_time)) %>% select(is_attributed) %>% summary()
train %>% filter(!is.na(attributed_time)) %>% select(is_attributed) %>% summary()

train %>% filter(!attributed_time>click_time)

train$click_time <- ymd_hms(train$click_time)
train$attributed_time <- ymd_hms(train$attributed_time)

attr_train <- train %>% filter(is_attributed==1) %>% mutate(wait_time=difftime(attributed_time,click_time))

# attr_train %>% ggplot(aes(wait_time)) + geom_histogram(bins=100)

# attr_train %>% select(wait_time) %>% sapply(as.numeric) %>% median()

attr_train %>% ggplot(aes(wait_time)) + geom_density(fill='firebrick',alpha=.7,col='firebrick') + 
  geom_vline(xintercept = 300, lwd=1, lty=2, alpha=.6)

attr_train %>% filter(wait_time<500) %>%  ggplot(aes(wait_time)) + geom_density(fill='firebrick',alpha=.7,col='firebrick') + 
  geom_vline(xintercept = 38, lwd=2, lty=2, alpha=.6)


train <- train %>% mutate(wday=weekdays(as.Date(click_time)),hour=hour(click_time))

head(train)












pos <- which(train$is_attributed==1)
neg <- sample(which(train$is_attributed==0),length(pos))

train_sample <- train[c(pos,neg),]

# train_sample %>% ggplot(aes(hour,fill=as.factor(is_attributed)))+geom_bar()
# train_sample %>% ggplot(aes(wday,fill=as.factor(is_attributed)))+geom_bar()

train_sample %>% group_by(hour) %>% summarise(avg_attr=mean(is_attributed),count=n())
train_sample %>% group_by(wday) %>% summarise(avg_attr=mean(is_attributed))

## Visualize Discrete Single Variables
vis_bar<-function(dataset,variable){
  
  dataset$variable<-dataset[[variable]]
  
  (g1<-dataset %>% filter(!is.na(is_attributed)) %>% 
      ggplot(aes(variable,fill=as.factor(is_attributed)))+geom_bar(position = "stack")+
      theme_economist()+labs(fill="is_attributed",x=variable))
  
  (g2<-dataset %>% filter(!is.na(is_attributed)) %>% 
      ggplot(aes(variable,fill=as.factor(is_attributed)))+geom_bar(position = "fill")+
      theme_economist()+labs(fill="is_attributed",x=variable))
  
  (g3<-dataset %>% filter(!is.na(is_attributed)) %>% 
      ggplot(aes(variable,fill=as.factor(is_attributed)))+geom_bar(position = "dodge")+
      theme_economist()+labs(fill="is_attributed",x=variable))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable," vs is_attributed \n multi_views"),size=5)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}

## Visualize Discrete Multiple Variables
vis_bar_multi<-function(dataset,variable1,variable2){
  
  dataset$variable1<-dataset[[variable1]]
  dataset$variable2<-dataset[[variable2]]
  
  (g1<-dataset %>% filter(!is.na(is_attributed)) %>% 
      ggplot(aes(variable1,fill=as.factor(is_attributed)))+geom_bar(position = "stack")+
      theme_economist()+facet_grid(.~variable2)+labs(fill="is_attributed",x=variable1))
  
  (g2<-dataset %>% filter(!is.na(is_attributed)) %>% 
      ggplot(aes(variable1,fill=as.factor(is_attributed)))+geom_bar(position = "fill")+
      theme_economist()+facet_grid(.~variable2)+labs(fill="is_attributed",x=variable1))
  
  (g3<-dataset %>% filter(!is.na(is_attributed)) %>% 
      ggplot(aes(variable1,fill=as.factor(is_attributed)))+geom_bar(position = "dodge")+
      theme_economist()+facet_grid(.~variable2)+labs(fill="is_attributed",x=variable1))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable1," vs ",variable2," vs is_attributed \n multi_views"),size=5)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}

vis_bar(train_sample,'hour')

vis_bar(train_sample,'wday')

vis_bar_multi(train_sample,'hour','wday')

train_sample$is_attributed <- train_sample$is_attributed %>% as.factor()

ggpairs(train_sample[,c('ip','app','device','os','channel',"wday",'hour',"is_attributed")],
        aes(color=is_attributed,alpha=.5))

