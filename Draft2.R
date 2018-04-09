pacman::p_load(GGally,ggthemes,lubridate,caret,tidyverse,e1071,gridExtra,data.table,rpart.plot,randomForest,rpart,ROSE,DMwR,xgboost)

train <-fread('train_sample.csv', stringsAsFactors = FALSE, data.table = FALSE, na.strings=c("NA","NaN","?", ""))

train %>% str()

sapply(train,function(x) sum(is.na(x)))
colSums(is.na(train))

train %>% filter(is.na(attributed_time)) %>% select(is_attributed) %>% summary()
train %>% filter(!is.na(attributed_time)) %>% select(is_attributed) %>% summary()

summary(train$click_time %>% as.Date())

train <- train %>% mutate(wday=weekdays(as.Date(click_time)),hour=hour(click_time))

head(train)

pos <- which(train$is_attributed==1)
neg <- sample(which(train$is_attributed==0),length(pos))

train_sample <- train[c(pos,neg),]

train_sample %>% ggplot(aes(hour,fill=as.factor(is_attributed)))+geom_bar()
train_sample %>% ggplot(aes(wday,fill=as.factor(is_attributed)))+geom_bar()

train_sample %>% group_by(hour) %>% summarise(avg_attr=mean(is_attributed),count=n()) %>% View()
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

ggplot(train,aes(x=wday,fill=is_attributed))+geom_density(col=NA,alpha=0.35)+
  ggtitle("days v/s click")+
  xlab("Day of a week v/s Is_attributed ") +
  ylab("Total Count") +
  labs(fill = "is_attributed")
