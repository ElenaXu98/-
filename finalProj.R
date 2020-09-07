library(readr)
library(ggplot2)
library(car)
library(dplyr)
library(readxl)
library(MASS)
play <- read_excel("C:\\Users\\WR\\Desktop\\my XMU\\大三下\\多元统计分析\\project\\play.xlsx")
mytable2 <- read_csv("C:\\Users\\WR\\Desktop\\my XMU\\大三下\\多元统计分析\\project\\mytable2.csv")
for (i in 1:367){
  for (j in 11:29){
    if (mytable2[i,j]==TRUE)
      mytable2[i,j]<-1
    else 
      mytable2[i,j]<-0
  }
}
mytable3<-inner_join(mytable2,play,by="maoyanID")
mytable3<-mytable3[,c(-39,-40,-41,-42)]

write.csv(mytable4,"mytable4")

set.seed(123456)
index <-  sort(sample(nrow(mytable3), 34))
test <- mytable3[index,]
train <-  mytable3[-index,]






#Model1
#normal test for box
#log(box)~normal
y<-rnorm(dim(train)[1])
ks.test(scale(train$box_million),y)
ks.test(scale(train$box_day_million),y)
ks.test(scale(log(train$box_million)),y)
ks.test(scale(log(train$box_day_million)),y)
#log(box) multiple regression with box_day
lmbox1<-lm(log(box_million)~log(box_day_million)+IMAX+spring+labor+national+summer+week_want_thousand+gen_爱情+gen_动画+gen_动作+gen_科幻+gen_惊悚+gen_犯罪+gen_悬疑+gen_纪录片+gen_喜剧+gen_家庭+gen_动作+gen_奇幻+gen_冒险+gen_剧情+gen_恐怖+region_中国大陆+region_中国港澳台+region_美国+region_印度+region_日本+region_其他+director_famous+actor_famous+IP_rewrite+dayplay_thousand,train)
vif(lmbox1)
#no multicollinearity
lmbox1step<-step(lmbox1,direction = "both")
summary(lmbox1step)
#heteroscedasticity
ncvTest(lmbox1step)
spreadLevelPlot(lmbox1step)
#no significant outliers
#cookD cutoff=0,4,hat value cutoff= 0.2
influencePlot(lmbox1step)


box1predict<-exp(predict(lmbox1step,test))
RMSE = function(m, o){sqrt(mean((m - o)^2))}
RMSEbox1<-RMSE(box1predict,test$box_million)




#normal test for online_days
#log(online_days)~normal
ks.test(scale(train$online_days),y)
ks.test(scale(log(train$online_days)),y)
#log(online_days) multiple regression with box_day
lmonline1<-lm(log(online_days)~log(box_day_million)+IMAX+spring+labor+national+summer+week_want_thousand+gen_爱情+gen_动画+gen_动作+gen_科幻+gen_惊悚+gen_犯罪+gen_悬疑+gen_纪录片+gen_喜剧+gen_家庭+gen_动作+gen_奇幻+gen_冒险+gen_剧情+gen_恐怖+region_中国大陆+region_中国港澳台+region_美国+region_印度+region_日本+region_其他+director_famous+actor_famous+IP_rewrite+dayplay_thousand,train)
vif(lmonline1)
#no multicollinearity
lmonline1step<-step(lmonline1,direction = "both")
summary(lmonline1step)
#heteroscedasticity
ncvTest(lmonline1step)
spreadLevelPlot(lmonline1step)
#no significant outliers
#cookD cutoff=0,4,hat value cutoff= 0.2
influencePlot(lmonline1step)

online1predict<-exp(predict(lmonline1step,test))
RMSEonline1<-RMSE(online1predict,test$online_days)



plot(log(train$box_million),log(train$online_days))
cor(log(train$box_million),log(train$online_days))

#multivariate multiple regression logbox,online_days 凉
lmall_all<-lm(cbind(log(box_million),log(online_days))~cbind(log(box_day_million),IMAX,spring,labor,national,summer,week_want_thousand,gen_爱情,gen_动画,gen_动作,gen_科幻,gen_惊悚,gen_犯罪,gen_悬疑,gen_纪录片,gen_喜剧,gen_家庭,gen_动作,gen_奇幻,gen_冒险,gen_剧情,gen_恐怖,region_中国大陆,region_中国港澳台,region_美国,region_印度,region_日本,region_其他,director_famous,actor_famous,IP_rewrite,dayplay_thousand),mytable3)

lmall1<-lm(cbind(log(box_million),log(online_days))~cbind(log(box_day_million),IMAX,spring,national,gen_动画,gen_动作,gen_奇幻,gen_犯罪,gen_喜剧,gen_纪录片,gen_爱情,gen_剧情,gen_恐怖,region_中国大陆,region_印度,region_日本,director_famous),mytable3)
summary(Manova(lmall1))$multivariate.tests
anova(lmall1,lmall_all,test = "Wilks")




#Model2
#log(box) multiple regression without box_day
lmbox2<-lm(log(box_million)~IMAX+spring+labor+national+summer+week_want_thousand+gen_爱情+gen_动画+gen_动作+gen_科幻+gen_惊悚+gen_犯罪+gen_悬疑+gen_纪录片+gen_喜剧+gen_家庭+gen_动作+gen_奇幻+gen_冒险+gen_剧情+gen_恐怖+region_中国大陆+region_中国港澳台+region_美国+region_印度+region_日本+region_其他+director_famous+actor_famous+IP_rewrite+dayplay_thousand,train)
vif(lmbox2)
#no multicollinearity
lmbox2step<-step(lmbox2,direction = "both")
summary(lmbox2step)
#heteroscedasticity
ncvTest(lmbox2step)
spreadLevelPlot(lmbox2step)
#no significant outliers
#cookD cutoff=0,4,hat value cutoff= 0.2
influencePlot(lmbox2step)


box2predict<-exp(predict(lmbox2step,test))
RMSEbox2<-RMSE(box2predict,test$box_million)


#log(online_days) multiple regression without box_day
lmonline2<-lm(log(online_days)~IMAX+spring+labor+national+summer+week_want_thousand+gen_爱情+gen_动画+gen_动作+gen_科幻+gen_惊悚+gen_犯罪+gen_悬疑+gen_纪录片+gen_喜剧+gen_家庭+gen_动作+gen_奇幻+gen_冒险+gen_剧情+gen_恐怖+region_中国大陆+region_中国港澳台+region_美国+region_印度+region_日本+region_其他+director_famous+actor_famous+IP_rewrite+dayplay_thousand,train)
vif(lmonline2)
#no multicollinearity
lmonline2step<-step(lmonline2,direction = "both")
summary(lmonline2step)
#heteroscedasticity
ncvTest(lmonline2step)
spreadLevelPlot(lmonline2step)
#no significant outliers
#cookD cutoff=0,4,hat value cutoff= 0.2
influencePlot(lmonline2step)





#multivariate multiple regression logbox,online_days 凉
lmall_all2<-lm(cbind(log(box_million),log(online_days))~cbind(IMAX,spring,labor,national,summer,week_want_thousand,gen_爱情,gen_动画,gen_动作,gen_科幻,gen_惊悚,gen_犯罪,gen_悬疑,gen_纪录片,gen_喜剧,gen_家庭,gen_动作,gen_奇幻,gen_冒险,gen_剧情,gen_恐怖,region_中国大陆,region_中国港澳台,region_美国,region_印度,region_日本,region_其他,director_famous,actor_famous,IP_rewrite,dayplay_thousand),mytable3)

lmall2<-lm(cbind(log(box_million),log(online_days))~cbind(IMAX,spring,national,summer,week_want_thousand,gen_爱情,gen_动画,gen_犯罪,gen_纪录片,gen_恐怖,gen_动作,gen_悬疑,gen_喜剧,region_中国大陆,region_美国,region_印度,director_famous,actor_famous,IP_rewrite,dayplay_thousand),mytable3)
summary(Manova(lmall2))$multivariate.tests
anova(lmall2,lmall_all2,test = "Wilks")







box1predict<-exp(predict(lmbox1step,test))
RMSE = function(m, o){sqrt(mean((m - o)^2))}
RMSEbox1<-RMSE(box1predict,test$box_million)




















set.seed(17)
require(caret)
folds <- createFolds(y=mytable3[,1],k=10)
library(pROC)
max=0
num=0
auc_value<-as.numeric()
for(i in 1:10){
  fold_test <- mytable3[folds[[i]],]
  fold_train <- mytable3[-folds[[i]],]
  fold_p<-lm(log(box_million)~log(box_day_million)+IMAX+spring+labor+national+summer+week_want_thousand+gen_爱情+gen_动画+gen_动作+gen_科幻+gen_惊悚+gen_犯罪+gen_悬疑+gen_纪录片+gen_喜剧+gen_家庭+gen_动作+gen_奇幻+gen_冒险+gen_剧情+gen_恐怖+region_中国大陆+region_中国港澳台+region_美国+region_印度+region_日本+region_其他+director_famous+actor_famous+IP_rewrite+dayplay_thousand,fold_train)
  fold_pre <- step(fold_p,direction = "both")
  fold_predict <- exp(predict(fold_pre,type='response',newdata=fold_test))
  auc_value<- append(auc_value,as.numeric(auc(fold_test$box_million,fold_predict)))
}
num<-which.max(auc_value)
print(auc_value)

fold_test <- data[folds[[num]],]
fold_train <- data[-folds[[num]],]
fold_pre <- lm(as.numeric(V61)~.,data=fold_train)
fold_predict <- predict(fold_pre,type='response',newdata=fold_test)
roc_curve <- roc(as.numeric(fold_test[,61]),fold_predict)
plot(roc_curve, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE,main="ROC curve for the set with the largest AUC value")
