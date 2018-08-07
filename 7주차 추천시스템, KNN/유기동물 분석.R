setwd("C:/Users/USER/Desktop/투빅스/7주차 추천")



a = read.csv('train.csv')

b = read.csv("test_nontarget.csv")



### 0. 데이터 전처리

my_data  = function(data){



  data = na.omit(data)  

  if("target" %in% colnames(data)){

    data = data[,c(-1,-2,-9,-10,-11,-12,-16)]

  }

  else{

    data_1 = data[,1:14]

    data_2 = data[,15:29]

    target = rep(0,nrow(data))

    data = cbind(data_1,target,data_2)

    data = data[,c(-1,-2,-9,-10,-11,-12,-16)]

  }

  # shelter

  shelter = character(0)

  shelter[data$shelter %in% c("강원","경기","대전","서울","세종","인천")] = "중부"

  shelter[is.na(shelter)] = "남부"

  data$shelter = as.factor(shelter)

  

  # color : 완전갈색or완전흰색 wb 

  color = character(0)

  color[data$color %in% c("white","brown")] = "wb"

  color[is.na(color)] = "nwb"

  data$color = as.factor(color)

  

  # year

  year = character(0)

  year[data$year<5] = "5개월미만"

  year[data$year>=5 & data$year<25] = "25개월미만"

  year[data$year>=25 & data$year<50] = "50개월미만"

  year[data$year>=50] = "50개월이상"

  data$year = as.factor(year)

  

  # data_site_year

  data$data_site_year = as.factor(data$data_site_year)



  # data_site_month

  month = character(0)

  month[data$data_site_month>=3 & data$data_site_month<6] = "봄"

  month[data$data_site_month>=6 & data$data_site_month<9] = "여름"

  month[data$data_site_month>=9 & data$data_site_month<12] = "가을"

  month[is.na(month)]="겨울"

  data$data_site_month = as.factor(month)

  

  # kind

  kind = character(0)

  kind[data$kind %in% c("non","잡종","바셋하운드","달마시안","샤페이","폭스테리어")] = "비주류"

  kind[is.na(kind)] = "주류"

  data$kind = as.factor(kind)

  

  # kg

  kg = ifelse(data$kg>4,0,1)

  data$kg = as.factor(kg)

  

  # factor

  data[,12:23] = lapply(data[,12:23],as.factor)

  data[,c(5,9)] = lapply(data[,c(5,9)], as.factor)

  

  return(data)

}



a = my_data(a)

b = my_data(b)

str(b)

str(a)

###########################################################



# train / test

library(caret)

set.seed(0816)

intrain = createDataPartition(y=a$target, p=0.7, list=F)

train = a[intrain,]

test = a[-intrain,]



# 너무 오래걸려서 10%씩 뽑아봄 > 별차이없음

intrain.10 = createDataPartition(y=train$target,p=0.1,list=F)

intest.10 = createDataPartition(y=test$target,p=0.1,list=F)

train.10 = train[intrain.10,]

test.10 = test[intest.10,]



# 유의하지 않은 변수를 제거 > 별차이없음

a.new = a[,c(-1,-15,-18)]

a.new = a.new[,-10]

colnames(a)

train.new = a.new[intrain,]

test.new = a.new[-intrain,]





# 1. glm



# 1-1. 전처리한 것 그대로 full model

fit.full = glm(target~., data=train, family=binomial(link='logit'))

fit.full.new = glm(target~., data=train.new, family=binomial(link='logit'))

summary(fit.full) # 대부분의 변수가 유효함

summary(fit.full.new)

library(car)

vif(fit.full) # 다중공선성 없음



my_predict = function(model){

  pred = predict(model,newdata=test,type='response')

  pred = round(pred)

  result = confusionMatrix(pred,test$target)

  return(result)

}



my_predict(fit.full) # 0.6631



# 1-2. back, forward, stepwise

fit.low = glm(target~data_site_month,data=train,family=binomial(link='logit'))

fit.back = step(fit.full,scope=list(lower=fit.low,upper=fit.full),direction='backward')

fit.forward = step(fit.low,scope=list(lower=fit.low,upper=fit.full),direction='forward')

fit.step = step(fit.full,scope=list(lower=fit.low,upper=fit.full),direction='both')



my_predict(fit.back) # 0.6631

my_predict(fit.forward) # 0.6631

my_predict(fit.step) # 0.6631



# full 모델이나 변수선택이나 모델이 같아서 정확도가 높아지지 않음



# 2. tree



# 2-1. rpart

library(rpart)



rp.model = rpart(target~.,data=train)

rp.model.1 <- rpart(target~.,data=train,minsplit=10,minbucket=5)

rp.model.2 <- rpart(target~.,data=train,minsplit=2,minbucket=1,cp=0.001)

rp.model.3 <- rpart(target~.,data=train,minsplit=1,minbucket=1,cp=0.0001)

rp.model.4 <- rpart(target~.,data=train,minsplit=1,minbucket=1,cp=0.0001,maxdepth=20)

rp.model.5 <- rpart(target~.,data=train,minsplit=1,minbucket=1,cp=0.0001,maxdepth=30,xval=20)



# 유의하지 않은 변수 제거한 모델 > 결과 비슷

rp.model = rpart(target~.,data=train.new)

rp.model.1 <- rpart(target~.,data=train.new,minsplit=10,minbucket=5)

rp.model.2 <- rpart(target~.,data=train.new,minsplit=2,minbucket=1,cp=0.001)

rp.model.3 <- rpart(target~.,data=train.new,minsplit=1,minbucket=1,cp=0.0001)

rp.model.4 <- rpart(target~.,data=train.new,minsplit=1,minbucket=1,cp=0.0001,maxdepth=20)

rp.model.5 <- rpart(target~.,data=train.new,minsplit=1,minbucket=1,cp=0.0001,maxdepth=30,xval=20)



# tree accuracy

my_tree_predict = function(model){ 

  pred = predict(model,newdata=test[,-9],type='class')

  result = confusionMatrix(pred,test$target)

  return(result)

}



# overfit인지 확인

my_tree_predict1 = function(model){ 

  pred = predict(model,newdata=train[,-9],type='class')

  result = confusionMatrix(pred,train$target)

  return(result)

}



my_tree_predict(rp.model) # 0.6532

my_tree_predict(rp.model.1) # 0.6532

my_tree_predict(rp.model.2) # 0.6656

my_tree_predict(rp.model.3) # 0.6701

my_tree_predict(rp.model.4) # 0.67

my_tree_predict(rp.model.5) # 0.6694

my_tree_predict(rp.model.6) # 0.6694



my_tree_predict1(rp.model) # 0.65

my_tree_predict1(rp.model.1) # 0.65

my_tree_predict1(rp.model.2) # 0.6707

my_tree_predict1(rp.model.3) # 0.6943

my_tree_predict1(rp.model.4) # 0.6939

my_tree_predict1(rp.model.5) # 0.6964



# model 3,4,5 약간 과적합



# 과적합된 model로 가지치기

opt.cp = rp.model.5$cptable[which.min(rp.model.5$cptable[,"xerror"]),1]

rp.model.prune = prune(rp.model.5, opt.cp)

?prune

my_tree_predict(rp.model.prune) # 0.6687



opt.cp.1 = rp.model.3$cptable[which.min(rp.model.3$cptable[,"xerror"]),1]

rp.model.prune.1 = prune(rp.model.3, opt.cp.1)

my_tree_predict(rp.model.prune.1) # 0.6693



# 나아진게 없음 하하



# 2-2. ctree



library(party)

party.model = ctree(target~.,data=train)

party.pred = predict(party.model,newdata=test[,-9],type="response")

confusionMatrix(test$target,party.pred) # 0.6694



party.model.1 = ctree(target~.,data=train,controls = ctree_control(minsplit = 20,mtry = 0, maxdepth = 1))

party.pred.1 = predict(party.model.1,newdata=test[,-9],type="response")

confusionMatrix(test$target,party.pred.1) # 0.6532



# 3. random forest



library(randomForest)

ranfo = randomForest(target~.,importance=TRUE,data=train)

ranfo_pred = predict(ranfo, newdata = test)

confusionMatrix(ranfo_pred, test$target) #0.672



ranfo2 = randomForest(target~.,importance=TRUE,data=train, ntree=10)

ranfo_pred2 = predict(ranfo2 , newdata = test)

confusionMatrix(ranfo_pred2, test$target) #0.669



ranfo3 = randomForest(target~.,importance=TRUE,data=train, ntree=30)

ranfo_pred3 = predict(ranfo3 , newdata = test)

confusionMatrix(ranfo_pred3, test$target) #0.6742



ranfo4 = randomForest(target~.,importance=TRUE,data=train, ntree=50, mtry=10)

ranfo_pred4 = predict(ranfo4 , newdata = test)

confusionMatrix(ranfo_pred4, test$target) #0.6634





# 4. bagging

library(ipred)

bagg.model = bagging(target~., data=train, nbagg=10, coob=TRUE)

bagg.pred<-predict(bagg.model,test[,-9])

confusionMatrix(bagg.pred, test$target) #0.6502

bagg.model$err



bagg.model.1 = bagging(target~., data=train, nbagg=50, coob=FALSE)

bagg.pred.1<-predict(bagg.model.1,test[,-9])

confusionMatrix(bagg.pred.1, test$target) #0.6541



# 5. boosting

library(adabag)

boost.model<-boosting(target~., data=train, mfinal=5, control=rpart.control(maxdepth = 3))

boost.pred<-predict(boost.model,test[,-9])

confusionMatrix(boost.pred, test$target)



##############################

# ranfo3 모델 선택 (정확도 0.67)

train.a = a

test.b = b

ranfo3 = randomForest(target~.,importance=TRUE,data=train.a, ntree=30)

ranfo_pred3 = predict(ranfo3, newdata = test.b)

b$target = ranfo_pred3

str(b)

write.csv(b,file="이현경.csv")