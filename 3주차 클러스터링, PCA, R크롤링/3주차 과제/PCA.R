rm(list=ls())
library(rgl)
setwd("C:/Users/USER/Desktop/투빅스/3주차 과제")
data = read.csv("2015_7차_직접측정 데이터.csv")
str(data)

#평균 절대 오차
mae <- function(actual, predict) { 
  if (length(actual) != length(predict)) {
    message("실제값과 예측값의 길이가 다릅니다\n")
    break
  }
  length <- length(actual)
  errorSum <- sum(abs(actual - predict))
  return(errorSum/length)
}


#평균 제곱근 오차
rmse <- function(actual, predict) { 
  if (length(actual) != length(predict)) {
    message("실제값과 예측값의 길이가 다릅니다\n")
    break
  }
  length <- length(actual)
  errorSum <- sum((actual - predict)^2)
  return(sqrt(errorSum/length))
}

## 1. 데이터 전처리

# 목둘레 변수 제거
data = data[,-which(names(data)=="목둘레")]
# 여자만 뽑기
data = data[which(data$성별=="여"),]
data = data[,-2]
# 결측치 제거
data = na.omit(data)
str(data)# 변수가 135 개이므로 차원축소가 필요해보임

## 2. test / train set 분리
set.seed(100)
index = sample(1:nrow(data),0.7*nrow(data),replace=F)
data_test = data[-index,-1]
data_train = data[index,-1]
test_label = data[-index,1]
train_label = data[index,1]

pca = prcomp(data_train,center=T,scale=T)
pca$rotation

## 3. 변수 줄이기
## 3-1. elbow point 를 이용해 변수를 줄이기
plot(pca,type="l") #elbow point = 3 이므로 변수 3개만 선택

trainprc = as.matrix(data_train) %*% pca$rotation
testprc = as.matrix(data_test) %*% pca$rotation

train = cbind(train_label,as.data.frame(trainprc))
test = cbind(test_label,as.data.frame(testprc))

colnames(train)[1] = "label"
colnames(test)[1] = "label"

str(train)
fit = lm(label~PC1+PC2+PC3,data=train)
summary(fit) # 모든 변수들이 유의함

fit_pred = predict(fit,type="response",newdata=test)
test_pred = round(fit_pred)

mae(test_label,test_pred) # 1.4 
rmse(test_label,test_pred) # 2.04

## 3-2. summary의 cummulative proportion 을 이용해 변수 개수 정하기
summary(pca)

# 80이 넘어가는 변수 기준 => 16개 선택
train1 = train[,1:17]
fit1 = lm(label~.,data=train1)
summary(fit1)
library(car)
vif(fit1) # 1,2의 vif가 매우 높음

fit_pred1 = predict(fit1,type="response",newdata=test)
test_pred1 = round(fit_pred1)
mae(test_label,test_pred1) # 0.87
rmse(test_label,test_pred1) # 1.49

train2 = train[,1:19]
train2 = train2[,-c(2,3)] # vif가 높았던 것들을 제거하고 다시 16개 선택
fit2 = lm(label~.,data=train2)
summary(fit2)
vif(fit2) 

fit_pred2 = predict(fit2,type="response",newdata=test)
test_pred2 = round(fit_pred2)
mae(test_label,test_pred2) # 1.95
rmse(test_label,test_pred2) # 2.61

# 90이 넘어가는 변수 기준 => 36개 선택
train3 = train[,1:37]
fit3 = lm(label~.,data=train3)
vif(fit3) # vif 가 10 이상인것들 제거 1,2,5,7,13,14,16

fit_pred3 = predict(fit3,type="response",newdata=test)
test_pred3 = round(fit_pred3)
mae(test_label,test_pred3) # 0.85
rmse(test_label,test_pred3) # 1.46

train4 = train[,1:44]
train4 = train3[,-c(2,3,6,8,14,15,17)] # vif가 높았던 것들을 제거하고 다시 36개 선택
fit4 = lm(label~.,data=train4)
vif(fit4)
summary(fit4) 

fit_pred4 = predict(fit4,type="response",newdata=test)
test_pred4 = round(fit_pred4)
mae(test_label,test_pred4) # 2.29
rmse(test_label,test_pred4) # 2.98

## 3-3. vif 말고 변수선택의 방법으로 해보기
# 두개 같이 하면 오차가 커지는데 왜그러는지는 모르겠음 ㅠㅠ
fit.up = lm(label~.,data=train1) # train1 은 cummulative 가 80이 되던 지점
fit.low = lm(label~1,data=train1)
fit.forward = step(fit.low, list(lower=fit.low,upper=fit.up),direction="forward")
fit5 = lm(label ~ PC1 + PC2 + PC5 + PC3 + PC14 + PC7 + PC12 + PC8 + PC4 + PC16 + PC10 + PC6 + PC13 , data=train1)
summary(fit5)

fit_pred5 = predict(fit5,type="response",newdata=test)
test_pred5 = round(fit_pred5)
mae(test_label,test_pred5) # 0.87
rmse(test_label,test_pred5) # 1.48

fit.up1 = lm(label~.,data=train3) # train3 은 cumulative 가 90이 되던 지점
fit.low1 = lm(label~1,data=train3)
fit.forward1 = step(fit.low1, list(lower=fit.low1,upper=fit.up1),direction="forward")

fit6 = lm(label ~ PC1 + PC2 + PC5 + PC3 + PC27 + PC7 + PC12 + PC14 + PC8 + PC33 + PC4 + PC22 + PC16 + PC6 + PC10 + PC31 + PC34 + PC20 + 
            PC28 + PC21 + PC9 + PC25 + PC19 + PC29 + PC11 + PC17 + PC18 + PC35, data=train3)
summary(fit6)
fit_pred6 = predict(fit6,type="response",newdata=test)
test_pred6 = round(fit_pred6)
mae(test_label,test_pred6) # 0.84
rmse(test_label,test_pred6) # 1.46

## 최종 모델 ##
# 여성의 몸무게를 가장 잘 설명하는 모델은 
# label ~ PC1 + PC2 + PC5 + PC3 + PC27 + PC7 + PC12 + PC14 + PC8 + PC33 + PC4 + PC22 + PC16 + PC6 + PC10 + PC31 + PC34 + PC20 + 
# PC28 + PC21 + PC9 + PC25 + PC19 + PC29 + PC11 + PC17 + PC18 + PC35 이다
# fit5 와 fit6의 오차는 0.02 정도밖에 차이가 나지 않으므로 더 간단한
# label ~ PC1 + PC2 + PC5 + PC3 + PC14 + PC7 + PC12 + PC8 + PC4 + PC16 + PC10 + PC6 + PC13 을 선택하는것도 좋을것 같다