rm(list=ls())
install.packages("caret")
install.packages("car")
install.packages("glmnet")
install.packages("nnet")

library(car)
library(glmnet)
library(caret)

##example##
getwd()
setwd("C:/Users/USER/Desktop/투빅스/2주차 glm")
full<-read.csv("project_data.csv", header=T, stringsAsFactor=F)
str(full)
#####
# 종속 설명
# 연속 연속 > 상관분석
# 범주 연속 > T검정
# 범주 범주 > 카이제곱
#####

# 0/1로 된 종속변수를 만들어보자!!
# 여기서 numeric과 factor는 동일한 효과
full$등급[full$등급!="청소년관람불가"] <-"청소년관람가" # 청소년 관람 or not
full$등급[full$등급=="청소년관람불가"]<-1
full$등급[full$등급=="청소년관람가"]<-0
full$등급 <- factor(full$등급,order=T)
full$구분[full$구분=="국내영화"]<-1
full$구분[full$구분=="국외영화"]<-0 
full$구분<-as.numeric(full$구분)
str(full)

#일반적으로 범주형변수들간에는 상관계수대신 독립성검정을 할 수있다
#분석이랑은 상관없지만그냥 예시
m <- with(full,table(등급,구분))
par(mfrow=c(1,1))
barplot(m,beside=T, ylab="수",names.arg=c("국외영화","국내영화")) # 국내영화의 차이가 큼
addmargins(m)
chisq.test(m) #독립성검정
# pvalue가 작으면 귀무가설을 지지하는 정도가 낮음
#귀무가설 기각. 독립

###logistic regression###
#glm 코드설명
#glm(종속~독립.,데이터,family = binomial,possion,gamma 등등이 있음..)
?glm
#회귀분석하고 기본적인 포뮬라는 같다. 그냥 뒤에 family =binomial만 붙여주면 돰
names(full)
fit1<-glm(등급~구분,data=full,family=binomial(link = 'logit'))
summary(fit1)
# 베타계수 > 0 이면 영향력이 있다 정도로 생각
fit2<-glm(등급~구분+주제+선정성+폭력성+공포+약물+대사.지속성등.+모방위협,data=full,family=binomial(link = 'logit'))
summary(fit2)
vif(fit2)

#roc curve 그려보기
#roc 커브를 이용해서 이 모델이 얼마나 적합한지 알아보자
install.packages("pROC")
library(pROC)
par(mfrow=c(1,2))
y<-roc(등급~구분+주제+선정성+폭력성+공포+약물+대사.지속성등.,data=full,plot=T)

###변수선택
# 이 역시 회귀분석과 동일합니다!
forward<-step(fit1,scope=list(lower=fit1,upper=fit2),direction="forward") # AIC : 모델의 타당성을 평가, but error를 기준으로 하는 방법은 아님
summary(forward)
backward<-step(fit2,scope=list(lower=fit1,upper=fit2),direction="backward")
summary(backward)
stepwise<-step(fit1,scope=list(lower=fit1,upper=fit2),direction="both")
summary(stepwise)

#vaildation/prediction 해보기
set.seed(1)
index <- sample(1:length(full[,1]),length(full[,1])*0.7,F) # F : 복원추출x
                                                           # train : test 는 7:3 이므로 *0.7 만큼 데이터 샘플을 뽑음

#train/test 쪼개보기
train <- full[index,] # 전체의 0.7
test <- full[-index,] # 1-0.7 = 전체의 0.3 
names(train)
fit.full <- glm(등급~구분+주제+선정성+폭력성+공포+약물+대사.지속성등.+모방위협,data=train,family=binomial(link = "logit"))
fit.con<-glm(등급~1,data=train,family=binomial(link = "logit"))
fit.step <- step(fit.con,scope=list(lower=fit.con,upper=fit.full),direction="both")
glm.pred <- predict(fit.step,newdata = test,type = "response") # 예측치
pred <- round(glm.pred)
table(pred,test$등급) # 여기서 틀린건 36개 
36/length(test$등급) # 오류일 확률 계산
sum(diag(table(pred,test$등급)))/length(pred)

#multinomial인 경우
library(nnet)
data(iris)
str(iris)
set.seed(0127)
index <- sample(1:length(iris[,1]),length(iris[,1])*0.7,F) 
train <- iris[index,]
test <- iris[-index,]
#multinom 이라는 함수 이용해주면 됨.
model <- multinom(Species~.,data=train, link = "logit")
summary(model)
p <- predict(model,newdata = test,type = "class")
table(p,test$Species) 

#다중공선성 문제가 잇는 데이터를 능형회귀를 통해 해결해보자

setwd("C:/Users/USER/Desktop/투빅스/2주차 glm")
digit <- read.csv("digit.csv")
digit$label <- as.factor(digit$label)
set.seed(0127)
#데이터 갯수가 많으니까 트레인셋을 10프로만 뽑아볼게요
index <- sample(1:length(digit[,1]),length(digit[,1])*0.1,F)
train <- digit[index,]
test <- digit[-index,]

fit <- glm(label~.,data = train,family = binomial(link = "logit"))
#적합된 확률값들이 0또는 1이다 => perfect multicollinearty 의미
vif(fit)
#Error in vif.default(fit) : there are aliased coefficients in the model
#라고다들 뜨실텐데 완전 공선성이 있어서 추정이 불가능한 상태를 의미한다.
#이정도로 완전공선성인 데이터일때는 vif값을 제대로 볼 수도 없어서 변수선택도
#하지 못하는 상태가 된다

#능형회귀 사용 >>>> 가변수 처리를 해주어야함 !!!!!!!!

#glmnet 코드
#glmnet(x=설명변수,y=종속변수
#       family = 사용하고자 하는 종속변수 level 수에 따라 "binomial" "multinomial"
#       alpha = 0인경우 ridge/1인경우 lasso 0과1사이인 경우 elastic net
#       nfold = cross-validation 할 폴드의 수)

#능형회귀 이용한 모델적합
reg.ridge <- cv.glmnet(x= data.matrix(train[,-1]),y= data.matrix(train[,1]),
                       family = "binomial", alpha = 0,nfold=10) 
coef(reg.ridge)
################################################################ 해당 람다에 대해 모델을 한번 더 적합 한 후 에러를 구해야함
plot(reg.ridge) # error율을 보여주는 것 (베타계수 축소하는것과 다른것)
reg.ridge$lambda.min # cv error 최소화하는 lambda값
pred <- predict(reg.ridge,data.matrix(test[,-1]),s= reg.ridge$lambda.min,type="class")
table(Actual= test[,1],Predicted = pred)
confusionMatrix(pred,test[,1]) #table 보다 test 셋에 대한 더많은 정보를 얻을 수 있음

# 라소
reg.lasso <- cv.glmnet(x= data.matrix(train[,-1]),y= data.matrix(train[,1]),
                       family = "binomial", alpha = 1, nfold=10)
# alpha = 0 릿지 / alpha = 1 라소
# alpha 0~1 이면 릿지라소 섞은 모델
coef(reg.lasso)
plot(reg.lasso)
# binomialDeri은 error와 비슷

reg.lasso$lambda.min
#cv error 최소화하는 lambda값
reg.lasso$lambda.1se
# 를 주로 사용한다 -> 과적합의 영향을 줄이는 람다!!!
# 에러는 람다민 보다는 크지만 과적합의 영향을 덜 받는

pred_2 <- predict(reg.lasso,data.matrix(test[,-1]), s= reg.lasso$lambda.min,
                  type="class")

table(Actual= test[,1],Predicted = pred_2)
confusionMatrix(pred,test[,1])
