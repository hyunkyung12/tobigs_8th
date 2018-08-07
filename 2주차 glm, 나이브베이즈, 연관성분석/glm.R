rm(list=ls())
library(arules)
data("AdultUCI")
adu <- AdultUCI
adu = na.omit(adu) # 결측치 제거

### 데이터 파악하기

str(adu) # 15개의 변수 중 반 이상이 factor 형임 => glm 이용

# age : int => factor => 0/1
age = ifelse(adu$age<=39,1,0) # 청장년층 / 중노년층
adu$age = factor(age,order=T) 

# workclass : self-emp-inc 제외하고 거의 비슷한 income값을 가짐 => 그냥 제거
plot(workclass,income)
names(adu)
adu = adu[,-2]

# fnlwgt : 의미를 모르겠는 변수 => 제거
adu = adu[,-2]

# education : education-num 과 같은 의미의 변수 => 제거
adu = adu[,-2]

# education-num : factor => 0/1
edunum = ifelse(adu$`education-num`<=9,0,1) # 고졸 / 고졸이상
adu$`education-num` = factor(edunum, order=T)

# marital-status : factor => 0/1 
plot(adu$marital,adu$income) # married-civ-spouse 과 아닌것의 차이가 큼
marital = ifelse(adu$`marital-status`=="Married-civ-spouse",1,0) # "Married-civ-spouse O" / "Married-civ-spouse X"
adu$`marital-status` = factor(marital, order=T)
table(adu$`marital-status`) 

# occupation : factor => 0/1
# 소득을 기준으로 임의로 나눔
occupation = character(0)
occupation[adu$occupation %in% c("Exec-managerial","Craft-repair","Prof-speciality","Sales","Tech-support","Protective-serv")]=1
occupation[is.na(occupation)]=0
adu$occupation = factor(occupation, order=T)

# relationship : factor => 0/1
relationship = character(0) 
relationship[adu$relationship %in% c("Husband","Wife")]=1 # 배우자인경우 / 아닌경우
relationship[is.na(relationship)]=0
adu$relationship = factor(relationship,order=T)
table(adu$relationship)

# race : factor
race = ifelse(adu$race=="White",1,0) # 백인인경우 / 아닌경우
adu$race = factor(race, order=T)

# sex
plot(adu$sex, adu$income)
sex = ifelse(adu$sex=="Male",1,0) # 남 / 여
adu$sex = factor(sex, order=T)

# capitalgain 
gain = ifelse(adu$`capital-gain`==0,0,1) # capital이 있는경우/ 없는경우
adu$`capital-gain` = factor(gain, order=T)

# capitalloss 
loss = ifelse(adu$`capital-loss`==0,0,1)
adu$`capital-loss` = factor(loss, order=T)

# hours-per-week
summary(adu$`hours-per-week`)
hist(adu$`hours-per-week`)
hours = c()
hours[adu$`hours-per-week`<=39]="40시간 미만"
hours[adu$`hours-per-week`>39 & adu$`hours-per-week`<45]="40시간 이상 45시간 미만"
hours[adu$`hours-per-week`>=45]="45시간 이상"
adu$`hours-per-week` = factor(hours, levels=c("40시간 미만","40시간 이상 45시간 미만","45시간 이상"))
table(adu$`hours-per-week`)

# income
income = ifelse(adu$income=="small",0,1)
adu$income = factor(income, order=T)

# native-country : 레벨이 너무 많음 => 제거
adu = adu[,-11]
str(adu)

### logistic regression
fit1 = glm(income~.,data=adu, family=binomial(link='logit'))
summary(fit1) 
library(car)
vif(fit1) # marital-num , relationship 의 vif > 5이므로 제거
adu1 = adu[,c(-3,-5)]
fit2 = glm(income~.,data=adu1, family=binomial(link='logit'))
summary(fit2)
vif(fit2) # vif > 5 인 변수들을 제거함으로서 다중공선성 해결

### 변수선택
fit_low = glm(income~occupation,data=adu1, family=binomial(link='logit'))
forward = step(fit_low, scope=list(lower=fit_low,upper=fit2), direction="forward")
summary(forward) # income ~ occupation + `hours-per-week` + `capital-gain` + `education-num` + age + sex + `capital-loss` + race
backward = step(fit2, scope=list(lower=fit_low,upper=fit2), direction="backward")
summary(backward) # income ~ age + `education-num` + occupation + race + sex + `capital-gain` + `capital-loss` + `hours-per-week`
stepwise = step(fit_low, scope=list(lower=fit_low,upper=fit2), direction="both")
summary(stepwise) # income ~ age + `education-num` + occupation + race + sex + `capital-gain` + `capital-loss` + `hours-per-week`

# validation/prediction
set.seed(0127)
index <- sample(1:length(adu1[,1]),length(adu1[,1])*0.7,F)
train <- adu1[index,]
test <- adu1[-index,]
names(train)

fit.full = glm(income~.,data=train,family=binomial(link="logit"))
fit.con = glm(income~1,data=train,family=binomial(link="logit"))
fit.step = step(fit.con, scope=list(lower=fit.con,upper=fit.full),direction="both")
# income ~ age + `education-num` + occupation + race + sex + `capital-gain` + `capital-loss` + `hours-per-week`
glm.pred = predict(fit.step, newdata = test, type = "response")
pred = round(glm.pred)
table(pred,test$income) # 틀린것이 넘나 많음

### 다중공선성을 능형회귀로 해결해보자
# level이 3인 변수에 대해 가변수처리 : base 는 40시간 이상 45시간 미만 => 00
summary(adu)
under_40 = rep(0,nrow(adu))
for(i in 1:nrow(adu)){
  under_40[i] = ifelse(adu[i,10]=="40시간 미만",1,0)
}
above_45 = rep(0,nrow(adu))
for(i in 1:nrow(adu)){
  above_45[i] = ifelse(adu[i,10]=="45시간 이상",1,0)
}
adu$under_40 = factor(under_40,order=T)
adu$above_45 = factor(above_45,order=T)
adu = adu[,-10]

# validation/prediction
str(adu)
set.seed(0127)
index <- sample(1:length(adu[,1]),length(adu[,1])*0.7,F)
train <- adu[index,]
test <- adu[-index,]
fit = glm(income~.,data=train,family=binomial(link="logit"))
vif(fit) # marital 과 relationship 의 vif 가 30이상

library(glmnet) 
reg.ridge = cv.glmnet(x=data.matrix(train[,-10]),y=data.matrix(train[,10]),family="binomial",alpha=0,nfold=10)
coef(reg.ridge)
par(mfrow=c(1,1))
plot(reg.ridge)
reg.ridge$lambda.min
pred = predict(reg.ridge, data.matrix(test[,-10]),s = reg.ridge$lambda.min, type="class")
table(Actual = test[,10], Predicted = pred)
library(caret)
confusionMatrix(pred,test[,10]) # accuracy : 0.8196

reg.lasso = cv.glmnet(x=data.matrix(train[,-10]),y=data.matrix(train[,10]),family="binomial",alpha=1,nfold=10)
coef(reg.lasso)
plot(reg.lasso)
reg.lasso$lambda.min
pred = predict(reg.lasso, data.matrix(test[,-10]),s = reg.lasso$lambda.min, type="class")
table(Actual = test[,10], Predicted = pred)
confusionMatrix(pred,test[,10]) # accuracy : 0.8243