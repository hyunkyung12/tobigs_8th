
setwd("C:/Users/USER/Desktop/1주차 회귀분석")



car = read.csv("car.csv")

str(car) 

attach(car)

boxplot(car) 

par(mfrow=c(1,1))

#### 1. 데이터 전처리



car = na.omit(car) # 결측치 제거



# 종류에 대한 가변수처리

size = matrix(0, nrow(car),4)

colnames(size) = paste0("종류",c("소형","준중형","중형","대형")) # 경형 : 0000 

for (i in 1:nrow(car)){

  if(car[i,4]=="소형"){size[i,1]=1}

  else if(car[i,4]=="준중형"){size[i,2]=1}

  else if(car[i,4]=="중형"){size[i,3]=1}

  else if(car[i,4]=="대형"){size[i,4]=1}

  else{}

}

car[,4]=size



#년식을 요인으로

car[,3]=as.factor(car[,3]) 



# 속도에 대한 가변수처리

speed = matrix(0, nrow(car),1)

colnames(speed) = paste0("변속기",c("자동")) # 수동 : 0

for (i in 1:nrow(car)){

  if(car[i,11]=="자동"){speed[i,1]=1}

  else{}

}

car[,11]=speed



#### 2. 정규성 검정



fit = lm(가격~.,data=car)

par(mfrow=c(2,2))

plot(fit) # qqplot이 아래로 누워있음

shapiro.test(fit$residual) # pvalue < 0.05 임



#### 3. 독립성 검정



par(mfrow=c(1,1))

plot(predict(fit), fit$residuals , type = 'o') # 점들을 이었을때 규칙성 없는것 같음

library(car)

vif(fit) # 마력, 토크에 대한 vif가 4.9 >> 다중공선성 의심

car = car[,c(-5,-6)]

fit = lm(가격~.,data=car)

vif(fit) # 다중공선성 해결



#### 4. 등분산 : 앞의 plot 의 결과 등분산성 만족x



#### 5. 이상치



outlierTest(fit)

car.out = subset(car,rownames(car)!="3"&rownames(car)!="10"&rownames(car)!="2"&rownames(car)!="2")

fit.out = lm(가격~., data=car.out)

plot(fit.out)



outlierTest(fit.out)

car.out2 = subset(car.out,rownames(car.out)!="4"&rownames(car.out)!="116")

fit.out2 = lm(가격~., data=car.out2)

plot(fit.out2)



outlierTest(fit.out2)

car.out3 = subset(car.out2,rownames(car.out2)!="5"&rownames(car.out2)!="7"&rownames(car.out2)!="115")

fit.out3 = lm(가격~., data=car.out3)

plot(fit.out3)



outlierTest(fit.out3)

car.out4 = subset(car.out3,rownames(car.out3)!="1"&rownames(car.out3)!="109"&rownames(car.out3)!="190")

fit.out4 = lm(가격~., data=car.out4)

plot(fit.out4)



outlierTest(fit.out4)

car.out5 = subset(car.out4,rownames(car.out4)!="24")

fit.out5 = lm(가격~., data=car.out5)

plot(fit.out5)



outlierTest(fit.out5)

car = subset(car.out5,rownames(car.out5)!="11")

fit = lm(가격~., data=car)

plot(fit.out6)



## 이상치를 몇번 제거했더니 아까보다 나은 그래프가 그려진다







#### 6. 변수선택

fit = lm(가격~.,data=car)

summary(fit) ## 몇몇은 유의하지 않음을 확인할수 있음



# 년식중 2015를 제외하면 유의하지 않으므로 년식을 제거

car = car[,-3]



fit.con = lm(가격~1, data=car)

fit.forward = step(fit.con, list(lower = fit.con, upper = fit), direction = 'forward')

# 최종모델 : 가격 ~ 배기량 + 변속기 + 토크 + 연료 + 회사명 + 종류 + 중량



fit.backward = step(fit, list(lower = fit.con, upper = fit), direction = 'backward')

# 최종모델 : 가격 ~ 회사명 + 종류 + 토크 + 연료 + 배기량 + 중량 + 변속기



fit.both = step(fit.con, list(lower = fit.con, upper = fit), direction = 'both')

# 최종모델 : 가격 ~ 배기량 + 변속기 + 토크 + 연료 + 회사명 + 종류 + 중량



summary(fit.both) 

# adjusted r-square 가 0.843 이므로 괜찮은 설명력을 가진 모델임을 알 수 있음

# 회사명 중 회사명쉐보레, 회사명현대는 유의하지 않음



plot(fit.both)

rsquare(fit.both)

## 잔차그래프가 아까보다 경향성을 덜 띄고, qqplot이 y=x 형태에 유사하고, 등분산성도 만족함

## 세가지 변수선택 결과가 동일함



#### 최종모델 : 가격 ~ 회사명 + 종류 + 토크 + 연료 + 배기량 + 중량 + 변속기