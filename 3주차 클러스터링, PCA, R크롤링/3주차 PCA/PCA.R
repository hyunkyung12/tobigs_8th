#############
#주성분 분석#
#############

rm(list=ls())

#scatter plot 을 3d로 표현하기 편한 library
install.packages("rgl")
library(rgl)

#random data 생성 (3개는 각각 분산이 다름)
set.seed(1234)
x<-rnorm(n=1000,mean=0,sd=100)
y<-rnorm(n=1000,mean=0,sd=10)
z<-rnorm(n=1000,mean=0,sd=50)

#가장 큰 축인 x축에 맞춰서 range를 변환 (x가 분산이 제일 커서)
range<-c(min(x),max(x))
plot3d(x,y,z,xlim=range,ylim=range,zlim=range)

#data의 point가 x 축을 기준으로 가장 넓게 분포
#y축의 샘플들이 굉장히 좁은 것을 볼 수 있습니다.

#x,z로 data 설명 할 수 있고 y는 data를 설명하기에 부족합니다.

#x축과 z축을 corr이 있도록 sorting을 해보겠습니다.
x<-sort(rnorm(n=1000,mean=0,sd=100))
y<-rnorm(n=1000,mean=0,sd=10)
z<-sort(rnorm(n=1000,mean=0,sd=50))

range<-c(min(x),max(x))
plot3d(x,y,z,xlim=range,ylim=range,zlim=range)

###################pca 구현########################

#1. 원 data matrix 생성
mat=cbind(x,y,z)
head(mat)

#2. covariance matrix 생성
cmat=cov(mat)
cmat

#3. eigen value, eigen vector
eig=eigen(cmat)
eig # eigen value, vector를 결과로 (value 큰 순서로 나열)
eig$vectors
#eigen value의 첫번째 component 는eigen vector에서 첫번째 열
#두번째 component는 두번째 열로 일대일 매칭
#주의! 완벽하게 값이 같지 않을 수도 있음 (system적인 오차 존재)

#4. 새로운 data set 생성( 원래 data의 선형 결합)
fin<- mat %*% eig$vectors

head(fin) #새로 생성한 data
head(mat) #원래 data 

#원래 data vs 새로 생성한 data //선형 변환인지 확인
range2=c(min(fin[,1]),max(fin[,1]))
plot3d(fin[,1],fin[,2],fin[,3],xlim=range2,ylim=range2,zlim=range2,col='red')
plot3d(x,y,z,xlim=range,ylim=range,zlim=range,add=T)
#차원을 축소한 것이 아니기 때문에 새로 생성된 변수 fin은
#원래 data인 mat 의 선형 변환 

# 새 데이터 1,2,3 각각의 설명력
sd(fin[,1])/(sd(fin[,1])+sd(fin[,2])+sd(fin[,3]))#90% 
sd(fin[,2])/(sd(fin[,1])+sd(fin[,2])+sd(fin[,3]))#7.9%
sd(fin[,3])/(sd(fin[,1])+sd(fin[,2])+sd(fin[,3]))#1.7%

plot(fin[,1],fin[,2]) #fin[,3]은 설명력이 적으니까 제외
#모든 data가 pc1 pc2 에 분포

pca<-prcomp(mat)#center=F,scale=F ==>sd를 1로 맞춤

pca$sdev # sd(fin[,1]) , 2, 3 과 같은것을 확인
pca$rotation # eig$vectors 의 축을 뒤집음 => 음/양 바뀜

#여러가지 펑션을 사용하면서 오는 오차
head(pca$x)
head(fin)

plot(fin[,1],fin[,2])
points(pca$x[,1],-pca$x[,2],col='red',pch=3) # 축이 뒤집어져서 마이너스

###################주성분 분석########################
rm(list=ls())

install.packages("mlbench")
library(mlbench)
data(Sonar)

str(Sonar)

#로지스틱을 사용하기 위해
#Sonar의 종속변수 Class를 0,1 값으로 바꿔줌
Sonar$Class <- as.character(Sonar$Class)
Sonar$Class[Sonar$Class==c("R")] <- 1
Sonar$Class[Sonar$Class==c("M")] <- 0
Sonar$Class <- as.factor(Sonar$Class)

str(Sonar)

set.seed(100)
index <- sample(1:nrow(Sonar), 0.7 * nrow(Sonar), replace = F)
sonar_test <- Sonar[-index,-61]
sonar_train <- Sonar[index,-61]
sonar_test_label <- Sonar[-index,61]
sonar_train_label <- Sonar[index,61]

head(sonar_train)

sn.pca<-prcomp(sonar_train,center=T,scale=T) 

#원 data의 covariance 로 구한 A
#즉, 원래 data에 곱해주는 계수
sn.pca$rotation 

#변수의 개수 결정(elbow point)
plot(sn.pca,type="l") # elbow point = 3 => 변수 3개만 선택하면 된다
summary(sn.pca) # elbow point가 명확하지 않으면 내가 임의로 cumultative proportion을 보고 찾아 (80%이면 0.8 인 13개선택)

trainPRC<-as.matrix(sonar_train) %*% sn.pca$rotation
testPRC<-as.matrix(sonar_test) %*% sn.pca$rotation

train<-cbind(as.data.frame(trainPRC),sonar_train_label)
test<-cbind(as.data.frame(testPRC),sonar_test_label)

colnames(train)[61] <- "label"
colnames(test)[61] <- "label"

str(train)
fit <- glm(label~PC1+PC2+PC3, family = "binomial", data=train)
summary(fit) # 각 변수들은 유의함
library(car)
vif(fit)

#예측
fit_pred <- predict(fit,type="response",newdata = test)
test_pred<- round(fit_pred)

table<-table(sonar_test_label,test_pred)
table
sum(diag(table))/sum(table)
