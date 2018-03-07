# -1. 데이터 불러오기

data <- iris
str(data) # target = species

## 거리계산함수 dist 사용 예제
x <- matrix(rnorm(100), nrow = 5)
x
hist(x)
dist(x) 

# method를 통해 거리유형 선택가능(default=euclidean)
x1 <- c(10,20,1,2,3,4)
y1 <- c(5,6,7,8,9,10)
dist(rbind(x1, y1), method="euclidean") 
dist(rbind(x1, y1), method ="manhattan") 

# method = "binary"인 경우 ( 데이터가 0,1 로 구성)
# 거리 = 하나만 1이 포함된 column의 개수 / 하나라도 1이 포함된 column의 개수
# 범주형 변수 거리로 정의했던 자카드 거리와는 달라요!
x1 <- c(0, 1, 1, 1, 1)
y1 <- c(0, 0, 1, 1, 0)
dist(rbind(x1, y1), method = "binary")
?dist

x <- matrix(rnorm(100), nrow = 5)
dist(x)
dist(x, diag = TRUE)
dist(x, upper = TRUE)
m <- as.matrix(dist(x))
d <- as.dist(m)
stopifnot(d == dist(x))

data = matrix(c(1,2,4,5,1,1,3,4),ncol=4,byrow=T)
dist(data)

## 참고: 범주형과 연속형 변수가 섞여있을 때, 거리를 계산해주는 cluster 패키지 내 daisy 함수
# daisy ( x, metric = , stand = )
# x : numeric 형태의 matrix 혹은 dataframe
# metric : 어떤 거리 척도?
# "euclidean" - default, "manhattan", "gower" - 연속형 + 범주형 변수 혼합인 경우 사용
# stand : True인 경우 연속형 변수들에 대해 표준화 후 거리 계산

# 1. 데이터 전처리
set.seed(802)
str(data)
data1 <- data[-5] # 비지도 학습이므로 라벨을 떼기
str(data1)


# 2. k means 함수 적용시켜보기
#install.packages("cluster")
library("cluster")
kmeans1 <- kmeans(data1, 3, nstart = 10) # 3개 군집으로 분류할것, nstart : 몇번 초기값을 돌려볼것인가
kmeans1$cluster 
#  cluster : 각 개체별 할당된 군집 번호, 1부터 k번까지 군집 숫자 
#  centers : 군집의 중심 (centroid) 좌표 행렬
#  totss : 제곱합의 총합
#  withinss : 군집 내 군집과 개체간 거리의 제곱합 벡터.
#  tot.withinss : 군집 내 군집과 개체간 거리의 제곱합의 총합, 즉, sum(withinss)
#  betweenss : 군집과 군집 간 중심의 거리 제곱합
#  size : 각 군집의 개체의 개수
#  iter : 반복 회수

# 내부평가 
# 외부평가 : 라벨이 잘 맞았나 검사 => 예측한 군집과 원래 군ㅈ

result_table <- table(kmeans1$cluster, data$Species)
result_table
sum(50, 48, 36) / sum(result_table) * 100

## 정규
# 변수마다 스케일의 차이로 인해 생긴 문제일 수 있으니, scale함수를 통해 단위를 맞춰주자.

scaled_data <- scale(data1)
kmeans_scaled <- kmeans(scaled_data, 3, nstart=10)
result_table <- table(kmeans_scaled$cluster, data$Species)
result_table
sum(50, 39, 36) / sum(result_table) * 100

# pch: 모양, col: 색
# 모든 변수들 간 plot 확인
plot(data, pch = kmeans_scaled$cluster, col = kmeans_scaled$cluster) # 대체적으로 군집이 잘 되었다

### 군집수를 모르는 경우 (위에는 k=3)
## 최적의 군집 수 k 결정
# elbow point 기법을 이용
# 군집의 개수 k = 2부터 k = 6인 경우까지만 확인해보자

visual <- NULL

for ( i in 2:6 ){
  result <- kmeans(data1, i, 10)
  visual[i] <- result$tot.withinss
}

plot(visual, type="l", xlab="k") 
abline(v=3, col="red")
# elbow point인 k = 2 또는 3으로 결정하는 것이 좋다
kmeans5 <- kmeans(data1, 4, 10) # elbowpoint 넣어서 돌려보기
kmeans5$cluster
result_table <- table(kmeans5$cluster, data$Species)
result_table
sum(28, 22, 27, 23, 27) / sum(result_table) * 100 ## 뭘 더하는지 이해!

# 3. H-Clust 함수 적용시켜보기
# 위에서 스케일된 데이터를 hclust 함수에 적용시켜보자
data_hclust <- data1
data_hclust
distance <- round(dist(data_hclust), 2) # 데이터 안의 객체들간의 거리 지정

# 덴드로그램 그려보기 : hclust 함수 이용

plot(h <- hclust(distance, method="single")) # method="single" 최단연결법
plot(h <- hclust(distance, method="complete")) # method="complete" 최장연결법
plot(h <- hclust(distance, method="average")) # method="average" 평균연결법
plot(h <- hclust(distance, method="centroid")) # method="centroid" 중심연결법 

# 분류 결과인 gender를 제외한 data_x
data_x <- data_hclust
distance <- round(dist(data_x), 2)

# 중심연결법을 이용한 Hclust 예시
hc <- hclust(distance, method = "centroid")
plot(hc)
rect.hclust(hc, k = 3, border = "red") # hclust 결과를 k값에 따라 분리 => 군집이 잘 안이루어짐
rect.hclust(hc, k = 6, border = "red") # 위보다는 괜찮은 결과 => 너무 작은 군집은 이상치로 제거
result <- cutree(hc, k = 6)
result
table(result)

# 각 군집 별 데이터 확인
data_x[result == 2, ]
data_x[result == 4, ]
data_x[result == 6, ]

## 이상치 빼주기
remove <- which(result %in% c(2,4,6))
data_r <- data[-remove,]
data_x <- data_x[-remove,]
distance <- round(dist(data_x), 2)

hc <- hclust(distance, method = "centroid")
plot(hc)
rect.hclust(hc, k = 3, border = "red")
result <- cutree(hc, k = 3) # cutree : 결과값 저장
result
table(result)

# 각 군집 별 centroid 계산
a <- NULL
for ( i in 1:2 ) a <- rbind(a, colMeans(data_x[result == i, ]))

# 확인
t <- table(data_r$Species, result)
sum(diag(t)) / sum(t)


### 외부평가 하기 
## 실루엣을 통해 군집화가 얼마나 잘되었는지 살펴보자
plot(silhouette(cutree(hc, k = 3), dist = distance, col = 1:3))
# Average silhouette width 값이 0.57

## fpc패키지의 pam함수를 통한 최대 Average silhouette width값을 가지는 군집 개수 k 추출
install.packages("fpc")
library("fpc")

# 군집의 개수를 2에서 5까지 조정해보자
# 군집 개수 k 별로 average silhoutte width를 저장할 average_sil
average_sil <- rep(0, 5)
for ( k in 2:5 ) average_sil[k] <- pam(data_x, k) $ silinfo $ avg.width
average_sil
k.best <- which.max(average_sil)
print(k.best) # 군집을 2로 했을때 실루엣이 최대이다

# 클러스터링 과제입니다.
# 8기 과제는 협동 과제입니다. 아래의 4가지 과제를 같은 멘토링에 속한 8기 인원들끼리 나눠하던, 협심하여하던, 4분께서 알아서 제출해주시면 됩니다. 완성된 코드는 4명의 과제 중 한 군데에만 올라와도 됩니다.
# 4가지 과제는 다음과 같습니다.
# 1. K-Means 알고리즘 구현
# 2. K-Median 알고리즘 구현 : 맨하탄 거리
# 3. 코사인 유사도를 이용한 클러스터링 알고리즘 구현
# 4. correlation을 이용한 클러스터링 알고리즘 구현
# kmeans 알고리즘을 제외한 나머지 알고리즘은 다음과 같이 구현하시면 됩니다.
# - K-Median 알고리즘 구현의 경우, 거리를 유클리드 거리 대신, 맨하탄 거리를 이용하여 구하시면 됩니다. 센트로이드 선정 역시, 한 개의 클러스터 내에서 맨하탄 거리의 합이 가장 짧은 점으로 잡으시면 됩니다.
# - 나머지 두 알고리즘 구현의 경우, K-median과 유사하지만, 거리값을 코사인 유사도, correlation값으로 바꾸고 구현하시면 됩니다.
# 7기 이상 분들의 과제는 코사인 유사도로 해주시면 되겠습니다.