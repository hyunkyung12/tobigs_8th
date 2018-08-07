rm(list=ls())

##############################################################################
## 거리를 구하는 기본 공식

# 유클리드 => K-Means 
euclidian <- function(x1, x2) {
  dist <- sqrt(sum((x1-x2)^2))
  return(dist)
}

# 맨하탄 => K-Median
manhattan <- function(x1, x2) {
  dist  <- sum(abs(x1 - x2))
  return(dist)
}

# 코사인유사도
cosine <- function(x1 ,x2) {
  dist <- sum(x1 * x2) / (sqrt(sum(x1^2)) * sqrt(sum(x2^2)))
  return(dist)
}

# correlation 
correlation <- function(x1, x2) {
  covariance <- sum((x1 - mean(x1)) * (x2 - mean(x2))) / (length(x1)-1)
  sigma.x1 <- sqrt(var(x1))
  sigma.x2 <- sqrt(var(x1))
  dist <- covariance / (sigma.x1 * sigma.x2) 
  
  return(dist)
}

###############################################################################
## 거리행렬 구하기

dist.mat <- function(data, center, method) { # 대상데이터, 중심점, 거리method를 입력
  k <- nrow(center) 
  n <- nrow(data)
  dmat <- matrix(0, k, n)  # k행 n열인 빈행렬 만들기
  
  # 각 중심점과 대상데이터의 거리를 구함
  
  if(method == 'euclidian') {
    for(i in 1:k){
      for(j in 1:n){
        dmat[i, j] <- euclidian(data[j, ], center[i, ]) 
      }
    }
  }
  
  if(method == 'manhattan') {
    for(i in 1:k){
      for(j in 1:n){
        dmat[i, j] <- manhattan(data[j,], center[i,])
      }
    }
  }
  
  if(method == 'cosine') {
    cdist = as.matrix(dist(data,method="cosine",upper=T))
    for(i in 1:k){
      for(j in 1:n){
        dmat[i,j] = cdist[i,j]
      }
    }
  }
  
  if(method == 'correlation')
    for(i in 1:k){
      for(j in 1:n){
        dmat[i, j] <- correlation(data[j,], center[i,])
      }
    }
  
  return(dmat)
}

##############################################################################


my.kmeans <- function(data, k) { # 대상데이터, 군집의 수 를 입력
  
  n <- nrow(data)
  p <- ncol(data)
  center.1 <- c() # 초기중심점
  center.1 <- data[c(sample(1:n, k)), ] 
  dmat <- matrix(0, k, n)
  acc <- c() # 중심점과 이전단계의 중심점의 차이 (군집을 한번더 뽑을지에 대한 선택)
  keep <- TRUE
  
  while(keep) {
    dmat <- dist.mat(data, center.1, 'euclidian')
    min.index <- apply(dmat, 2, which.min)
    
    center.2 = matrix(0, k, p)
    
    for(i in 1:k) {
      center.2[i, ] <- colMeans(data[which(min.index == i), ])
    }
    
    for(i in 1:k){
      acc[i] <- euclidian(center.1[i,],center.2[i,])
    }
    
    if(sum(acc < 1e-04) == k) break
    center.1 <- center.2
  }
  
  return(min.index)
}


my.kmedian <- function(data, k, rep) { # 대상데이터, 군집의 수 를 입력
  
  n <- nrow(data)
  p <- ncol(data)
  center.1 <- c() # 초기중심점
  center.1 <- data[c(sample(1:n, k)), ]
  count = 1
  dmat <- matrix(0, k, n)
  acc <- c() # 중심점과 이전단계의 중심점의 차이 (군집을 한번더 뽑을지에 대한 선택)
  keep <- TRUE
  
  while(keep) {
    dmat <- dist.mat(data, center.1, 'manhattan')
    min.index <- apply(dmat, 2, which.min)
    
    center.2 = matrix(0, k, p)
    
    for(i in 1:k) {
      for(j in 1:p){
      center.2[i,j] <- median(data[which(min.index == i), j])
      }
    }
    
    for(i in 1:k){
      acc[i] <- manhattan(center.1[i,],center.2[i,])
    }
    
    if(sum(acc < 1e-04) == k) break
    center.1 <- center.2
  }
  
  return(min.index)
}

my.cos<- function(data, k) { # 대상데이터, 군집의 수 를 입력
  
  n <- nrow(data)
  p <- ncol(data)
  center.1 <- c() # 초기중심점
  center.1 <- data[c(sample(1:n, k)), ] 
  dmat <- matrix(0, k, n)
  acc <- c() # 중심점과 이전단계의 중심점의 차이 (군집을 한번더 뽑을지에 대한 선택)
  keep <- TRUE
  
  while(keep) {
    dmat <- dist.mat(data, center.1, 'cosine')
    min.index <- apply(1-dmat, 2, which.min) # 코사인 유사도는 클수록 좋음 : 1-dmat 이 작은것 <=> 유사도가 큰것
    
    center.2 = matrix(0, k, p)
    
    for(i in 1:k) {
      center.2[i, ] <- colMeans(data[which(min.index == i), ])
    }
    
    cdist = as.matrix(dist(data,method="cosine",upper=T))
    for(i in 1:k){
      acc[i] <- cdist[center.1[i,],center2[i,]]
    }
    
    if(sum(acc < 1e-04) == k) break
    center.1 <- center.2
  }
  
  return(min.index)
}

data <- iris
data1 <- data[-5] 

r1 = my.kmeans(data1,3)
r2 = my.kmedian(data1,3)
r3 = my.cos(data1,1)
table(r3,iris[,5])``
silhouette(my.kmeans(data1,3),dist = dist(data1))
?silhouette
mean(silhouette(my.kmeans(data1,3), dist = dist)[,'sil_width'])
install.packages("cluster")
library(cluster)

my.machine <- function(funcname,data,k){
  list = c()
  for(i in c("my.kmeans","my.kmedian")){
    list = c(mean(silhouette(i(data,k)),dist = dist(data1)))
  }
  return(max(list))
}

my.machine("my.kmeans",data1,3)

voting_machine <- function(data,k){
  
  list = c(mean(silhouette(my.kmeans(data1,3),dist = dist(data1))[,'sil_width']),mean(silhouette(my.kmedian(data1,3),dist = dist(data1))[,'sil_width']))
  
  return (max(list))
}
install.packages("clv")
library(clv)
install.packages("clValid")
library(clValid)
voting_machine(data1,3)
dunn(dist(data1),r1,data1,method="euclidean")
dunn(dist(data1),r2,data1,method="manhattan")

dunn(dist(data1),r2,data1,method="cosine")

dunn(dist(data1),r2,data1,method="correlation")
my.machine <- function(funcname,data,k){
  list = c()
  r1 = my.kmeans(data,k)
  r2 = my.kmedian(data,k)
  r3 = my.cosine(data,k)
  r4 = my.corr(data,k)
  
  for(i in c(r1,r2,r3,r4)){
    if(funcname=="silhouette"){
      list = c(mean(silhouette(i,dist = dist(data))))
    }
    if(funcname=="dunn"){
      for(j in c("euclidean","manhattan","cosine","corrleation")){
        list = c(dunn(dist(data),i,data,method=j))
      }
    }
  }
  
  return(max(list))
}
