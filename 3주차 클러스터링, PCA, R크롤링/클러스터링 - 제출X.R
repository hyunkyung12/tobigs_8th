##KNN 구현하기

# 여러가지 거리를 중심으로 

# iris data를 이용해 다양한 거리로 클러스터링 해본다.

# 최적의 k 값을 찾는 과정은 생략하고 k = 3으로 고정했다. 이유는 거리마다 최적 k가 달라져서(예상이지만?)

# 원하는 vote하여 클러스터링하는 방법을 사용할 수 없기 때문이다.

#############################################################################

## Distance functions



euclidean <- function(x1, x2) { 

  dist <- sqrt(sum((x1-x2)^2))

  return(dist)

}



manhattan <- function(x1, x2) {

  dist  <- sum(abs(x1 - x2))

  return(dist)

}



cosine <- function(x1 ,x2) { # 코사인 거리

  dist <- sum(x1 * x2) / (sqrt(sum(x1^2)) * sqrt(sum(x2^2)))

  return(dist)

}



correlation <- function(x1, x2) { 

  dist <- abs(cor(x1, x2))

  return(dist)

}



#Chebyshev Distance

maximum <- function(x1, x2) {

  dist <- max(abs(x1 - x2))

  return(dist)

}



minkowski <- function(x1, x2, m) {

  dist <- (sum((abs(x1 - x2))^m))^(1/m)

  return(dist)

}



# 이 방법은 벡터끼리 일치하지 않는 성분의 개수를 거리로 잡지만 continous variable에서 완벽한 일치는

# 불가능하기 때문에 데이터를 0-1사이로 스케일링 한 후, 반올림한 소수점 첫째 자리가 일치하지 않는 성분

# 의 개수를 거리라고 정의하겠 습니다.

binary <- function(x1, x2) {

  dist <- sum(x1 != x2)

  return(dist)

}





# 분모가 매우 작을 때의 고려가 되지 않음. iris data는 모두 적절하게 크기 때문에 고려하지 않았습니다.

# 작을 수록 가깝다.

canberra <- function(x1, x2) {

  dist <- sum(abs(x1 - x2) / (abs(x1) + abs(x2)))

  return(dist)

}





###############################################################################

##Distance matrix function



dist.mat <- function(data, center, method) {

  k <- nrow(center)

  n <- nrow(data)

  dmat <- matrix(0, k, n)  

  

  if(method == 'euclidean') {

    for(i in 1:k){

      for(j in 1:n){

        dmat[i, j] <- euclidean(data[j, ], center[i, ])

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

    for(i in 1:k){

      for(j in 1:n){

        dmat[i, j] <- cosine(data[j,], center[i,])

      }

    }

  }

  

  if(method == 'correlation')

    for(i in 1:k){

      for(j in 1:n) {

        dmat[i, j] <- correlation(data[j,], center[i,])

      }

    }



  if(method == 'maximum')

    for(i in 1:k){

      for(j in 1:n) {

        dmat[i, j] <- maximum(data[j,], center[i,])

      }

    }

  

  if(method == 'minkowski')

    for(i in 1:k){

      for(j in 1:n) {

        dmat[i, j] <- minkowski(data[j,], center[i,], 3)

      }

    }

  

  if(method == 'binary')

    for(i in 1:k){

      for(j in 1:n) {

        dmat[i, j] <- binary(data[j,], center[i,])

      }

    }

  

  if(method == 'canberra')

    for(i in 1:k){

      for(j in 1:n) {

        dmat[i, j] <- canberra(data[j,], center[i,])

      }

    }

  

  

  return(dmat)

}



##############################################################################



##Simulation Data

data('iris')

aa <- iris[, -5]

aa <- as.matrix(aa)



min.vector <- apply(aa, 2, min)

max.vector <- apply(aa, 2, max)

scale.aa <- t(apply(aa, 1, function(x) (x - min.vector)/(max.vector - min.vector)))

scale.aa <- t(apply(scale.aa, 1, function(x) round(x, 1)))



#모델 비교를 위한 초기 중심점 index 고정/ 집단을 3개로 분류하겠다.

k = 3

init <- sample(1:nrow(aa), 3)



##############################################################################





kmeans <- function(data, k, tol) {

  

  n <- nrow(data)

  p <- ncol(data)

  center.1 <- c()

  #random 하게 초기 중심값 설정.

  center.1 <- data[init, ]

  dmat <- matrix(0, k, n)

  acc <- c()

  keep <- TRUE

  limit <- 1

  

  while(keep) {

    dmat <- dist.mat(data, center.1, 'euclidean')

    min.index <- apply(dmat, 2, which.min)

    

    center.2 = matrix(0, k, p)

    

    for(i in 1:k) {

      set <- data[which(min.index == i), ]

      #중심값 조정 // 만약 전 중심값에 의해 분류된 집합이 없다면 전 중심값을 그대로 가져가

      if(nrow(set) == 0) { center.2[i, ] <- center.1[i, ] }

      else { center.2[i, ] <- apply(set, 2, mean) }

    }

    

    for(i in 1:k){

      acc[i] <- euclidean(center.1[i,],center.2[i,])

    }

    if(limit > tol) { print('Diverge'); break }

    if(sum(acc < 1e-04) == k) { print('Converge'); break }

    center.1 <- center.2

  }

  

  return(min.index)

}





kmedian <- function(data, k, tol) {

  

  n <- nrow(data)

  p <- ncol(data)

  center.1 <- c()

  center.1 <- data[init, ]

  dmat <- matrix(0, k, n)

  acc <- c()

  keep <- TRUE

  limit <- 1

  

  while(keep) {

    dmat <- dist.mat(data, center.1, 'manhattan')

    min.index <- apply(dmat, 2, which.min)

    center.2 = matrix(0, k, p)

    for(i in 1:k) {

      set <- data[which(min.index == i), ]

      if(nrow(set) == 0) { center.2[i, ] <- center.1[i, ] }

      else { center.2[i, ] <- apply(set, 2, median) }

    }

    

    for(i in 1:k){

      acc[i] <- manhattan(center.1[i,],center.2[i,])

    }

    if(limit > tol) { print('Diverge'); break }

    if(sum(acc < 1e-04) == k) { print('Converge'); break }

    center.1 <- center.2

  }

  

  return(min.index)

}





kcosine <- function(data, k, tol) {

  

  n <- nrow(data)

  p <- ncol(data)

  center.1 <- c()

  center.1 <- data[init, ]

  dmat <- matrix(0, k, n)

  acc <- c()

  keep <- TRUE

  limit <- 1

  

  while(keep) {

    dmat <- dist.mat(data, center.1, 'cosine')

    max.index <- apply(dmat, 2, which.max)

    center.2 = matrix(0, k, p)

    for(i in 1:k) {

      set <- data[which(max.index == i), ]

      if(nrow(set) == 0) { center.2[i, ] <- center.1[i, ] }

      else { center.2[i, ] <- apply(set, 2, mean) }

    }

    

    for(i in 1:k){

      acc[i] <- cosine(center.1[i,],center.2[i,])

    }

    limit <- limit + 1

    if(limit > tol) { print('Diverge'); break }

    if(sum(acc > 0.99) == k) { print('Converge'); break }

    center.1 <- center.2

  }

  

  return(max.index)

}





kcorr <- function(data, k, tol) {

  

  n <- nrow(data)

  p <- ncol(data)

  center.1 <- c()

  center.1 <- data[init, ]

  dmat <- matrix(0, k, n)

  acc <- c()

  keep <- TRUE

  limit <- 1

  

  while(keep) {

    dmat <- dist.mat(data, center.1, 'correlation')

    max.index <- apply(dmat, 2, which.max)

    center.2 = matrix(0, k, p)

    for(i in 1:k) {

      set <- data[which(max.index == i), ]

      if(nrow(set) == 0) { center.2[i, ] <- center.1[i, ] }

      else { center.2[i, ] <- apply(set, 2, mean) }

    }

    

    for(i in 1:k){

      acc[i] <- correlation(center.1[i,],center.2[i,])

    }

    limit <- limit + 1

    if(limit > tol) { print('Diverge'); break }

    if(sum(acc >= 0.99) == k) { print('Converge'); break }

    center.1 <- center.2

  }

  

  return(max.index)

}





kmaximum <- function(data, k, tol) {

  

  n <- nrow(data)

  p <- ncol(data)

  center.1 <- c()

  center.1 <- data[init, ]

  dmat <- matrix(0, k, n)

  acc <- c()

  keep <- TRUE

  limit <- 1

  

  while(keep) {

    dmat <- dist.mat(data, center.1, 'maximum')

    min.index <- apply(dmat, 2, which.min)

    center.2 = matrix(0, k, p)

    for(i in 1:k) {

      set <- data[which(min.index == i), ]

      if(nrow(set) == 0) { center.2[i, ] <- center.1[i, ] }

      else { center.2[i, ] <- apply(set, 2, mean) }

    }

    

    for(i in 1:k){

      acc[i] <- maximum(center.1[i,],center.2[i,])

    }

    limit <- limit + 1

    if(limit > tol) { print('Diverge'); break }

    if(sum(acc < 0.1) == k) { print('Converge'); break }

    center.1 <- center.2

  }

  

  return(min.index)

}



##minkowski m 값은 3으로 하였다.



kminkowski <- function(data, k, tol) {

  

  n <- nrow(data)

  p <- ncol(data)

  center.1 <- c()

  center.1 <- data[init, ]

  dmat <- matrix(0, k, n)

  acc <- c()

  keep <- TRUE

  limit <- 1

  

  while(keep) {

    dmat <- dist.mat(data, center.1, 'minkowski')

    min.index <- apply(dmat, 2, which.min)

    center.2 = matrix(0, k, p)

    for(i in 1:k) {

      set <- data[which(min.index == i), ]

      if(nrow(set) == 0) { center.2[i, ] <- center.1[i, ] }

      else { center.2[i, ] <- apply(set, 2, mean) }

    }

    

    for(i in 1:k){

      acc[i] <- minkowski(center.1[i,],center.2[i,], 3)

    }

    limit <- limit + 1

    if(limit > tol) { print('Diverge'); break }

    if(sum(acc < 1e-04) == k) { print('Converge'); break }

    center.1 <- center.2

  }

  

  return(min.index)

}



kbinary <- function(scaled.data, k, tol) {

  

  n <- nrow(scaled.data)

  p <- ncol(scaled.data)

  center.1 <- c()

  center.1 <- scaled.data[init, ]

  dmat <- matrix(0, k, n)

  acc <- c()

  keep <- TRUE

  limit <- 1

  

  while(keep) {

    dmat <- dist.mat(scaled.data, center.1, 'binary')

    for(i in 1:n) {

      if(length(unique(dmat[,i])) == 1) dmat[,i] <- sample(1:3, 3)

    }

    min.index <- apply(dmat, 2, which.min)

    center.2 = matrix(0, k, p)

    for(i in 1:k) {

      set <- scaled.data[which(min.index == i), ]

      if(nrow(set) == 0) { center.2[i, ] <- center.1[i, ] }

      else { center.2[i, ] <- apply(set, 2, mean) }

    }

    

    for(i in 1:k){

      acc[i] <- binary(center.1[i,],center.2[i,])

    }

    limit <- limit + 1

    if(limit > tol) { print('Diverge'); break }

    if(sum(acc == 0) == k) { print('Converge'); break }

    center.1 <- center.2

  }

  

  return(min.index)

}





kcanberra <- function(data, k, tol) {

  

  n <- nrow(data)

  p <- ncol(data)

  center.1 <- c()

  center.1 <- data[init, ]

  dmat <- matrix(0, k, n)

  acc <- c()

  keep <- TRUE

  limit <- 1

  

  while(keep) {

    dmat <- dist.mat(data, center.1, 'canberra')

    min.index <- apply(dmat, 2, which.min)

    center.2 = matrix(0, k, p)

    for(i in 1:k) {

      set <- data[which(min.index == i), ]

      if(nrow(set) == 0) { center.2[i, ] <- center.1[i, ] }

      else { center.2[i, ] <- apply(set, 2, mean) }

    }

    

    for(i in 1:k){

      acc[i] <- cosine(center.1[i,],center.2[i,])

    }

    limit <- limit + 1

    if(limit > tol) { print('Diverge'); break }

    if(sum(acc > 0.99) == k) { print('Converge'); break }

    center.1 <- center.2

  }

  

  return(min.index)

}







#########################################################

## 8가지 기준에 의한 클러스터링 + 

## 투표를 통한 집단 클러스터링 + 가장 적절하지 못하 거리인 binary 빼고 7개만 사용한 클러스터링 



clustering <- list()

clustering[[1]] <- kmeans(aa, 3, 100)

clustering[[2]] <- kmedian(aa, 3, 100)

clustering[[3]] <- kcosine(aa, 3, 100)

clustering[[4]] <- kcorr(aa, 3, 100)

clustering[[5]] <- kmaximum(aa, 3, 100)

clustering[[6]] <- kminkowski(aa, 3, 100)

clustering[[7]] <- kbinary(scale.aa, 3, 100)

clustering[[8]] <- kcanberra(aa, 3, 100)



library(nnet)

# which.is.max // max인 index random하게 뽑아주

# 9번째 clustering은 로 1-8번 방법의 major voting에 의해 결정됩니다.

clustering[[9]] <- rep(0, 150)

lv <- length(clustering) + 1

n <- length(clustering[[1]])

  

for(i in 1:n) {

  vote <- rep(0, 3)

  for(j in 1:8) {

    if(clustering[[j]][i] == 1) vote[1] <- vote[1] + 1

    if(clustering[[j]][i] == 2) vote[2] <- vote[2] + 1

    if(clustering[[j]][i] == 3) vote[3] <- vote[3] + 1

  }

  clustering[[lv]][i] = which.is.max(vote)

}



# 10번째 clustering은 1-8방법에서 binary 방법을 제외한 vote로 결정됩니다.

# binary 방법을 뺀 이유는 거리가 [0~4]에 한정되서 모두 같은 거리인 경우가 많았기 때문입니다.

# 또한 유일하게 100번 시도를 했을 때 Diverge 하는 방법이였다.

clustering[[10]] <- rep(0, 150)

lv <- 10

n <- length(clustering[[1]])

cl <- c(1:6, 8)



for(i in 1:n) {

  vote <- rep(0, 3)

  for(j in 1:7) {

    if(clustering[[cl[j]]][i] == 1) vote[1] <- vote[1] + 1

    if(clustering[[cl[j]]][i] == 2) vote[2] <- vote[2] + 1

    if(clustering[[cl[j]]][i] == 3) vote[3] <- vote[3] + 1

  }

  clustering[[10]][i] = which.is.max(vote)

}



clustering[[10]]





##############################################################################

##Evaluation 10 clustering

# Silhouette와 Dunn Index를 평가 척도로 사용합니다.



library(cluster)

library(clValid)



par(mfrow=c(1, 2))

method <-c('euclidean','manhattan','cosine','correlation','maximum','minkowski','binary', 'canberra', 'vote1','vote2') 



# 1) Silhouette



make.plot <- function(cluster) {

  mean.sil <- rep(0, 10)

  for(i in 1:10) {

    plot(silhouette(cluster[[i]], dist=dist(aa)), main = method[i])

    mean.sil[i]<- mean(silhouette(clustering[[i]], dist=dist(aa))[, 'sil_width'])

  }

  return(mean.sil)

}



make.plot(clustering)



# binary 방법이 매우 안 좋으며 kmeans 모델이 가장 좋다. vote1 보다 binary를 뺀 vote2가 성능이 더 좋은 것으로 보아 

# 안좋은 것들을 제거 하면 더 성능이 좋아질 수도 있겠다. 평균 실루엣이 0.55이상인 것들로만 투표해보자지

# kmeans kmedian kmaximum kminkowski



clustering[[11]] <- rep(0, 150)

lv <- 11

n <- length(clustering[[1]])

cl <- c(1, 2, 5, 6)



for(i in 1:n) {

  vote <- rep(0, 3)

  for(j in 1:4) {

    if(clustering[[cl[j]]][i] == 1) vote[1] <- vote[1] + 1

    if(clustering[[cl[j]]][i] == 2) vote[2] <- vote[2] + 1

    if(clustering[[cl[j]]][i] == 3) vote[3] <- vote[3] + 1

  }

  clustering[[11]][i] = which.is.max(vote)

}

               

clustering[[11]]

mean(silhouette(clustering[[11]], dist=dist(aa))[, 'sil_width'])

#k-means 이상으로 올라가진 않지만 성능이 더 좋아졌다.



## 2) Dunn Index

#dist를 사용한다. 모델의 평가는 다섯 가지 거리를 기준으로 하겠다.



dunn.method <-c('euclidean','manhattan','maximum', 'minkowski', 'canberra')



dunn.index <- rep(0, 11)

for(k in 1:5) {

  for (i in 1:11){

    d <- dist(aa, method = dunn.method[k])

    dunn.index[i] <- dunn(d, clustering[[i]])

  }

  cat(dunn.method[k], '\n', dunn.index, '\n')

}





#minkowski 모델과 vote1 모델이 가장 로버스트하게 좋다.