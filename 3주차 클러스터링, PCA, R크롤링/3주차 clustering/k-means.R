data <- iris
str(data) # target = species
data1 <- data[-5] 
cosine(data1,3,10)
# 1. 유클리드 거리
euc = function(v1,v2){
  result = sum((v1-v2)^2)
  return(result)
} 

k_means(data1,3,10)
kmeans(data1,3,10)$cluster
k_means = function(data,k,n){
  center = c()
  center = data[c(sample(1:nrow(data),k)),]
  dmat = matrix(rep(0,k*nrow(data)),nrow=k)
  
  for(i in 1:k){
    for(j in 1:nrow(data)){
        dmat[i,j] = sqrt(euc(data[j,],center[i,]))
    }
  }
  
  min = apply(dmat,2,which.min)
  
  center2 = matrix(0,k,ncol(data))

  for(i in 1:k){
    center2[i,] = colMeans(data[which(min==i),])
  }
  
  for(i in 1:k){
  accu = sqrt(euc(center[i,],center2[i,]))
  }
  
  while(accu>1e-04){
    center = center2
    dmat = matrix(rep(0,k*nrow(data)),nrow=k)
    
    for(i in 1:k){
      for(j in 1:nrow(data)){
        dmat[i,j] = sqrt(euc(data[j,],center[i,]))
      }
    }
    min = apply(dmat,2,which.min)
    
    center2 = matrix(0,k,ncol(data))
    
    for(i in 1:k){
      center2[i,] = colMeans(data[which(min==i),])
    }
    
    for(i in 1:k){
      accu = sqrt(euc(center[i,],center2[i,]))
    }
  }
  return(min)
}


rm(list=ls())

k_median(data1,3,10)

man = function(v1,v2){ # 맨하탄거리 구하기
  result = sum(abs(v1-v2)) 
  return(result)
} 

k_median = function(data,k,n){

  center = c()
  center = data[c(sample(1:nrow(data),k)),]
  dmat = matrix(rep(0,k*nrow(data)),nrow=k) # 거리행렬
  
  # 맨하탄 거리를 이용해 거리행렬을 만듦
  for(i in 1:k){
    for(j in 1:nrow(data)){
      dmat[i,j] = man(data[j,],center[i,])
    }
  }
  
  # 군집을 나타내는 벡터
  min = apply(dmat,2,which.min)
  
  # 두번째 센터를 정함
  center2 = matrix(0,k,ncol(data))
  
  for(i in 1:k){
    for(j in 1:ncol(data)){
    center2[i,j] = median(data[which(min==i),j])  
    }
  }
  
  # 정확도를 구함
  accu = c()
  for(i in 1:k){
    accu[i] = man(center[i,],center2[i,])
  }

  # accu 벡터의 모든 원소가 1e-04 보다 큰 경우 앞의 과정을 반복
  
  while(sum(accu < 1e-04)==0){
    center = center2
    dmat = matrix(rep(0,k*nrow(data)),nrow=k)
    
    for(i in 1:k){
      for(j in 1:nrow(data)){
        dmat[i,j] = man(data[j,],center[i,])
      }
    }
    
    min = apply(dmat,2,which.min)
    center2 = matrix(0,k,ncol(data))

    for(i in 1:k){
      for(j in 1:ncol(data)){
        center2[i,j] = median(data[which(min==i),j])  
      }
    }
    
    accu = c()
    for(i in 1:k){
      accu[i] = man(center[i,],center2[i,])
    }
  }
  return(min)
}

var(data1)
result <- k_means(data1,3,10)
table(result,iris[,5])

cos = function(v1,v2){ # 맨하탄거리 구하기
  result = sum(v1*v2) / sqrt(v1*v1) * sqrt(v2*v2) 
  return(result)
} 

cosine = function(data,k,n){
  
  center = c()
  center = data[c(sample(1:nrow(data),k)),]
  dmat = matrix(rep(0,k*nrow(data)),nrow=k) # 거리행렬
  
  # 맨하탄 거리를 이용해 거리행렬을 만듦
  for(i in 1:k){
    for(j in 1:nrow(data)){
      dmat[i,j] = sum(cos(data[j,],center[i,]))
    }
  }
  
  # 군집을 나타내는 벡터
  min = apply(1-dmat,2,which.min)
  
  # 두번째 센터를 정함
  center2 = matrix(0,k,ncol(data))
  
  for(i in 1:k){
      center2[i,] = colMeans(data[which(min==i),])  
  }
  
  # accu 벡터의 모든 원소가 1e-04 보다 큰 경우 앞의 과정을 반복
  
  while(sum(center==center2)>nrow(data)*0.95){
    center = center2
    dmat = matrix(rep(0,k*nrow(data)),nrow=k)
    
    for(i in 1:k){
      for(j in 1:nrow(data)){
        dmat[i,j] = sum(cos(data[j,],center[i,]))
      }
    }
    
    min = apply(dmat,2,which.min)
    center2 = matrix(0,k,ncol(data))
    
    for(i in 1:k){
        center2[i,] = median(data[which(min==i),])  
    }
    
  }
  return(min)
}

