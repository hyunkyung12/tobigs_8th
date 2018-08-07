rm(list = ls())

setwd("C:/Users/USER/Desktop/투빅스/4주차 트리")

data = read.csv("mroz.csv")
str(data)
attach(data)

## train / test 나누기
set.seed(1234)
intrain = createDataPartition(y=data$work, p=0.7, list=FALSE) 
train = data[intrain,]
test  = data[-intrain,]

table(train$work)/sum(table(train$work))
table(test$work)/sum(table(test$work))

## gini function
gini = function(train,y,x){

  gy = train[,y]
  gx = train[,x]
  
  if(is.factor(gx)){
    #print("factor")
    return("factor")

  }else{
    
    lev = levels(gy) # no yes
    quan = summary(gx)[2:5] # 1st Median Mean 3rd
    result = c()
    
    for(i in 1:4){
      temp = ifelse(gx<=quan[i],0,1) # quantile 기준으로 왼쪽은 0 오른쪽은 1
      gl = gy[which(temp==0)] 
      gr = gy[which(temp==1)]
      pl = table(gl)[1]/length(gl) # no를 기준으로 p계산 / yes는 1-p
      pr = table(gr)[1]/length(gr) 
      w = length(gl)/length(gx)
      result[i] = (1-(pl)^2-(1-pl)^2)*w + (1-(pr)^2-(1-pr)^2)*(1-w) 
    }
    #print(which.min(result))
    return(which.min(result))
  }
}

for(i in 2:18){
  gini(train,1,i)
}

## tree function

tree = function(train,test,y,x){

  try = train[,y]
  trx = train[,x]
  tex = test[,x]
  
  g = gini(train,y,x)
  result = c()
  
  if(g=="factor"){
    
    lev = levels(trx) # no yes
    temp = table(try[which(trx==lev[1])])

    left = names(temp)[which.max(temp)]
    right = lev[lev!=left]

    result[which(tex=="no")] = left
    result[which(tex=="yes")] = right
    
  }else{
    
    trq = summary(trx)[2:5] # train set 의 quantile
    temp1 = ifelse(trx<=trq[g],0,1) # 왼쪽0 오른쪽 1
    tl = try[which(temp1==0)] 
    tr = try[which(temp1==1)]
    left = names(table(tl))[which.max(table(tl))] # 왼쪽에서 yes no 를 비교해서 더 많은 것을 반환
    right = names(table(tr))[which.max(table(tr))] 
    
    temp2 = ifelse(tex<=trq[g],0,1) # train set 의 quantile로 test set 비교, 왼쪽 0 오른쪽 1
    result[which(temp2==0)] = left # 왼쪽은 train에서 지정했던 왼쪽변수
    result[which(temp2==1)] = right
  }
  
  
  return(result)
}

## randomforest function

randomforest = function(train,test,mtry){
  
  mtry=9
  if(mtry%%2==0){
    return("홀수로 뽑아주세요")
  }else{
    rand = sample(2:length(names(test)),mtry)
    vote = matrix(0,mtry,nrow(test)) 

    for(i in 1:mtry){
      vote[i,] = tree(train,test,1,rand[i]) # vote 의 각 행별로 결과값 저장
    }
    
    vtable = apply(vote,2,table) # 각 인덱스에 대한 table 생성 (list)
    result = c()
    for(i in 1:length(vtable)){
      result[i] = names(vtable[[i]])[which.max(vtable[[i]])] # 더 많은쪽의 name 을 반환
    }
  }
  
  return(result)
}

randomforest(train,test,9)
