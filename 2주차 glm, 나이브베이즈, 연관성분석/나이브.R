rm(list=ls())
setwd("C:/Users/USER/Desktop/투빅스/2주차 나이브베이즈/test_X")
train_x = read.csv("train_X.csv")
train_y = read.csv("train_y.csv")
test_x = read.csv("test_X.csv")
test_y = read.csv("test_y.csv")
train_y = train_y[,2] # 필요없는 행 제거

# convert_x : train_x 데이터가 yes / no 로 구성되어있는데, 확률계산을 위해 1 / 0 으로 바꿔주는 함수
convert_x = function(x){
  x = x[,-1] # 필요없는 행 제거
  x = ifelse(x=="Yes",1,0) 
  return(x)
}

# model : train set 에서 조건부 확률과 사전확률을 구해주는 함수
model = function(train_x,train_y){
  x = convert_x(train_x)
  
  # 각 단어가 포함되었을때 ham / spam 으로 분류된 빈도 
  # 이때 빈도가 0인 것이 있으면 해당 단어가 포함된 것은 항상 확률이 0이기 때문에 laplace smoothing 을 해줌 (+1)
  ham = apply(x[which(train_y=="ham"),],2,sum) + 1   
  spam = apply(x[which(train_y=="spam"),],2,sum) + 1
  
  # 사전확률 구하기 (train_y 중에 ham / spam 이 몇번 나왔는지)
  # 확률값이 너무 작아 비교하기 힘들기 때문에 log 변환을 해줌
  target_h = log(length(train_y[which(train_y=="ham")])/length(train_y)) 
  target_s = log(length(train_y[which(train_y=="spam")])/length(train_y))
  
  # 조건부확률 구하기 ( P(각 단어가 ham인 빈도 | ham ) )
  # 이때도 확률값이 너무 작기 때문에 log 변환을 해줌
  prob_h = log(ham/sum(ham)) 
  prob_s = log(spam/sum(spam))
  
  # 사전확률과 조건부 확률을 return 
  return(list(target_h,target_s,prob_h,prob_s))
}
mm = model(train_x,train_y)

# predict : test_x 를 넣었을때 ham / spam 으로 분류해주는 함수
predict = function(test_x){
  test_x = convert_x(test_x)
  
  pred = c() # ham / spam 을 저장할 변수 (return 해줄 값)
  for(i in 1:nrow(test_x)){
    
    # 사후확률 구하기 : 사전확률 * 조건부확률
    # 앞에서 확률값들에 log변환을 해주었기 때문에 합으로 계산
    pham = sum(mm[[3]][test_x[i,]==1]) + mm[[1]] 
    pspam = sum(mm[[4]][test_x[i,]==1]) + mm[[2]]
    
    # 사후확률값을 비교, 큰쪽으로 분류
    if(pham > pspam){ 
      pred[i]="ham"
    }else{ 
      pred[i]="spam"
    }
  }
  return(pred)
}

# 정확도 계산 
sum(predict(test_x)==test_y[,2])/length(test_y[,2])