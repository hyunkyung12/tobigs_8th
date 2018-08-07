rm(list=ls())
setwd("C:/Users/USER/Desktop/투빅스/4주차 앙상블")

data = read.csv("churn.csv")
str(data) 

## train / test set 나누기

library(caret)
set.seed(1000)
intrain = createDataPartition(y=data$churn, p=0.7, list=F)
train = data[intrain,]
test = data[-intrain,]

## 1-1 . tree 

library(tree)
train1 = train[,-1] # factor predictors must have at most 32 levels 의 오류가 나서 state변수 삭제
tree = tree(churn~.,data=train1)
tree.control = list(mincut = 1, minsize = 3, nmax = 50) 
tree2 = tree(churn~.,data=train1,control = tree.control) # tree is too big

# pruning
tree.prune = cv.tree(tree,FUN=prune.misclass) # 자꾸 세션이 종료되어 그만두었습니다ㅠㅠ
plot(tree.prune) 


## 1-2 . rpart
# 자꾸 세션이 종료되어 그만두었습니다ㅠㅠ

library(rpart)
rpart_tree = rpart(churn~., data= train)

## 1-3 . party 

library(rpart)
library(party)
rpart_tree = rpart(churn~., data= train)

# tree : depth에 제한을 두지 않음
tree = ctree(churn~., data=train)
plot(tree)
str(train)
pred1 = predict(tree, newdata= test, type="response")
confusionMatrix(pred1, test$churn) # Accuracy : 0.9493

# tree2 : depth=1 인 tree
# tree는 accuracy 가 높지만 overfitting 되었을 가능성이 높기 때문
tree2 = ctree(churn~.,data=train,controls = ctree_control(minsplit = 20,mtry = 0, maxdepth = 1)) 
plot(tree2)

pred2 = predict(tree2, newdata= test, type="response")
confusionMatrix(pred2, test$churn) # Accuracy : 0.8586

# tree3 : depth=5 인 tree
tree3 = ctree(churn~.,data=train,controls = ctree_control(minsplit = 20,mtry = 0, maxdepth = 5)) 
plot(tree3)

pred3 = predict(tree3, newdata= test, type="response")
confusionMatrix(pred3, test$churn) # Accuracy : 0.9326

### tree3 은 accuarcy도 tree와 별로 차이가 나지 않고, 가지의 depth는 줄어든 모델

### 2. Rf 분석
library(randomForest)
ranfo = randomForest(churn~.,importance=TRUE,data=train)
ranfo$ntree # 500개
ranfo2 = randomForest(churn~.,importance=TRUE,data=train, ntree=10)  
ranfo3 = randomForest(churn~.,importance=TRUE,data=train, ntree=100)
ranfo4 = randomForest(churn~.,importance=TRUE,data=train, ntree=300)

par(mfrow=c(2,2))
importance(ranfo)
varImpPlot(ranfo)
varImpPlot(ranfo2)
varImpPlot(ranfo3)
varImpPlot(ranfo4)


ranfo_pred = predict(ranfo, newdata = test)
confusionMatrix(ranfo_pred, test$churn) #  Accuracy : 0.9306
ranfo_pred2 = predict(ranfo2, newdata = test)
confusionMatrix(ranfo_pred2, test$churn) #  Accuracy : 0.9253
ranfo_pred3 = predict(ranfo3, newdata = test)
confusionMatrix(ranfo_pred3, test$churn) #  Accuracy : 0.9293
ranfo_pred4 = predict(ranfo4, newdata = test)
confusionMatrix(ranfo_pred4, test$churn) #  Accuracy : 0.9253

### ntree가 클수록 정확도가 높아지는것은 아님
### accuarcy 기준으로 보면 ranfo3이 가장 높은데, accuarcy만 보고 좋다고 판단하는게 맞는지는 잘 모르겠습니다..ㅠㅠ

### 3. bagging

library(ipred)
library(doBy)

# bagg_mod : nbag=50
bagg_mod = bagging(churn~., data=train, nbagg=50, coob=T)
bagg_mod$mtrees[[1]]
bagg_mod$err
bagg_pred = predict(bagg_mod,test[,-20])
result = table(bagg_pred, test[,20])
sum(dist(result))/sum(result) # 0.6709

# bagg_mod2 : nbag=100
bagg_mod2 = bagging(churn~., data=train, nbagg=100, coob=T)
bagg_mod2$err
bagg_pred2 = predict(bagg_mod2,test[,-20])
result2 = table(bagg_pred2, test[,20])
sum(dist(result2))/sum(result2) # 0.8609

# bagg_mod3 : nbag=300
bagg_mod3 = bagging(churn~., data=train, nbagg=300, coob=T)
bagg_mod3$err
bagg_pred3 = predict(bagg_mod3,test[,-20])
result3 = table(bagg_pred3, test[,20])
sum(dist(result3))/sum(result3) # 0.6659

### nbagg 가 클수록 오분류율이 낮아지는것은 아니다
### 여기서는 bagging을 10번 했을때가 좋은 모델이라고 할 수 있다.

### 4. boosting

library(adabag)

boost_mod = boosting(churn~., data=train, mfinal=10, control=rpart.control(maxdepth = 5))
boost_mod$weights 
boost_mod$votes[1:10,] 
boost_mod$importance 

boost_pred = predict(boost_mod, newdata=test[,-20], type="class") 
result = table(boost_pred$class, test[,20])
sum(dist(result))/sum(result) #0.6791

boost_mod2 = boosting(churn~., data=train, mfinal=30, control=rpart.control(maxdepth = 5))

boost_pred2 = predict(boost_mod2, newdata=test[,-20], type="class") 
result2 = table(boost_pred2$class, test[,20])
sum(dist(result2))/sum(result2) #0.8351

### 부스팅을 많이하면 test셋에 대해 예측했을때 정확도가 증가하는것을 볼 수 있다
### boost_mod2는 잘 학습된 모델이라고 할 수 있다

### 5. h2o package

library(h2o)
h2o.init()
str(train)

dim(train)
dim(test)

train = as.h2o(train)
test = as.h2o(test)

y = "churn"
x = names(train[-20])

nfolds = 10

## 1. GBM + RF

my_gbm = h2o.gbm(x = x,
                  y = y,
                  training_frame = train,
                  distribution = "bernoulli",
                  ntrees = 10,
                  max_depth = 3,
                  min_rows = 2, 
                  learn_rate = 0.2,
                  nfolds = nfolds, 
                  fold_assignment = "Modulo", 
                  keep_cross_validation_predictions = TRUE, 
                  seed = 1)

my_rf = h2o.randomForest(x = x,
                          y = y,
                          training_frame = train,
                          ntrees = 50,
                          nfolds = nfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)

ensemble = h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                model_id = "my_ensemble_binomial",
                                base_models = list(my_gbm@model_id, my_rf@model_id)) 


perf = h2o.performance(ensemble, newdata = test)
perf_gbm_test = h2o.performance(my_gbm, newdata = test)
perf_rf_test = h2o.performance(my_rf, newdata = test)
best_auc_test = max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test))
ensemble_auc_test = h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", best_auc_test)) # 0.9263
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test)) # 0.9236

### h2o 패키지를 사용해보려고 했지만 패키지에 대한 이해를 다 하지 못해 해석에 어려움이 있습니다..ㅠㅠ