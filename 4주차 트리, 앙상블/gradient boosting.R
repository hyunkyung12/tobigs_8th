#simple gradient boosting
install.packages("lars")
library("lars")
data("diabetes")
niter = 100
learn = 0.05 #학습률
x = diabetes$x #
y = diabetes$y #실제값
y = as.numeric(y>mean(y))
yhat = rep(0.0,length(y)) #예측값 
w = matrix(0.2,11,1) #가중치 
log_loss = c()

for (i in seq(niter)){
  yhat = 1/(1+exp(-cbind(1,x)%*%w))
  dyhat = exp(-cbind(1,x)%*%w)
  loss = t(dyhat*yhat)%*%(-y*cbind(1,x)) + t(dyhat*yhat)%*%((1-y)*cbind(1,x))
  log_loss[i] = loss
  w = w + t(learn*loss)
}  
plot(log_loss)
