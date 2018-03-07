# 시각화용 코드입니다.
smoothing <- function(vec)
{
  vec1 <- c(vec[-1], vec[length(vec)])
  vec2 <- c(vec[1], vec[-length(vec)])
  return((vec1 + vec2 + vec) / 3)
}

visualize_loss <- function(loss_log)
{
  for(i in 1:100)
  {
    loss_log <- smoothing(loss_log)
    plot(loss_log)
    Sys.sleep(0.01)
  }
}
# 여기까지 그냥 실행시켜 주세요!

# 단순회귀 구현
x <- rnorm(1000, 0)
y <- 2 * x + 1 #+ rnorm(1000,0) * 0.5
w <- 0.001
b <- 0.001
lr <- 0.01
loss_log <- c()
for(i in 1:length(x))
{
  ###                 ###
  # 여기를 채워 주세요! #
  ###                 ###
}
visualize_loss(loss_log)

#다중회귀 구현(변수 11개)
x <- as.data.frame(matrix(rnorm(3000,0), nrow = 300, ncol = 10))
y <- x$V1 * 1 + x$V2 * 2 + x$V3 * 3 + x$V4 * 4 + x$V5 * 5 + x$V6 * 6 + x$V7 * 7 + x$V8 * 8 + x$V9 * 9 + x$V10 * 10 + 11
w <- rnorm(10,0)
b <- rnorm(1,0)
lr <- 0.01
loss_log <- c()
for(i in 1:nrow(x))
{
  ###                 ###
  # 여기를 채워 주세요! #
  ###                 ###
}
visualize_loss(loss_log)

#다중회귀 구현(변수 n개)
linear_regression <- function(n)
{
  x <- as.data.frame(matrix(rnorm(30*n*n,0), nrow = 30*n, ncol = n))
  y <- rowSums(x*1:n) + (n+1)
  w <- rnorm(n,0)
  b <- rnorm(1,0)
  lr <- 0.01
  loss_log <- c()
  for(i in 1:nrow(x))
  {
    ###                 ###
    # 여기를 채워 주세요! #
    ###                 ###
  }
  visualize_loss(loss_log)
}

linear_regression(12)
