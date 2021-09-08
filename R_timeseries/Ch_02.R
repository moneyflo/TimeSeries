# Chapter 02 Fundamental Concepts

library(TSA)

## Exhibit 2.1 Time Series Plot of a Random Walk
win.graph(width=4.875, height=2.5, pointsize=8)
data(rwalk) # rwalk contains a simulated random walk
plot(rwalk, type='o', ylab='Random Walk')

# Path 여러개를 발생 시킨후 겹쳐 그리기
RW <- function(n){
  X <- rep(0, n)
  e <- rnorm(n, 0, 1)
  
  for(i in 2:n) X[i] <- X[i-1] + e[i]
  X
}

plot(RW(100), type='l', ylim=c(-30,30))
for (i in 2:10) lines(RW(100),col=i)

# random walk는 행적이 불규칙함을 볼 수 있음

# 500개
plot(RW(100), type='l', ylim=c(-30, 30))
for (i in 2:500) lines(RW(100), col=i)


# A Moving Average
# 분산이 일정함
ex2 <- function(n){
  X<-rep(0,n)
  e<-rnorm(n,0,1)
  
  for(i in 3:n) X[i]<-0.5*e[i-1]+0.5*e[i-2]
  X
}

plot(ex2(100), type="l", ylim=c(-2,2))
for (i in 2:3) lines(ex2(100), col=i)

# 100 번
plot(ex2(100), type="l", ylim=c(-3,3))
for (i in 2:100) lines(ex2(100), col=i)
