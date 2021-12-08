# LAB 5: Arima{forecast} 2

### EX 4
X <- arima.sim(n=200, list(order=c(1,1,0), ar=0.3))[1:200]
ts.plot(X)

Y <- 1000 + X
ts.plot(Y)

library(forecast)
Arima(X, order=c(1,1,0))

Arima(Y, order=c(1,1,0))

# 차분하면 상수항 영향 안받는듯

# 잔차분석
Y.fit <- Arima(Y, order=c(1,1,0))
r <- Y.fit$res
ts.plot(r)

# acf, pacf 확인
layout(t(1:2))
acf(r); pacf(r)

# Box test
Box.test(r, lag=12, type="Lj")

# 예측
Y.fore <- forecast(Y.fit, h=30)
layout(1)
plot(Y.fore)


### EX 5
# 선형회귀에 오차항으로 ARIMA(1,1,0) 사용한 자료
tt <- 1:200
Y <- 100 + 0.2*tt + X
ts.plot(Y)

# Yt 차분은 ARIMA(1,0,0) with non-zero mean으로 식별
Y.fit <- Arima(Y, include.drift=T, order=c(1,1,0))
Y.fit
# Xt에 해당하는 부분에 차분이 들어가서 절편은 추정 안됨

## cf.
Arima(diff(Y), order=c(1,0,0))

# 잔차분석
r <- Y.fit$res
ts.plot(r)

layout(t(1:2))
acf(r); pacf(r)

Box.test(r, lag=12, type="Lj")

# 최종모형: 상수항이 있는 ARIMA(1,1,0) 모형
# (1 -piB)(1 - B)(Yt - mu) = et
# 여기서 et는 W.N.(0, sigma^2)
# 예측값 ^pi = 0.311, ^mu = 0.02, ^sigma**2 = 0.896

# 예측
layout(1)
plot(forecast(Y.fit, h=30))


## 지난 실습 예제
Y <- read.table("C:/Users/naseo/mine/study/R_timeseries/ex3-2.txt", header=T)[,1]
ts.plot(Y)

# 지난 실습에서 위 자료는 선형추세 + 확률적 추세로 판단되었음.
# 확률적 추세부분에 대한 모형 식별

AIC.table <- matrix(0,4,4)
for (p in 0:3)for(q in 0:3){
  AIC.table[p+1, q+1] <- AIC(Arima(diff(Y), order=c(p,0,q)))
}

rownames(AIC.table) <- paste("p=",0:3,sep="")
colnames(AIC.table) <- paste("q=",0:3,sep="")
AIC.table
min(AIC.table)

# 차분Yt는 Arima(3,0,2)가 적절해보임

# 잔차분석
Y.fit <- Arima(Y, include.drift=T, order=c(3,1,2))
r <- Y.fit$res
ts.plot(r)

layout(t(1:2))
acf(r); pacf(r)

Box.test(r, lag=12, type="Lj")

Y.fit

# 위 잠정모델을 최종모형으로 확정.
# 추정된 모수는 위 실행에서 볼수있음

# 예측
layout(1)
plot(forecast(Y.fit, h=30))