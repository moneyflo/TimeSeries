# LAB 6: 계절성이 있는 자료에서의 Seasonal ARIMA 모형적합

library(TSA)
data(co2)
co2 <- ts(co2)
ts.plot(co2)
# 뚜렷한 계절성과 선형추세가 관찰됨. 분산은 안정되어 보임.
# 계절성이 뚜렷하여 sacf값이 주기의 배수 부분에서 천천히 감소할 것으로 보임.
# 따라서 계절차분 필요.
# 또한 선형추세가 있기 때문에 계절차분후에는 기대값이 0이 아닌
# 시계열 형태가 될 것으로 예상됨.

## 모형 식별
acf(co2, lag.max=48)
# 예상과 동일하게 lag 1,2,3년에서의 sacf값이 천천히 감소
ts.plot(Dc <-diff(co2, 12))
# 계절 차분된 자료는 평균이 0이 아닌 시계열로 보임.
# 정상 or 확률적 판단해야
layout(t(1:2))
acf(Dc); pacf(Dc)
# 선형적으로 감소하는것으로 보아 추가적 차분 수행
layout(1)
ts.plot(dDc<-diff(Dc))

layout(t(1:2))
acf(dDc, lag.max=36); pacf(dDc, lag.max=36)

# Aic를 기준으로 차분과 계절차분된 자료의 p,q,P,Q를 결정
library(forecast)
aic <- 1000

for (p in 0:2) for (q in 0:2) for (P in 0:1) for (Q in 0:1){
  aic.tmp<-AIC(Arima(co2, order=c(p,1,q), seasonal = list(order = c(P,1,Q), period = 12)))
  if (aic.tmp<aic){
    aic <- aic.tmp
    cat("p=", p, "q=", q, "P=", P, "Q=", Q, " AIC=", aic, "\n")
  }
}

# 계절차분과 일반차분, 즉 두 번의 차분이 적용되는 경우에는
# Arima함수 사용시 include.drift 옵션을 사용하지 않음.(사용하더라도 적용이 안됨.)

## 잔차분석
co2.fit <- Arima(co2, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
r <- co2.fit$res
layout(matrix(c(1,2,1,3),2,2))
ts.plot(r); acf(r); pacf(r)

Box.test(r, lag=12, type="Lj")

# cf.위 잔차의 ARIMA(0,1,1)(0,1,1)_12 모형 적합도 검정
Box.test(r, lag=12, type="Lj", fitdf=1+1)
# p-value가 크므로 맞다고 볼수있음.

## 모수추정 결과
co2.fit

hat.co2 <- co2.fit$fitted  # 적합값(추정된 모형에 의해서 계산된 값)
layout(1) 
ts.plot(co2)
lines(hat.co2, col="red", lty=2) # 적합값 그래프

## 예측
plot(forecast(co2.fit, h=12))
