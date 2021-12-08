# LAB 6[2]:계절성이 있는 자료에서의 Seasonal ARIMA 모형 적합 2

data("AirPassengers")
AP <- AirPassengers
ts.plot(AP)
# 뚜렷한 계절성과 선형추세가 관찰됨. 분산 안정화도 필요해보임.

ts.plot(lAP <- log(AP))

## 모형식별
acf(lAP, lag.max=48)
# lag 1,2,3년에서 sacf값이 천천히 감소함

ts.plot(D.lAP<-diff(lAP, 12))
# 계절 차분된 자료는 평균이 0이 아닌 시계열로 보임.
# sacf, spacf를 확인해볼 필요가 있어보임
layout(t(1:2))
acf(D.lAP)
pacf(D.lAP)
# acf가 선형적으로 감소하므로 확률적 추세로 보고 추가적인 차분

layout(1)
ts.plot(dD.lAP<-diff(D.lAP))

layout(t(1:2))
acf(dD.lAP, lag.max=36); pacf(dD.lAP, lag.max=36)

library(forecast)
aic <- 1000
for (p in 0:2) for (q in 0:2) for (P in 0:2) for (Q in 0:2){
  try({aic.tmp<-AIC(Arima(lAP, order=c(p,1,q), seasonal=list(order=c(P,1,Q)
      , period=12)))}, TRUE)
  if (aic.tmp<aic){
    aic<-aic.tmp
    cat("p=",p,"q=",q,"P=",P,"Q=",Q,"AIC=",aic, "\n")
  }
}

## 잔차분석
library(forecast)
lAP.fit<-Arima(lAP, order=c(0,1,1), seasonal=list(order=c(0,1,1),
              period=12))
r <-lAP.fit$res
layout(matrix(c(1,2,1,3),2,2))
ts.plot(r); acf(r); pacf(r)

Box.test(r, lag=12, type="Lj")

## 모수추정결과
lAP.fit

layout(1)
hat.lAP <- lAP.fit$fitted # 적합값
ts.plot(AP)
lines(exp(hat.lAP), col="red", lty=2)

plot(forecast(lAP.fit, h=24))


lAP.fore <- forecast(lAP.fit, h=24)
AP.mean <- ts(exp(lAP.fore$mean), start=1961, freq=12)
AP.U95 <- ts(exp(lAP.fore$upper[,2]), start=1961, freq=12)
AP.L95 <- ts(exp(lAP.fore$lower[,2]), start=1961, freq=12)
ts.plot(AP, xlim=c(1949, 1960+3), ylim=c(80,max(AP.U95)+50))
lines(AP.mean, col="blue")
lines(AP.U95, col="blue", lty=2)
lines(AP.L95, col="blue", lty=2)

## auto.arima
auto.arima(lAP)

plot(forecast(auto.arima(lAP), h=24))

lAP.fore <- forecast(auto.arima(lAP), h=24)
AP.mean <- ts(exp(lAP.fore$mean), start=1961, freq=12)
AP.U95 <- ts(exp(lAP.fore$upper[,2]), start=1961, freq=12)
AP.L95 <- ts(exp(lAP.fore$lower[,2]), start=1961, freq=12)
ts.plot(AP, xlim=c(1949, 1960+3), ylim=c(80,max(AP.U95)+50))
lines(AP.mean, col="blue")
lines(AP.U95, col="blue", lty=2)
lines(AP.L95, col="blue", lty=2)
