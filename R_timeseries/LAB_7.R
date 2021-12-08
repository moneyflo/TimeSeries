# LAB 7: GARCH 모형 적합 예제

data(EuStockMarkets)
EuStockMarkets[1:10,]

ts.plot(stock<-EuStockMarkets[,2])

# 일별 주가자료, 주식, 환율, 원자재 등 금융자산의 가격 시계열 자료는
# 거의 대부분의 경우 log 변환, 차분후 분석해야함

## log 차분 변환
ts.plot(log.r <- diff(log(stock))*100)

# acf, pacf 확인
layout(t(1:2))
acf(log.r); pacf(log.r)
# 변동성 집중이 관찰되고, 변환된 자료의 acf와 pacf는 w.n.처럼 보임.

layout(t(1:2))
acf(log.r^2); pacf(log.r^2)
# 변환된 자료^2의 acf, pacf는 선형종속관계가 존재함.
# 위 특징으로부터 주어진 자료는 GARCH모형으로 적합 가능해보임

## 모형식별
library(tseries)
get.pq <- function(X, p.max=2, q.max=2){
  options(warn=-1)
  aic<-aic.tmp<-10000
  for (p in 0:p.max) for (q in 0:q.max){
    try(aic.tmp<-AIC(garch(X, order=c(q,p), trace=F)), T)
      if (aic.tmp<aic){
        aic<-aic.tmp
        cat("p=",p,"q=",q,"AIC=",aic,"\n")
      }
  }
  options(warn=0)
}

get.pq(log.r)

# AIC를 기준으로 GARCH(1,2)모형을 잠정모형으로 선택
fit <- garch(log.r, order=c(2,1), trace=F)
summary(fit)
# b2가 유의하지 않으므로 GARCH(1,1)모형 채택.

fit <- garch(log.r, order=c(1,1), trace=F)

## 잔차분석
r <- fit$residuals
r <- r[-1] # r[!is.na(r)]
layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow=T))
ts.plot(r)
acf(r); pacf(r)
acf(r^2); pacf(r^2)

Box.test(r^2, lag=25, type="Lj")

# GARCH모형에서 잔차분석 시 잔차추정량 제곱 시계열의 종속성이 존재하는지
# 특별히 더 확인해야함.
# 위 잔차의 경우 잔차추정량, 제곱의 모두 선형종속관계가 존재하지 않아보임

fit # ^w, ^a1, ^b1

## 조건부 분산(또는 표준편차) 추정
fit$fitted.values[1:5,]

layout(1)
ts.plot(log.r)
lines(fit$fitted[,1], col="red")
lines(fit$fitted[,2], col="red")

## 조건부 분산에 대한 예측
fit$coef

hat.w <- fit$coef[1]
hat.a <- fit$coef[2]
hat.b <- fit$coef[3]

hat.sigma2 <- fit$fitted.values[,1]^2
n <- length(hat.sigma2)

hat.sig2_1 <- hat.w+hat.a*log.r[n]^2+hat.b*hat.sigma2[n]
hat.sig2_2 <- hat.w+(hat.a+hat.b)*hat.sig2_1
c(sqrt(hat.sig2_1), sqrt(hat.sig2_2))

get.con.var <- function(X,h){
  fit<-garch(X, order=c(1,1), trace=F)
  hat.w<-fit$coef[1]
  hat.a<-fit$coef[2]
  hat.b<-fit$coef[3]
  hat.sigma2<-fit$fitted.values[,1]^2
  n<-length(hat.sigma2)
  
  con.var<-rep(0,h)
  hat.sig2_1<-hat.w+hat.a*X[n]^2+hat.b*hat.sigma2[n]
  con.var[1]<-hat.sig2_1
  
  for (i in 2:h)con.var[i]<-hat.w+(hat.a+hat.b)*con.var[i-1]
  con.var
}

get.con.var(log.r, 10)

## 조건부 표준편차에 대한 예측
ts.plot(fit$fitted.values[,1], xlim=c(1991.5, 1999))
hat.sigma<-ts(sqrt(get.con.var(log.r,100)), st=c(1998,170), fr=260)
lines(hat.sigma, col="red")

EuStockMarkets

# 수익률에 대한 95% 예측구간
ts.plot(log.r, xlim=c(1991.5,1999))
lines(1.96*hat.sigma, col="Red")
lines(-1.96*hat.sigma, col="Red")

# cf. W.N. 즉, ARMA(0,1)모형으로 적합 시 예측
library(forecast)
plot(forecast(Arima(log.r, order=c(0,0,1)), h=50))
