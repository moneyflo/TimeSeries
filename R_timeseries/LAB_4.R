# LAB 4: Arima{forecast} 함수를 이용한 추정 1

### EX 1

X <- arima.sim(n=200, list(order=c(1,0,0), ar=0.7)) 
# AR(1), 모수 = 0.7, 200개의 데이터

ts.plot(X)
# 위에거 제외 그래프만 있다고 생각
# AR(1) 모형을 적합한다고 생각했을떄 추정해야할 모수는
# mean, ar 계수, 오차항의 Var

# 모수 추정
library(forecast)
Arima(X, order=c(1,0,0))
# mean이 유효해보이지 않으므로 제외

Arima(X, order=c(1,0,0), include.mean=F)
# d=0일 때에는 include.mean옵션의 default=T

# 또는 fixed 옵션을 사용하면
Arima(X, order=c(1,0,0), fixed=c(NA, 0))
# fixed 옵션은 모수의 값을 주어진 값으로 고정시킴.

## 잔차분석
X.fit <- Arima(X, order=c(1,0,0), include.mean=F)
r <- X.fit$res
ts.plot(r)
# 0을 중심으로 나타나는게 보임

# acf, pacf 확인
layout(t(1:2))
acf(r); pacf(r)
# W.N.로 보이기 때문에 더이상 모형화할 부분이 없는것으로 보임

Box.test(r, lag=12, type="Lj")
# 일반적으로 lag값은 비계절성은 10, 계절성은 주기x2r값을 사용

# 주어진 잔차가 AR(1)모형에서의 잔차로 간주할 수 있는지에
# 대한 검정을 위해 fitdf옵션값을 줌
Box.test(r, lag=12, type="Lj", fitdf=1)

## 잔차분석 결과 더이상 모형화할만한 종속성이 관찰되지 않음.
## 따라서 위 모형을 최종 모형으로 확정

## 최종 모형
layout(1)
X.fit

ts.plot(X)
hat.X <- X.fit$fitted
hat.X
lines(hat.X, col="red")

## 예측
forecast(X.fit, h=5)

X.fore <- forecast(X.fit, h=30)
plot(X.fore)

### EX 3
X <- 2+arima.sim(n=200, list(order=c(1,0,1), ar=0.3, ma=0.5))
ts.plot(X)
# mean=2인 정상 ARMA(1,1)모형으로부터 path 발생
# (1-0.3B)(Xt - 2) = (1 + 0.5B)et
# 여기서 et ~ i.i.d.N(0, 1)

# ARMA(1,1)모형에 적합.
# 추정할 모수는 mean, ar,ma 모수, w.n.분산
Arima(X, order=c(1,0,1))

X.fore <- forecast(Arima(X, order=c(1,0,1)), h=30)
plot(X.fore)

### EX 3
tt <- 1:200
X <- -1 + 0.2*tt + arima.sim(n=200, list(order=c(1,0,1), ar=0.3, ma=0.5))
ts.plot(X)

# 오차항이 ARMA(1,1)인 선형회귀모형 path 생성
# 오차항의 오차항은 i.i.d.N(0,1)

## 선형추세에 정상과정을 따르는 오차항이 결합된 형태는
## include.drift옵션을 이용하여 추정 가능

# 모수추정
Arima(X, include.drift=T, order=c(1,0,1))

# 잔차분석
# 여기서의 잔차는 오차항의 오차에 해당

X.fit <- Arima(X, order=c(1,0,1), include.drift=T)
hat.eta <- X.fit$res
ts.plot(hat.eta)

# 오차항의 오차에 대한 acf, pacf
layout(t(1:2))
acf(hat.eta); pacf(hat.eta)
# 검증선 안쪽에서 나타나는게 보임

# Box 테스트
Box.test(hat.eta, lag=12, type="Lj")


# 예측
X.fore <- forecast(X.fit, h=30)
layout(1)
plot(X.fore)
