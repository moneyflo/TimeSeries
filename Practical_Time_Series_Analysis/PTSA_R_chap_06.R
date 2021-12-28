# Chapter 06 시계열의 통계 모델

# 6.2.1 자기회귀 모델(Autoregressive)

o2 <- Daily_Demand_Forecasting_Orders[11]
o2
o3 <- Daily_Demand_Forecasting_Orders[11]
o3

## 차수를 지정하지 않는 경우에는 ar() 함수가 자동으로 AR 모델의 차수를 선택
# ar() 함수 사용
fit <- ar(o2[[1]], method="mle")
fit
# AR(3) 모델이 채택된걸 볼 수 있음
# ar() 함수는 AIC값을 기준으로 차수를 선택함

# arima() 함수 사용
est <- arima(x=o2[[1]], order=c(3,0,0))
est

# 차수 1을 제한
est.1 <- arima(x=o2[[1]], order=c(3,0,0), fixed=c(0,NA,NA,NA))
est.1

# 학습 데이터에 대한 모델의 성능 검사

# 잔차에 대한 ACF
acf(est.1$res)

# 륭-박스 검정
Box.test(est.1$res, lag=10, type='Lj', fitdf=3)

# p값이 매우 작으면 기각 (일반적으로 0.05를 기준값으로 정함)


## AR(p) 과정으로 예측하기
# 시간을 한 단계 앞서 예측

# 사전에 적합된 est.1 모델로 작업진행
# forecast 패키지의 fitted() 함수를 사용해 예측 그래프 그리기
require(forecast)
plot(o2[[1]], type='l')
# fitted는 모델 적합에 사용된 데이터에 대한 예측값을 반환하는 함수로,
# 예측 기간을 지정하기 위한 h 인수를 추가로 전달 가능
lines(fitted(est.1), col=3, lwd=2)

# 예측 기간이 늘어날수록 미래의 예측은 실제 데이터의 평균에 가까워짐
# 따라서 예측값이 무조건적인 평균값에 가까워지면서 예측값 및 오차항 
# 모두에 대한 분산은 0으로 줄어듬
var(fitted(est.1, h=3), na.rm=TRUE)
var(fitted(est.1, h=5), na.rm=TRUE)
var(fitted(est.1, h=10), na.rm=TRUE)
var(fitted(est.1, h=20), na.rm=TRUE)
var(fitted(est.1, h=30), na.rm=TRUE)


# 여러 시간 단계를 앞서 예측
layout(1:3)

est.1.3 <- fitted(est.1, h=3)
plot(est.1.3)

est.1.10 <- fitted(est.1, h=10)
plot(est.1.10)

est.1.30 <- fitted(est.1, h=30)
plot(est.1.30)

summary(est.1)

# 6.2.2 이동평균 모델(MA 모델)

# MA(q) 파라미터 선택
# ACF를 사용해 결정
layout(1)
acf(o2[[1]])

# 지연 3과 9에서 임계 유의값이 확인됨
ma.est = arima(x = o2[[1]], order=c(0,0,9), fixed=c(0,0,NA, rep(0,5), NA, NA))
ma.est

# 모델의 적합 검사
# fitdf 인수로 자유도를 설정(특정 값으로 제한하지 않고 자유롭게 추정될 모델의 파라미터 개수 지정)
Box.test(ma.est$res, lag=10, type="Lj", fitdf=3)

acf(ma.est$res)

# MA(q) 과정의 예측
fitted(ma.est, h=1)

mean(o2[[1]])

# 10단계 앞선 예측
fitted(ma.est, h=10)


# 6.2.3 자기회귀누적이동평균 모델(ARIMA)

# 수동적으로 모델 적합시키는 방법
# 박스-젠킨스 방법
# 1. 데이터, 시각화, 기반 지식을 사용하여 데이터에 적합한 모델의 종류를 고름.
# 2. 주어진 학습용 데이터로 파라미터 추정
# 3. 학습용 데이터를 기반으로 모델 성능 평가 및 모델의 파라미터를 조정하여 성능 진단상 나타나는 약점 해결

# 예시로 사용하기 위한 데이터 생성
require(forecast)
set.seed(1017)
y = arima.sim(n = 1000, list(ar=c(0.8, -0.4), ma=c(-0.7)))

plot(y)
layout(c(1:2))
acf(y); pacf(y)
# 그래프 모양은 정상성을 띄는 것으로 보이고
# acf, pacf 둘 다 지수적으로 감소하는 것으로 보임
# 따라서 ARMA모델에 적합시켜야 될것으로 보임
# 그중에서도 가장 간단한 ARIMA(1,0,1) 모델을 적합시키는 것으로 시작
ar1.ma1.model = Arima(y, order=c(1,0,1))
par(mfrow=c(2,1))
acf(ar1.ma1.model$res)
pacf(ar1.ma1.model$res)

# 잔차의 pacf를 봤을때 특히 큰 값을 보여주므로
# 자기회귀 동작을 완전히 설명하지 못하는 것으로 보임
# 따라서 AR 차수를 높여야함

ar2.ma1.model = Arima(y, order=c(2,0,1))
layout(1)
plot(y, type='l')
lines(ar2.ma1.model$fitted, col=2)
plot(y, ar2.ma1.model$fitted)
par(mfrow=c(2,1))
acf(ar2.ma1.model$res)
pacf(ar2.ma1.model$res)

# 모델의 단순성과 과적합에 대한 위험성을 고려했을때는 여기서 멈추는 것이 타당해 보임

# 추가적인 검증
ar2.ma2.model = Arima(y, order=c(2,0,2))
ar2.d1.ma2.model = Arima(y, order=c(2,1,2))

cor(y, ar1.ma1.model$fitted)
cor(y, ar2.ma1.model$fitted)
cor(y, ar2.ma2.model$fitted)
cor(y, ar2.d1.ma2.model$fitted)

ar2.ma1.model

# 자동으로 모델을 적합시키는 방법
est = auto.arima(o2[[1]], stepwise=FALSE, ## 느리지만 좀 더 완전한 검색이 가능
                 max.p=3, max.q=9)
est

auto.model = auto.arima(y)
auto.model


# 6.2.4 벡터자기회귀
require(vars)
require(data.table)

VARselect(Daily_Demand_Forecasting_Orders[, 11:12, with=FALSE], lag.max=4, type="const")

# 다음은 약간 낮은 AIC를 가진 지연 3에 대해 VAR이 동작하는 방식을 살펴보는 코드

est.var <- VAR(Daily_Demand_Forecasting_Orders[, 11:12, with=FALSE], p=3, type="const")
est.var

# 순차적 상관관계를 위해 포트맨토 검정 적용
serial.test(est.var, lags.pt=8, type="PT.asymptotic")
