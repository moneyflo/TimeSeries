# 시계열의 탐색적 자료 분석

# 3.1 친숙한 방법

# 3.1.1 도표 그리기

## R이 제공하는 유럽 증권거래 시계열 데이터
## 1991-1998 유럽 4대 주요 주가지수 일일 종가 기록
## 데이터셋은 입회일만 포함
head(EuStockMarkets)

## 단순 plot() 함수만으로도 데이터를 서로 다른 시계열 그래프로 자동 분할 가능
plot(EuStockMarkets)

## mts 객체를 사용함을 볼 수 있음.
class(EuStockMarkets)

## ts와 관련된 객체는 적절한 도표를 그려주는 함수를 자동호출

## ts 객체는 다음과 같은 몇가지 편리한 함수 제공

frequency(EuStockMarkets)

start(EuStockMarkets)

end(EuStockMarkets)

window(EuStockMarkets, start=1997, end=1998)


# 3.1.2 히스토그램
layout((1:2))
hist(EuStockMarkets[, "SMI"], 30)
hist(diff(EuStockMarkets[, "SMI"], 30))

## 시계열(특히 금융)에서 가장 흥미로운 점은 실제 측정치 자체가 아니라,
## 한 측정치가 다음 측정치로 변화한 정도(차분 말하는듯)


# 3.1.3 산점도

## 시간에 따른 서로 다른 두 주식의 가치
layout(t(1:2))
plot(EuStockMarkets[, "SMI"], EuStockMarkets[, "DAX"])
plot(diff(EuStockMarkets[, "SMI"]), diff(EuStockMarkets[, "DAX"]))

## 시간상 먼저 알게 된 한 주가의 변동으로 나중의
## 다른 주가의 변동을 예측하기위해 둘 중 하나 1만큼 시간 앞당김
layout(1)
plot(lag(diff(EuStockMarkets[, "SMI"]), 1),
     diff(EuStockMarkets[, "DAX"]))

## 위 산점도를 통해 lag 함수를 사용하자마자 두 주가의 상관관계가 사라졌고,
## SMI가 DAX를 예측하지 못하는 것을 보여줌


# 3.2 시계열에 특화된 탐색법
## * 정상성
## * 자체상관
## * 허위상관

# 3.2.1 정상성 이해하기
## 정상인것을 판별하는 것보단 비정상인것을 배제하는것이 더 쉬움

## 대표적인 비정상 시계열 자료
data("AirPassengers")
plot(AirPassengers)

## 정상과정의 간단한 정의는 모든 시차 k에 대하여 
## 데이터의 분포가 시간 t에 의존 X

# 3.2.2 윈도 함수 적용

## 롤링 윈도

## rnorm 함수로 정규분포를 따르는 난수 100개 호출
x <- rnorm(n=100, mean=0, sd=10) + 1:100

## rep 함수로 1/n 값을 n번 반복하는 배열을 만드는 함수 생성
mn <- function(n) rep(1/n, n)

plot(x, type='l', lwd=1)
## 기본 R의 filter 함수로 롤링 평균을 계산.
## 각각 5개, 50개 단위로 롤링
lines(filter(x, mn(5)), col=2, lwd=3, lty=2)
lines(filter(x, mn(50)), col=3, lwd=3, lty=3)

## 기능을 좀 더 '사용자 정의'하여 사용할 수도 있음.
require(zoo)

## x를 zoo 객체로 만들어서 각 데이터를 인덱싱
## rollapply 함수는 데이터, 윈도 크기, 적용 함수, 롤링 적용 정렬 방향,
## 윈도 크기만큼 데이터가 없어도 적용할 것인지 등에 대한 인수를 지정
f1 <- rollapply(zoo(x), 20, function(w) min(w), align='left', partial=TRUE)
f2 <- rollapply(zoo(x), 20, function(w) min(w), align='right', partial=TRUE)

plot(x, lwd=1, type='l')
lines(f1, col=2, lwd=3, lty=2)
lines(f2, col=3, lwd=3, lty=3)

## 확장 윈도
plot(x, type='l', lwd=1)
lines(cummax(x), col=2, lwd=3, lty=2) # 최댓값
lines(cumsum(x)/1:length(x), col=3, lwd=3, lty=3) # 평균

## R 내장 함수가 아닌 rollapply() 함수 사용
plot(x, type='l', lwd=1)
# seq_along 함수는 1부터 시작하여 입력된 인수의 길이에서 끝나는 순서를 만듬
lines(rollapply(zoo(x), seq_along(x), function(w) max(w),
                partial=TRUE, align="right"),
      col=2,lwd=3,lty=2)
lines(rollapply(zoo(x), seq_along(x), function(w) mean(w),
                partial=TRUE, align="right"),
      col=3,lwd=3,lty=3)

# 3.2.3 자체상관의 파악과 이해

## 자기상관 기능
x <- 1:100
y <- sin(x * pi/3)

layout(1:2)
plot(y, type="b", xlim=c(0,30))
acf(y)

## data.table의 shift() 함수를 통해 ACF 직접 계산

## cor 함수는 상관계수를 계산하는 용도로 사용
## 첫 번째와 두 번째 파라미터가 비교 대상 둘에 대한 것
## use 파라미터는 누락된 값을 처리하는 방법으로,
## pairwise.complete.obs는 계산 대상 변수만을 대상으로 누락된 값을 제거
## y와 y로부터 시차 1과 2만큼 움직인 것과의 상관계수를 계산
library(data.table)

cor(y, shift(y, 1), use='pairwise.complete.obs')
cor(y, shift(y, 2), use='pairwise.complete.obs')

## 편자기상관함수(PACF)
layout(t(1:2))
y <- sin(x * pi / 3)
plot(y[1:30], type='b')
pacf(y)

## 노이즈 유무에 따른 그래프 확인
par(mfrow=c(2,3))

## 1.노이즈가 없는 도표
y1 <- sin(x*pi/3)
plot(y1, type="b")
acf(y1)
pacf(y1)

y2 <- sin(x*pi/10)
plot(y2, type="b")
acf(y2)
pacf(y2)

## 두 계열을 더하여 결합하고, 결합된 계열에 대한 도표 그림
par(mfrow=c(3,1))
y <- y1 + y2
plot(y, type='b')
acf(y)
pacf(y)

## 동일 상황에서 노이즈를 많이 줬을때
noise1 <- rnorm(100, sd=0.05)
noise2 <- rnorm(100, sd=0.05)

y1 <- y1 + noise1
y2 <- y2 + noise2
y <- y1 + y2

par(mfrow=c(3,3))

plot(y1, type='b')
acf(y1)
pacf(y1)

plot(y2, type='b')
acf(y2)
pacf(y2)

plot(y, type='b')
acf(y)
pacf(y)

## 주기성은 없지만, 추세를 가진 계열의 ACF와 PACF
layout(1:3)
x <- 1:100
plot(x)
acf(x)
pacf(x)

## AirP - 에 대해
plot(AirPassengers)
acf(AirPassengers)
pacf(AirPassengers)

## 항상 허위상관에 주의할것!

# 3.3 유용한 시각화

# 3.3.1 1차원 시각화

require(timevis)
donations <- fread("https://raw.githubusercontent.com/PracticalTimeSeriesAnalysis/BookRepo/master/Ch02/data/donations.csv")
d <- donations[, .(min(timestamp), max(timestamp)), user]
names(d) <- c("content", "start", "end")
d <- d[start != end]
timevis(d[sample(1:nrow(d), 20)])

# 3.3.2 2차원 시각화

## AirPassengers로 부터 ts 객체 추출 후 적절한 행렬 형태로 만듬
t(matrix(AirPassengers, nrow=12, ncol=12))

layout(1)
colors <- c("green", "red", "pink", "blue",
            "yellow", "lightsalmon", "black", "gray",
            "cyan", "lightblue", "maroon", "purple")
matplot(matrix(AirPassengers, nrow=12, ncol=12),
        type='l', col=colors, lty=1, lwd=2.5,
        xaxt="n", ylab="Passenger Count")
legend("topleft", legend=1949:1960, lty=1, lwd=2.5, col=colors)
axis(1, at=1:12, labels=c("Jan", "Feb", "Mar", "Apr",
                          "May", "Jun", "Jul", "Aug",
                          "Sep", "Oct", "Nov", "Dec"))

## forecast 사용하면 동일한 도표를 쉽게 만들 수 있음
require(forecast)
seasonplot(AirPassengers)

## 연도별 월별 곡선의 도표
months <- c("Jan", "Feb", "Mar", "Apr",
            "May", "Jun", "Jul", "Aug",
            "Sep", "Oct", "Nov", "Dec")

matplot(t(matrix(AirPassengers, nrow=12, ncol=12)),
        type='l', col=colors, lty=1, lwd=2.5)
legend("left", legend=months, col=colors, lty=1, lwd=2.5)

## forecast 쓰면 유사한 시각화 가능
monthplot(AirPassengers)

## 2차원 히스토그램
hist2d <- function(data, nbins.y, xlabels){
  ## 최댓값과 최솟값을 포함하는 균등한 크기의 ybins를 만듭니다.
  ymin=min(data)
  ymax=max(data) * 1.0001
  ## 포함/불포함의 걱정을 피하기 위한 게으른 방법
  
  ybins=seq(from=ymin, to=ymax, lengh.out=nbins.y+1)
  ## 적절한 크기의 제로 행렬을 만듬
  hist.matrix=matrix(0, nrow=nbins.y,ncol=ncol(data))
  
  ## 행렬의 각 행은 하나의 데이터 점을 표현함
  for(i in 1:nrow(data)) {
    ts = findInterval(data[i, ], ybins)
    for (j in 1:ncol(data)) {
      hist.matrix[ts[j], j] = hist.matrix[ts[j], j] + 1 > hist.matrix
    }
  }
  hist.matrix
}

## 히트맵 컬러의 히스토그램 만들기
h = hist2d(t(matrix(AirPassengers, nrow=12, ncol=12)), 5, months)


# 3.3.3 3차원 시각화
require(plotly)
require(data.table)

month <- 1:12
ap <- data.table(matrix(AirPassengers, nrow=12, ncol=12))
names(ap) <- as.character(1949:1960)
ap[, month := months]
ap  <- melt(ap, id.vars='month')
names(ap) <- c("month", "year", "count")

p <- plot_ly(ap, x = ~month, y = ~year, z = ~count,
             color = ~as.factor(month)) %>%
  add_markers()  %>%
  layout(scene=list(xaxis = list(title='Month'),
                    yaxis = list(title='Year'),
                    zaxis = list(title='PassingerCount')))  
p


## 두 축 중 하나는 위치, 다른 하나는 시간에 사용한 예
file.location <- 'http://raw.githubusercontent.com/plotly/datasets/master/_3d-line-plot.csv'
data <- read.csv(file.location)
p <- plot_ly(data,x=~x1,y=~y1,z=~z1,
             type = 'scatter3d', mode='lines',
             line = list(color='#1f77b4', width=1))
p
