### 2.4 데이터 정리

## 2.4.1 누락된 데이터 다루기

#누락된 데이터 다루는방법에는 대치법, 보간법, 영향받은 기간 삭제 등이 있음.

# 대치법
# 미정부가 발표한 원간 실업 자료로 부터 데이터셋 두개 생성
# 하나는 데이터를 임의로 누락, 다른 하나는 시계열 기록에서 실업이 가장 높은 월

# 라이브러리 설치
install.packages("zoo"); install.packages("data.table")

library(zoo)
library(data.table)

require(zoo) # 시계열 기능 제공
require(data.table) # 고성능 자료구조

unemp <- fread("https://raw.githubusercontent.com/PracticalTimeSeriesAnalysis/BookRepo/master/Ch02/data/UNRATE.csv")
unemp[, DATE := as.Date(DATE)]
setkey(unemp, DATE)

# 임의로 누락된 데이터로 구성된 데이터셋 생성
rand.unemp.idx <- sample(1:nrow(unemp), .1*nrow(unemp))
rand.unemp <- unemp[-rand.unemp.idx]  # 음수 인덱스 제거

# 실업률이 높을 떄 누락될 가능성이 더 높은
# 데이터로 구성된 데이터셋을 생성
high.unemp.idx <- which(unemp$UNRATE > 8)
num.to.select <- .2 * length(high.unemp.idx)

high.unemp.idx <- sample(high.unemp.idx,)
bias.unemp <- unemp[-high.unemp.idx]

########### data.table 사용법 공부 ###############
dim(unemp)
class(unemp$UNRATE)

rep('a',5)

sample(1:nrow(unemp), 5, replace=TRUE)  # 기본값 FALSE
a <- 1:10
a

#################################################

all.dates <- seq(from=unemp$DATE[1], to=tail(unemp$DATE, 1), by="months")
all.dates

tail(unemp$DATE, 1)
View(unemp)

rand.unemp = rand.unemp[J(all.dates), roll=0]
bias.unemp = bias.unemp[J(all.dates), roll=0]
rand.unemp[, rpt := is.na(UNRATE)]
# 그래프를 쉽게 그리기 위해서 누락된 데이터 레이블링

length(all.dates)
all.dates[1]
tail(all.dates, 1)
is.na(unemp$UNRATE)

is.na(rand.unemp)

## 포워드 필
## 직전값으로 누락된값을 채우는 방법

rand.unemp[, impute.ff := na.locf(UNRATE, na.rm=FALSE)]
bias.unemp[, impute.ff := na.locf(UNRATE, na.rm=FALSE)]

# 평평한 부분을 보여주는 샘플 그래프
unemp[350:400, plot(DATE, UNRATE, col=1, lwd=2, type='b')]
rand.unemp[350:400, lines(DATE, impute.ff, col=2, lwd=2, lty=2)]
rand.unemp[350:400][rpt==TRUE, points(DATE, impute.ff, col=2, pch=6, cex=2)]


## 이동평균
# 사전관찰이 없는 롤링 평균
rand.unemp[, impute.rm.nolookahead := rollapply(c(NA, NA, UNRATE), 3, 
            function(x) {
              if (!is.na(x[3])) x[3] else mean(x, na.rm = TRUE)
            })]


bias.unemp[, impute.rm.nolookahead := rollapply(c(NA, NA, UNRATE), 3, 
            function(x) {
              if (!is.na(x[3])) x[3] else mean(x, na.rm=TRUE)
            })]

# 사전관찰을 포함한 롤링 평균
rand.unemp[, complete.rm := rollapply(c(NA, UNRATE, NA), 3,
             function(x) {
               if (!is.na(x[2])) x[2]
               else mean(x, na.rm = TRUE)
             })]

## 보간법
## 과거와 미래의 데이터를 모두 사용 or 둘 중하나만

## 여기서는 둘 다 사용하는 방식 사용

## 선형 보간법
rand.unemp[, impute.li := na.approx(UNRATE)]
bias.unemp[, impute.li := na.approx(UNRATE)]

## 다항식 보간법
rand.unemp[, impute.sp := na.spline(UNRATE)]
bias.unemp[, impute.sp := na.spline(UNRATE)]

use.idx = 90:120
unemp[use.idx, plot(DATE, UNRATE, col=1, type='b')]
rand.unemp[use.idx, lines(DATE, impute.li, col=2, lwd=2, lty=2)]
rand.unemp[use.idx, lines(DATE, impute.sp, col=3, lwd=2, lty=3)]

## 전체 비교
# 각 방법의 MSE 비교

sort(rand.unemp[, lapply(.SD, function(x) mean((x-unemp$UNRATE)^2, na.rm = TRUE)),
                .SDcols = c("impute.ff", "impute.rm.nolookahead", "impute.li", "impute.sp")])

sort(bias.unemp[, lapply(.SD, function(x) mean((x-unemp$UNRATE)^2, na.rm = TRUE)),
                .SDcols = c("impute.ff", "impute.rm.nolookahead", "impute.li", "impute.sp")])


## 2.4.2 업샘플링과 다운샘플링

# 다운샘플링
## 1월에 측정된 것에만 초점을 맞춘 부분 시계열
## 즉, 이 과정애서는 데이터를 연간 빈도로 다운샘플링한것
unemp[seq.int(from = 1, to = nrow(unemp), by = 12)]

## 더 낮은 빈도의 데이터에 맞추는 경우
unemp[, mean(UNRATE), by = format(DATE, "%Y")]


# 업샘플링

## 시계열이 불규칙적인 상황
## 롤링조인 활용
all.dates <- seq(from = unemp$DATE[1], to = tail(unemp$DATE, 1), by = "months")
# rand.unemp = rand.unemp[J(all.dates), roll=0]  #오류 왜??


## 입력이 서로 다른 빈도로 샘플링된 상황
## 새로운 일이 월 초마다 생긴다는 사실을 안다고 가정
all.dates <- seq(from = unemp$DATE[1], to = tail(unemp$DATE, 1), by = "days")
daily.unemployment = unemp[J(all.dates), roll=31]
daily.unemployment


## 2.4.3 데이터 평활


## 2.5 계절성 데이터
plot(stl(AirPassengers, "periodic"))

