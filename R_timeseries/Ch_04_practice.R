# 실습 1
# 시계열 데이터를 만드는 방법에 대한 간단한 설명

X <- rnorm(15)
X

data("AirPassengers")
AP <- AirPassengers

AP  # 시간에 대한 정보 포함되어있음

class(AP)  # ts(time series) 자료형

plot(AP) # 시간 정보가 들어있어서 산점도와는 달리 시간 또한 표시

# 시작 시점
start(AP)

# 끝 시점
end(AP)

# 1년에 몇번 관찰 되었는지
frequency(AP)

## 시간 정보가 없는 데이터들에 대해 시간정보를 입력해주는 함수들

# 데이터 불러오기
Maine.month <- read.table("C://Users/naseo/mine/study/R_timeseries/data/Maine.txt", header=TRUE)

# 작업 공간 할당하면 경로 따로 안쳐줘도됨.
dat <- read.table("Maine.txt")  # 헤더없이
dat[1:10,]

dat <- read.table("Maine.txt", header=T)

head(dat, n=10)

plot(dat$unemploy)
plot(dat$unemploy, type="l")

# 시간 정보 넣기
# ts function

dat.ts <- ts(dat, start=c(1996,1), frequency=12)
dat.ts

dat[1:10,]

plot(dat.ts)


# window func 특정 시점만

window(dat.ts, st=1997, end=1998)
window(dat.ts, st=1997, end=1999)
window(dat.ts, st=2000)
window(dat.ts, st=c(1997,3), end=c(1998,12))

# 5월의 데이터만
window(dat.ts, st=c(1996,5), freq=1)

# Start = 1996.333  <- 4/12 로 쓴거 index가 0부터라 4


### 다른 데이터
CBE <- read.table("cbe.txt", header=TRUE)
CBE       

Elec.ts <- ts(CBE[, 3], st=1958, freq=12)
Beer.ts <- ts(CBE[, 2], st=1958, freq=12)
Choc.ts <- ts(CBE[, 1], st=1958, freq=12)

layout(1:3)  # R에서는 col벡터로 생각
plot(Elec.ts); plot(Beer.ts); plot(Choc.ts)

### Real data: S&P 500 realized volatility

rvk <- read.csv("oxford-kernel.csv", header=TRUE)
head(rvk, n=10)

plot(rvk[,1],rvk[,2])

# strptime func 쓰면 깔끔하게 그려줌
rvk[1:10, 1]
strptime(rvk[1:10, 1], format="%Y%m%d")  # 대소문자 지켜서!

date <- strptime(rvk[,1], format="%Y%m%d")

rvk[1:20,]

# Na 제거
is.na(rvk[1:20,])

id <- !is.na(rvk[,2])
id[1:20]

date <- date[id]
Z <- rvk[id, 2]
plot(date, Z, type="l")
