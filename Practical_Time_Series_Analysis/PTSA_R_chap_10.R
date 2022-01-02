# Chpater 10 시계열을 위한 딥러닝

# 10.3.1 데이터셋 살펴보기
require(data.table)
install.packages('R.utils')

elec <- fread("electricity.txt.gz")
elec

# 행과 열의 개수는 파악 가능하지만, 타임스탬프 관련 정보가 없음
# 시간별로 타임스탬프가 찍혔다는 사실만 알고있음

ncol(elec)
nrow(elec)

# 데이터 24개의 도표를 통해 하루 전체에 관한 데이터 파악
elec[125:148, plot(V4, type='l', col=1, ylim=c(0, 1000))]
elec[125:148, lines(V14, type='l', col=2)]
elec[125:148, lines(V114, type='l', col=3)]

# 주별 도표
elec[1:168, plot(V4, type='l', col=1, ylim=c(0, 1000))]
elec[1:168, lines(V14, type='l', col=2)]
elec[1:168, lines(V114, type='l', col=3)]

# 차분
elec.diff <- diff(elec, lag=1)

elec.diff[1:168, plot(V4, type='l', col=1, ylim=c(-250, 350))]
elec.diff[1:168, lines(V14, type='l', col=2)]
elec.diff[1:168, lines(V114, type='l', col=3)]
