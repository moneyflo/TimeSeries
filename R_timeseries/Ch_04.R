# Chapter 04 Model for Stationary Time Series

library(TSA)

## Time Plot of an MA(1) Process with theta = -0.9
win.graph(width=4.875, height=3, pointsize=8)
data(ma1.2.s); plot(ma1.2.s, ylab=expression(Y[t]), type='o')


## Plot of Yt versus Yt-1 for MA(1) Series in above plot
win.graph(width=4.875, height=4.875, pointsize=8)
plot(y=ma1.2.s, x=zlag(ma1.2.s), ylab=expression(Y[t]),
     xlab=expression(Y[t-1]), type='p')
# 그래프를 보면 양의 선형관계가 보임

## Plot of Yt versus Yt-2 for MA(1) Series in Above plot
win.graph(width=4.875, height=4.875, pointsize=8)
plot(y=ma1.2.s, x=zlag(ma1.2.s, 2), ylab=expression(Y[t]),
     xlab=expression(Y[t-2]), type='p')
# 아무런 관계가 없어보임
# order 가 1이라서 2칸 차이에서는 상관관계를 볼 수 없음


## Time Plot of an MA(1) Process with theta = +0.9
win.graph(width=4.875, height=3, pointsize=8)
data(ma1.1.s); plot(ma1.1.s, ylab=expression(Y[t]), type='o')


## Plot of Yt versus Yt-1 for MA(1) Series in above plot
win.graph(width=4.875, height=4.875, pointsize=8)
plot(y=ma1.1.s, x=zlag(ma1.1.s), ylab=expression(Y[t]),
     xlab=expression(Y[t-1]), type='p')
# 그래프를 보면 음의 선형관계가 보임

## Plot of Yt versus Yt-2 for MA(1) Series in Above plot
win.graph(width=4.875, height=4.875, pointsize=8)
plot(y=ma1.1.s, x=zlag(ma1.1.s, 2), ylab=expression(Y[t]),
     xlab=expression(Y[t-2]), type='p')
# 아무런 관계 x



### The Second-Order Moving Average Process

## Exhibits 4.8 Time Plot of an MA(2) Process with theta1 = 1 and theta2 = -0.6
win.graph(width=4.875, height=3, pointsize=8)
data(ma2.s); plot(ma2.s, ylab=expression(Y[t]), type='o')

## Plot of Yt versus Yt-1 for MA(2) Series in Exhibit 4.8
win.graph(width=3, height=3, pointsize=8)
plot(y=ma2.s, x=zlag(ma2.s), ylab=expression(Y[t]),
     xlab=expression(Y[t-1]), type='p')
# 음의 상관관계 보임

## Plot of Yt versus Yt-2 for MA(2) Series in Exhibit 4.8
win.graph(width=3, height=3, pointsize=8)
plot(y=ma2.s, x=zlag(ma2.s, 2), ylab=expression(Y[t]),
     xlab=expression(Y[t-2]), type='p')
# 약하지만 양의 상관관계 보임

## Plot of Yt versus Yt-3 for MA(2) Series in Exhibit 4.8
win.graph(width=3, height=3, pointsize=8)
plot(y=ma2.s, x=zlag(ma2.s, 3), ylab=expression(Y[t]),
     xlab=expression(Y[t-1]), type='p')
# 아무 상관관계 보이지 않음.
# lag3 부터는 상관관계가 0 임
