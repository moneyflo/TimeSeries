# Time_series chap 01

library(TSA)

# Time Series Plot of Los Angeles Annual Rainfall
win.graph(width=4.875, height=2.5, pointsize=8)
data(larain)
plot(larain, ylab='Inches', xlab='Year', type='o')

# Time Series Plot of Color Property from a Chemocal Process
win.graph(width=4.875, height=2.5, pointsize=8)
data(color)
plot(color, ylab='Color Property', xlab='Batch', type='o')

# Abundance of Canadian Hare
win.graph(width=4.875, height=2.5, pointsize=8)
data(hare)
plot(hare, ylab='Abundance', xlab='Year', type='o')

# Average Monthly Temperatures, Dubuque, lowa
win.graph(width=4.875, height=2.5, pointsize=8)
data(tempdub)
plot(tempdub, ylab='Temperature', type='o')

# Monthly Oil Filtter Sales
data(oilfilters)
plot(oilfilters, type='o', ylab='Sales')

## 경제쪽에선 뚜렷하진 않은 주기성을 가지는 경우가 많음,
## 계절같은 자연 현상은 주기성이 일반적으로 뚜렷해보임
## 시계열 분석에 관해서는 자연 현상이 더 쉬워보임

# TSA 패키지의 데이터 유형에 대한 파악
class(larain)  # ts로 time series 자료형으로 보임

larain
## 1878년 부터 1992년까지 1년 단위로 조사한 데이터

# iid 자료 보기
X <- rnorm(100) # 표준정규분포에서 서로 독ㄹ비되게 100개 뽑음
class(X)  # "numeric"

X

## 위와 같은 자료형에서는 데이터를 바탕으로 모분포를
## 추론하는게 목적이자 목표

## 시계열 자료의 특징
## * Not independent!!
## 대부분의 경우 데이터 사이에 어떤 관계(종속성)이 존재

## 인접할수록 그러한 관계가 강해지는 경향이 있음.

## 멀리 떨어져 있을수록 종속성이 약해지나
## 1. 빠르게 감소하는 경우 : Short range dependence
## 2. 천천히 감소하는 경우 : long range dependence
## 이 책에서는 주로 빠르게 감소하는 경우에 대해 배울 예정

## 즉,
## iid 자료에서는 그 자료를 만들어 내는 확률분포가 관심의 대상

## 시계열 자료분석에서는 종속관계를 나타낼 수 있는 모형화에 주된 관심


# Scatterplot of LA Rainfall versus Last Year's LA Rainfall
## t의 차이가 1인 데이터 간의 관계를 산점도로 표현

win.graph(width=3, height=3, pointsize=8)
plot(y=larain, x=zlag(larain), ylab='inches',
     xlab='Previous Year Inches')

# zlag 함수
A <- c(1,5,2,3,6)

zlag(A)

B <- zlag(A)

A[1]
A[2]
B[1]
B[2]


# Scatterplot of color Value versus Previous Color Value
win.graph(width=3, height=3, pointsize=8)
plot(y=color, x=zlag(color), ylab='Color Property',
     xlab='Previous Batch Color Property')
## 양의 관계가 보임 
## x값 증가할수록 y값도 증가

# iid에서는 데이터관에 관계가 없음을 보여줌
X <- rnorm(500)
Z.X <- zlag(X)

plot(x=Z.X, y=X, pch=20)


# 주기성(Seasonality)을 갖는 시계열 자료도 많음

# 1.2 A Model-Building Strategy
## 관찰된 자료에 제일 적합해 보이는 모형을 선택해 사용하는 것

## 관찰된 데이터와 모델은 구분해서 봐줘야됨
## 1. Find(observe)
## the characteristics of observed data

## 2. You should know
## the properties of your models
## choose your best model <- 분석자가 알고있는 한계내에서

# "All models are wrong,
# but
# some are useful.
# - George.E.P.Box

# model-building strategy by Box and Jenkins

## 1. model specification( or identification)
## 2. model fitting, and
## 3. model diagnostics
