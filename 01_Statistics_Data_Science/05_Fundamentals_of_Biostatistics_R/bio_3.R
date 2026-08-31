# =======================================================
# 바이오통계 Chapter 3: 추정 (Estimation)
# =======================================================

# -------------------------------------------------------
# [기본 설정] 작업 디렉토리 설정 및 확인
# -------------------------------------------------------
setwd("~/KNOU_stat/R_codes")  # 작업 경로 설정
getwd()                      # 현재 실행 중인 작업 경로 확인

# =======================================================
# 1. 표본 평균의 모의실험 (Monte Carlo Simulation)
# =======================================================

## 1-1. 모집단 벡터 생성
# -4부터 4까지의 정수로 구성된 모집단 생성 (모평균 = 0)
pop <- (-4):4  

# 모집단에서 크기가 4인 표본을 복원추출(replace=TRUE)로 임의 추출
sample(pop, size=4, replace = TRUE) 


## 1-2. 난수 고정 및 단순 표본 추출
set.seed(1000)  # 동일한 난수를 생성하기 위한 난수 씨앗값(seed) 고정

# 표본 추출 실행
sam <- sample(pop, size=4, replace = TRUE)  
mean(sam)       # 추출된 표본 4개의 평균(표본평균, x_bar) 계산


## 1-3. 반복문(for문) 기초 연습
# 1부터 10까지 순차적으로 출력
for(i in 1:10){
  print(i)
}

# 빈 벡터를 생성한 후 반복문으로 값 할당하기
vec <- rep(NA, 10) # 결측값(NA) 10개로 채워진 벡터 생성
for(i in 1:10){
  vec[i] <- i
}
vec  # 결과: [1]  1  2  3  4  5  6  7  8  9 10


## 1-4. 반복 횟수(m) 변화에 따른 표본평균의 분포 관찰

### (1) m = 10회 반복 (표본크기 n = 4)
m <- 10
xbar.vec <- rep(NA, m) # 표본평균 10개를 저장할 빈 벡터

for(i in 1:m){
  set.seed(1000 + i)                   # 루프마다 세부 시드 변경
  xx <- sample(pop, size=4, replace=TRUE) # 크기 4인 표본 추출
  xbar.vec[i] <- mean(xx)              # 해당 표본의 평균 저장
}

xbar.vec      # 추출된 10개의 표본평균 값들
table(xbar.vec) # 각 표본평균 값의 빈도표 생성
hist(xbar.vec, main="n = 4, m = 10", xlab = bquote(bar(x))) # 히스토그램 작성

mean(xbar.vec) # 10개 표본평균들의 평균 (모평균 0에 근접하는지 확인) -> 0.05
sd(xbar.vec)   # 표본평균들의 표준편차(표준오차의 추정치) -> 1.147461


### (2) m = 1,000회 반복 (표본크기 n = 4)
m <- 1000
xbar.vec <- rep(NA, m)

for(i in 1:m){
  set.seed(1000 + i)
  xx <- sample(pop, size=4, replace=TRUE)
  xbar.vec[i] <- mean(xx)
}

table(xbar.vec)
hist(xbar.vec, main="n=4, m=1000", xlab=bquote(bar(X)))

mean(xbar.vec) # 반복 횟수(m)가 늘어날수록 평균은 모평균(0)에 더욱 가까워짐 -> 0.004
sd(xbar.vec)   # -> 1.29249


### (3) m = 10,000회 반복 (표본크기 n = 4)
m <- 10000
xbar.vec <- rep(NA, m)

for(i in 1:m){
  set.seed(1000 + i)
  xx <- sample(pop, size=4, replace=TRUE)
  xbar.vec[i] <- mean(xx)
}

table(xbar.vec)
hist(xbar.vec, main="n=4, m=10000", xlab=bquote(bar(X)))

mean(xbar.vec) # 대수의 법칙에 의해 모평균(0)에 매우 수렴 -> -0.00255
sd(xbar.vec)   # -> 1.282247


# =======================================================
# 2. 표본 크기(n)에 따른 표본평균 분산의 변화 모의실험
# =======================================================

## 2-1. 표본 크기 n = 40으로 증가 (m = 10,000회)
m <- 10000
xbar.vec2 <- rep(NA, m)

for(i in 1:m){
  set.seed(1000 + i)
  xx <- sample(pop, size=40, replace=TRUE) # 표본 크기를 4에서 40으로 확대
  xbar.vec2[i] <- mean(xx)
}

# 표본 크기(n) 변화 비교
# - n이 커질수록 표본평균의 산포도(분산)는 1/n로 줄어듦 (중심극한정리)
hist(xbar.vec, main="n=4, m=10000", xlab=bquote(bar(X)))
hist(xbar.vec2, main="n=40, m=10000", xlab=bquote(bar(X)), xlim=c(-4, 4))
# 결과: n=40일 때 그래프의 폭이 훨씬 좁아지고 중앙으로 조밀하게 모임


# =======================================================
# 3. 췌장암 실습 데이터 - 모평균 추정 (t-분포 활용)
# =======================================================

## 3-1. 데이터 불러오기 및 전처리
dat0 <- read.csv("biostat_ex_data.csv")
library(dplyr)

# 범주형(명목형) 변수들을 요인(factor) 형태로 일괄 변환
dat1 <- dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, 
                                obesity, Recur_1y, 
                                post.CA19.9.binary,
                                post.CA19.9.3grp),
                           as.factor)

## 3-2. 체중(weight)의 모평균 점추정 및 구간추정
mean(dat1$weight)  # 점추정량(표본평균): 68.12562 kg

# t-검정 함수를 이용한 95% 신뢰구간 구하기 (기본값 conf.level = 0.95)
t.test(dat1$weight)
# - t = 58.814, 자유도(df) = 155
# - 95% 신뢰구간: [65.83748, 70.41377]

# 99% 신뢰구간 구하기 (신뢰수준이 높아질수록 신뢰구간의 길이는 넓어짐)
t.test(dat1$weight, conf.level = 0.99)$conf.int  
# - 99% 신뢰구간: [65.10480, 71.14644]


# =======================================================
# 4. 췌장암 실습 데이터 - 모비율 추정
# =======================================================

## 4-1. 성별(sex) 변수 분포 확인
table(dat1$sex)

## 4-2. 정규근사 방법에 의한 모비율 추정 (prop.test)
# x: 관심 사건의 수 (예: sex==1 인 인원 수)
# n: 전체 표본 수
prop.test(x = sum(dat1$sex == 1), n = nrow(dat1)) 
# - 점추정량(표본비율): 0.6538462 (약 65.38%)
# - 95% 신뢰구간(정규근사): [0.5730209, 0.7269634]

## 4-3. Exact 방법에 의한 모비율 추정 (binom.test)
# 표본 크기가 작거나 이항분포의 성질을 그대로 이용할 때 사용 (이항검정)
binom.test(x = sum(dat1$sex == 1), n = nrow(dat1)) 
# - 점추정량(표본비율): 0.6538462
# - 95% 신뢰구간(Exact): [0.5736048, 0.7281183]