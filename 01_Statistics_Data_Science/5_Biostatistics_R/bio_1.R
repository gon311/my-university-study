# =======================
# 바이오통계 Chapter 1: 기초 통계 분석
# ======================= 

## 1-1: 작업 디렉토리 설정
# 현재 작업 디렉토리를 지정된 경로로 설정
# 이 경로 아래의 파일들을 쉽게 접근할 수 있다
setwd("~/KNOU_stat/R_codes")

# 현재 설정된 작업 디렉토리 확인
# 작업 디렉토리가 제대로 설정되었는지 검증하는 단계
getwd()

## 1-2: 패키지 설치 및 로드
# dplyr 패키지 설치
# dplyr: 데이터 전처리 및 조작을 위한 강력한 패키지
# 파이프 연산자(%>%)를 사용하여 직관적인 데이터 처리 가능
install.packages("dplyr")

# dplyr 패키지를 현재 세션에 로드
# 이후 dplyr의 함수들(mutate, filter, select 등)을 사용할 수 있다
library(dplyr)

## 1-3: 데이터 로드 및 기초 통계 확인
# CSV 파일을 데이터프레임으로 읽어오기
# 바이오통계 예제 데이터셋 로드
dat0 <- read.csv("biostat_ex_data.csv")

# 데이터의 요약 통계 출력
# 각 변수의 기초통계(최소값, 1사분위수, 중앙값, 평균, 3사분위수, 최대값) 표시
summary(dat0)

# *** 중요한 관찰 ***
# sex, Recur과 같이 범주형 데이터(categorical variables)가
# 연속형 데이터(numeric variables)로 잘못 인식되어 있음을 확인할 수 있다
# 이는 데이터 타입을 명시적으로 변환해야 함을 의미한다

## 1-4: 범주형 변수 타입 변환
# dplyr 패키지 재로드 (명시적 선언)
library(dplyr)

# mutate_at() 함수: 지정한 여러 변수의 타입을 일괄 변환
# dat0에서 다음 변수들을 factor(범주형)으로 변환:
#   - sex: 성별 (남/여)
#   - Recur: 재발 여부 (Yes/No)
#   - stage: 질병 단계 (Stage 1/2/3/4)
#   - smoking: 흡연 여부 (Yes/No)
#   - obesity: 비만 여부 (Yes/No)
#   - Recur_1y: 1년 내 재발 여부
#   - post.CA19.9.binary: 수술 후 CA19.9 이분화 변수
#   - post.CA19.9.3grp: 수술 후 CA19.9 3그룹 분류
# as.factor: 숫자형 또는 문자형을 범주형으로 변환하는 함수
dat1 <- dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y,
                                post.CA19.9.binary, post.CA19.9.3grp),
                           as.factor)

# 타입 변환 후 데이터 요약 확인
# 이제 범주형 변수들은 각 범주의 빈도수로 표시된다
summary(dat1)

## 1-5: 범주형 변수의 빈도 분석
# stage 변수의 요약 통계 출력
# 각 단계별 관측치 수를 표시
summary(dat1$stage)

# stage 변수의 빈도표 출력
# 각 범주의 정확한 빈도수를 보여주는 표 형식
table(dat1$stage)

## 1-6: 범주형 변수의 시각화 - 막대 그래프
# ggplot2 패키지 로드 (자동으로 함께 설치됨)
library(ggplot2)

# geom_bar()를 사용한 막대 그래프 작성
# x축: stage 변수 (각 단계별로 구분)
# y축: 빈도수 (자동으로 계산)
# 각 단계별 환자 수를 시각적으로 비교 가능
ggplot(dat1) + geom_bar(aes(x=stage))

## 1-7: 연속형 변수의 기초 통계 계산
# age 변수의 평균값 계산
# 표본의 중심 경향을 나타내는 대표값
mean(dat1$age)

# age 변수의 중앙값(중위수) 계산
# 자료를 크기 순서로 배열했을 때 중간에 위치한 값
# 극값에 영향을 받지 않는 강건한 대표값
median(dat1$age)

# age 변수의 분산 계산
# 데이터가 평균으로부터 얼마나 퍼져있는지를 나타내는 척도
# 분산이 크면 데이터의 변동성이 크다
var(dat1$age)

# age 변수의 표준편차 계산
# 분산의 제곱근으로, 평균으로부터의 평균적인 편차를 나타낸다
# 원래 변수와 같은 단위로 표현되어 해석이 쉽다
sd(dat1$age)

# age 변수의 5개 요약수 계산
# 최소값, 1사분위수, 중앙값, 3사분위수, 최대값을 반환
# 데이터의 전반적인 분포를 한눈에 파악 가능
fivenum(dat1$age)

# age 변수의 전체 요약 통계 출력
# 최소값, 1사분위수, 중앙값, 평균, 3사분위수, 최대값 모두 표시
summary(dat1$age)

## 1-8: 연속형 변수의 시각화
# 1. 기본 히스토그램
# geom_histogram(): 연속형 변수의 분포를 보여주는 그래프
# age 변수의 분포를 구간별로 표시
ggplot(dat1) + geom_histogram(aes(x=age))

# 2. 커스터마이징된 히스토그램
# breaks=seq(20, 80, 10): 20세부터 80세까지 10세 간격으로 구간 설정
# color="black": 막대의 테두리를 검은색으로 지정
# fill="skyblue": 막대의 내부를 하늘색으로 채움
# 이를 통해 age의 분포 패턴을 더 명확하게 관찰 가능
ggplot(dat1) + geom_histogram(aes(x=age), breaks=seq(20, 80, 10),
                              color="black", fill="skyblue")

# 3. 박스 플롯 (상자 그림)
# geom_boxplot(): 사분위수를 기반으로 한 분포 시각화
# x=1: 박스 플롯을 한 개의 그룹으로 표시 (x축 그룹 없음)
# y=age: age 변수를 y축에 표시
# scale_x_continuous(breaks=NULL): x축의 눈금을 제거
# theme(axis.title.x = element_blank()): x축 제목을 제거
# 박스 플롯에서:
#   - 상자의 아래 선 = 1사분위수
#   - 상자 내 두꺼운 선 = 중앙값
#   - 상자의 위 선 = 3사분위수
#   - 위/아래 가는 선 = 수염(whiskers)
#   - 동그란 점 = 이상치(outliers)
ggplot(dat1) + geom_boxplot(aes(x=1, y=age)) + 
  scale_x_continuous(breaks=NULL) + 
  theme(axis.title.x = element_blank())

## 1-9: 연속형 변수의 분포 비교 - 평균과 중앙값
# CA19.9 변수의 분포를 히스토그램으로 표시
# geom_vline(): 수직선을 그려서 특정 값을 표시
# 첫 번째 geom_vline(): 평균값을 빨간색 수직선으로 표시
# 두 번째 geom_vline(): 중앙값을 파란색 수직선으로 표시
# 이 시각화를 통해 평균과 중앙값의 차이를 관찰할 수 있다
# (한쪽으로 치우친 분포에서는 평균과 중앙값이 크게 다를 수 있다)
ggplot(dat1) + geom_histogram(aes(x=CA19.9), color="black", fill="grey") + 
  geom_vline(xintercept = mean(dat1$CA19.9), color = "red") + 
  geom_vline(xintercept = median(dat1$CA19.9), color = "blue")

# CA19.9의 평균값 출력
mean(dat1$CA19.9)

# CA19.9의 중앙값 출력
median(dat1$CA19.9)

## 1-10: 기술통계 표 자동 생성 (전체 변수 한번에)
# tableone 패키지 설치
# CreateTableOne() 함수를 사용하여 Table 1(기술통계표) 자동 생성
install.packages("tableone")

# tableone 패키지 로드
library(tableone)

# CreateTableOne() 함수로 Table 1 생성
# vars: 포함할 변수 목록
#   - age: 나이 (연속형)
#   - sex: 성별 (범주형)
#   - CA19.9, CRP, CEA: 종양 마커 (연속형)
#   - stage: 질병 단계 (범주형)
#   - smoking, obesity: 생활습관 변수 (범주형)
# data=dat1: 분석에 사용할 데이터프레임
t1 <- CreateTableOne(vars=c("age", "sex", "CA19.9", "CRP", "CEA", 
                            "stage", "smoking", "obesity"), data=dat1)

# Table 1을 요약 형태로 출력
# digits=4: 소수점 자리수를 4자리로 설정하여 정확한 수치 표시
summary(t1, digits = 4)

# Table 1을 기본 형태로 출력
# 연속형 변수는 평균±표준편차로, 범주형 변수는 n(%)으로 표시
print(t1)

# Table 1을 비정규분포 변수를 고려하여 출력
# nonnormal=c("CA19.9", "CRP", "CEA"): 
#   이 세 변수가 정규분포를 따르지 않음을 지정
#   정규분포하지 않는 변수는 중앙값과 사분위범위(IQR)로 표시된다
#   (평균±표준편차 대신)
print(t1, nonnormal=c("CA19.9", "CRP", "CEA"))