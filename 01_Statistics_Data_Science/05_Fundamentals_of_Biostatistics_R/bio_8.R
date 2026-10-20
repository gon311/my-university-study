# ============================================================
# 바이오통계학 Chapter 8: 선형회귀분석 I (상관분석 및 선형회귀)
# ============================================================

# ------------------------------------------------------------
# [1. 기본 설정 및 데이터 전처리]
# dplyr 패키지를 활용해 범주형 변수(Factor) 변환 및 파생변수를 생성합니다.
# ------------------------------------------------------------
setwd("~/KNOU_stat/R_codes")
dat0 <- read.csv("biostat_ex_data.csv")
library(dplyr)

dat3 <- dat0 %>% 
  # 명목형/서열형 데이터들을 회귀모형에 맞게 요인(factor)형으로 일괄 변환
  mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y, 
                 post.CA19.9.binary, post.CA19.9.3grp), as.factor) %>% 
  # 조건에 따라 0과 1을 가지는 새로운 이항 파생변수 생성
  mutate(HTM = as.factor(ifelse(SBP>=140, 1, 0)),             # 고혈압 여부
         CEA.grp = as.factor(ifelse(CEA>5, 1, 0)),            # 수술 전 CEA 수치 기준
         post.CEA.grp = as.factor(ifelse(post.CEA>5, 1, 0)))  # 수술 후 CEA 수치 기준


# ------------------------------------------------------------
# [2. 산점도 및 상관계수 (Correlation Analysis)]
# 연속형 변수 간의 선형적/단조적 관계 강도를 측정합니다.
# ------------------------------------------------------------
library(ggplot2)

# 1) 연령(age)과 수축기 혈압(SBP)의 상관관계
ggplot(dat3) + geom_point(aes(age, SBP))

cor(dat3$age, dat3$SBP) # 피어슨(Pearson): 0.3304311 (두 변수 간의 '선형' 관계 측정)
cor(dat3$age, dat3$SBP, method="spearman") # 스피어만(Spearman): 0.3037352 (값의 크기 대신 '순위'를 기반으로 비선형적 단조 관계도 측정 가능)

# 💡 해석 포인트:
# 0.3 정도면 공학에서는 약한 상관관계로 보지만, 변동성이 큰 '바이오/의학 데이터'에서는 
# 이 정도 수치도 유의미한 상관관계(생물학적 연관성)가 있는 것으로 해석하는 경우가 많습니다.

# 2) 수술 전후 CEA 수치의 상관관계 (로그 변환 적용)
# 바이오 마커(CEA 등)는 값이 한쪽으로 심하게 치우친(Skewed) 경우가 많아 log를 씌워 정규분포에 가깝게 맞춰줍니다.
ggplot(dat3) + geom_point(aes(log(CEA), log(post.CEA)))
cor(log(dat3$CEA), log(dat3$post.CEA)) # 0.7657396 (강한 양의 상관관계)
cor(log(dat3$CEA), log(dat3$post.CEA), method = "spearman") # 0.7309942


# ------------------------------------------------------------
# [3. 단순선형회귀분석 (Simple Linear Regression)]
# 수술 전 수치(독립변수)로 수술 후 수치(반응변수)를 예측하는 모델을 만듭니다.
# ------------------------------------------------------------
# 회귀분석에 사용할 로그 변환 변수를 데이터프레임에 영구적으로 추가
dat4 <- dat3 %>% mutate(log.CEA = log(CEA),
                        log.post.CEA = log(post.CEA))

# lm(): 수술 전 log(CEA) 수치가 수술 후 log(post.CEA) 수치에 미치는 영향을 모델링
obj <- lm(log.post.CEA ~ log.CEA, data=dat4)
summary(obj)
# 적합된 회귀직선의 식: log.post.CEA = -0.65 + 1.24 * log.CEA
# (수술 전 수치가 1단위 증가할 때, 수술 후 수치가 평균적으로 1.24 증가함을 의미)

# 산점도 위에 추정된 회귀직선 겹쳐 그리기
ggplot(dat4, aes(log.CEA, log.post.CEA)) + 
  geom_point() + 
  geom_smooth(method="lm") 

cor(dat4$log.CEA, dat4$log.post.CEA) # 0.765 양의 상관관계 존재 


# ------------------------------------------------------------
# [4. 회귀모형을 이용한 예측 (Prediction)]
# 완성된 회귀직선을 활용해 새로운 환자의 수술 후 결과를 예측합니다.
# ------------------------------------------------------------
# 새로운 데이터(log.CEA 값이 1, 2, 3인 환자 3명) 생성
dat.new <- data.frame(log.CEA = c(1, 2, 3))

# predict(): 만들어진 모델(obj)에 새로운 데이터(dat.new)를 넣어 결괏값 도출
predict(obj, newdata = dat.new)
#        1        2        3 
# 0.588962 1.825803 3.062644 
# (위의 회귀식 y = -0.65 + 1.24x 에 x=1, 2, 3을 대입한 결과와 동일합니다)