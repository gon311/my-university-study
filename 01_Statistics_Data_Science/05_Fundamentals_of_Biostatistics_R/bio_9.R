# ============================================================
# 바이오통계학 Chapter 9: 선형회귀분석 II
# (회귀직선 적합도 / 중선형회귀분석 / 추론 / 잔차분석)
# ============================================================
# Major Topics:
# 1. 중선형회귀분석 (Multiple Linear Regression) 적합 및 해석
# 2. 결정계수($R^2$)와 조정 결정계수(Adjusted $R^2$)
# 3. 회귀모형의 통계적 추론 (F-검정, t-검정, 회귀계수 신뢰구간)
# 4. 잔차분석 (Residual Analysis: 잔차도 및 Q-Q plot을 이용한 가정 검정)
# ------------------------------------------------------------

# ------------------------------------------------------------
# [1. 기본 설정 및 데이터 전처리]
# ------------------------------------------------------------

setwd("~/KNOU_stat/R_codes")
dat0 <- read.csv("biostat_ex_data.csv")
library(dplyr)

# 데이터 변환: 요인형 변환 및 임상적 임계값 기준 파생변수 생성
dat3 <- dat0 %>% 
  # 명목형/서열형 변수 일괄 factor 변환
  mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y, 
                 post.CA19.9.binary, post.CA19.9.3grp), as.factor) %>% 
  # 고혈압, CEA 수치 기준 이분형 파생변수 생성
  mutate(HTM = as.factor(ifelse(SBP >= 140, 1, 0)),             # 수축기혈압 140 이상: 고혈압
         CEA.grp = as.factor(ifelse(CEA > 5, 1, 0)),            # 수술 전 CEA > 5: 이상치
         post.CEA.grp = as.factor(ifelse(post.CEA > 5, 1, 0)))  # 수술 후 CEA > 5: 이상치

# ------------------------------------------------------------
# [2. 단순선형회귀분석 복습 (로그 변환)]
# ------------------------------------------------------------

# 치우친(skewed) 임상 수치에 자연로그(log) 변환 적용
dat4 <- dat3 %>% mutate(log.CEA = log(CEA),
                        log.post.CEA = log(post.CEA))

# 단순선형회귀 적합: 수술 전 CEA 로그값으로 수술 후 CEA 로그값 예측
obj <- lm(log.post.CEA ~ log.CEA, data = dat4)
summary(obj)

# [결과 해석 및 복습 포인트]
# - Multiple R-squared: 0.5864
# - 해석: 수술 전 CEA(로그값) 하나만으로 수술 후 CEA(로그값) 변동의 약 58.6%를 설명할 수 있음.


# ------------------------------------------------------------
# [3. 중선형회귀분석 (Multiple Linear Regression)]
# 반응변수(Y): 수축기혈압(SBP)
# 설명변수(X1, X2): 나이(age), 몸무게(weight)
# ------------------------------------------------------------

obj2 <- lm(SBP ~ age + weight, data = dat4)
summary(obj2)

# [결과 해석 및 복습 포인트]
# 1. 회귀방정식: \hat{SBP} = -9.49 + 0.86*(age) + 1.12*(weight)
#    - 나이가 1세 증가할 때 SBP는 평균 0.86 mmHg 증가 (몸무게 고정 시)
#    - 몸무게가 1kg 증가할 때 SBP는 평균 1.12 mmHg 증가 (나이 고정 시)
# 2. R-squared = 0.2937 / Adjusted R-squared = 0.2845
#    - 독립변수가 여러 개일 때는 페널티가 적용된 '조정 결정계수(Adjusted R^2)'를 확인.
#    - 본 모형은 수축기혈압 전체 변동량의 약 28.5%를 설명함.


# ------------------------------------------------------------
# [4. 회귀분석에서의 통계적 추론]
# ------------------------------------------------------------

# (1) 분산분석표(ANOVA Table)를 통한 변수별 F-검정
# Sequential Sum of Squares (Type I SS): 변수가 순차적으로 투입될 때의 기여도 측정
anova(obj2)
# - age    : F = 23.653 (p < 0.001) -> 나이는 SBP를 설명하는 데 유의함.
# - weight : F = 39.978 (p < 0.001) -> 나이가 투입된 상태에서 몸무게의 추가 설명력도 유의함.

# (2) 회귀모형 전체의 유의성 검정 (Overall F-test)
summary(obj2)
# summary(obj2)의 맨 아래 결과 참조
# - F-statistic: 31.82 (df1 = 2, df2 = 153), p-value: 2.795e-12 (< 0.001)
# - 귀무가설(H0: \beta_1 = \beta_2 = 0) 기각 -> 적어도 하나 이상의 독립변수는 SBP에 유의한 영향을 줌.

# (3) 회귀계수의 신뢰구간 (Confidence Interval)
# broom 패키지의 tidy() 함수를 사용하여 각 회귀계수의 95% 신뢰구간 도출
library(broom)
tidy(obj2, conf.int = TRUE)

# [결과 해석]
# - age (95% CI: [0.461, 1.260]): 구간 내 0을 포함하지 않으므로 \beta_age ≠ 0 (유의함).
# - weight (95% CI: [0.773, 1.480]): 구간 내 0을 포함하지 않으므로 \beta_weight ≠ 0 (유의함).


# ------------------------------------------------------------
# [5. 잔차분석 (Residual Analysis)]
# 회귀모형 추론의 기본 가정: 오차항(Error term) ~ i.i.d N(0, \sigma^2)
# (독립성, 등분산성, 정규성 가정 검정)
# ------------------------------------------------------------

# (1) 표준화잔차(Standardized Residuals) 및 예측값(\hat{Y}) 산출
std.res <- rstandard(obj2) # 잔차를 표준편차로 나눈 값 (대부분 -2 ~ +2 사이에 위치해야 함)
yhat <- predict(obj2)       # 모형으로 예측한 SBP 값

# (2) 잔차도 (Residual Plot): 등분산성 및 선형성 검정
plot(yhat, std.res, 
     xlab = "Fitted Values (yhat)", 
     ylab = "Standardized Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red")                  # 잔차 0 기준선
abline(h = c(-2, 2), lty = 2, col = "blue") # ±2 경계선

# [잔차도 해석 기준]
# - 깔때기 모양이나 곡선 등 별다른 경향성(Pattern)이 보이지 않아야 함.
# - 0을 중심으로 위아래 대칭을 이루며, 잔차의 대부분이 [-2, 2] 사이에 위치하면 등분산성과 선형성 가정을 만족함.

# (3) 잔차 정규확률도 (Normal Q-Q Plot): 정규성 검정
qqnorm(std.res, main = "Normal Q-Q Plot of Residuals")
qqline(std.res, col = "red")

# [Q-Q Plot 해석 기준]
# - 점들이 붉은 직선(qqline) 상에 가깝게 나열될수록 잔차가 정규분포를 따른다는 가정을 잘 만족함.