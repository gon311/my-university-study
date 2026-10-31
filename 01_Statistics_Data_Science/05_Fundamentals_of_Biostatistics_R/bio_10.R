# ============================================================
# 바이오통계학 Chapter 10: 선형회귀분석 III
# (범주형 독립변수, 가변수(Dummy Variable), 기준범주 변경, 전체 유의성 검정)
# ============================================================
# Major Topics:
# 1. 범주형 독립변수의 가변수(Dummy Variable) 처리 및 계수 해석
# 2. relevel() 함수를 이용한 참조범주(Reference Category) 변경
# 3. 다범주 변수(K ≥ 3)의 전체적인 통계적 유의성 평가
#    - 가능도비 검정 (Likelihood Ratio Test: lrtest)
#    - 분산분석 F-검정 (ANOVA F-test: anova)
# ------------------------------------------------------------

# ------------------------------------------------------------
# [1. 기본 설정 및 데이터 전처리]
# ------------------------------------------------------------

setwd('~/KNOU_stat/R_codes')
library(dplyr)

dat0 <- read.csv('biostat_ex_data.csv')

# 범주형 변수 요인화(factor) 및 임상 지표 기준 파생변수 생성
dat3 <- dat0 %>% 
  mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y, 
                 post.CA19.9.binary, post.CA19.9.3grp), as.factor) %>% 
  mutate(HTM = as.factor(ifelse(SBP >= 140, 1, 0)),             # 고혈압 여부 (140 이상)
         CEA.grp = as.factor(ifelse(CEA > 5, 1, 0)),            # 수술 전 CEA 임계값 초과 여부
         post.CEA.grp = as.factor(ifelse(post.CEA > 5, 1, 0)))  # 수술 후 CEA 임계값 초과 여부

# 치우친 CEA 변수의 로그 변환 (반응변수로 활용)
dat4 <- dat3 %>% mutate(log.CEA = log(CEA), 
                        log.post.CEA = log(post.CEA)) # 원본 코드의 log.post.CED 오탈자 수정


# ------------------------------------------------------------
# [2. 범주형 독립변수와 가변수(Dummy Variable)]
# 질문: 암 병기(stage 1, 2, 3)에 따라 수술 전 CEA 로그값(log.CEA)에 차이가 있는가?
# ------------------------------------------------------------

# stage 변수의 범주(Level) 확인
levels(dat4$stage)
# [1] "1" "2" "3" (총 3개 범주 -> (3-1)=2개의 가변수 자동 생성)
# - 기준범주(Reference): stage = 1 (Intercept에 흡수됨)
# - 가변수 X2: stage = 2이면 1, 아니면 0
# - 가변수 X3: stage = 3이면 1, 아니면 0
# - 회귀식: E(Y|X) = \beta_0 + \beta_2 * X2 + \beta_3 * X3

model.1 <- lm(log.CEA ~ stage, data = dat4)
summary(model.1)

# [회귀계수 해석 및 복습 포인트]
# 1. (Intercept) = 0.6287: stage=1 환자 그룹의 log.CEA 평균 예측값.
# 2. stage2 = 0.4026 (p = 0.0504): 
#    - stage=1 그룹 대비 stage=2 그룹의 log.CEA 평균은 0.4026 만큼 큼.
#    - 유의확률이 약 0.05로, 유의수준 0.05 경계선상에서 근소한 차이를 보임.
# 3. stage3 = 0.4316 (p = 0.0184): 
#    - stage=1 그룹 대비 stage=3 그룹의 log.CEA 평균은 0.4316 만큼 큼.
#    - p < 0.05 이므로 stage=1과 stage=3 간에는 통계적으로 유의미한 차이가 존재함.


# ------------------------------------------------------------
# [3. 참조범주(Reference Category) 변경 분석]
# 질문: stage=2와 stage=3 환자 그룹 간의 평균 차이는 유의한가?
# ------------------------------------------------------------

# relevel(variable, ref = 2): stage=2를 새로운 기준범주(Reference)로 설정
dat5 <- dat4 %>% mutate(stage.new = relevel(stage, ref = 2))
levels(dat5$stage.new)
# [1] "2" "1" "3" (stage=2가 첫 번째 범주로 변경됨)

model.2 <- lm(log.CEA ~ stage.new, data = dat5)
summary(model.2)

# [결과 해석 및 복습 포인트]
# 1. (Intercept) = 1.03134: stage=2 환자 그룹의 log.CEA 평균 예측값.
# 2. stage.new1 = -0.40265 (p = 0.0504): 
#    - stage=2 그룹 대비 stage=1 그룹의 차이 (model.1의 stage2 계수와 부호만 반대이고 동일).
# 3. stage.new3 = 0.02894 (p = 0.8666):
#    - stage=2 그룹 대비 stage=3 그룹은 log.CEA 값이 0.02894 만큼 높으나,
#    - p-value가 0.8666으로 매우 크므로 stage=2와 stage=3 간에는 통계적으로 유의미한 차이가 없음.


# ------------------------------------------------------------
# [4. 다범주 독립변수의 전체적 유의성 검정 (Overall Significance)]
# 질문: 나이(age)와 성별(sex)을 통제한 후에도 stage 변수 전체가 log.CEA를 설명하는 데 유의한가?
# ------------------------------------------------------------

# 풀 모형 (Full Model: age, sex, stage 모두 포함)
model.3 <- lm(log.CEA ~ age + sex + stage, data = dat4)
summary(model.3)
# [개별 계수 p-value의 한계]
# - stage2 (p = 0.170)와 stage3 (p = 0.170) 각 가변수 계수는 비유의하게 나옴.
# - 하지만 가변수 각각의 t-검정만으로는 "stage 변수 전체(3개 범주)"의 유의성을 결론지을 수 없음.

# (방법 1) 가능도비 검정 (Likelihood Ratio Test: LRT)
# - 축소 모형 (Reduced Model: stage 제외 모형) 적합 후 풀 모형과 비교
model.4 <- lm(log.CEA ~ age + sex, data = dat4)

library(lmtest)
lrtest(model.3, model.4)

# [LRT 결과 해석]
# - #Df 6 vs 4 (자유도 차이 = 2, 가변수 2개의 제거 효과 검정)
# - Chisq = 2.3851, Pr(>Chisq) = 0.3034
# - p-value > 0.05 이므로, age와 sex가 이미 통제된 모형에 stage 변수를 추가하는 것은 통계적으로 유의미한 설명력 향상을 가져오지 못함.

# (방법 2) 분산분석 F-검정 (ANOVA F-test)
# - 순차적 분산분석(Type I SS)을 통해 stage 변수 집단 전체에 대한 F-검정 수행
anova(model.3)

# [ANOVA 결과 해석]
# - age   : F = 0.0604 (p = 0.806)
# - sex   : F = 13.2878 (p = 0.00037 ***) -> 성별은 매우 유의함.
# - stage : Df = 2, F = 1.1632, Pr(>F) = 0.3153 -> stage 변수 전체의 F-검정 p-value가 0.3153 (> 0.05)으로 비유의함.
# - 결론: LRT와 ANOVA F-검정 모두에서 다른 변수 통제 시 stage 변수의 전체 유의성은 인정되지 않음.