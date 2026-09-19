# ============================================================
# 실험계획과 응용 Chapter 4: 일원배치법 II
# ============================================================
# Major Topics:
# 1. ANOVA 사후분석 (Post-Hoc Analysis: LSD, Bonferroni, Tukey HSD)
# 2. 비등반복 일원배치 분산분석 (Unbalanced One-way ANOVA)
# 3. 변량모형 / 랜덤모형 (Random Effects Model & Variance Components)
# ------------------------------------------------------------

# ------------------------------------------------------------
# [PART 1] 데이터 정의 및 사후분석 (Post-Hoc Analysis)
# ------------------------------------------------------------

# 1. 인자 수준(Factor Level)별 반응치 생성 (각 수준 n = 4)
a1 <- c(1.93, 2.38, 2.20, 2.25)
a2 <- c(2.55, 2.72, 2.75, 2.70)
a3 <- c(2.40, 2.68, 2.32, 2.28)
a4 <- c(2.33, 2.38, 2.28, 2.25)

# 2. 단일 벡터 병합 (총 N = 16)
wear <- c(a1, a2, a3, a4)

# 3. 집단 라벨링 (a1 ~ a4 각 4회 반복)
group <- rep(c("a1", "a2", "a3", "a4"), c(4, 4, 4, 4))

# 4. 분석용 데이터프레임 생성
wear.dat <- data.frame(group, wear)

# [사후검정 (Post-Hoc Comparison) 실행]
# 분산분석에서 귀무가설(모든 그룹의 평균이 같다)이 기각되었을 때, 
# '어느 그룹 간에 차이가 있는지' 구체적으로 규명하기 위해 수행함.

# (1) LSD (Least Significant Difference) 기준 다중비교
# - p.adj = "none": 1종 오류(Type I Error) 보정을 적용하지 않음.
# - 검정력이 높지만, 다중비교 시 1종 오류가 누적(Family-wise Error Rate 증가)되는 단점이 있음.
pairwise.t.test(wear.dat$wear, wear.dat$group, p.adj = "none")
# [결과 해석]
# - a2-a1 (p = 0.00036), a3-a1 (p = 0.03971), a3-a2 (p = 0.02290), a4-a2 (p = 0.00298)에서 유의한 차이 보임.

# (2) Bonferroni 보정 적용
# - p.adj = "bonferroni": 전체 비교 횟수(k)만큼 유의확률을 조정(p * k).
# - 1종 오류를 엄격하게 통제하지만, 보정이 지나치게 보수적이어서 2종 오류 위험이 높아짐.
pairwise.t.test(wear.dat$wear, wear.dat$group, p.adj = "bonferroni")
# [결과 해석]
# - 보정 후 a2-a1 (p = 0.0021), a4-a2 (p = 0.0179) 두 쌍만 유의수준 0.05에서 유의함.

# (3) Tukey의 HSD (Honestly Significant Difference) 검정
# - 일원배치 ANOVA에서 각 집단의 반복수가 같을 때 1종 오류율을 유의수준 \alpha 수준으로 잘 유지하는 표준적인 방법.
aov.out <- aov(wear ~ group, data = wear.dat)
TukeyHSD(aov.out)
# [결과 해석]
# - a2-a1 (p adj = 0.00175) 및 a4-a2 (p adj = 0.01368) 간 평균 차이가 유의함.
# - diff: 두 집단 간 평균 차이 / lwr, upr: 95% 신뢰구간 하한값 및 상한값

# (4) Tukey HSD 결과 시각화
plot(TukeyHSD(aov.out))
# [시각화 해석]
# - 신뢰구간 선분(Horizontal Lines)이 '0'(점선)을 포함하지 않는 쌍(a2-a1, a4-a2)이 통계적으로 유의미한 차이를 보이는 집단임.


# ------------------------------------------------------------
# [PART 2] 반복수가 같지 않은 일원배치법 (Unbalanced One-Way ANOVA)
# ------------------------------------------------------------

# 각 집단별 표본 크기가 다름 (n1 = 5, n2 = 6, n3 = 6, n4 = 6, 총 N = 23)
a1 <- c(62, 60, 63, 59, 61)
a2 <- c(63, 67, 71, 64, 65, 66)
a3 <- c(68, 66, 71, 67, 68, 68)
a4 <- c(56, 62, 60, 61, 63, 64)

y <- c(a1, a2, a3, a4)
x <- rep(c("a1", "a2", "a3", "a4"), c(5, 6, 6, 6))

# 분산분석 실행
anova_unbal <- aov(y ~ x)
summary(anova_unbal)
# [결과 해석]
# - F-value = 13.31, Pr(>F) = 6.49e-05 (***)
# - p-value < 0.05 이므로 최소한 한 집단의 평균은 나머지 집단과 다르다.

# 임계값 계산 (유의수준 0.01, 자유도 3, 19)
qf(p = 0.01, df1 = 3, df2 = 19, lower.tail = FALSE) # 5.010287
# - 계산된 F값(13.31)이 임계값(5.01)보다 크므로 귀무가설 기각.

# 각 집단별 기술통계량 산출 (평균 및 개수)
tapply(y, x, mean)   # a1: 61, a2: 66, a3: 68, a4: 61
tapply(y, x, length) # a1: 5,  a2: 6,  a3: 6,  a4: 6

# 상자그림(Boxplot) 시각화
boxplot(y ~ x, col = "lightgray", main = "Unbalanced Data Distribution")
# [분석]
# - a2, a3의 중앙값이 a1, a4에 비해 확연히 높음.
# - a3 수준에서 70 이상의 이상치(Outlier) 관찰.

# 사후검정 비교
pairwise.t.test(y, x, p.adj = "none")       # 보정 없음
pairwise.t.test(y, x, p.adj = "bonferroni") # 본페로니 보정
TukeyHSD(anova_unbal)                       # Tukey HSD (비등반복 시 Kramer 수정 방식 적용)
# [결과]
# - a2-a1, a3-a1, a4-a2, a4-a3 비교 쌍에서 유의미한 평균 차이가 존재함.


# ------------------------------------------------------------
# [PART 3] 변량모형 / 랜덤모형 (Random Effects Model)
# ------------------------------------------------------------
# 모형: Y_ij = \mu + A_i + \epsilon_ij
# - 고정모형(Fixed Model): 관심 있는 특정 처리(Factor Level)의 평균 차이 검정이 목적.
# - 변량모형(Random Model): 모집단에서 무작위로 추출된 처리들의 '변동성(Variance Components)' 추정이 목적.

b1 <- c(74, 76, 75)
b2 <- c(68, 71, 72)
b3 <- c(75, 77, 77)
b4 <- c(72, 74, 73)
b5 <- c(79, 81, 79)

y_rnd <- c(b1, b2, b3, b4, b5)
x_rnd <- rep(c("b1", "b2", "b3", "b4", "b5"), c(3, 3, 3, 3, 3))

# 배치 간 분포 시각화
boxplot(y_rnd ~ x_rnd, main = "Random Effect Levels")

# 일원배치 분산분석표
anova_rnd <- aov(y_rnd ~ x_rnd)
summary(anova_rnd)
# [분산분석표 해석]
# - MS_A (x) = 36.93, MS_E (Residuals) = 1.80
# - F = MS_A / MS_E = 20.52 (p = 8.25e-05) -> 처리 간 변동이 유의함.

# 혼합모형 분석 패키지 로드 및 혼합모형 적합
# (1|x): x를 변량 요인(Random Intercept)으로 지정
library(lme4)

fit_rand <- lmer(y_rnd ~ (1 | x_rnd))
summary(fit_rand)

# [분산성분(Variance Components) 해석]
# - Random Effects:
#   - Group (x_rnd) Std.Dev. = 3.422 -> 집단 간 표준편차 (\hat{\sigma_A})
#   - Residual Std.Dev. = 1.342      -> 오차 표준편차 (\hat{\sigma_E})
# - 분산 추정치:
#   - \hat{\sigma_E}^2 = 1.80 (= MS_E)
#   - \hat{\sigma_A}^2 = (MS_A - MS_E) / n = (36.93 - 1.80) / 3 = 11.71 (3.422^2)

# 변량 성분 및 모평균의 95% 프로파일 신뢰구간 산출
confint(fit_rand)
# [결과 해석]
# - .sig01     : 집단 간 표준편차(\sigma_A)의 95% 신뢰구간 [1.692, 6.794]
# - .sigma     : 오차 표준편차(\sigma_E)의 95% 신뢰구간 [0.915, 2.240]
# - (Intercept): 전체 모평균(\mu)의 95% 신뢰구간 [71.492, 78.241]