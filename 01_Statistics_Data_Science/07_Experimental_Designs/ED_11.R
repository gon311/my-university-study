# ============================================================
# 실험계획과 응용 Chapter 11: 요인배치법(3)
# (2^k 요인배치, 요인 회귀모형, 포화모형, 효과 크기 및 파레토 플롯)
# ============================================================
# Major Topics:
# 1. $2^2$ 요인배치법(Factorial Design)의 회귀모형 및 교차작용 해석
# 2. $2^4$ 요인배치법(16개 실행)과 포화모형(Saturated Model)
# 3. 반복이 없는 $2^k$ 실험에서의 파레토 플롯(Pareto Plot)을 통한 주효과/교차작용 시각화
# ------------------------------------------------------------

# ------------------------------------------------------------
# [1. 2^2 요인배치법: 팝콘 예제]
# 독립변수(요인) 코딩: -1 (Low level), +1 (High level)
# - 요인 A: 가열시간 [ -1: 160초, +1: 200초 ]
# - 요인 B: 알맹이 종류 [ -1: white, +1: yellow ]
# ------------------------------------------------------------

# 요인 수준(-1, 1) 정의 및 요인 조합(Grid) 생성
A <- B <- c(-1, 1)
design <- expand.grid(A = A, B = B)

A <- design$A
B <- design$B
y <- c(52, 74, 62, 80) # 반응변수 (팝콘이 튀겨진 비율/량)

# (1) 풀 모형 적합 (주효과 A, B 및 2원 교차작용 A:B 포함)
popped_cornM1 <- lm(y ~ A + B + A:B)
summary(popped_cornM1)

# [회귀방정식 및 계수 해석]
# 회귀식: \hat{Y_i} = 67 + 10*A_i + 4*B_i - 1*A_i*B_i
# - (Intercept) = 67: 요인 레벨이 (-1, +1)로 직교 코딩(Orthogonal coding)되었을 때 절편은 전체 평균(\bar{Y})을 의미함.
# - A 계수 = 10: 요인 A가 1단위 증가할 때의 평균 변화량 -> 주효과(Main Effect of A)는 계수의 2배인 20.
# - B 계수 = 4 : 요인 B의 계수 -> 주효과(Main Effect of B)는 8.
# - A:B 계수 = -1: 요인 A와 B 간의 교차작용 계수.

# (2) 축소 모형 적합 (교차작용 제거 후 주효과 모형 적합)
popped_cornM2 <- lm(y ~ A + B)
summary(popped_cornM2)
# - 직교설계(Orthogonal Design)이므로 A와 B의 주효과 회귀계수(10, 4) 및 절편(67)은 교차작용 항을 제거해도 변화하지 않음.


# ------------------------------------------------------------
# [2. 2^4 요인배치법: 제품 강도 예제]
# 요인: 온도(A/temp), 습도(B/humid), 압력(C/press), 진동(D/vib)
# 총 실행 횟수 N = 2^4 = 16 (반복 없음, n = 1)
# ------------------------------------------------------------

# 반응변수 (제품 강도)
strength <- c(-1, 5, 9, 11, 0, 3, 4, 8, -1, -9, 1, -5, -9, -13, 5, -4)

# 요인 수준 설계 벡터 생성 (직교 배열)
temp  <- rep(c(rep(-1, 4), rep(1, 4)), 2) # 온도 (A)
humid <- rep(c(-1, -1, 1, 1), 4)           # 습도 (B)
press <- rep(c(-1, 1), 8)                  # 압력 (C)
vib   <- c(rep(-1, 8), rep(1, 8))          # 진동 (D)

# (1) 포화 모형 적합 (Saturated Model: 4원 교차작용까지 모두 포함)
# formula: (A+B+C+D)^4 -> 주효과 4개 + 2원 교차 6개 + 3원 교차 4개 + 4원 교차 1개 + 절편 1개 = 총 16개 모수
linear.model1 <- lm(strength ~ (temp + humid + press + vib)^4)
summary(linear.model1)

# [포화모형 통계적 유의점]
# - 관측치 수(N = 16)와 추정할 모수 수(p = 16)가 동일함.
# - 따라서 잔차 자유도(Residual Degrees of Freedom) = 16 - 16 = 0 이 됨.
# - Residual Standard Error, t-value, p-value가 계산되지 않음 (NaN/NA 발생).
# - 해결책: 고차 교차작용(3원, 4원)을 오차항(Residuals)으로 풀링(Pooling)하거나, 파레토 플롯/정규확률도로 유의한 효과 선별.

# (2) 3원 교차작용까지 포함된 축소 모형 (4원 교차작용을 잔차로 환원)
linear.model2 <- lm(strength ~ (temp + humid + press + vib)^3)
summary(linear.model2)
# - 4원 교차작용 1개 항목이 잔차로 전환되어 잔차 자유도 df = 1 확보.


# ------------------------------------------------------------
# [3. 파레토 플롯 (Pareto Plot) 함수 작성 및 평가]
# - 반복이 없는 포화모형에서 주효과 및 교차작용 효과의 절대적 크기(|Estimate|)를 시각화하는 핵심 분석 도구
# ------------------------------------------------------------

paretoPlot <- function(lsmodel) {
  # ggplot2 시각화 패키지 로드
  library(ggplot2)
  
  # 절편(Intercept, 1번째 계수)을 제외한 나머지 모든 계수 추정치 추출
  coeff.full <- coef(lsmodel)[2:length(coef(lsmodel))]
  coeff.full <- na.omit(coeff.full) # 결측치 제거
  
  # 계수의 절대값 계산 (효과 크기 진단)
  coeff.abs <- unname(abs(coeff.full))
  
  # 크기순 정렬 및 인덱스 확보
  coeff <- sort(coeff.abs, index.return = TRUE)
  
  # 부호(양수/음수) 구별 라벨링
  grouping <- unname((coeff.full > 0)[coeff$ix])
  grouping[grouping == FALSE] <- "Negative"
  grouping[grouping == TRUE]  <- "Positive"
  
  # 요인 이름 정렬 및 요인형(factor) 변환
  temp_names <- names(coeff.full)[coeff$ix]
  fnames <- factor(temp_names, levels = temp_names, ordered = TRUE)
  group.colors <- c("Negative" = "grey", "Positive" = "black")
  
  # ggplot 시각화용 데이터프레임 구성
  dat <- data.frame(
    label = fnames,
    value = coeff$x, 
    group = grouping
  )
  
  # 가로 막대그래프 형태로 파레토 플롯 생성
  p <- ggplot(dat, aes(x = label, y = value, fill = group)) +
    geom_bar(stat = "identity") +
    coord_flip() + 
    theme_bw() +
    scale_fill_manual(values = group.colors, name = "Sign of coefficient") +
    xlab("Effect") +
    ylab("Magnitude of effect") +
    ggtitle("Pareto plot")
  
  p # 그래프 출력
  return(p)
}

# 파레토 플롯 실행: 포화모형(linear.model1)의 요인별 효과 크기 시각화
paretoPlot(linear.model1)

# [파레토 플롯 시각화 해석 기준]
# - 막대의 길이가 길수록 반응변수(제품 강도)에 미치는 영향(효과)이 큼.
# - 상단에 위치한 요인(예: 주효과 및 주요 2원 교차작용)을 선별하여 최종 모형을 재구성(Pooling)함.