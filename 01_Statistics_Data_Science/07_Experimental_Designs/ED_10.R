# ============================================================
# 실험계획과 응용 Chapter 10: 요인배치법(2) - 반복 없는 2^k 요인배치
# ============================================================

# ------------------------------------------------------------
# [1] 2^3 요인배치 분산분석 (수동 풀링 및 단계적 제거)
# 반복(Replication)이 없는 실험에서는 최고차 상호작용을 오차항으로 간주해야 합니다.
# ------------------------------------------------------------
gang <- c(2, -5, -12, -17, 15, 13, -2, -7)
temp <- rep(c(0,1), 4)
humidity <- c(rep(0, 4), rep(1, 4))
pressure <-  rep(c(0,0,1,1), 2)

data <- data.frame(gang, temp, humidity, pressure)

# 범주형 요인(Factor)으로 변환
data$temp <- factor(data$temp, levels=c(0, 1), labels = c("a0", "a1"))
data$humidity <- factor(data$humidity, levels=c(0, 1), labels = c("b0", "b1"))
data$pressure <- factor(data$pressure, levels=c(0, 1), labels=c("c0", "c1"))

# 💡 포화모형 (Saturated Model) 적합 시도
ano <- aov(gang ~ temp*humidity*pressure, data=data)
summary(ano)
# [오류 원인] 관측치가 8개인데 추정해야 할 모수도 8개(주효과3+2차교호3+3차교호1+절편1)입니다.
# 잔차 자유도(Df)가 0이 되어 오차분산을 구할 수 없으므로 F-검정이 불가능합니다.

# 💡 수동 풀링(Pooling) 및 후진 제거(Backward Elimination)
# 1단계: 3차 교호작용(temp:humidity:pressure)을 오차항으로 넘기고 2차까지만 적합
ano1 = aov(gang ~ temp+humidity+pressure+temp:humidity+temp:pressure+humidity:pressure, data=data)
summary(ano1) 
# 해석: temp:pressure의 p-value(0.8743)가 가장 크므로 모형에서 제거합니다.

# 2단계: temp:pressure 제거 후 재적합
ano2 = aov(gang ~ temp+humidity+pressure+temp:humidity+humidity:pressure, data=data)
summary(ano2)
# 해석: temp:humidity의 p-value(0.2998)가 유의수준 0.05보다 크므로 제거합니다.

# 3단계: 최종 모형 적합
ano3 = aov(gang ~ temp+humidity+pressure+humidity:pressure, data=data)
summary(ano3)
# 해석: 주효과 A(temp), B(humidity), C(pressure) 모두 매우 유의하며, B와 C 간의 상호작용도 유의합니다.

# 요인별 평균을 확인하여 강도(gang)가 가장 높게 나오는(최적화) 수준을 찾습니다.
tapply(gang, temp, mean)     # A0 수준일 때 높음
tapply(gang, humidity, mean) # B1 수준일 때 높음
tapply(gang, pressure, mean) # C0 수준일 때 높음
# 🌟 최적 조건 도출: A0, B1, C0 


# ------------------------------------------------------------
# [2] 2^4 요인배치 분산분석 (AIC 기반 단계적 선택법)
# -1과 1로 코딩된 직교 데이터에서 알고리즘을 이용해 최적의 모형을 탐색합니다.
# ------------------------------------------------------------
strength <- c(-1, 5, 9, 11, 0, 3, 4, 8, -1, -9, 1, -5, -9, -13, 5, -4)
temp <- rep(c(rep(-1, 4), rep(1, 4)), 2) 
humid <- rep(c(-1, -1, 1, 1), 4)         
press <- rep(c(-1, 1), 8)                
vib <- c(rep(-1, 8), rep(1, 8))          

strength.data <- data.frame(strength, temp, humid, press, vib)

# 분석용 팩터 데이터프레임 생성
temp=as.factor(strength.data[,2])
humid=as.factor(strength.data[,3])
press=as.factor(strength.data[,4])
vib=as.factor(strength.data[,5])
df=data.frame(temp, humid, press, vib, strength)

# 1) 최대 3차 교호작용까지만 포함하여 초기 모형 적합 (^3 기호 사용)
upto3 = aov(strength~(temp+humid+press+vib)^3, data=df)
summary(upto3) 
# 잔차 자유도가 낮아 검정력이 떨어지므로 불필요한 변수를 쳐내야 합니다.

# 2) AIC(아카이케 정보 기준) 기반 후진 제거법 (Stepwise Regression)
# AIC는 모형의 '설명력'과 '복잡도(변수 개수)'의 균형을 맞추는 지표로, 값이 작을수록 좋습니다.
sreg=step(upto3, direction="backward")  
summary(sreg)
# step() 함수가 자동으로 불필요한 항을 제거해주지만, 
# 남은 항들 중에서도 변동(Sum Sq)이 지나치게 작은 것들은 수동으로 추가 제거(풀링)하여 모형을 더 단순화합니다.

# 3) 특정 교호작용 항을 직접 계산하여 독립적인 변수로 추가
# 데이터가 -1과 1로 코딩되어 있기 때문에, 변수끼리 직접 곱하면 교호작용 대비(Contrast)가 됩니다.
t=strength.data[,2]; h=strength.data[,3]; p=strength.data[,4]; v=strength.data[,5]

p.v = p*v        # 압력과 진동의 2차 교호작용
t.h.v = t*h*v    # 온도, 습도, 진동의 3차 교호작용

df$press.vib = as.factor(p.v)
df$temp.humid.vib = as.factor(t.h.v)

# 4) 유의미하다고 판단된 핵심 변수들만 모아 최종 축소 모형(Reduced Model) 적합
redmod=aov(formula=strength ~ temp+humid+press+vib+press.vib+temp.humid.vib)
summary(redmod)
# [핵심 인사이트] 
# 3차 교호작용(temp.humid.vib)의 제곱합(36.0)이 주효과(temp)의 제곱합(16.0)보다 큽니다.
# 이는 단순히 주효과들의 조합만으로 결과를 예측할 수 없으며, 요인들 간의 복잡한 시너지(상호작용)를 반드시 고려해야 함을 뜻합니다.

# 5) 최종 모델을 이용한 최적 조합 탐색
# 현재 데이터프레임의 모든 조건(16개)에 대해 최종 모델로 예측값(pred)을 계산합니다.
df$pred=predict(redmod, newdata=df)

# 예측값이 가장 높은(최대 강도) 행의 인덱스를 추출하여 최적 조건 확인
best.index=which.max(df$pred)
best=df[best.index,]
best
# 🌟 최적의 조합 = A(-1, 0수준), B(1, 1수준), C(1, 1수준), D(-1, 0수준)
# 모델이 추정한 예측값: 12.625 / 실제 관측값: 11