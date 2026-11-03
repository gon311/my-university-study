# ============================================================
# 실험계획과 응용 Chapter 9: 요인배치법(1) 
# ============================================================

# 3^2 요인배치 및 대비
# 1) 데이터 준비 및 범주형 팩터(Factor) 변환
gang <- c(11, 18, 25, 1, 6, 14, 6, 15, 18)  
wol <- c(rep(0, 3), rep(1, 3), rep(2, 3))   
temp <- c(rep(c(0, 1, 2), 3))               

plastic.data <- data.frame(gang, wol, temp)

# 수치형 데이터를 분석을 위해 명목형 팩터로 변환합니다.
plastic.data$wol <- factor(plastic.data$wol, levels=c(0, 1, 2),
                           labels=c("a0", "a1", "a2"))  # a0=자사, a1=국내 타사, a2=해외
plastic.data$temp <- factor(plastic.data$temp, levels=c(0, 1, 2), 
                            labels=c("b0", "b1", "b2")) # b0=100도, b1=110도, b2=120도

# 2) 기본 이원배치 분산분석 (Two-way ANOVA)
anova <- aov(gang ~ wol + temp, data=plastic.data)
summary(anova)
# [결석 해석] wol(p=0.0017)과 temp(p=0.0009) 모두 유의수준 0.05보다 작아 강도에 유의미한 영향을 미침.

# 3) 직교 대비(Orthogonal Contrasts) 행렬 설정
# 계수의 합이 0이 되도록 설정하여 그룹 간의 '특정 비교'를 수행합니다.

# [wol 대비]
# c1: 국산(a0, a1)의 평균 vs 외제(a2) 비교 -> 계수 c(1, 1, -2)와 동일한 비율
c1 = c(1/6, 1/6, -1/3) 
# c2: 국산 중 자사(a0) vs 타사(a1) 비교 (a2는 제외하므로 0) -> 계수 c(1, -1, 0)과 동일한 비율
c2 = c(1/3, -1/3, 0)    
mat.wol = cbind(c1, c2)
contrasts(plastic.data$wol) = mat.wol # wol 팩터에 대비 행렬 적용

# [temp 대비]
# 온도는 간격이 일정한 연속형 성격을 띠므로 다항식 대비(Polynomial Contrast)를 적용합니다.
c3 = c(-1, 0, 1) # Linear (1차 선형 효과 검정: 100도 vs 120도)
c4 = c(1, -2, 1) # Quadratic (2차 곡선 효과 검정: 양끝 평균 vs 110도)
mat.temp = cbind(c3, c4)
contrasts(plastic.data$temp) = mat.temp

# 4) 대비가 적용된 세부 분산분석
mod.contrast = aov(gang ~ wol+temp, data=plastic.data)
# split 인자를 사용하여 주효과의 자유도와 제곱합(Sum Sq)을 대비 행렬 기준으로 쪼개어 출력합니다.
summary(mod.contrast, split=list(wol=list("국산과 외제"=1, "자사와 국내 타 회사"=2),
                                 temp=list("linear"=1, "quadratic"=2)))

# [💡 핵심 분석 결과]
# - wol의 총 제곱합 182 중 181.5가 '자사와 국내 타 회사'의 차이에서 발생 (국산vs외제 차이는 거의 없음)
# - temp의 총 제곱합 254 중 253.5가 'linear(선형)' 효과에서 발생 (온도 증가에 따라 강도가 선형적으로 변함)

# ============================================================
# 2^2 요인배치 분산분석 및 교호작용(Interaction) 플롯
# ============================================================

# 1) 데이터 준비 (2수준 요인 A, B)
gang = c(4, 6, 3, 7, -2, 2, -4, -6)
temp = c(0,0,0,0,1,1,1,1)
humidity = c(0,0,1,1,0,0,1,1)
ex7.2data = data.frame(temp, humidity, gang)

ex7.2data$a = factor(ex7.2data$temp, levels=c(0,1), labels=c("A0", "A1"))
ex7.2data$b = factor(ex7.2data$humidity, levels=c(0, 1), labels=c("B0", "B1"))

# 2) 교호작용도 (Interaction Plot) 그리기
# x축을 temp로, 선을 humidity 그룹으로 나누어 강도의 평균(fun=mean)을 그립니다.
with(ex7.2data, interaction.plot(x.factor=temp, trace.factor = humidity,
                                 response=gang, fun=mean, type="b",
                                 legend = T, ylab="강도",
                                 main="Interaction Plot", pch=c(1, 19)))
# [해석] 두 선이 평행에 가깝게 그려지면 "온도와 습도 간에 상호작용이 없다"고 시각적으로 판단합니다.

# 3) 기술통계 및 상자그림(Boxplot) 시각화
# 요인 조합별 평균 계산
with(ex7.2data, tapply(gang, list(a, b), mean))

boxplot(gang ~ temp)     # A0와 A1 간의 상자 높낮이 차이가 뚜렷함 (온도의 영향 큼)
boxplot(gang ~ humidity) # B0와 B1 간의 차이가 겹침 (습도의 영향 불확실)

# 4) 교호작용을 포함한 이원배치 분산분석
# 식에서 '+' 대신 '*'를 사용하면 두 주효과(temp, humidity)와 상호작용(temp:humidity)을 모두 모델링합니다.
aov.out = aov(gang ~ temp * humidity, data=ex7.2data)
summary(aov.out)

# [💡 핵심 분석 결과]
# A(temp)의 p-value는 0.00901로 유의함.
# B(humidity)와 AB교호작용(temp:humidity)은 p-value가 0.189로 0.05보다 커서 통계적으로 유의하지 않음.
# 즉, 제품 강도에 영향을 주는 것은 '온도'뿐이며 습도나 두 요인의 결합 효과는 무시해도 됩니다.
