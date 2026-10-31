# ============================================================
# 바이오통계학 Chapter 11: 진단검사의 평가 
# 
# ============================================================


setwd('~/KNOU_stat/R_codes')
dat0 <- read.csv('biostat_ex_data.csv')
library(dplyr)


dat4 <- dat0 %>% 
  mutate_at(vars(sex, Recur, stage, smoking, obesity, 
                 Recur_1y, post.CA19.9.binary, post.CA19.9.3grp),
            as.factor)
table(dat4$post.CA19.9.binary)
#  0  1 
# 61 95 

tapply(dat4$post.CA19.9, dat4$post.CA19.9.binary, summary)  # 범주화 체크 
# 전자 max 36.82, 후자 min 37.06 // 37을 기준으로 잘 나뉘어져 있음 

# 2x2 table 
xtabs(~post.CA19.9.binary+Recur_1y, data=dat4)
#                  Recur_1y
#post.CA19.9.binary  0  1
#                 0 51 10
#                 1 51 44


#install.packages("epiR")
library(epiR)
epi.tests(xtabs(~post.CA19.9.binary+Recur_1y, data=dat4)[2:1, 2:1])
# 1행 위혐요소 노출, 2행 위험요소 비노출
# 1열 질병 유, 2열 질병 무 
#           Outcome +    Outcome -      Total
#Test +           44           51         95
#Test -           10           51         61
#Total            54          102        156

#Sensitivity *                          0.81 (0.69, 0.91)
#Specificity *                          0.50 (0.40, 0.60)
#Positive predictive value *            0.46 (0.36, 0.57)
#Negative predictive value *            0.84 (0.72, 0.92)


# ROC 곡선
#install.packages("pROC")
library(pROC)

fit <- roc(Recur_1y~post.CA19.9, data=dat4)
fit # AUC = 0.7259
plot(fit)

coords(fit)
coords(fit, x="best", best.method="youden")
#  threshold specificity sensitivity
#1  45.96444   0.7745098   0.6111111

coords(fit, x ="best", best.method="closest.topleft")
#  threshold specificity sensitivity
#1  45.96444   0.7745098   0.6111111