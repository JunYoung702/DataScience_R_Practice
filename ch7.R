# Title     : TODO
# Objective : TODO
# Created by: zoon0
# Created on: 2021-01-29


library(tidyverse)
mpg_tbl <- tbl_df(mpg)

head(mpg_tbl)

opar <- par(mfrow = c(2, 2))
summary(mpg_tbl$hwy)
hist(mpg_tbl$hwy)
boxplot(mpg_tbl$hwy)
qqnorm(mpg_tbl$hwy);qqline(mpg_tbl$hwy)
par(opar)

# 수량형 일변량 변수
# 1. 시각화
# 2. 요약 통계량 계산
# 3. 정규성 검사
# 4. 가설검정, 신뢰구간
# 5. 이상점 찾기
hwy <- mpg_tbl$hwy
n <- length(hwy)
mu0 <- 22.9
t.test(hwy, mu = mu0, alternative = "greater")
t.test(hwy, mu = 23.0)
sd(hwy)
# 이상점: [Q1 - 1.5 * IQR, Q3 + 1.5 * IQR] 밖의 점
# IQR = Q3 - Q1
# 로버스트 통계 방법
# (mean, sd) <-> (median, mad) (mad: median absolute deviance)
mad(hwy)


# 성공-실패 범주형 변수
# 1. 요약 통계량 계산: table(), xtabs(), prop.table()
# 2. 시각화
# 3. 가설검정, 신뢰구간

set.seed(1606)
n <- 100
p <- 0.5
x <- rbinom(n, 1, p)
x <- factor(x, levels = c(0, 1), labels = c("no", "yes"))
table(x)
prop.table(table(x))
barplot(table(x))

binom.test(x = length(x[x=='yes']), n = length(x), p = 0.5, alternative = "two.sided")
# 오차한계: 신뢰구간의 크기의 절반
binom.test(x = 5400, n = 10000)

# 설명변수와 반응변수
# 수량형 x, 수량형 y

ggplot(mpg_tbl, aes(cty, hwy)) + geom_jitter() + geom_smooth(method = "lm")

# 상관계수
# pearson
# kendall, spearman (robust)
cor(mpg_tbl$cty, mpg_tbl$hwy)
with(mpg, cor(cty, hwy))
with(mpg, cor(cty, hwy, method="kendall"))
with(mpg, cor(cty, hwy, method="spearman"))

# 선형회귀
(hwy_lm <- lm(hwy ~ cty, data = mpg_tbl))
summary(hwy_lm)

# 예측값 계산
# predict()
# resid()

predict(hwy_lm, newdata = data.frame(cty = c(10, 20, 30)))
resid(hwy_lm)
predict(hwy_lm, newdata = data.frame(cty = c(10, 20, 30)), se.fit = TRUE)
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
plot(hwy_lm, las = 1)
par(opar)

# 로버스트 선형회귀
library(MASS)
set.seed(123)
lqs(stack.loss ~ ., data = stackloss)
lm(stack.loss ~ ., data = stackloss)
# 비선형 / 비모수적 방법
# 평활법
# LOESS

plot(hwy ~ displ, data = mpg)
mpg_lo <- loess(hwy ~ displ, data = mpg)
mpg_lo
summary(mpg_lo)

ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth()

# 범주형 x, 수량형 y
# ANOVA
(hwy_lm2 <- lm(hwy ~ class, data = mpg))
summary(hwy_lm2)

opar <- par(mfrow=c(2,2), oma = c(0, 0, 1.1, 0))
plot(hwy_lm2, las = 1)
par(opar)

# 수량형 x, 범주형 y

chall <- read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/challenger.csv')
chall <- tbl_df(chall)
glimpse(chall)

chall %>% ggplot(aes(temperature, distress_ct)) + geom_point()
chall %>% ggplot(aes(factor(distress_ct), temperature)) + geom_boxplot()

(chall_glm <- glm(cbind(distress_ct, o_ring_ct - distress_ct) ~ temperature, data = chall, family = "binomial"))
summary(chall_glm)
predict(chall_glm, data.frame(temperature = 30), type="response")

logistic <- function (x) {exp(x) / (exp(x) + 1)}
plot(c(20, 85), c(0, 1), type = "n", xlab = "temperature", ylab = "prob")
tp <- seq(20, 85, 1)
chall_glm_pred <- predict(chall_glm, data.frame(temperature = tp), se.fit = TRUE)
lines(tp, logistic(chall_glm_pred$fit))