# Title     : TODO
# Objective : TODO
# Created by: zoon0
# Created on: 2021-01-29

library(tidyverse)

y <- sleep$extra[sleep$group == 1]

summary(y)
sd(y)
par(mfrow = c(2, 2))
hist(y)
boxplot(y)
qqnorm(y); qqline(y)
hist(y, prob = TRUE)
lines(density(y), lty = 2)

t.test(y)
t.test(y, alternative = "greater")
curve(dnorm(x, 0, 1.8), -4, 4)

options(digits = 3)
set.seed(1606)
(y_star <- rnorm(10, 0, 1.8))
mean(y_star - 0); sd(y_star)
(t_star <- mean(y_star - 0) / (sd(y_star) / sqrt(length(y_star))))


set.seed(1606)
B <- 1e4
n <- 10
xbars_star <- rep(NA, B)
sds_star <- rep(NA, B)
ts_star <- rep(NA, B)

for (b in 1:B)
{
  y_star <- rnorm(n, 0, 1.789)
  m <- mean(y_star)
  s <- sd(y_star)
  xbars_star[b] <- m
  sds_star[b] <- s
  ts_star[b] <- m / (s / sqrt(n))
}

opar <- par(mfrow = c(2, 2))
hist(xbars_star, nclass = 100)
abline(v = 0.75, col = 'red')
hist(sds_star, nclass = 100)
abline(v = 1.789, col = 'red')
hist(ts_star, nclass = 100)
abline(v = 1.3257, col = 'red')
qqnorm(ts_star); qqline(ts_star)
par(opar)

length(which(ts_star > 1.3257)) / B
