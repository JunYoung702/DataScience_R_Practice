# Title     : Ch4. 시각화
# Objective : 데이터 시각화 연습
# Created by: JunYoung
# Created on: 2021-01-17

# library
library(dplyr)
library(ggplot2)
library(gapminder)

# glimpse
glimpse(gapminder)

# opar = par(mfrow = c(2, 2))
opar = par(mfrow = c(2, 2))

hist(gapminder$lifeExp)
hist(gapminder$lifeExp, nclass = 50)
hist(gapminder$gdpPercap, nclass = 50)
# 변수 변환: 치우친 데이터에 사용
hist(sqrt(gapminder$gdpPercap), nclass = 50)
hist(log10(gapminder$gdpPercap), nclass = 50)
plot(log10(gapminder$gdpPercap), gapminder$lifeExp, cex = .5)
par(opar)
plot(gapminder$lifeExp, gapminder$gdpPercap)

# 상관계수 증가
cor(gapminder$lifeExp, gapminder$gdpPercap)
cor(gapminder$lifeExp, log10(gapminder$gdpPercap))
cor(gapminder$lifeExp, sqrt(gapminder$gdpPercap))

# cor() default: method = 'pearson' (선형 관계)
#                method = 'kendall', method = 'spearman' (비선형일 때)

# r base graphic package:
# plot(x, y): 산점도
# hist(x): 히스토그램
# boxplot(x): 상자그림
# mosaicplot(): 모자익 플롯
# point(x, y): 저차원 점 그리는 함수
# lines(x, y): 저차원 선 그리는 함수

# ggplot2
gapminder %>% ggplot(aes(x = lifeExp)) + geom_histogram()
gapminder %>% ggplot(aes(x = gdpPercap)) + geom_histogram()
gapminder %>% ggplot(aes(x = gdpPercap)) +
  geom_histogram() +
  scale_x_log10()
gapminder %>% ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth()

example(ggplot)

# 히스토그램, 로그변환변수 히스토그램, 도수폴리곤, 커널밀도추정
gapminder %>% ggplot(aes(x = gdpPercap)) + geom_histogram()
gapminder %>% ggplot(aes(x = gdpPercap)) +
  geom_histogram() +
  scale_x_log10()
gapminder %>% ggplot(aes(x = gdpPercap)) +
  geom_freqpoly() +
  scale_x_log10()
gapminder %>% ggplot(aes(x = gdpPercap)) +
  geom_density() +
  scale_x_log10()

diamonds %>% ggplot(aes(cut)) + geom_bar()
# 도수분포, 상대도수, 퍼센트
table(diamonds$cut)
prop.table(table(diamonds$cut))
round(prop.table(table(diamonds$cut)) * 100, 1)

# with dplyr
diamonds %>%
  group_by(cut) %>%
  tally() %>%
  mutate(pct = round(n / sum(n) * 100, 1))

diamonds %>% ggplot(aes(carat, price)) + geom_point()
diamonds %>% ggplot(aes(carat, price)) + geom_point(alpha = 0.01)
mpg %>% ggplot(aes(cyl, hwy)) + geom_point()
mpg %>% ggplot(aes(cyl, hwy)) + geom_jitter()
mpg %>% select(cyl, hwy)
# pairs: 산점도행렬(r base)
pairs(diamonds %>% sample_n(1000))

# X, Y 변수: X가 범주형 변수일 때
mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()

mpg %>% ggplot(aes(class, hwy)) +
  geom_jitter(col = 'gray') +
  geom_boxplot(alpha = .5)
mpg %>%
  mutate(class = reorder(class, hwy, median)) %>%
  ggplot(aes(class, hwy)) +
  geom_jitter(col = 'gray') +
  geom_boxplot()
mpg %>%
  mutate(class = factor(class, levels = c("2seater", "subcompact", "compact", "midsize", "minivan", "suv", "pickup"))) %>%
  ggplot(aes(class, hwy)) +
  geom_jitter(col = 'gray') +
  geom_boxplot(alpha = .5)
mpg %>%
  mutate(class = factor(class, levels = c("2seater", "subcompact", "compact", "midsize", "minivan", "suv", "pickup"))) %>%
  ggplot(aes(class, hwy)) +
  geom_jitter(col = 'gray') +
  geom_boxplot(alpha = .5) +
  coord_flip()

# 범주형 변수 2개(잘안쓰임)
# xtabs: 도수 분포를 알아내는 용도 (고차원 행렬)
glimpse(data.frame(Titanic))
xtabs(Freq ~ Class + Sex + Age + Survived, data.frame(Titanic))

# mosaicplot
mosaicplot(Titanic, main = "Survival on the Titanic", color = TRUE)

apply(Titanic, c(3, 4), sum)
round(prop.table(apply(Titanic, c(3, 4), sum), margin = 1), 3)
# with dplyr
t2 = data.frame(Titanic)
t2 %>%
  group_by(Sex) %>%
  summarise(n = sum(Freq), survivors = sum(ifelse(Survived == "Yes", Freq, 0))) %>%
  mutate(rate_survival = survivors / n)

# 2차원 이상의 시각화:
# 색깔, 모양 등
# 플롯을 변수의 각 범주별로 나열
gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point(aes(size = pop, col = continent)) +
  scale_x_log10() +
  ggtitle("Gapminder data for 2007")

gapminder %>% ggplot(aes(year, lifeExp, group = country, col = continent)) + geom_line()
gapminder %>% ggplot(aes(year, lifeExp, group = country)) +
  geom_line() +
  facet_wrap(~continent)
