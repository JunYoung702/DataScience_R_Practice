# Title     : Ch3. 데이터 가공
# Objective : 데이터 확인, 가공
# Created by: JunYoung
# Created on: 2021-01-17

# library
library(gapminder)
library(dplyr)
#----------------------- without dplyr -------------------------#
# 행과 열 선택
gapminder[gapminder$country=='Korea, Rep.', c('pop', 'gdpPercap')]

# 행 선택
gapminder[gapminder$country=='Korea, Rep.',]
gapminder[gapminder$year==2007,]
gapminder[gapminder$continent == 'Asia' & gapminder$year==2007,]
gapminder[gapminder$continent == 'Korea, Rep.' & gapminder$year==2007,]
gapminder[1:10,]
head(gapminder, 10)

# 정렬
# order(c1, c2, ...) c1 정렬 -> c2정렬 -> ...
# order(-c1) -> 역순
gapminder[order(gapminder$year, gapminder$country),]

# 변수 선택
gapminder[, c('pop', 'gdpPercap')]
gapminder[, 1:3]

# 변수명 바꾸기
f2 = gapminder
names(f2)
names(f2)[6] = "gdp_per_cap"

# 변수 변환과 변수 생성
f2$total_gdp = f2$pop + f2$gdp_per_cap
f2$total_gdp
f2[, "total_gdp"]

# 요약 통계량 계산
# apply(array, 방향, 함수)
# 방향: 1 == 행, 2 == 열
median(gapminder$gdpPercap)
apply(gapminder[, 4:6], 2, mean)
summary(gapminder)

#------------------------dplyr------------------------#
# dplyr load
# filter(df, 조건) (and slice()): 행 선택
# arrange(df, x1, x2, ...): 행 정렬
# select(df, x1, x2, ...): 변수/열 선택
# mutate(df, 타겟변수1 = 변환, ...): 변수 변환
# summarize(df, 타겟변수1 = 통계함수, ...): 변수 요약
# distinct()
# sample_n() and sample_frac()

# tbl_df(df): df가 tbl_df 클래스 속성을 가지게 되고 일부분을 출력한다.
# my_big_data <- tbl_df(read.csv('big_data.csv'))
i2 <- tbl_df(iris)

# glimpse(df): df를 전치하여 모든 변수를 볼 수 있게 하고, 데이터형을 나타내고, 처음 몇 개의 값을 출력한다.
glimpse(i2)

# pipe 연산자( %>% ) x %>% f(y) === f(x, y)
iris %>% head
i2 %>% head

gapminder %>% filter(country == 'Korea, Rep.')
f2 = gapminder
f3 = f2 %>% mutate(total_gdp = pop * gdpPercap, le_gdp_ratio = lifeExp / gdpPercap, lgrk = le_gdp_ratio)
# Vector -> scalar 함수(요약 통계량 함수)
# group_by와 같이 쓰면 더 강력
# n(): 현재 그룹의 관측치 개수
# n_distinct(x): x변수의 고유한 값 개수
# first(x), last(x), nth(x, n) == x[1], x[length(x)], x[n]
gapminder %>% summarise(n_obs = n(), 
                        n_countries = n_distinct(country), 
                        n_years = n_distinct(year), 
                        med_gdpc = median(gdpPercap),
                        max_gdppc = max(gdpPercap))

# sample_n()
# sample_frac()
sample_n(gapminder, 10) # 10줄 랜덤 샘플링
sample_frac(gapminder, 0.01) # 1% 랜덤 샘플링

# distinct
gapminder %>% select(country) %>% distinct()
gapminder %>% filter(country == 'Albania')
# slice
gapminder %>% slice(1, 3) # 1, 3번째 행
gapminder %>% slice(1:3) # 1 <= x <= 3
gapminder %>% head()

# group_by(dataset, grouping_variable)
# group_by 이후 summarize: 그룹별로 요약통계량 계산
#               select(): 그룹 변수를 항상 포함
#               sample_n(), sample_frac(): 그룹별로 랜덤 샘플링
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent)  %>%
  summarise(median(lifeExp))

gapminder %>% filter(year == 2007) %>% group_by(continent) %>% select(country)
gapminder %>% filter(year == 2007) %>% select(country)


df1 <- data_frame(x = c(1, 2), y = 2:1)
df2 <- data_frame(x = c(1, 3), a = 10, b = "a")
df1 %>% inner_join(df2)
df1 %>% left_join(df2)
df1 %>% right_join(df2)
df1 %>% full_join(df2)

df1 %>% intersect(df2)
df1 %>% union(df2)
df1 %>% setdiff(df2) # df1 - df2
