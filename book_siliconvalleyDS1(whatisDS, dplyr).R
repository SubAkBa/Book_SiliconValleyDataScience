# 1. 데이터 과학 프로세스
# - 데이터 과학의 과정
# (1) 문제 정의(Problem Definition) : 현실의 구체적인 문제를 명확하게 표현하고 
#                                     통계적, 수리적 언어로 '번역'하는 작업
# (2) 데이터 정의(Data Definition) : 변수(variable), 지표(metric) 등을 정의
# (3) 실험 계획(Design of Experiment) or 표본화(Sampling)
#     : 데이터를 수집하는 목적 -> (3)-1 : 어떤 처리의 효과를 알아내기 위한 통제 실험(Randomized Controlled Experiment)
#                                         - 실험 / 통제 집단을 어느정도 크기로 정의할지를 정하는 것과 같은 문제를 결정하는 분야가 실험 계획
#                                 (3)-2 : 모집단을 대표하는 표본을 얻기 위한 표본화(Sampling)
#                                         - 표본 크기(sample size)가 커지면 통계적 정확도가 높아지고 검정력(Statistical Power)도 높아지므로
#                                           어느 정도의 정확도를 원하는지 결정해야 실제 조사를 진행할 때의 표본 크기를 결정할 수 있다.
# (4) 데이터 취득(Data acquisition) : 다양한 형태, 다양한 시스템에 저장된 원데이터를 분석 시스템으로 가져오는 활동이다.
# (5) 데이터 가공(Data processing, Data Wrangling) : 데이터를 분석하기 적당한 표 형태로 가공하는 작업이다.
# (6) 탐색적 분석과 데이터 시각화(Exploratory Data Analysis, Data Visualization)
#     : 시각화와 간단한 통계량을 통하여 데이터의 패턴을 발견하고 이상치를 점검하는 분석이다.
# (7) 모형화(Modeling) : 모수 추정, 가설검정 등의 활동과 모형분석, 예측분석 등을 포괄한다.
# (8) 분석 결과 정리(Reporting) : 분석 결과를 현실적인 언어로 이해하기 쉽도록 번역해내는 작업이다.
# ※ 이러한 선형적이면서 직선적인 과정은 현실적이지 못하다. 

# 2. 데이터 과학자가 갖춰야 할 능력
# (1) 실제적인 문제를 통계적으로 표현
# (2) 컴퓨터 도구를 사용하여 시각화와 데이터 가공과 모형화
# (3) 그를 이용하여 실제적인 언어로 의미 있는 결과를 만들어내는 능력

# 3. R의 기본 연산자
# (1) str() : 데이터 구조
# (2) [ : Vector와 Array 인덱싱
# (3) [[ : List 인덱싱
# (4) $ : List 이름 인덱싱
# (5) 집합 연산자 : intersect, union, setdiff, which
# (6) Vector와 Matrix 연산자 : c, matrix, array, length, dim, ncol, nrow, t, diag, as.matrix
# (7) Vector 생성 : c, rep, seq, rev
# (8) List 생성과 조작 : list, unlist, split, expand.grid
# (9) Factor 데이터 조작 : factor, levels, reorder, relevel, cut, findInterval
# (10) Array 데이터 생성 / 조작 : array, dim, dimnames, aperm
## 데이터를 로드한다. gapminder 패키지를 설치한다.
install.packages("gapminder")
library(gapminder)

## 행과 열 선택
gapminder[gapminder$country == 'Korea, Rep.', c('pop', 'gdpPercap')]

## 행 선택
gapminder[gapminder$country == 'Korea, Rep.', ]
gapminder[gapminder$year == 2007, ]
gapminder[gapminder$country == 'Korea, Rep.' & gapminder$year == 2007, ]
gapminder[1 : 10, ]
head(gapminder, 10)

## 정렬
gapminder[order(gapminder$year, gapminder$country), ]

## 변수 선택
gapminder[, c('pop', 'gdpPercap')]
gapminder[, 1 : 3]

## 변수명 바꾸기 : gdpPercap를 gdp_per_cap으로 변경
f2 <- gapminder
names(f2)
names(f2)[6] <- 'gdp_pre_cap'

## 변수 변환과 변수 생성
f2$total_gdp <- f2$pop * f2$gdp_pre_cap

## 요약 통계량 계산
median(gapminder$gdpPercap)
apply(gapminder[, 4 : 6], 2, mean)
summary(gapminder)


# 4. R dplyr package
library(dplyr)
# 4-1. dplyr의 유용한 유틸리티 : glimpse, tbl_df(), %>%
## tbl_df() : tbl_df 클래스 속성을 가지게 된다. 현재 스크린에 예쁘게 표시될 정도의 행과 열만 출력해준다.
i2 <- tbl_df(iris)
class(i2); i2

## glimpse : data.frame을 전치하여 모든 변수를 다 볼 수 있게 해주고, 데이터형을 나타내며, 처음 몇 데이터 값을 출력해준다.
glimpse(i2)

## %>% : 파이프 연산자이며 x %>% f(y) == f(x, y)
iris %>% head # head(iris)와 같다.
iris %>% head(1) # head(iris, 10)와 같다.


# 4-2. dplyr 핵심 동사
## filter() : 행을 선택한다.
filter(gapminder, country == 'Korea, Rep.')
filter(gapminder, year == 2007)
filter(gapminder, country == 'Korea, Rep.' & year == 2007)
gapminder %>% filter(country == 'Korea, Rep.')
gapminder %>% filter(year == 2007)
gapminder %>% filter(country == 'Korea, Rep.' & year == 2007)

## arrange() : 행을 정렬한다.
arrange(gapminder, year, country)
gapminder %>% arrange(year, country)

## select() : 열을 선택한다.
select(gapminder, pop, gdpPercap)
gapminder %>% select(pop, gdpPercap)

## mutate() : 변수를 변환한다.
gapminder %>% mutate(total_gdp = pop * gdpPercap,
                     le_gdp_ratio = lifeExp / gdpPercap,
                     lgrk = le_gdp_ratio * 100)

## summarize(), summarise() - Australia / UK식 스펠링 : 요약 통계량을 계산한다.
gapminder %>% summarize(n_obs = n(),
                        n_countries = n_distinct(country),
                        n_years = n_distinct(year),
                        med_gdpc = median(gdpPercap),
                        max_gdppc = max(gdpPercap))
                        # 요약 함수의 종류 - n() : 현재 그룹의 관측치 개수
                        #                    n_distinct() : 그룹 내 x 변수의 고유한 값 개수
                        #                    first(x), last(x), nth(x, n) : 그룹 내 x 변수의 첫 번째, 마지막, n번째 관측치. 
                        #                    ex) x[1] x[length(x)], x[n]

## sample_n(),       sample_frac() : 랜덤 샘플을 해준다.
## 정해진 숫자의 행, 정해진 비율의 행
sample_n(gapminder, 10)
sample_frac(gapminder, 0.01) # default는 비복원추출(sampling without replacement), weigth를 통해 가중치 지정

## distinct() : 고유한 행을 찾아준다.
distinct(select(gapminder, country))
distinct(select(gapminder, year))
gapminder %>% select(country) %>% distinct()
gapminder %>% select(year) %>% distinct()


# 4-3. group_by를 이용한 그룹 연산
gapminder %>% filter(year == 2007) %>% group_by(continent) %>% summarize(median(lifeExp))


# 4-4. dplyr 명령의 공통점, 함수형 프로그래밍, 체이닝
# (1) 첫 번째 입력은 data.frame이다.
# (2) 두 번째 입력은 주로 열이름으로 이루어진 조건 / 계산문이다. $ 인덱싱이 필요없다.
# (3) 결과는 data.frame이다.(tbl_df의 속성도 가진다.)
gapminder %>% filter(year == 2007) %>% group_by(continent) %>% summarize(lifeExp = median(lifeExp)) %>% 
  arrange(-lifeExp)


# 4-5. dplyr에서 테이블을 결합하는 조인 연산자
(df1 <- data.frame(x = c(1, 2), y = 2 : 1))
(df2 <- data_frame(x = c(1, 3), a = 10, b = "a")) # data.frame()함수의 dplyr버전

## inner_join() : x와 y에 모두 매칭되는 행만 포함한다. (교집합)
## left_join() : x테이블의 모든 행을 포함한다. 매칭되지 않은 y테이블 변수들은 NA가 된다.
## right_join() : y테이블의 모든 행을 포함한다. left_join(y, x)와 행의 순서만 다를 뿐 동일한 결과를 준다.
## full_join() : x와 y의 모든 행을 포함한다. (합집합)
df1 %>% inner_join(df2)
df1 %>% left_join(df2)
df1 %>% right_join(df2)
df1 %>% full_join(df2)
