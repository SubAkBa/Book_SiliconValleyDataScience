# Chapter4 - library :: ggplot2
# 갭마인더 데이터
library(gapminder)
## country : 142개의 다른 값(levels)을 가진 인자 변수
## continent : 5가지 값을 가진 인자 변수
## year : 숫자형의 연도 변수. 1952년에서 2007년까지 5년 간격으로 되어 있다.
## lifeExp : 이 해에 태어난 이들의 평균 기대 수명(life expectancy)
## pop : 인구
## gdPercap : 일인당 국민소득(GDP per capita)
head(gapminder)
tail(gapminder)

# 데이터를 한눈에 살펴보는데 유용한 명령 dplyr::glimpse()
library(dplyr)
glimpse(gapminder)

# 각 변수에 대한 요약 통계량(평균, 중간값, 최솟값, 최댓값, 사분위수), 두 변수 간의 상관 관계(correlation)
summary(gapminder$lifeExp)
summary(gapminder$gdpPercap)
cor(gapminder$lifeExp, gapminder$gdpPercap)

# 시각화
opar <- par(mfrow = c(2, 2))
hist(gapminder$lifeExp)
hist(gapminder$gdpPercap, nclass = 50) # 일인당 평균 소득의 분포가 양의 방향으로 치우친 것
hist(log10(gapminder$gdpPercap), nclass = 50) # 평균 소득의 변수는 로그변환이 데이터를 균등한 모양으로 만들어준다.
plot(log10(gapminder$gdpPercap), gapminder$lifeExp, cex = .5) # 로그변환된 평균 소득과 평균 기대 수명 간의 관계가
par(opar)                                                     # 강한 양의 상관 관계가 있음을 보여준다.

# 로그 변환 후에는 상관 관계가 증가한다.
cor(gapminder$lifeExp, log10(gapminder$gdpPercap)) # 피어슨 상관계수 : 변수의 관계가 직선인 선형 관계를 측정


# 앤스콤의 사인방 : 시각화 없는 통계량은 위험하다.
# https://en.wikipedia.org/wiki/Anscombe's_quartet
## 동일한 평균, 분산, 상관 관계, 선형 모형을 가졌다고 하더라도 그래프는 다를 수 있다.


# Base R Graph and ggplot2
## plot(x, y) : 산점도
## hist(x) : 히스토그램
## boxplot(x) : 상자그림
## mosaicplot() : 모자이크 플롯
## points(x, y) : 저차원 점 그리는 함수
## lines(x, y) : 저차원 선 그리는 함수

# ggplot2 : grammar of graphics
library(ggplot2)
library(dplyr)
library(gapminder)
gapminder %>% ggplot(aes(x = lifeExp)) + geom_histogram()
gapminder %>% ggplot(aes(x = gdpPercap)) + geom_histogram()
gapminder %>% ggplot(aes(x = gdpPercap)) + geom_histogram() + scale_x_log10()
gapminder %>% ggplot(aes(x = gdpPercap, y = lifeExp)) + geom_point() + scale_x_log10() + geom_smooth()
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`. -> geom_histogram(bins = 30)처럼 막대 개수를 지정해 줄 수 있음을 말해준다.

df <- data.frame(gp = factor(rep(letters[1 : 3], each = 10)), y = rnorm(30))
glimpse(df)
(ds <- df %>% group_by(gp) %>% summarize(mean = mean(y), sd = sd(y)))

# 첫 번째
ggplot(df, aes(x = gp, y = y)) + geom_point() + geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)
# 두 번째
ggplot(df) + geom_point(aes(x = gp, y = y)) + geom_point(data = ds, aes(x = gp, y = mean), colour = 'red', size = 3)
# 세 번째
ggplot() + geom_point(data = df, aes(x = gp, y = y)) + geom_point(data = ds, aes(x = gp, y = mean), colour = 'red', size = 3) +
  geom_errorbar(data = ds, aes(x = gp, y= mean, ymin = mean - sd, ymax = mean + sd), colour = 'red', width = 0.4)


# ggplot과 dplyr의 %>%
ggplot(gapminder, aes(lifeExp)) + geom_histogram()
gapminder %>% ggplot(aes(lifeExp)) + geom_histogram()

# 예제 데이터
glimpse(diamonds)
glimpse(mpg)

# 변수의 종류에 따른 시각화 기법
## 1. 한 개의 수량형 변수 - 도수 히스토그램, 도수폴리곤, 분포밀도추정함수
## - 해석할때 살펴볼것 : (1) 이상점 유무
##                       (2) 분포의 모양? ex) 종모양인가, 오른쪽 or 왼쪽으로 치우쳤는가, 두 개의 피크를 가졌는가
##                       (3) 어떤 변환을 하면 데이터가 종모양에 가까워지는가?
##                       (4) 히스토그램이 너무 자세하거나 거칠지 않은가? 그럴 경우 다양한 binwidth 값을 사용한다.
library(gapminder)
library(ggplot2)
library(dplyr)
gapminder %>% ggplot(aes(x = gdpPercap)) + geom_histogram()
gapminder %>% ggplot(aes(x = gdpPercap)) + geom_histogram() + scale_x_log10() # 히스토그램
gapminder %>% ggplot(aes(x = gdpPercap)) + geom_freqpoly() + scale_x_log10() # 도수폴리곤
gapminder %>% ggplot(aes(x = gdpPercap)) + geom_density() + scale_x_log10() # 커널밀도추정함수
summary(gapminder)

## 2. 한 개의 범주형 변수 - 막대 그래프, 많을 경우 table()함수를 통한 통계량을 바로 출력해주는 것도 도움이 된다.
diamonds %>% ggplot(aes(cut)) + geom_bar()
table(diamonds$cut) # 도수 분포
prop.table(table(diamonds$cut)) # 상대도수
round(prop.table(table(diamonds$cut)) * 100, 1)
## library::dplyr 이용
diamonds %>% group_by(cut) %>% tally() %>% mutate(pct = round(n / sum(n) * 100, 1))

## 3. 두 개의 수량형 변수 - 산점도, 한 x, y 좌표에 여러 개의 중복된 관측치가 있을 때는 geom_jitter()사용
diamonds %>% ggplot(aes(carat, price)) + geom_point()
diamonds %>% ggplot(aes(carat, price)) + geom_point(alpha = .01)
mpg %>% ggplot(aes(cyl, hwy)) + geom_point()
mpg %>% ggplot(aes(cyl, hwy)) + geom_jitter()
## - 산점도 시각화 주의 사항 : (1) 데이터의 개수가 너무 많을 경우 천 여 개 정도의 점들을 표본화한다.
##                             (2) 데이터의 개수가 너무 많을 경우 alpha 값을 줄여 점들을 투명하게 만들어본다.
##                             (3) 일변량 데이터의 예처럼 x나 y 변수에 제곱근 혹은 로그변환이 필요한지 살펴본다.
##                             (4) 데이터의 상관 관계가 강한지 혹은 약한지 살펴본다.
##                             (5) 데이터의 관계가 선형인지 혹은 비선형인지 살펴본다.
##                             (6) 이상점이 있는지 살펴본다.
##                             (7) X, Y변수가 변수 간의 인과 관계를 반영하는지 생각해본다.
##                                 한 변수가 다른 변수에 영향을 미치는 자연스러운 관계가 있다면 
##                                 원인이 되는 변수를 X, 결과가 되는 변수를 Y로 놓는 것이 자연스럽다.
# 산점도 행렬
pairs(diamonds %>% sample_n(1000))

## 4. 수량형 변수와 범주형 변수 - X가 범주형일 경우 병렬 상자그림
mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()
## 각 class 그룹의 관측치는 얼마나 될까?
mpg %>% ggplot(aes(class, hwy)) + geom_jitter(col = 'gray') + geom_boxplot(alpha = .5)
mpg %>% mutate(class = reorder(class, hwy, median)) %>% ggplot(aes(class, hwy)) + 
  geom_jitter(col = 'gray') + geom_boxplot(alpha = .5) # reorder를 통해 오름차순으로
mpg %>% mutate(class = factor(class, levels = c("2seater", "subcompact", "compact", "midsize", "minivan",
                                                "suv", "pickup"))) %>% ggplot(aes(class, hwy)) +
  geom_jitter(col = 'gray') + geom_boxplot(alpha = .5) # 수동으로 범주의 순서를 지정해준다.
mpg %>% mutate(class = factor(class, levels = c("2seater", "subcompact", "compact", "midsize", "minivan",
                                                "suv", "pickup"))) %>% ggplot(aes(class, hwy)) +
  geom_boxplot(alpha = .5) + coord_flip() # y축에 가로로 나타나게 해준다.
## 병렬상자그림을 통해서 (1) 차량의 무게가 무거워질수록 연비가 나빠진다.
##                       (2) 2seater와 midsize까지의 등급에서는 큰 차이가 없다.
##                       (3) 2seater는 차는 가벼울지 모르나 스포츠카일 수 있으므로 연비가 평균적으로 더 나쁘다.
##                       (4) 일부 subcompact / compact 차량은 연비가 40mpg보다도 높은 고연비 차량임을 알 수 있다.(이상점)
## - 병렬상자그림 사용 주의 사항 : (1) 범주형 x 변수의 적절한 순서를 고려한다. reorder() 명령처럼 통계량에 기반을 둔 범주의 순서를 정할 수 도 있고,
##                                     factor(levels = ) 명령을 사용하여 수동으로 정하는 것이 나을 수도 있다.
##                                 (2) 수량형 y 변수의 제곱근과 로그변환이 도움이 될 수 있다.
##                                 (3) 수량형 y 변수의 분포가 어떠한가? 종모양인가? 왼쪽 혹은 오른쪽으로 치우쳐 있는가?
##                                     이상점이 있는가?
##                                 (4) 각 x 범주 그룹의 관측치는 충분한가? 이것을 알아내기 위해서는 alpha = 옵션으로
##                                     반투명 상자를 그리고, geom_points()로 개별 관측치를 표현한다.
##                                 (5) x와 y축을 교환할 필요는 없는가? coord_flip() 함수를 사용한다.
##                                 (6) 유용한 차트를 얻기 위해서는 다양한 옵션을 시도해야 한다.

## 5. 두개의 범주형 변수 - 도수 분포를 알아내기 위해서는 xtabs(), 시각화는 mosiacplot()
glimpse(data.frame(Titanic))
xtabs(Freq ~ Class + Sex + Age + Survived, data.frame(Titanic))
mosaicplot(Titanic, main = "Survival on the Titanic")
mosaicplot(Titanic, main = "Survival on the Titanic", color = T)
## 모자이크플롯을 통해서 (1) 탑승 인원은 선원이 가장 많고, 3등실, 2등실, 1등실 순이다.
##                       (2) 아이의 비율이 가장 많은 승객 층은 3등실이다.
##                       (3) 1등실 여성은 거의 대부분 생존했고, 2등실, 3등실 순이다.
##                       (4) 가장 사망률이 높은 조합은 2등실 남자 어른이다.
## 어른과 아이 중 누가 더 생존율이 높을까?
apply(Titanic, c(3, 4), sum)
## 남녀 생존율 비교
apply(Titanic, c(2, 4), sum)
t2 <- data.frame(Titanic)
t2 %>% group_by(Sex) %>% summarize(n = sum(Freq), survivors = sum(ifelse(Survived == "Yes", Freq, 0))) %>% 
  mutate(rate_survival = survivors / n)

## 6. 더 많은 변수를 보여주기 1 : 각 geom의 다른 속성들을 사용(geom_레이어에 색깔, 모양, 선모양 등의 다른 속성을 더해주는 것)
gapminder %>% filter(year == 2007) %>% ggplot(aes(gdpPercap, lifeExp)) + 
  geom_point() + scale_x_log10() + ggtitle("Gapminder data for 2007")
gapminder %>% filter(year == 2007) %>% ggplot(aes(gdpPercap, lifeExp)) + 
  geom_point(aes(size = pop, col = continent)) + scale_x_log10() + ggtitle("Gapminder data for 2007")

## 7. 더 많은 변수를 보여주기 2 : facet_* 함수를 사용
gapminder %>% ggplot(aes(year, lifeExp, group = country)) + geom_line()
gapminder %>% ggplot(aes(year, lifeExp, group = country, col = continent)) + geom_line()
gapminder %>% ggplot(aes(year, lifeExp, group = country)) + geom_line() + facet_wrap(~ continent)

## 데이터 시각화의 유용한 여러 원칙
## (1) 비교, 대조, 차이를 드러내라.
## (2) 인과 관계와 상관 관계를 보여라.
## (3) 한 도표에 여러 변수를 보여라. ggplot은 통합적으로 이것을 지원한다.
## (4) 텍스트, 숫자, 이미지, 그래프 같은 데이터들을 한 곳에 통합하라.
## (5) 사용된 데이터의 출처를 그래프 안이나 각주로 밝혀라.
## (6) 의미 있는 내용을 담아라.

