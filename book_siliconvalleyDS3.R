# 1. 모든 데이터에 행해야 할 분석
# (1) 데이터 내용, 구조, 타입을 파악한다. ex) glimpse(), str(), head()
# (2) 데이터의 요약 통계량을 파악한다. ex) summary()
# (3) 결측치가 있는지 살펴본다. ex) summary()
# (4) 무작정 시각화를 해본다. ex) plot(), pairs(), 데이터 관측치가 많을 경우 sample_n()
library(dplyr)
library(ggplot2)
glimpse(mpg)
head(mpg)
summary(mpg)

# 2. 수량형 변수의 분석 - 일변량 변수에 대한 통계량 분석
# (1) 데이터 분포의 시각화 : hist(), boxplot(), ggplot() + geom_{histogram, density}()
# (2) 요약 통계량 계산 : summary(), mean(), median(), var(), sd(), mad(), quantile()
# (3) 데이터의 정규성 검사 : qqplot(), qqline() -> 정규분포와 얼마나 유사한지 검사
# (4) 가설검정과 신뢰구간 : t.test() -> 일변량 t-검정, 신뢰구간을 구할 수 있다.(정규분포가 아니여도 상관 없다.)
# (5) 이상점 찾아보기 : 로버스트 통계량 계산
summary(mpg$hwy)
mean(mpg$hwy)
median(mpg$hwy)
range(mpg$hwy)
quantile(mpg$hwy)
opar <- par(mfrow = c(2, 2))
hist(mpg$hwy)
boxplot(mpg$hwy)
qqnorm(mpg$hwy)
qqline(mpg$hwy)
par(opar)

# 2-1. 일변량 t-검정
# 가설 = H0 : mu <= 22.9 / H1 : mu > 22.9
hwy <- mpg$hwy
n <- length(hwy)
mu0 <- 22.9
t.test(hwy, mu = mu0, alternative = "greater")
# 결론 : 실제 모 평균 고속도로 연비가 22.9라면 우리가 관측한 것만큼 
#        큰 표본평균값과 t 통계량(t = 1.3877)이 관측될 확률이 8.3%라는 것이다.
#        따라서 유의수준 alpha = 10%라면 고속도로 연비가 22.8보다 크다고 결론지을 수 있지만,
#        유의수준이 5%라면 고속도로 연비가 22.8보다 크다고 결론지을 만한 증거가 충분하지 않다고 할 수 있다.
t.test(hwy) # 신뢰구간 = [22.6, 24.2]

# 2-2. 이상점과 로버스트 통계 방법
# 상자그림에서의 이상점 : [Q1 - 1.5 * IQR, Q3 + 1.5 * IQR] (IQR[Inter-Quartile range] = Q3 - Q1)
# 로버스트 통계 방법 : 평균 대신 중앙값(median), 표준편차 대신 중위수절대편차(median absolute deviance)를 사용한다.
c(mean(hwy), sd(hwy))
c(median(hwy), mad(hwy))


# 3. 성공-실패값 범주형 변수의 분석
# (1) 요약 통계량 계산 : table(), xtabs()등이 있다. prop.table()함수는 도수를 상대도수로 바꿔준다.
# (2) 데이터 분포의 시각화 : barplot()
# (3) 가설검정과 신뢰구간 : binom.test()함수를 사용하면 '성공률'에 대한 검정과 신뢰구간을 구할 수 있다.
# ex) 전국 성인들 사이에서의 대통령 지지율을 알고 싶다. 참 지지율(천만 명 중)이 p = 50%라고 하자. 0 = "no", 1 = "yes"
set.seed(1606)
n <- 100
p <- 0.5
x <- rbinom(n, 1, p)
x <- factor(x, levels = c(0, 1), labels = c("no", "yes")); x
table(x)
prop.table(table(x))
barplot(table(x))
# 가설 = H0 : p = 0.5 / H1 : p ≠ 0.5
binom.test(x = length(x[x == 'yes']), n = length(x), p = 0.5, alternative = "two.sided")

# 3-1. 오차한계, 표본 크기, sqrt(n)의 힘
# 오차한계 : 주어진 신뢰수준에서 신뢰구간의 크기의 절반으로 주어진다.
# if) 54 / 100가 아니라 표본이 100배 큰 10000이고, 이 중 5,400이 '성공'이라면? 추정값은 0.54일 것이다.
# but) 신뢰구간?
binom.test(x = 5400, n = 10000) # 신뢰 구간 = [0.530, 0.550]
                                # 오차한계 = 0.00982, 즉 약 1%로 10배 줄었다. 표본 크기는 100배 늘었다.
n <- c(100, 1000, 2000, 10000, 1e6)
data.frame(n = n, moe = round(1.96 * sqrt(1 / (4 * n)), 4))
curve(1.96 * sqrt(1 / (4 * x)), 10, 10000, log = "x"); grid()


# 4. 수량형 X, 수량형 Y의 분석
# (1) 산점도를 통해 관계의 모양을 파악한다. plot(), ggplot2의 geom_point() 사용한다.
#     관계가 선형인지, 강한지 약한지, 이상치는 있는지 등을 파악한다.
# (2) 상관계수를 계산한다. 상관계수는 선형 관계의 강도만을 재는 것을 염두에 둔다.
# (3) 선형 모형을 적합한다. 잔차에 이상점은 있는가? 잔차가 예측변수에 따라서 다른 분산을 갖지는 않은가?
# (4) 이상치가 있을 때는 로버스트 회귀분석을 사용한다.
# (5) 비선형 데이터에는 LOESS 등의 평활법을 사용한다.

# 4-1. 산점도
ggplot(mpg, aes(x = cty, y = hwy)) + geom_jitter() + geom_smooth(method = "lm")

# 4-2. 상관계수
cor(mpg$cty, mpg$hwy)
with(mpg, cor(cty, hwy))
with(mpg, cor(cty, hwy, method = "kendall"))
with(mpg, cor(cty, hwy, method = "spearman"))

# 4-3. 선형회귀 모형 적합
(hwy_lm <- lm(hwy ~ cty, data = mpg))
summary(hwy_lm)

# 4-4. 모형 적합도 검정
# SST(total sum of squares) = (n ~ i = 1)∑(yi - ybar)^2 = SSR + SSE
# SSR(regression sum of squares) = (n ~ i = 1)∑(yhati - ybar)^2
# SSE(error sum of squares) = (n ~ i = 1)∑(yi - yhati)^2
# R^2(Multiple R-squared) = SSR / SST = coefficient of determination
# Adjusted R^2 = 1 - (1 - R^2) * (n - 1) / (n - p - 1) = R^2 - (1 - R^2) * p / (n - p - 1)

# 4-5. 선형회귀 모형 예측
predict(hwy_lm)
resid(hwy_lm) # 잔차(residual) ei = yi - yhati
predict(hwy_lm, newdata = data.frame(cty = c(10, 20, 30)))
predict(hwy_lm, newdata = data.frame(cty = c(10, 20, 30)), se.fit = T)

# 4-6. 선형회귀 모형의 가정 진단
# (1) x와 y의 관계가 선형이다.
# (2) 잔차의 분포가 독립이다.
# (3) 잔차의 분포가 동일하다.
# (4) 잔차의 분포가 N(0, ∂^2)이다.
class(hwy_lm)
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
plot(hwy_lm, las = 1) # Residuals vs Fitted : 잔차의 분포가 예측값과 독립적인지
par(opar)             # Normal Q-Q : 표준화된 잔차의 분포가 정규분포에 가까운지
                      # Scale-Location : 잔차의 절댓값의 제곱근(sqrt(|ei|))과 예측값 사이의 관계
                      # Residuals vs Leverage : 표준화된 잔차와 레버리지 간의 관계

# 4-7. 로버스트 선형회귀분석
# MASS:lqs() - 데이터 중 '좋은' 관측치만 적합에 사용한다.
#              n 관측치와 p 독립변수가 있을 때 'lqs'는 제곱오차의 floor((n + p + 1) / 2) quantile을 최소화한다.
library(MASS)
set.seed(123)
lqs(stack.loss ~ ., data = stackloss)
lm(stack.loss ~ ., data = stackloss) # 보통 선형 모형

# 4-8. 비선형 / 비모수적 방법, 평활법과 LOESS
# - 비선형적인 x-y관게를 추정해내기 위해 모형 아무 가정도 하지 않는 평활법(smoothing) 사용
#   그 중 국소 회귀(local regression) 방법인 LOESS(locally weighted scatterplot smoothing)
#   -> 각 예측변수 x0값에서 가까운 k개의 (xi, yi)관측치들을 사용하여 2차 다항회귀 모형을 적합하여
#      fhat(x0)를 추정하고, 이것을 다양한 x0 값에 대해 반복하는 것이다.
#      평활의 정도인 파라미터 k 값은 교차검증(cross-validation)으로 최적화한다.
plot(hwy ~ displ, data = mpg)
(mpg_lo <- loess(hwy ~ displ, data = mpg))
summary(mpg_lo)

xs <- seq(2, 7, length.out = 100)
mpg_pre <- predict(mpg_lo, newdata = data.frame(displ = xs), se = T)
lines(xs, mpg_pre$fit)
lines(xs, mpg_pre$fit - 1.96 * mpg_pre$se.fit, lty = 2)
lines(xs, mpg_pre$fit + 1.96 * mpg_pre$se.fit, lty = 2)
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point() + geom_smooth()


# 5. 범주형 x, 수량형 y
# (1) 벙렬상자그림(side-by-side boxplot)을 이용하여 데이터를 시각화한다.
#     집단 간에 평균과 중앙값의 차이가 존재하는지, 이상치는 존재하는지, 각 집단의 분산은 유사한지 등 살펴본다.
# (2) lm() 함수로 ANOVA 선형 모형을 적합한다. summary.lm() 함수로 심도 있는 결과를 얻는다.
# (3) plot.lm() 함수로 잔차의 분포를 살펴본다. 이상점은 없는가? 모형의 가정은 만족하는가?

# 5-1. 분산분석 예
mpg %>% ggplot(aes(x = class, y = hwy)) + geom_boxplot()
(hwy_lm2 <- lm(hwy ~ class, data = mpg))
summary(hwy_lm2)
predict(hwy_lm2, newdata = data.frame(class = "pickup"))

# 5-2. 분산분석의 진단
# (1) 잔차의 분포가 독립이다.
# (2) 잔차의 분산이 동일하다.
# (3) 잔차의 분포가 N(0, ∂^2)이다.
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
plot(hwy_lm2, las = 1)
par(opar)


# 6. 수량형 x, 범주형 y(성공 - 실패)
# (1) X와 (jitter된) Y 변수의 산점도를 그려본다. 그리고 Y 변ㅅ의 그룹별로 X 변수의 병렬 상자그림을 그려본다.
#     Y 변수 값에 따라 X 변수의 분포가 차이가 있는지, 두 변수 간에 어떤 관계가 있는지, 이상치는 존재하는지,
#     표본 로그오즈(log odds, 성공확률이 Mu라고 할 때 log(Mu / (1 - Mu))를 로그오즈라 한다.)와 x의 산점도에
#     선형 패턴이 있는지 등을 살펴본다.
# (2) glm() 함수로 일반화 선형 모형을 적합한다. summary.glm() 함수로 심도 있는 결과를 얻는다.
# (3) plot.glm()으로 잔차의 분포를 살펴본다. 이상점은 없는가? 모형의 가정은 만족하는가?

# 6-1. 챌린저 데이터 분석
chall <- read.csv("https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/challenger.csv")
chall <- tbl_df(chall)
glimpse(chall)
chall %>% ggplot(aes(temperature, distress_ct)) + geom_point()
chall %>% ggplot(aes(factor(distress_ct), temperature)) + geom_boxplot()
# (1) 반응변수 yi가 0에서 1사이의 숫자이면 yi = si / ai로 간주된다. si = '성공' 횟수, ai = '시도' 횟수다.
#     ai는 weights = 옵션에 정의해야 한다. 모든 si = 0 혹은 1이면 weights를 지정하지 않아도 된다.(ai = 1로 간주된다.)
# (2) 반응변수가 0 ~ 1 숫자 벡터일 경우에는 #1처럼 간주된다. 물론, weights = 1이다. 
#     TRUE / FALSE 논리 벡터이면 0 = FALSE = '실패'로, 1 = TRUE = '성공'으로 간주된다. 
#     2-레벨 이상의 factor변수는 첫 번째 레벨은 '실패', 나머지 레벨은 '성공'으로 간주된다.
# (3) 반응변수가 2-차원 매트릭스이면 첫 열은 '성공' 횟수 si를, 두 번째 열은 '실패'횟수 ai - si를 나타낸다.
(chall_glm <- glm(cbind(distress_ct, o_ring_ct - distress_ct) ~ temperature, data = chall, family = 'binomial'))
summary(chall_glm) # Number of Fisher Scoring iterations : 수치적인 방법으로 답을 점근적으로 찾아내는데
                   # 이 반복횟수가 6번이었다는 것이다.

# 6-2. GLM의 모형 적합도
## Null deviance : 20.706 on 22 degrees of freedom - 모형을 적합하기 전의 deviance
## Residual deviance : 9.527 on 21 degress of freedom - 모형을 적합한 후의 deviance
## - 위의 둘 사이가 '충분히' 줄었다면 이 모형은 적합하다고 판단한다. 모형이 적합하지 않다는 가정하에서는
##   두 deviance의 차이는 대략 chi-squared 분포를 따른다. 두 deviance 차이는 20.7 - 9.52 = 11.2다.
##   자유도 1인 카이제곱 분포에서 이 값은 아주 큰값(1 - pchisq(11.2, 1))이므로 모형 적합의 P-값은 실질적으로 0이며,
##   이 모형은 데이터를 의미 있게 설명한다고 결론을 내릴 수 있다.
## AIC : 24.865 (Akaike Information Criterion) (2k - 2ln(L)) - k : 모형 모수의 개수(복잡도), L(주어진 모형으로 최대화 우도)

# 6-3. 로지스틱 모형 예측, 링크와 반응변수
# if) temperature = 30이라면 '성공확률'은 얼마일까?
predict(chall_glm, data.frame(temperature = 30)) # 선형예측값
exp(3.45) / (exp(3.45) + 1)
predict(chall_glm, data.frame(temperature = 30), type = 'response') # 확률

# 6-4. 로지스틱 모형 적합결과의 시각화
logistic <- function(x){
  exp(x) / (exp(x) + 1)
}
plot(c(20, 85), c(0, 1), type = "n", xlab = "temperature", ylab = "prob")
tp <- seq(20, 85, 1)
chall_glm_pred <- predict(chall_glm, data.frame(temperature = tp), se.fit = T)
lines(tp, logistic(chall_glm_pred$fit))
lines(tp, logistic(chall_glm_pred$fit - 1.96 * chall_glm_pred$se.fit), lty = 2)
lines(tp, logistic(chall_glm_pred$fit + 1.96 * chall_glm_pred$se.fit), lty = 2)

# 6-5 GLM 모형의 일반화
# family    default link function   적용 예
# binomial  link = "logit"          성공-실패 반응변수
# gaussian  link = "identity"       선형 모형이다! lm() 함수와 같다.
# Gamma     link = "inverse"        양의 값을 가지는 수량형 반응변수 ex) 강우량, 점원이 10명의 손님을 처리하는 데 걸리는 시간
# poisson   link = "log"            0, 1, 2 ... 값을 가진, '개수'를 타나내는 반응변수 ex) 일일 교통사고 횟수, 단위지역의 연간 지진발생 횟수
