# 지도학습(Supervised Learning) : 설명변수로부터 반응변수를 예측해내는 작업
# 비지도학습(Unsupervised Learning) : 입력변수에 근거하여 범주형 반응변수를 예측하는 작업
# 회귀분석(Regression Prediction) : 연속형과 수치형 반응변수를 예측하는 작업
binomial_deviance <- function(y_obs, yhat){
  epsilon <- 0.0001
  yhat <- ifelse(yhat < epsilon, epsilon, yhat)
  yhat <- ifelse(yhat > 1 - epsilon, 1-epsilon, yhat)
  a <- ifelse(y_obs == 0, 0, y_obs * log(y_obs / yhat))
  b <- ifelse(y_obs == 1, 0, (1 - y_obs) * log((1 - y_obs) / (1 - yhat)))
  
  return (2 * sum(a + b))
}
# 모형의 복잡도에 따른 예측오차의 변동 : https://goo.gl/w3LLVN

# 1. 데이터 준비하기
install.packages("ISLR")
install.packages("glmnet")
install.packages("gbm")

library(dplyr)
library(ggplot2)
library(ISLR)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(rpart)
library(boot)

adult <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", 
                  header = F, strip.white = T)
names(adult) <- c('age', 'workclass', 'fnlwgt', 'education', 'education_num', 'marital_status', 'occupation',
                  'relationship', 'race', 'sex', 'capital_gain', 'capital_loss', 'hours_per_week',
                  'native_country', 'wage')
glimpse(adult)
summary(adult)

# 2. 범주형 반응변수의 factor 레벨
levels(adult$wage)
# (1) glm() 함수에서 binomial 패밀리를 사용할 때 범주형 반응변수가 사용되면 첫째 레벨이 'Failure',
#     이외의 모든 레벨이 'Success'로 간주된다. 따라서 wage에서 '<=50K'는 실패로, '>50K'는 성공으로 간주된다.
# (2) glmnet(), cv.glmnet() 함수에서 binomial 패밀리를 사용할 때 범주형 반응변수는 두 레벨을 가져야만 하고,
#     첫째 레벨은 'Failure', 둘째 레벨은 'Success'다.
# (3) randomForest() 함수에서 범주형 반응변수는 여러 레벨을 가져도 된다. 예측은 각각의 레벨에 대해 확률값으로 주어진다.
# (4) gbm() 함수에서 범주형 변수는 distribution = 'bernoulli' 옵션으로 다룰 수 있다. 이 경우 0은 실패, 1은 성공으로 간주된다. 
#     레벨이 여럿일 경우에는 distribution = 'multinomial'을 사용한다. 이 경우에는 {1, 2}값을 {0, 1}로 바꿔줘야 한다.

# 3. 범주형 설명변수에서 문제의 복잡도
levels(adult$race)
adult$race[1 : 5]
levels(adult$sex)
x <- model.matrix(~ race + sex + age, adult)
glimpse(x)
x_org <- adult %>% dplyr::select(sex, race, age)
x_mod <- model.matrix(~ sex + race + age, adult)
x <- model.matrix(~ . - wage, adult)
dim(x)

# 4. 훈련, 검증, 테스트세트의 구분
set.seed(1601)
n <- nrow(adult)
idx <- 1 : n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)
length(training_idx)
length(validate_idx)
length(test_idx)
training <- adult[training_idx, ]
validation <- adult[validate_idx, ]
test <- adult[test_idx, ]

# 5. 시각화
training %>% ggplot(aes(age, fill = wage)) + geom_density(alpha = .5) # 나이와 임금
training %>% filter(race %in% c('Black', 'White')) %>% ggplot(aes(age, fill = wage)) + geom_density(alpha = .5) +
  ylim(0, 0.1) + facet_grid(race ~ sex, scales = 'free_y') # 나이-중산층 여부의 관계가
                                                           # 남성일 경우에는 흑인과 백인 유사하다.
                                                           # 여성일 경우 다르다.
training %>% ggplot(aes(education_num, fill = wage)) + geom_bar() # 교육기간

# 6. 로지스틱 회귀모형
# 6-1. 모형적합
ad_glm_full <- glm(wage ~ ., data = training, family = binomial)
               # error? 일부 설명변수의 조합에서 반응변수가 완벽하게 0 또는 1일 경우가 있기 때문이다.
               #        데이터의 양에 비해 설명변수의 양이 클 때, 즉 p는 많은데 n이 적을 경우에 발생하기 쉽다.
               #        데이터 양에 비해 모형이 너무 복잡한 것이다.
               # sol? 1) glmnet 같은 정규화된 모형을 사용한다.
               #      2) 모형을 변경, 변수 선택을 시도, 범주형 변수의 경우에는 레벨을 줄여본다.
               #      3) 베이지안 분석을 한다.
               #      4) 내버려 둔다. 신뢰구간은 왜곡되지만 모형 자체는 쓸 만할수 있다.
summary(ad_glm_full)

# 6-2. 완벽한 상관 관계, collinearity
alias(ad_glm_full) # education-num과 occupation Transport-moving이 다른 일부 변수의 조합과 정확하게 일치한다.

# 6-3. glm 예측, 분계점
predict(ad_glm_full, newdata = adult[1 : 5, ], type = "response")

# 6-4. 예측 정확도 지표
y_obs <- ifelse(validation$wage == ">50K", 1, 0)
yhat_lm <- predict(ad_glm_full, newdata = validation, type = "response")
library(gridExtra)
p1 <- ggplot(data.frame(y_obs, yhat_lm), aes(y_obs, yhat_lm, group = y_obs, fill = factor(y_obs))) + 
  geom_boxplot()
p2 <- ggplot(data.frame(y_obs, yhat_lm), aes(yhat_lm, fill = factor(y_obs))) + geom_density(alpha = .5)
grid.arrange(p1, p2, ncol = 2)
binomial_deviance(y_obs, yhat_lm) # 이항편차
library(ROCR)
pred_lm <- prediction(yhat_lm, y_obs)
perf_lm <- performance(pred_lm, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col = 'black', main = "ROC Curve for GLM")
abline(0, 1)
performance(pred_lm, "auc")@y.values[[1]]
