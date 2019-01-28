# 1. glmnet 함수를 통한 라쏘 모형, 능형회귀, 변수 선택
# 1-1. 라쏘, 능형 모형, 일래스틱넷, glmnet
## GLM 모형 : MLE(Maximum Likelihood Estimator, 최대우도추정치) βhat을 찾는것.
## minβ(-우도(β, X, Y) + 모형의 복잡도(β))를 최소화 한다. -> penalized maximum likelihood
##                       모형의 복잡도를 나타내는 항 : 벌점(penalty) or regularization term
## (1) 라쏘 회귀(LASSO regression) 모형의 복잡도 : L1-norm ||β||2^2 = |β1| + .. + |βp|
##                                                 minβ 1/n ∑(i = 1 ~ n)[-l(yi, xi*β) + λ||β||]
## (2) 능형 회귀(ridge regression) 모형의 복잡도 : L2-norm ||β||2^2 = β1^2 + .. + βp^2
##                                                 minβ 1/n ∑(i = 1 ~ n)[-l(yi, xi*β) + λ||β||2^2]
## (3) 일래스틱넷(elasticnet) 모형 : minβ 1/n ∑(i = 1 ~ n)[-l(yi, xi*β) + λ[(1-α)||β||2^2 + α||β||]]
##                                   α = 1 -> 라쏘, α = 0 -> 능형, λ -> 벌점의 정도를 조절
## url : https://goo.gl/DkhDv8

binomial_deviance <- function(y_obs, yhat){
  epsilon <- 0.0001
  yhat <- ifelse(yhat < epsilon, epsilon, yhat)
  yhat <- ifelse(yhat > 1 - epsilon, 1-epsilon, yhat)
  a <- ifelse(y_obs == 0, 0, y_obs * log(y_obs / yhat))
  b <- ifelse(y_obs == 1, 0, (1 - y_obs) * log((1 - y_obs) / (1 - yhat)))
  
  return (2 * sum(a + b))
}

library(glmnet)
library(ROCR)
library(rpart)
library(randomForest)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(gbm)

# 1-2. glmnet과 모형행렬
adult <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", 
                  header = F, strip.white = T)
names(adult) <- c('age', 'workclass', 'fnlwgt', 'education', 'education_num', 'marital_status', 'occupation',
                  'relationship', 'race', 'sex', 'capital_gain', 'capital_loss', 'hours_per_week',
                  'native_country', 'wage')
set.seed(1601)
n <- nrow(adult)
idx <- 1 : n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)
training <- adult[training_idx, ]
validation <- adult[validate_idx, ]
test <- adult[test_idx, ]

xx <- model.matrix(wage ~ . -1, adult)
x <- xx[training_idx, ]
y <- ifelse(training$wage == ">50K", 1, 0)
dim(x)

ad_glmnet_fit <- glmnet(x, y) # default - α = 1(라쏘 모형)
plot(ad_glmnet_fit) # x축 : λ가 변함에 따라서 전체 모수벡터의 L1-norm 값
                    # 상단 숫자 : 0이 아닌 모수의 개수(실질적인 모형의 자유도)
ad_glmnet_fit # %Dev(변이의 얼마나 많은 부분이 현재 모델로 설명되는가)

coef(ad_glmnet_fit, s = c(.1713, .1295)) # DF = 1, 2가 되는 λ값에 해당하는 모수 추정값들을 보고자 할 때
# λ = 0.1713일 때의 모형 : eta(learning rate) = 0.22324268 + 0.03341216 * marital_statusMarried-civ-spouse
# λ = 0.1295일 때의 모형 : eta = 0.154062004 + 0.115932495 * marital_statusMarried-civ-spouse +
#                                0.003112064 * education_num


# 1-3. 자동 모형 선택, cv.glmnet
ad_cvfit <- cv.glmnet(x, y, family = "binomial")
plot(ad_cvfit) # Binomial Deviance(이항 편차)가 작을수록 정확한 모형
               # 빨간점 : 주어진 λ에서의 k개의 교차검증의 평균
               # 최적의 λ값 - (1) 교차검증 오차의 평균값을 최소화하는 lambda.min -> 최적의 예측력
               #              (2) 교차검증 오차의 평균값이 최소값으로부터 
               #                  1 - 표준편차 이상 떨어지지 않은 가장 간단한 lambda.1se(가장 오른쪽) -> 해석 가능한 모형
log(ad_cvfit$lambda.min); log(ad_cvfit$lambda.1se) # 두 점선의 위치와 일치한다.
coef(ad_cvfit, s = ad_cvfit$lambda.1se)
coef(ad_cvfit, s = "lambda.1se")

# 선택된 모수의 개수
length(which(coef(ad_cvfit, s = "lambda.min") > 0))
length(which(coef(ad_cvfit, s = "lambda.1se") > 0))


# 1-4. α값의 선택
# cv.glmnet을 이용한 교차검증 : 주어진 α값에 최적의 λ값을 찾아준다.
#                               if) 다른 α값을 살펴보려면 수동으로 cv.glmnet함수를 다양한 α값에 실행한 후, 값들을 비교한다.
set.seed(1607)
foldid <- sample(1 : 10, size = length(y), replace = T)
cv1 <- cv.glmnet(x, y, foldid = foldid, alpha = 1, family = 'binomial')
cv.5 <- cv.glmnet(x, y, foldid = foldid, alpha = .5, family = 'binomial')
cv0 <- cv.glmnet(x, y, foldid = foldid, alpha = 0, family = 'binomial')

par(mfrow = c(2, 2))
plot(cv1, main = "Alpha = 1.0")
plot(cv.5, main = "Alpha = 0.5")
plot(cv0, main = "Alpha = 0.0")
plot(log(cv1$lambda), cv1$cvm, pch = 19, col = "red", xlab = "log(Lambda)", ylab = cv1$name, main = "alpha = 1.0")
points(log(cv.5$lambda), cv.5$cvm, pch = 19, col = "grey")
points(log(cv0$lambda), cv0$cvm, pch = 19, col = "blue")
legend("topleft", legend = c("alpha = 1", "alpha = .5", "alpha 0"),
       pch = 19, col = c("red", "grey", "blue"))
# α = 0일 경우, (능형 회귀) 항상 모든 변수가 선택된다.(복잡한 모형)


# 1-5. 예측, predict.glmnet
predict(ad_cvfit, s = "lambda.1se", newx = x[1 : 5, ], type = "response")


# 1-6. 모형 평가
y_obs <- ifelse(validation$wage == ">50K", 1, 0)
yhat_glmnet <- predict(ad_cvfit, s = "lambda.1se", newx = xx[validate_idx, ], type = "response")
yhat_glmnet <- yhat_glmnet[, 1] # change to a vectro from [n * 1] matrix
binomial_deviance(y_obs, yhat_glmnet)

pred_glmnet <- prediction(yhat_glmnet, y_obs)
perf_glmnet <- performance(pred_glmnet, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col = "black", main = "ROC Curve")
plot(perf_glmnet, col = "blue", add = T)
abline(0, 1)
legend('bottomright', inset = .1, legend = c("GLM", "glmnet"), col = c("black", "blue"), lty = 1, lwd = 2)
performance(pred_glmnet, "auc")@y.values[[1]]
# glmnet은 비교적 적은 22개의 변수만을 사용해서, 모든 변수를 사용한(복잡한 모형인) GLM 모형과 같은 예측 성능을 낸다.


# 2. 나무 모형
## 이항나무분할은 각 분할 시에 RSS(Residual Sum of Squares, 회귀분석의 경우), 지니지수(Gini index, 분류분석의 경우)를 최대한 줄여주도록 이루어진다.


# 2-1. 나무 모형 적합
(cvr_tr <- rpart(wage ~ ., data = training))
# 더 자세한 정보
printcp(cvr_tr)
summary(cvr_tr)

opar <- par(mfrow = c(1, 1), xpd = NA)
plot(cvr_tr)
text(cvr_tr, use.n = T) # use.n = T -> 각 노드에 속한 관측치 개수를 표시
par(opar)


# 2-2. 나무 모형 평가
yhat_tr <- predict(cvr_tr, validation)
yhat_tr <- yhat_tr[, ">50K"]
binomial_deviance(y_obs, yhat_tr)
pred_tr <- prediction(yhat_tr, y_obs)
perf_tr <- performance(pred_tr, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col = "black", main = "ROC Curve")
plot(perf_tr, col = "blue", add = T)
abline(0, 1)
legend("bottomright", inset = .1, legend = c("GLM", "Tree"), col = c("black", "blue"), lty = 1, lwd = 2)
performance(pred_tr, "auc")@y.values[[1]]


# 3. 랜덤 포레스트
# 3-1. 랜덤 포레스트 적용
set.seed(1607)
(ad_rf <- randomForest(wage ~ ., training))
plot(ad_rf) # 나무 수 증가에 따른 OOB 오차의 감소를 보여준다.
tmp <- importance(ad_rf)
head(round(tmp[order(-tmp[, 1]), 1, drop = F], 2), n = 10) # 변수중요도
                                                           # (1) 분류분석의 경우 : 평균지니지수감소량(mean decrease in Gini index)
                                                           # (2) 회귀분석의 경우 : 평균잔차제곱합감소량(mean decrease in RSS)
varImpPlot(ad_rf) # 변수중요도 그림


# 3-2. 랜덤 포레스트 예측
predict(ad_rf, newdata = adult[1 : 5, ])
predict(ad_rf, newdata = adult[1 : 5, ], type = "prob")


# 3-3. 모형 평가
yhat_rf <- predict(ad_rf, newdata = validation, type = "prob")[, ">50K"]
binomial_deviance(y_obs, yhat_rf)
pred_rf <- prediction(yhat_rf, y_obs)
perf_rf <- performance(pred_rf, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col = "black", main = "ROC Curve")
plot(perf_glmnet, add = T, col = "blue")
plot(perf_rf, add = T, col = "red")
abline(0, 1)
legend("bottomright", inset = .1, legend = c("GLM", "glmnet", "RF"), col = c("black", "blue", "red"), lty = 1, lwd = 2)
performance(pred_rf, "auc")@y.values[[1]]


# 3-4. 예측 확률값 자체의 비교
p1 <- data.frame(yhat_glmnet, yhat_rf) %>% ggplot(aes(yhat_glmnet, yhat_rf)) + geom_point(alpha = .5) +
  geom_abline() + geom_smooth()
p2 <- melt(data.frame(yhat_glmnet, yhat_rf)) %>% ggplot(aes(value, fill = variable)) + geom_density(alpha = .5)
grid.arrange(p1, p2, ncol = 2)


# 4. 부스팅
# 4-1. gbm 모형 적용
set.seed(1607)
adult_gbm <- training %>% mutate(wage = ifelse(wage == ">50K", 1, 0))
ad_gbm <- gbm(wage ~ ., dat = adult_gbm,
              distribution = "bernoulli", n.trees = 50000, cv.folds = 3, verbose = T)
(best_iter <- gbm.perf(ad_gbm, method = "cv"))
ad_gbm2 <- gbm.more(ad_gbm, n.new.trees = 10000)
(best_iter <- gbm.perf(ad_gbm2, method = "cv"))
## gbm 명령 실행 시 기억해두면 좋은 점들
## (1) gbm(distribution = "bernoulli") 함수는 0 - 1 반응변수를 사용하므로 
##     wage = 0.1 변수형을 가진 adult_gbm 데이터 프레임을 사용하였다.
## (2) gbm() 함수 안에서 교차검증을 시행하는 것을 권장한다. (cv.folds= 옵션) K가 크면 시간이 오래 걸리므로 3 ~ 5 사이
## (3) verbose = T를 선택하면 계산 과정 매 단계를 화면에 출력해준다.
## (4) gbm은 랜덤한 알고리즘이므로 재현가능성을 위해 set.seed()를 사용하자.
## (5) gbm.perf() 함수에서 method = "cv" 옵션은 교차검증 오차를 최소화하는 트리 숫자를 리턴한다.
## (6) gbm은 보통 상당히 많은 반복 후에 모형의 성능이 좋아지므로 n.trees= 옵션을 충분히 크게 잡아주어야 한다.
## (7) gbm을 한 번 실행했을 때 반복횟수가 부족하면 gbm.more() 함수를 사용하여 반복을 추가할 수 있다.
##     단, 결과에서 교차검증 오차가 제공되지 않는다.


# 4-2. 부스팅 예측
predict(ad_gbm, n.trees = best_iter, newdata = adult_gbm[1 : 5, ], type = "response")


# 4-3. 부스팅 모형 평가
yhat_gbm <- predict(ad_gbm, n.trees = best_iter, newdata = validation, type = "response")
binomial_deviance(y_obs, yhat_gbm)
pred_gbm <- prediction(yhat_gbm, y_obs)
perf_gbm <- performance(pred_gbm, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col = "black", main = "ROC Curve")
plot(perf_glmnet, add = T, col = "blue")
plot(perf_rf, add = T, col = "red")
plot(perf_gbm, add = T, col = "cyan")
abline(0, 1)
legend("bottomright", inset = .1, legend = c("GLM", "glmnet", "RF", "GBM"), col = c("black", "blue", "red", "cyan"),
       lty = 1, lwd = 2)
performance(pred_gbm, "auc")@y.values[[1]]


# 5. 모형 비교, 최종 모형 선택, 일반화 능력 평가
# 5-1. 모형의 예측 확률값의 분포 비교
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8 / strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(data.frame(y_obs = y_obs, yhat_lm = yhat_lm, 
                 yhat_glmnet = c(yhat_glmnet), yhat_rf = yhat_rf, yhat_gbm = yhat_gbm), 
      lower.panel = function(x, y){
        points(x, y); abline(0, 1, col = "red")
      },  upper.panel = panel.cor)
## 산점도를 통해 알 수 있는 것.
## (1) gbm의 예측 확률값이 관측된 y 값과의 상관계수가 가장 높다.
## (2) GLM과 glmnet의 결과는 매우 유사하다.
## (3) RF는 다른 모형과 비교적 덜 유사하다. 이에 반해, 다른 모형들끼리는 유사하다.


# 5-2. 테스트세트를 이용한 일반화 능력 계산
y_obs_test <- ifelse(test$wage == ">50K", 1, 0)
yhat_gbm_test <- predict(ad_gbm, n.trees = best_iter, newdata = test, type = "response")
binomial_deviance(y_obs_test, yhat_gbm_test)
pred_gbm_test <- prediction(yhat_gbm_test, y_obs_test)
performance(pred_gbm_test, "auc")@y.values[[1]]

# 6. 우리가 다루지 않은 것들 : https://goo.gl/yvwvso
# CARET(Classification And REgression Training) 패키지 : http://topepo.github.io/caret/index.html