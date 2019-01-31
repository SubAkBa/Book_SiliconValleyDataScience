# 1. 회귀분석(Regression)
# 정확도 지표, RMSE(Root Mean Squared Error)
# RMSE = √(1 / n)∑(i = 1 ~ n) (yi - yhati)^2
rmse <- function(yi, yhat_i){
  sqrt(mean((yi - yhat_i)^2))
}


# 2. 회귀분석 예제 : 부동산 가격 예측
# 2-1. 변수 설명
# (1) crim : 범죄발생률
# (2) zn : 주거지 중 25000 ft^2 이상 크기의 대형주택이 차지하는 비율
# (3) indus : 소매상 이외의 상업지구의 면적 비율
# (4) chas : 찰스강과 접한 지역은 1, 아니면 0인 더미변수
# (5) nox : 산화질소 오염도
# (6) rm : 주거지당 평균 방 개수
# (7) age : 소유자 주거지(비 전세 / 월세) 중 1940년 이전에 지어진 집들의 비율
# (8) dis : 보스턴의 5대 고용중심으로부터의 가중 평균 거리
# (9) rad : 도시 순환 고속도로에의 접근 용이 지수
# (10) tax : 만달러당 주택 재산세율
# (11) ptratio : 학생-선생 비율
# (12) black : 흑인 인구 비율(Bk)이 0.63과 다른 정도의 제곱, 1000(Bk - 0.63)^2
# (13) lstat : 저소득 주민들의 비율 퍼센트
# (14) medv : 소유자 주거지(비 전세 / 월세) 주택 가격


# 3. 환경 준비와 기초 분석
binomial_deviance <- function(y_obs, yhat){
  epsilon <- 0.0001
  yhat <- ifelse(yhat < epsilon, epsilon, yhat)
  yhat <- ifelse(yhat > 1 - epsilon, 1 - epsilon, yhat)
  a <- ifelse(y_obs == 0, 0, y_obs * log(y_obs / yhat))
  b <- ifelse(y_obs == 1, 0, (1 - y_obs) * log((1 - y_obs) / (1 - yhat)))
  
  return (2 * sum(a + b))
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor) {
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits = digits)[1] 
  txt <- paste(prefix, txt, sep = "") 
  if(missing(cex.cor)) cex <- 0.8 / strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex = cex, col=2) 
}

library(dplyr)
library(ggplot2)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(rpart)
library(boot)
library(data.table)
library(ROCR)
library(gridExtra)

data <- tbl_df(read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data",
                          strip.white = T))
names(data) <- c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "b", "lstat", "medv")
glimpse(data)
summary(data)
pairs(data %>% sample_n(min(1000, nrow(data))), 
      lower.panel = function(x, y){ points(x, y); abline(0, 1, col = "red") },
      upper.panel = panel.cor)


# 4. 훈련, 검증, 테스트 세트의 구분
set.seed(1606)
n <- nrow(data)
idx <- 1 : n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)
training <- data[training_idx, ]
validation <- data[validate_idx, ]
test <- data[test_idx, ]


# 5. 선형회귀 모형
data_lm_full <- lm(medv ~ ., data = training)
summary(data_lm_full)
predict(data_lm_full, newdata = data[1 : 5, ])

# 5-1. 선형회귀 모형에서 변수 선택
# 복잡한 모형은 모든 이차상호작용(2nd degree interaction effect)을 고려한 모형
data_lm_full_2 <- lm(medv ~ .^2, data = training)
summary(data_lm_full_2)
data_step <- stepAIC(data_lm_full, scope = list(upper = ~ .^2, lower = ~1))
                                   # scope : 가장 간단한 모형 = 표본평균 하나의 상수로만 이루어진 모형
                                   #         가장 복잡한 모형 = 앞서 사용한 모든 상호작용을 포함한 모형 사이를 탐색
data_step
anova(data_step)
summary(data_step)
length(coef(data_step)) # 최종 모형의 모수 갯수 = 37

# 5-2. 모형 평가
y_obs <- validation$medv
yhat_lm <- predict(data_lm_full, newdata = validation)
yhat_lm_2 <- predict(data_lm_full_2, newdata = validation)
yhat_step <- predict(data_step, newdata = validation)
rmse(y_obs, yhat_lm); rmse(y_obs, yhat_lm_2); rmse(y_obs, yhat_step)


# 6. 라쏘 모형 적합
xx <- model.matrix(medv ~ .^2 - 1, data)
x <- xx[training_idx, ]
y <- training$medv
glimpse(x)

data_cvfit <- cv.glmnet(x, y)
plot(data_cvfit)
coef(data_cvfit, s = c("lambda.1se")) # 67
coef(data_cvfit, s = c("lambda.min")) # 54

# 6-1. 모형 평가
predict.cv.glmnet(data_cvfit, s = "lambda.min", newx = x[1 : 5, ])

y_obs <- validation$medv
yhat_glmnet <- predict(data_cvfit, s = "lambda.min", newx = xx[validate_idx, ])
yhat_glmnet <- yhat_glmnet[, 1]
rmse(y_obs, yhat_glmnet)


# 7. 나무 모형
(data_tr <- rpart(medv ~ ., data = training))
printcp(data_tr)
summary(data_tr)

opar <- par(mfrow = c(1, 1), xpd = NA)
plot(data_tr)
text(data_tr, use.n = T)
par(opar)

yhat_tr <- predict(data_tr, validation)
rmse(y_obs, yhat_tr)


# 8. 랜덤 포레스트
set.seed(1607)
(data_rf <- randomForest(medv ~ ., data = training))
plot(data_rf)
varImpPlot(data_rf)

yhat_rf <- predict(data_rf, newdata = validation)
rmse(y_obs, yhat_rf)


# 9. 부스팅
set.seed(1607)
data_gbm <- gbm(medv ~ ., data = training, n.trees = 40000, cv.folds = 3, verbose = T)
(best_iter <- gbm.perf(data_gbm, method = "cv"))

yhat_gbm <- predict(data_gbm, n.trees = best_iter, newdata = validation)
rmse(y_obs, yhat_gbm)


# 10. 최종 모형 선택과 테스트세트 오차 계산
data.frame(lm = rmse(y_obs, yhat_step),
           glmnet = rmse(y_obs, yhat_glmnet),
           rf = rmse(y_obs, yhat_rf),
           gbm = rmse(y_obs, yhat_gbm)) %>% melt(value.name = "rmse", variable.name = "method")
rmse(test$medv, predict(data_rf, newdata = test))


# 11. 회귀분석 오차의 시각화
boxplot(list(lm = y_obs - yhat_step,
             glmnet = y_obs - yhat_glmnet,
             rf = y_obs - yhat_rf,
             gbm = y_obs - yhat_gbm), ylab = "Error in Validation Set")
abline(h = 0, lty = 2, col = "blue")
pairs(data.frame(y_obs = y_obs,
                 yhat_lm = yhat_lm,
                 yhat_glmnet = yhat_glmnet,
                 yhat_rf = yhat_rf,
                 yhat_gbm = yhat_gbm),
      lower.panel = function(x, y){ points(x, y); abline(0, 1, col = "red") },
      upper.panel = panel.cor)
