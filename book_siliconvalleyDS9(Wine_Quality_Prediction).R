# 1. 와인 품질 데이터 소개
# (1) fixed_acidity : 비휘발성 산
# (2) volatile_acidity : 휘발성 산
# (3) citric_acid : 구연산(시트르산)
# (4) residual_sugar : 잔당
# (5) chlorides : 염화물
# (6) free_sulfur_dioxide : 유리 이산화황
# (7) total_sulfur_dioxide : 총 이산화황
# (8) density : 밀도
# (9) pH : 산도(수소 이온 농도)
# (10) sulphates : 황산염
# (11) alcohol : 알코올
# (12) quality : 사람이 평가한 와인 품질. 0(나쁨)에서 10(좋음)까지의 값을 가진다.


# 2. 환경 준비와 기초 분석
rmse <- function(yi, yhat_i){
  sqrt(mean((yi - yhat_i)^2))
}
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

data <- tbl_df(read.table("winequality-white.csv", strip.white = T, sep = ";", header = T))
glimpse(data)
summary(data)


# 3. 데이터의 시각화
pairs(data %>% sample_n(min(1000, nrow(data))),
      lower.panel = function(x, y){ points(x, y); abline(0, 1, col = "red")},
      upper.panel = panel.cor)

p1 <- data %>% ggplot(aes(quality)) + geom_bar()
p2 <- data %>% ggplot(aes(factor(quality), alcohol)) + geom_boxplot()
p3 <- data %>% ggplot(aes(factor(quality), density)) + geom_boxplot()
p4 <- data %>% ggplot(aes(alcohol, density)) + geom_point(alpha = .1) + geom_smooth()
grid.arrange(p1, p2, p3, p4, ncol = 2)


# 4. 훈련, 검증, 테스트세트의 구분
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
data_lm_full <- lm(quality ~ ., data = training)
summary(data_lm_full)
predict(data_lm_full, newdata = data[1 : 5, ])

# 5-1. 선형회귀 모형에서 변수 선택
data_lm_full_2 <- lm(quality ~ .^2, data = training)
summary(data_lm_full_2)
length(coef(data_lm_full_2)) # 이차상호작용 모수 갯수 = 67개

data_step <- stepAIC(data_lm_full_2, scpoe = list(upper = ~ .^2, lower = ~1))
data_step
anova(data_step)
summary(data_step)

# 5-2. 모형 평가
y_obs <- validation$quality
yhat_lm <- predict(data_lm_full, newdata = validation)
yhat_lm_2 <- predict(data_lm_full_2, newdata = validation)
yhat_step <- predict(data_step, newdata = validation)
rmse(y_obs, yhat_lm); rmse(y_obs, yhat_lm_2); rmse(y_obs, yhat_step)


# 6. 라쏘 모형 적합
xx <- model.matrix(quality ~ .^2 - 1, data)
x <- xx[training_idx, ]
y <- training$quality
glimpse(x)

data_cvfit <- cv.glmnet(x, y)
plot(data_cvfit)
coef(data_cvfit, s = c("lambda.1se"))
coef(data_cvfit, s = c("lambda.min"))

# 6-1. 모형 평가
predict.cv.glmnet(data_cvfit, s = "lambda.min", newx = x[1 : 5, ])

y_obs <- validation$quality
yhat_glmnet <- predict(data_cvfit, s = "lambda.min", newx = xx[validate_idx, ])
yhat_glmnet <- yhat_glmnet[, 1]
rmse(y_obs, yhat_glmnet)


# 7. 나무 모형
(data_tr <- rpart(quality ~ ., data = training))
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
(data_rf <- randomForest(quality ~ ., training))
plot(data_rf)
varImpPlot(data_rf)

yhat_rf <- predict(data_rf, newdata = validation)
rmse(y_obs, yhat_rf)


# 9. 부스팅
set.seed(1607)
data_gbm <- gbm(quality ~ ., data = training, n.trees = 40000, cv.folds = 3, verbose = T)
(best_iter <- gbm.perf(data_gbm, method = "cv"))

yhat_gbm <- predict(data_gbm, n.trees = best_iter, newdata = validation)
rmse(y_obs, yhat_gbm)


# 10. 최종 모형 선택과 테스트세트 오차 계산
data.frame(lm = rmse(y_obs, yhat_step),
           glmnet = rmse(y_obs, yhat_glmnet),
           rf = rmse(y_obs, yhat_rf),
           gbm = rmse(y_obs, yhat_gbm)) %>% melt(value.name = "rmse", variable.name = "method")
rmse(test$quality, predict(data_rf, newdata = test))

# 10-1. 회귀분석의 예측값의 시각화
pairs(data.frame(y_obs = y_obs,
                 yhat_lm = yhat_lm,
                 yhat_glmnet = yhat_glmnet,
                 yhat_rf = yhat_rf,
                 yhat_gbm = yhat_gbm),
      lower.panel = function(x, y){ points(x, y); abline(0, 1, col = "red") },
      upper.panel = panel.cor)
