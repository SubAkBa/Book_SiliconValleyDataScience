# 1. 위스콘신 유방암 데이터
# (1) radius : 반지름
# (2) texture : 그레이스케일 값의 표준편차
# (3) perimeter : 둘레
# (4) area : 면적
# (5) smoothness : 반지름의 국소적 변화 정도(local variation)
# (6) compactness : (perimeter^2 / area - 1.0)
# (7) concavity : 오목한 정도(severity of concave portions of the contour)
# (8) concave_points : 오목한 점들의 개수(number of concave portions of the contour)
# (9) symmetry : 대칭도
# (10) fractal dimension : 프랙탈 차원("coastline approximation" - 1)
#                           url : https://goo.gl/Sw0M9z


# 2. 환경 준비와 기초 분석
binomial_deviance <- function(y_obs, yhat){
  epsilon <- 0.0001
  yhat <- ifelse(yhat < epsilon, epsilon, yhat)
  yhat <- ifelse(yhat > 1 - epsilon, 1 - epsilon, yhat)
  a <- ifelse(y_obs == 0, 0, y_obs * log(y_obs / yhat))
  b <- ifelse(y_obs == 1, 0, (1 - y_obs) * log((1 - y_obs) / (1 - yhat)))
  
  return (2 * sum(a + b))
}
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}

library(dplyr)
library(ggplot2)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(boot)
library(rpart)
library(data.table)
library(ROCR)
library(gridExtra)
library(lattice)

data <- tbl_df(read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",
                          strip.white = T, sep = ",", header = F))
feature_names <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity",
                   "concave_points", "symmetry", "factal_dim")
names(data) <- c("id", "class", paste0("mean_", feature_names), paste0("se_", feature_names),
                 paste0("worst_", feature_names))
glimpse(data)
summary(data)

# id 변수 제거
data <- data %>% dplyr::select(-id)
# class 변수를 factor 변수로 변환
data$class <- factor(ifelse(data$class == "B", 0, 1))


# 3. 데이터의 시각화
pairs(data %>% dplyr::select(class, starts_with("mean_")) %>% sample_n(min(1000, nrow(data))),
      lower.panel = function(x, y){ points(x, y); abline(0, 1, col = "red") },
      upper.panel = panel.cor)

p1 <- data %>% ggplot(aes(class)) + geom_bar()
p2 <- data %>% ggplot(aes(class, mean_concave_points)) + geom_jitter(col = "gray") + geom_boxplot(alpha = .5)
p3 <- data %>% ggplot(aes(class, mean_radius)) + geom_jitter(col = "gray") + geom_boxplot(alpha = .5)
p4 <- data %>% ggplot(aes(mean_concave_points, mean_radius)) + geom_jitter(col = "gray") + geom_smooth()
grid.arrange(p1, p2, p3, p4, ncol = 2)


# 4. 훈련 검증, 테스트세트의 구분
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


# 5. 로지스틱 회귀분석
data_lm_full <- glm(class ~ ., data = training, family = "binomial")
summary(data_lm_full)
predict(data_lm_full, newdata = data[1 : 5, ], type = "response")

# 5-1. 모형 평가
y_obs <- as.numeric(as.character(validation$class))
yhat_lm <- predict(data_lm_full, newdata = validation, type = "response")
pred_lm <- prediction(yhat_lm, y_obs)
performance(pred_lm, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_lm) # 이항편차 = 73.70452, AUC = 0.956


# 6. 라쏘 모형 적합
xx <- model.matrix(class ~ . -1, data)
x <- xx[training_idx, ]
y <- as.numeric(as.character(training$class))
glimpse(x)
data_cvfit <- cv.glmnet(x, y, family = "binomial")
plot(data_cvfit) # λ가 증가함에 따라 선택되는 모수의 개수는 줄어들고, 모형은 간단해진다.
                 # 즉, 편향은 커지미나 분산은 줄어드는 편향-분산 트레이드오프 현상이 일어난다.
                 # 왼쪽 점선 : 가장 정확한 예측값을 낳는 lambda.min
                 # 오른쪽 점선 : 간단하게 해석 가능한 모형을 위한 lambda.1se
coef(data_cvfit, s = c("lambda.1se"))
coef(data_cvfit, s = c("lambda.min"))

# 6-1. 모형 평가
predict.cv.glmnet(data_cvfit, s = "lambda.min", newx = x[1 : 5, ], type = "response")
yhat_glmnet <- predict(data_cvfit, s = "lambda.min", newx = xx[validate_idx, ], type = "response")
yhat_glmnet <- yhat_glmnet[, 1]
pred_glmnet <- prediction(yhat_glmnet, y_obs)
performance(pred_glmnet, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_glmnet)


# 7. 나무 모형
(data_tr <- rpart(class ~ ., data = training))
printcp(data_tr)
summary(data_tr)
opar <- par(mfrow = c(1, 1), xpd = NA)
plot(data_tr)
text(data_tr, use.n = T)
par(opar)

yhat_tr <- predict(data_tr, validation)
yhat_tr <- yhat_tr[, "1"]
pred_tr <- prediction(yhat_tr, y_obs)
performance(pred_tr, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_tr)


# 8. 랜덤 포레스트
set.seed(1607)
(data_rf <- randomForest(class ~ ., data = training))
opar <- par(mfrow = c(1, 2))
plot(data_rf)
varImpPlot(data_rf)
par(opar)

yhat_rf <- predict(data_rf, newdata = validation, type = "prob")[, "1"]
pred_rf <- prediction(yhat_rf, y_obs)
performance(pred_rf, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_rf)


# 9. 부스팅
set.seed(1607)
data_for_gbm <- training %>% mutate(class = as.numeric(as.character(class)))
data_gbm <- gbm(class ~ ., data = data_for_gbm, distribution = "bernoulli", n.trees = 30000, cv.folds = 3, verbose = T)
(best_iter <- gbm.perf(data_gbm, method = "cv"))

yhat_gbm <- predict(data_gbm, n.trees = best_iter, newdata = validation, type = "response")
pred_gbm <- prediction(yhat_gbm, y_obs)
performance(pred_gbm, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_gbm)


# 10. 최종 모형 선택과 테스트세트 오차 계산
data.frame(method = c("lm", "glmnet", "rf"), auc = c(performance(pred_lm, "auc")@y.values[[1]],
                                                     performance(pred_glmnet, "auc")@y.values[[1]],
                                                     performance(pred_rf, "auc")@y.values[[1]]),
           bin_dev = c(binomial_deviance(y_obs, yhat_lm),
                       binomial_deviance(y_obs, yhat_glmnet),
                       binomial_deviance(y_obs, yhat_rf)))
perf_lm <- performance(pred_lm, measure = "tpr", x.measure = "fpr")
perf_glmnet <- performance(pred_glmnet, measure = "tpr", x.measure = "fpr")
perf_rf <- performance(pred_rf, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col = "black", main = "ROC Curve")
plot(perf_glmnet, add = T, col = "blue")
plot(perf_rf, add = T, col = "red")
abline(0, 1)
legend("bottomright", inset = .1, legend = c("GLM", "glmnet", "RF"),
       col = c("black", "blue", "red"), lty = 1, lwd = 2)

# 10-1. 테스트 세트
y_obs_test <- as.numeric(as.character(test$class))
yhat_glmnet_test <- predict(data_cvfit, s = "lambda.min", newx = xx[test_idx, ], type = "response")
yhat_glmnet_test <- yhat_glmnet_test[, 1]
pred_glmnet_test <- prediction(yhat_glmnet_test, y_obs_test)
performance(pred_glmnet_test, "auc")@y.values[[1]]
binomial_deviance(y_obs_test, yhat_glmnet_test)

# 10-2. 예측값의 시각화
pairs(data.frame(y_obs = y_obs, yhat_lm = yhat_lm, yhat_glmnet = c(yhat_glmnet), yhat_rf = yhat_rf), 
      lower.panel = function(x, y){ points(x, y); abline(0, 1, col = "red") },
      upper.panel = panel.cor)
