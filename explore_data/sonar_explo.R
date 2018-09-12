## LOAD & CLEAN 'Pima Indians Diabetes' DATA
library(mlbench)
library(pROC) # roc
library(ROCR) # prediction, performance
library(ggplot2)
data("Sonar") # no missing values
data <- Sonar

## PARTITION DATA
smp_size <- floor(0.75 * nrow(data)) # 75% of the sample size
set.seed(125) # set the seed to make partition reproducible
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind,]
test <- data[-train_ind,]

## TEST FIT
log.fit <- glm(Class~., family=binomial(link="logit"), data=train)
#summary(pro.fit)
#summary(log.fit)
log.prob <- predict.glm(log.fit, newdata=test, type="response") # ? another warning
pro.prob <- predict.glm(pro.fit, newdata=test, type="response") # ? another warning

## PLOT ROC CURVE
plot(roc(test$diabetes, log.prob), print.auc=TRUE)
plot(roc(test$diabetes, pro.prob), print.auc=TRUE)
auc(test$salary, log.prob)
auc(test$salary, pro.prob)
log.pred <- prediction(log.prob, test$diabetes)
pro.pred <- prediction(pro.prob, test$diabetes)
log.perf <- performance(log.pred, measure = "tpr", x.measure = "fpr")
pro.perf <- performance(pro.pred, measure = "tpr", x.measure = "fpr")
log.dd <- data.frame(FP = log.perf@x.values[[1]], TP = log.perf@y.values[[1]])
pro.dd <- data.frame(FP = pro.perf@x.values[[1]], TP = pro.perf@y.values[[1]])
# add unique colors? when graphs are layered
ggplot() + 
  geom_line(data = log.dd, aes(x = FP, y = TP, color = "Logistic Regression")) +
  geom_line(data = pro.dd, aes(x = FP, y = TP, color = "Probit Regression")) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
  ggtitle('ROC Curve') + 
  labs(x = '1 - Specificity', y = 'Sensitivity') 