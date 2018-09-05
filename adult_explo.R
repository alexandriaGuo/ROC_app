library(pROC) # roc
library(ROCR) # prediction, performance
library(ggplot2)

setwd("/Users/alexandriaguo/Desktop/thesis/ROC_app/ROC_app")
# https://archive.ics.uci.edu/ml/datasets/adult
# https://www.kaggle.com/flyingwombat/logistic-regression-with-uci-adult-income
# https://rpubs.com/H_Zhu/235617

## LOAD DATA
data <- read.table("data/adult.txt", sep=",", header = FALSE)

## CLEAN DATA
str(data)
colnames(data) <- c("age", "workclass", "fnlwgt", "education",
                    "education.num", "marital.status", "occupation",
                    "relationship", "race", "sex", "capital.gain",
                    "capital.loss", "hours.per.week", "native.country",
                    "salary")
data[data == "?"] <- NA
data <- na.omit(data) # omit entries with missing data
data$education <- NULL # redundant, see "education.num"
data$fnlwgt <- NULL # final weight of response, unnecessary
data$relationship <- NULL # somewhat redundant, see "marital.status"
# OMITTING ADVISED STEPS TO ORGANIZE/COMBINE LEVELS IN CERTAIN PREDICTORS
summary(data)

## MULTICOLLINEARITY CHECK? (for logistic regression? convert to scale and regress?)
## VARIABLE SELECTION? (t b d)

## PARTITION DATA
smp_size <- floor(0.75 * nrow(data)) # 75% of the sample size
set.seed(125) # set the seed to make partition reproducible
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind,]
test <- data[-train_ind,]

## TEST FIT
log.fit <- glm(salary~., family=binomial(link="logit"), data=train) # perfect separation warning
pro.fit <- glm(salary~., family=binomial(link="probit"), data=train) # perfect separation warning
#summary(pro.fit)
#summary(log.fit)
log.prob <- predict.glm(log.fit, newdata=test, type="response") # ? another warning
pro.prob <- predict.glm(pro.fit, newdata=test, type="response") # ? another warning

## PLOT ROC CURVE
plot(roc(test$salary, log.prob), print.auc=TRUE)
plot(roc(test$salary, pro.prob), print.auc=TRUE)
auc(test$salary, log.prob)
auc(test$salary, pro.prob)
log.pred <- prediction(log.prob, test$salary)
pro.pred <- prediction(pro.prob, test$salary)
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
