library(pROC) # roc
library(ROCR) # prediction, performance
library(ggplot2)

setwd("/Users/alexandriaguo/Desktop/thesis/ROCapp")
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
full.fit <- glm(salary~., family=binomial(link="probit"), data=train) # perfect separation warning
summary(full.fit)
prob <- predict.glm(full.fit, newdata=test, type="response") # ? another warning

## PLOT ROC CURVE
plot(roc(test$salary, prob), print.auc=TRUE)
auc(test$salary, prob)
pred <- prediction(prob, test$salary)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
dd <- data.frame(FP = perf@x.values[[1]], TP = perf@y.values[[1]])
# add unique colors? when graphs are layered
ggplot() + 
  geom_line(data = dd, aes(x = FP, y = TP, color = "Logistic Regression")) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
  guides(color=FALSE) +
  ggtitle('ROC Curve') + 
  labs(x = '1 - Specificity', y = 'Sensitivity') 
