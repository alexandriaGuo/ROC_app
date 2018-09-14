## LOAD & CLEAN 'Adult Salaries' DATA
setwd("/Users/alexandriaguo/Documents/2018-2019/thesis/ROC_app/ROC_app")
adult <- read.table("data/adult.txt", sep=",", header = FALSE)
colnames(adult) <- c("age", "workclass", "fnlwgt", "education",
                    "education.num", "marital.status", "occupation",
                    "relationship", "race", "sex", "capital.gain",
                    "capital.loss", "hours.per.week", "native.country",
                    "salary")
adult[adult == "?"] <- NA
adult <- na.omit(adult) # omit entries with missing data
adult$education <- NULL # redundant, see "education.num"
adult$fnlwgt <- NULL # final weight of response, unnecessary
adult$relationship <- NULL # somewhat redundant, see "marital.status"
# OMITTING ADVISED STEPS TO ORGANIZE/COMBINE LEVELS IN CERTAIN PREDICTORS
adult$label <- adult$salary
levels(adult$label) <- list("1" = ">50K",
                            "0" = "<=50K")
adult$salary <- NULL

## LOAD & CLEAN 'Pima Indians Diabetes' DATA
library(mlbench)
data("PimaIndiansDiabetes2") # has 0 values set to NA where physically impossible
pima <- na.omit(PimaIndiansDiabetes2) # n = 768 to n = 392
rm(PimaIndiansDiabetes2)
pima$label <- pima$diabetes
levels(pima$label) <- list("1" = "pos",
                           "0" = "neg")
pima$diabetes <- NULL

## LOAD & CLEAN 'Sonar' DATA
data("Sonar") # has no NA values
sonar <- Sonar
rm(Sonar)
sonar$label <- sonar$Class
levels(sonar$label) <- list("1" = "M",
                            "0" = "R")
sonar$Class <- NULL

## FUNCTION FOR FINDING SPECIFIC THRESHOLD TPR/FPR
find_thresh <- function(test, thresh) {
  ind <- which(test$prob >= thresh)
  if (length(ind) != 0) {
    lab_pos <- test$label[ind]
    lab_neg <- test$label[-ind]
    tp <- length(lab_pos[lab_pos=="1"])
    tn <- length(lab_neg[lab_neg=="0"])
    fp <- length(lab_pos[lab_pos=="0"])
    fn <- length(lab_neg[lab_neg=="1"])
  } else {
    tp <- 0
    tn <- length(test$label[test$label=="0"])
    fp <- 0
    fn <- length(test$label[test$label=="1"])
  }
  tpr <- tp / (tp + fn)
  fpr <- fp / (fp + tn)
  return(data.frame(FP = fpr, TP = tpr))
}
