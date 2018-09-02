## LOAD & CLEAN 'Adult Salaries' DATA
setwd("/Users/alexandriaguo/Desktop/thesis/ROC_app/ROC_app")
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