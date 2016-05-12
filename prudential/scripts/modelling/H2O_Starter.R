# H2O Starter Script
library(h2o)
library(readr)

h2o.init(nthreads=-1)

categoricalVariables = c("Product_Info_1", "Product_Info_2", "Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", "Employment_Info_2", "Employment_Info_3", 
                         "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2", 
                         "InsuredInfo_3", "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", 
                         "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", "Insurance_History_4", "Insurance_History_7", 
                         "Insurance_History_8", "Insurance_History_9", "Family_Hist_1", "Medical_History_2", "Medical_History_3", "Medical_History_4", 
                         "Medical_History_5", "Medical_History_6", "Medical_History_7", "Medical_History_8", 
                         "Medical_History_9", "Medical_History_10", "Medical_History_11", "Medical_History_12", "Medical_History_13", "Medical_History_14", 
                         "Medical_History_16", "Medical_History_17", "Medical_History_18", "Medical_History_19", 
                         "Medical_History_20", "Medical_History_21", "Medical_History_22", "Medical_History_23", "Medical_History_25", "Medical_History_26", 
                         "Medical_History_27", "Medical_History_28", "Medical_History_29", "Medical_History_30", 
                         "Medical_History_31", "Medical_History_33", "Medical_History_34", "Medical_History_35", "Medical_History_36", "Medical_History_37", 
                         "Medical_History_38", "Medical_History_39", "Medical_History_40", "Medical_History_41")
specifiedTypes = rep("Enum", length(categoricalVariables))

cat("reading the train and test data\n")
trainlocal <- read_csv("../../data/train.csv")
testlocal  <- read_csv("../../data/test.csv")

trainlocal[is.na(trainlocal)] = 0
testlocal[is.na(testlocal)] = 0

cat("loading into h2o")
train <- as.h2o(trainlocal)
test <- as.h2o(testlocal)

cat("Converting to categorical variables")
for (f in categoricalVariables) {
  train[[f]] <- as.factor( train[[f]] )
  test[[f]]  <- as.factor( test[[f]]  )
}

independentVariables = names(train)[3:ncol(train)-1]
dependentVariable = names(train)[128]

cat("Training gbm")
#1st attempt
h2oGbm <- h2o.gbm(x=independentVariables, y=dependentVariable, training_frame = train, 
                  learn_rate=0.025, ntrees=235, max_depth=22, min_rows=3)

#2nd attempt. Increase number of trees significantly
h2oGbm <- h2o.gbm(x=independentVariables, y=dependentVariable, training_frame = train, 
                  learn_rate=0.025, ntrees=5000, max_depth=22, min_rows=3)

cat("Creating submission frame")
prediction <- as.data.frame( predict(h2oGbm, test) )
submission <- as.data.frame(test$Id)
submission <- cbind(submission, round(prediction$predict))
names(submission) <- c("Id", "Response")

# I pretended this was a regression problem and some predictions may be outside the range
submission[submission$Response<1, "Response"] <- 1
submission[submission$Response>8, "Response"] <- 8

cat("saving the submission file\n")
write_csv(submission, "../../submissions/h2ogbm-4-1-v2.csv")

###################################################################################
# H2O Starter Script v2
library(h2o)
library(readr)

h2o.init()


categoricalVariables <- c("Product_Info_1", "Product_Info_2", "Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", 
                          "Employment_Info_2", "Employment_Info_3", "Employment_Info_5", 
                          "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", 
                          "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", "Insurance_History_4", "Insurance_History_7", 
                          "Insurance_History_8", "Insurance_History_9", 
                          "Family_Hist_1", "Medical_History_2", 
                          "Medical_History_3", "Medical_History_4", "Medical_History_5", "Medical_History_6", "Medical_History_7", 
                          "Medical_History_8", "Medical_History_9", "Medical_History_10", "Medical_History_11", "Medical_History_12", 
                          "Medical_History_13", "Medical_History_14", "Medical_History_16", "Medical_History_17", "Medical_History_18", 
                          "Medical_History_19", "Medical_History_20", "Medical_History_21", "Medical_History_22", "Medical_History_23", 
                          "Medical_History_25", "Medical_History_26", "Medical_History_27", "Medical_History_28", "Medical_History_29", 
                          "Medical_History_30", "Medical_History_31", "Medical_History_33", "Medical_History_34", "Medical_History_35", 
                          "Medical_History_36", "Medical_History_37", "Medical_History_38", "Medical_History_39", "Medical_History_40", "Medical_History_41")
specifiedTypes <- rep("Enum", length(categoricalVariables))
col.types <- list(by.col.name=categoricalVariables,types=specifiedTypes)

independentVariables = names(train)[2:(ncol(train)-1)]
dependentVariable = names(train)[128]

cat("creating model")

#Third attempt --> Best model as of Jan 4th., 2016 .59621 on the board
gbm_model <- h2o.gbm(x=independentVariables, y=dependentVariable, training_frame = train,
                     ntrees=400, max_depth=10)

#Fourth attempt
gbm_model <- h2o.gbm(x=independentVariables, y=dependentVariable, training_frame = train,
                     ntrees=500, max_depth=20)

#Fifth attempt
gbm_model <- h2o.gbm(x=independentVariables, y=dependentVariable, training_frame = train,
                     ntrees=1000, max_depth=10)

#Sixth attempt
gbm_model <- h2o.gbm(x=independentVariables, y=dependentVariable, training_frame = train,
                     ntrees=100, max_depth=10)

cat("make predictions")
prediction <- as.data.frame( predict(gbm_model, test) )
submission <- as.data.frame(test$Id)
submission <- cbind(submission, round(prediction$predict))
names(submission) <- c("Id", "Response")

submission[submission$Response<1, "Response"] <- 1
submission[submission$Response>8, "Response"] <- 8

# without this line the score is 0.58903
# with this line the score is 0.59656
submission[submission$Response==3,"Response"] <- 2

cat("saving the submission file\n")
write_csv(submission, "../../submissions/h2ogbm-5-1-v7.csv")
##########################################################################################################################
library(readr)
library(xgboost)

# Set a random seed for reproducibility
set.seed(1)

cat("reading the train and test data\n")
train <- read_csv("../../data/train.csv")
test  <- read_csv("../../data/test.csv")

feature.names <- names(train)[2:(ncol(train)-1)]

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

train[is.na(train)] = 0
test[is.na(test)] = 0

cat("training a XGBoost classifier\n")
clf <- xgboost(data        = data.matrix(train[,feature.names]),
               label       = train$Response,
               eta         = 0.025,
               depth       = 10,
               nrounds     = 2500,
               objective   = "reg:linear",
               eval_metric = "rmse")

cat("making predictions\n")
submission <- data.frame(Id=test$Id)
submission$Response <- as.integer(round(predict(clf, data.matrix(test[,feature.names]))))

# I pretended this was a regression problem and some predictions may be outside the range
submission[submission$Response<1, "Response"] <- 1
submission[submission$Response>8, "Response"] <- 8

cat("saving the submission file\n")
write_csv(submission, "../../submissions/5-1-v7.csv")
