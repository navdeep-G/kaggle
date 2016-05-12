# H2O Grid
library(h2o)
library(readr)

h2o.init(nthreads=-1)

categoricalVariables = c("Product_Info_1", "Product_Info_2", "Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", "Employment_Info_2", "Employment_Info_3", 
                         "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", "Insurance_History_1",
                         "Insurance_History_2", "Insurance_History_3", "Insurance_History_4", "Insurance_History_7", "Insurance_History_8", "Insurance_History_9", "Family_Hist_1", 
                         "Medical_History_2", "Medical_History_3", "Medical_History_4", "Medical_History_5", "Medical_History_6", "Medical_History_7", "Medical_History_8", 
                         "Medical_History_9", "Medical_History_10", "Medical_History_11", "Medical_History_12", "Medical_History_13", "Medical_History_14", "Medical_History_16", 
                         "Medical_History_17", "Medical_History_18", "Medical_History_19", "Medical_History_20", "Medical_History_21", "Medical_History_22", "Medical_History_23", 
                         "Medical_History_25", "Medical_History_26", "Medical_History_27", "Medical_History_28", "Medical_History_29", "Medical_History_30", "Medical_History_31", 
                         "Medical_History_33", "Medical_History_34", "Medical_History_35", "Medical_History_36", "Medical_History_37", "Medical_History_38", "Medical_History_39", 
                         "Medical_History_40", "Medical_History_41")
specifiedTypes = rep("Enum", length(categoricalVariables))

cat("reading the train and test data\n")
trainlocal <- read_csv("../../data/train.csv")
testlocal  <- read_csv("../../data/test.csv")

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


learn_rate_opt <- c(0.1, 0.2, 0.3)
#learn_rate_opt <- c(0.01, 0.05)
max_depth_opt <- c(3, 5, 7, 9, 11)
sample_rate_opt <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
col_sample_rate_per_tree_opt <- c(0.2, 0.5, 0.8, 1.0)
hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt, 
                     sample_rate = sample_rate_opt)

grid <- h2o.grid(algorithm = "gbm",
                 hyper_params = hyper_params,
                 training_frame = train,
                 x = independentVariables,
                 y = dependentVariable,
                 distribution = "gaussian",
                 stopping_rounds = 3,
                 stopping_tolerance = 0,
                 ntrees = 400,
                 nfolds = 5,
                 fold_assignment = "Modulo",
                 keep_cross_validation_predictions = TRUE,
                 seed = 1)
path = "/Users/navdeepgill/Desktop/Kaggle/prudential/scripts/modelling/models"

model.list = list()
# print out the Test MSE for all of the models
for (model_id in grid@model_ids) {
  print(model_id)
  model <- h2o.getModel(model_id)
  model.list[model_id] = h2o.mse(model, xval = TRUE)
  print(sprintf("CV MSE: %f", mse))
  #model_folder <- h2o.saveModel(object = model, path = path, force = TRUE)
}
print("done!")

model.list.df = data.frame(t(as.data.frame(model.list)))
colnames(model.list.df) = c("mse")
model.list.df = cbind(rownames(model.list.df),model.list.df$mse)
model.list.df = data.frame(model.list.df)
colnames(model.list.df) = c("model_id","mse")
model.list.df[which.min(model.list.df$mse),]

#Best model from grid search
#Grid_GBM_RTMP_140_model_R_1451863399815_46_model_10 3.538430

#Load best model from grid
best.grid = h2o.loadModel("/Users/navdeepgill/Desktop/Kaggle/prudential/scripts/modelling/models/Grid_GBM_RTMP_140_model_R_1451863399815_46_model_10")

#Make predictions from best model, best.grid()
cat("make predictions using best grid search model")
prediction <- as.data.frame( predict(best.grid, test) )
submission <- as.data.frame(test$Id)
submission <- cbind(submission, round(prediction$predict))
names(submission) <- c("Id", "Response")

submission[submission$Response<1, "Response"] <- 1
submission[submission$Response>8, "Response"] <- 8

cat("saving the submission file\n")
write_csv(submission, "../../submissions/h2ogbm-5-1-v1.csv")


