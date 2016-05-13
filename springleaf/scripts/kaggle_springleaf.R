#Model building using h20. 

#Load in h2o package
library(h2o)

#StartH2Oonlocalhost,port54321,with4gofmemoryusingallCPUs
localH2O<-h2o.init(ip='localhost',port=54321,nthreads=-1,max_mem_size='4g')

#Convert to h2o frame
train.hex = as.h2o(train,destination_frame="train.hex")
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Model Buidling:
#First run using h2o.glm. Using data as is without any feature selection/removal of features.
#Note, can't seem to run h2o.glm with nfold argument as there is an error of "null". Need to look into this further...

#Logistic model. AUC reported of .5
train.hex["target"] = as.factor(train.hex["target"])
#binomial.fit = h2o.glm(y = "target", x = feature.names, training_frame = train.hex, family = "binomial",nfolds=5)
#print(binomial.fit)
#print(paste("training auc: ", binomial.fit@model$training_metrics@metrics$AUC))
#print(paste("cross-validation auc:", binomial.fit@model$cross_validation_metrics@metrics$AUC))

#---------------------------------------------------------------------------------------------------------------------------------------------------------
#First run using h2o.gbm. 
#Using default settings.
#GBM model with 10 fold cv.
#Training AUC = .79. CV AUC = .77 with ntrees=50 and no variable reduction. 
#Training AUC = .81. CV AUC = .77 with ntrees=100 and no variable reduction.
#Train AUC = .78. CV AUC = .77 with ntrees=50 and variable reduction.
#Train AUC = .80. CV AUC = .77 with ntrees=100 and variable reduction.
#Since CV AUC stayed the same regardless of ntrees parameter we can go on to do the grid search the default ntree setting. 
gbm.model.50tree <- h2o.gbm(y = "target", x = feature.names, distribution="bernoulli", training_frame = train.hex,nfolds = 10,ntrees=50)
#gbm.model.50tree.lowlr <- h2o.gbm(y = "target", x = feature.names, distribution="bernoulli", training_frame = train.hex,nfolds = 10,ntrees=50,learn_rate=.01)
gbm.model.100tree <- h2o.gbm(y = "target", x = feature.names, distribution="bernoulli", training_frame = train.hex,nfolds = 10,ntrees=100)
#gbm.model.100tree.lowlr <- h2o.gbm(y = "target", x = feature.names, distribution="bernoulli", training_frame = train.hex,nfolds = 10,ntrees=100,learn_rate=.01)

#gbm.model.100tree.10 <- h2o.gbm(y = "target", x = feature.names, distribution="bernoulli", training_frame = train.hex,nfolds = 10,ntrees=100,max_depth=10)
#gbm.model.50tree.10 <- h2o.gbm(y = "target", x = feature.names, distribution="bernoulli", training_frame = train.hex,nfolds = 10,ntrees=50,max_depth=10)
#gbm.model.100tree.15 <- h2o.gbm(y = "target", x = feature.names, distribution="bernoulli", training_frame = train.hex,nfolds = 10,ntrees=100,max_depth=15)
#gbm.model.50tree.15 <- h2o.gbm(y = "target", x = feature.names, distribution="bernoulli", training_frame = train.hex,nfolds = 10,ntrees=50,max_depth=15)

#gbm.model.100tree.10.lowlr <- h2o.gbm(y = "target", x = feature.names, distribution="bernoulli", training_frame = train.hex,nfolds = 10,ntrees=100,max_depth=10,learn_rate=.01)
#gbm.model.50tree.10.lowlr <- h2o.gbm(y = "target", x = feature.names, distribution="bernoulli", training_frame = train.hex,nfolds = 10,ntrees=50,max_depth=10,learn_rate=.01)
#gbm.model.100tree.15.lowlr <- h2o.gbm(y = "target", x = feature.names, distribution="bernoulli", training_frame = train.hex,nfolds = 10,ntrees=100,max_depth=15,learn_rate=.01)
#gbm.model.50tree.15.lowlr <- h2o.gbm(y = "target", x = feature.names, distribution="bernoulli", training_frame = train.hex,nfolds = 10,ntrees=50,max_depth=15,learn_rate=.01)

#Deep Learning
train.deep <- h2o.deeplearning(x = feature.names,
                               y = "target",
                               training_frame = train.hex,
                               distribution = "bernoulli",
                               activation = "RectifierWithDropout",
                               hidden = c(200,200,200),
                               input_dropout_ratio = 0.2,
                               l1 = 1e-5,
                               epochs = 50,
                               nfolds = 10)

#Get some model stats
print(paste("training auc: ", gbm.model.50tree@model$training_metrics@metrics$AUC))
print(paste("cross-validation auc:", gbm.model.50tree@model$cross_validation_metrics@metrics$AUC))

#Get some model stats
print(paste("training auc: ", gbm.model.100tree@model$training_metrics@metrics$AUC))
print(paste("cross-validation auc:", gbm.model.100tree@model$cross_validation_metrics@metrics$AUC))

#Get some model stats
print(paste("training auc: ", train.deep@model$training_metrics@metrics$AUC))
print(paste("cross-validation auc:", train.deep@model$cross_validation_metrics@metrics$AUC))

#Get model summary
summary(gbm.model.50tree)

summary(train.deep)
summary(gbm.model.100tree)

#Extract variable importance
var_importance50 = h2o.varimp(gbm.model.50tree)
var_importance100 = h2o.varimp(gbm.model.100tree)

#----------------------------------------------------------------------------------------------------------------------------------------------------------

#An h2o.gbm model with a grid search using variety of options for parameters. Note, features have remained untouched thus far. 
#ntrees_opt <- list(ntrees=c(50,70,100))
maxdepth_opt <- c(5,10,15)
learnrate_opt <- c(0.1,0.2)
hyper_parameters <- list(max_depth=maxdepth_opt, learn_rate=learnrate_opt)

grid <- h2o.grid("gbm", hyper_params = hyper_parameters, y = "target", x = feature.names, 
                 distribution="bernoulli", training_frame = train.hex,nfolds = 10)

# print out the cv auc for all of the models
grid_models <- lapply(grid@model_ids, function(model_id) { model = h2o.getModel(model_id) })
for (i in 1:length(grid_models)) {
  print(sprintf("cross validation auc: %f", h2o.auc(grid_models[[i]],xval=T)))
  print(grid_models[i])
  }

