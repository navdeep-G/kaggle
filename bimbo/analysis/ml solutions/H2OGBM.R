## Version 1: all defaults for H2O GBM
## Version 2: fewer trees, row and column sample rate to restrict runtime
## Version 3: fixed mistake - inconsistent scoring and trees (15 v 20)
## Version 4: reduced memory limit back to original 6G from 8G
## Version 5: using product means as offset, dev/val/final splits to judge accuracy
## Version 6: reduced memory limit again to 6G (export into frame for submission uses extra RAM?)
## Version 7: write file directly from H2O (not pushing to R first); memory back to 7G
## Version 8: re-insert exp() on target (accidentally removed with transform to direct output

#################
## Set up Cluster (H2O is a Java ML Platform, with R/Python/Web/Java APIs)
#################
print(paste("Set up Cluster",Sys.time()))
library(h2o) # R API is just a library
## start a cluster; default is 2 cores for CRAN reasons; -1 = all
h2o.init(nthreads=-1,max_mem_size='7G')  

#############
## Load Data 
#############
print(paste("Load Data",Sys.time()))
## load the full training file, using all cores; load into H2O Frame known to R as train; H2O as train.hex
train<-h2o.uploadFile("../../data/train.csv",destination_frame = "train.hex")

train[1:2,] ## take a look at a few rows
## train on the log of the target
train$target<-log(train$Demanda_uni_equil+1)

#################
## Partition Data
#################
print(paste("Partition Data",Sys.time()))
## This model will use three splits, partitioned by week: 
##   one to generate product averages, a second to fit a model, and a third to evaluate the model
dev<-train[train$Semana <= 5,]                    ## gets Semana 3,4,5
val<-train[train$Semana > 5 & train$Semana < 8,]  ## gets Semana 6, 7
val[1:5,]
final<-train[train$Semana == 8,]                  ## gets Semana 8
final[1:5,]

##############################
## Model: Product Groups & GBM
##############################
print(paste("Model: Product Groups & GBM",Sys.time()))
## use the fields available in test to predict; removing ID and Semana
##   for iteration, Semana would probably be a good field to use to control loops
##   in H2O you can directly use it as a "fold column" if you'd like
predictors<-c("Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID")

## first part of model: use product averages, created on the dev set
##  this is the only time we will use the dev set
groups<-h2o.group_by(data=dev,by="Producto_ID",mean("target"))
groups[1:5,]

## apply groups back into dev and validation data sets as "mean_target"
## if there are NAs for this (new products), use a constant; used median of entire train target
newVal<-h2o.merge(x=val,y=groups,all.x = T)
newVal$mean_target[is.na(newVal$mean_target)]<-0.7
newVal[1:5,]
newFinal<-h2o.merge(x=final,y=groups,all.x = T)
newFinal$mean_target[is.na(newFinal$mean_target)]<-0.7
newFinal[1:5,]


## train a GBM; use aggressive parameters to keep overall runtime within 20 minutes
## this model is fit on Semana 6 & 7, and evaluated on Semana 8.
g<-h2o.gbm(
  training_frame = newVal,      ## H2O frame holding the training data
  validation_frame = newFinal,  ## extra holdout piece for three layer modeling
  x=predictors,                 ## this can be names or column numbers
  y="target",                   ## target: using the logged variable created earlier
  model_id="gbm1",              ## internal H2O name for model
  ntrees = 200,                  ## use fewer trees than default (50) to speed up training
  learn_rate = 0.3,             ## lower learn_rate is better, but use high rate to offset few trees
  score_tree_interval = 3,      ## score every 3 trees
  sample_rate = 0.5,            ## use half the rows each scoring round
  col_sample_rate = 0.8,        ## use 4/5 the columns to decide each split decision
  offset_column = "mean_target"
)

## look at model diagnostics
summary(g)

# clean up frames no longer needed
h2o.rm(train)
h2o.rm(dev)
h2o.rm(val)
h2o.rm(newVal)

#####################
## Create Predictions
#####################
print(paste("Create Predictions",Sys.time()))
## load test file
test<-h2o.uploadFile("../data/test.csv",destination_frame = "test.hex")
test[1:2,] ## take a look at a few rows of the test data
## merge in the offset column, just as with val and final
newTest<-h2o.merge(x=test,y=groups,all.x = T)
newTest$mean_target[is.na(newTest$mean_target)]<-0.7
newTest[1:5,]
p<-h2o.predict(g,newTest)
p<-exp(p)-1
summary(p)

####################
## Create Submission
####################
print(paste("Create Submission",Sys.time()))
submissionFrame<-h2o.cbind(test$id,p)
colnames(submissionFrame)<-c("id","Demanda_uni_equil")
h2o.exportFile(submissionFrame,path="h2o_gbmv2.csv")  ## export submission