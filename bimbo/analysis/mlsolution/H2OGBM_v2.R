
library(data.table)

print("Reading data")
train <- fread('../../data/train.csv')
val <- train[Semana>=8]
train <- train[Semana<8]
test <- fread('../../data/test.csv')

print("Computing means")
#transform target variable to log(1 + demand) - this makes sense since we're 
#trying to minimize rmsle and the mean minimizes rmse:
train$log_demand<-log1p(train$Demanda_uni_equil+1)
mean_total <- mean(train$log_demand) #overall mean

#Get group bys for train(semana < 8)
#mean by product
mean_P <-  train[, .(MP = mean(log_demand)), by = .(Producto_ID)]
#mean by cliente
mean_C <-  train[, .(MC = mean(log_demand)), by = .(Cliente_ID)]
#mean by product and agencia
mean_PA <- train[, .(MPA = mean(log_demand)), by = .(Producto_ID, Agencia_ID)] 
#mean by product and ruta
mean_PR <- train[, .(MPR = mean(log_demand)), by = .(Producto_ID, Ruta_SAK)] 
#mean by product, client, agencia
mean_PCA <- train[, .(MPCA = mean(log_demand)), by = .(Producto_ID, Cliente_ID, Agencia_ID)]

#print("Merging means with train set")
#train <- merge(train, mean_PCA, all.x = TRUE, by = c("Producto_ID", "Cliente_ID", "Agencia_ID"))
#train <- merge(train, mean_PR, all.x = TRUE, by = c("Producto_ID", "Ruta_SAK"))
#train <- merge(train, mean_PA, all.x = TRUE, by = c("Producto_ID", "Agencia_ID"))
#train <- merge(train, mean_C, all.x = TRUE, by = "Cliente_ID")
#train <- merge(train, mean_P, all.x = TRUE, by = "Producto_ID")

print("Merging means with validation set")
val_submit <- merge(val, mean_PCA, all.x = TRUE, by = c("Producto_ID", "Cliente_ID", "Agencia_ID"))
val_submit <- merge(val_submit, mean_PR, all.x = TRUE, by = c("Producto_ID", "Ruta_SAK"))
val_submit <- merge(val_submit, mean_PA, all.x = TRUE, by = c("Producto_ID", "Agencia_ID"))
val_submit <- merge(val_submit, mean_C, all.x = TRUE, by = "Cliente_ID")
val_submit <- merge(val_submit, mean_P, all.x = TRUE, by = "Producto_ID")

print("Merging means with test set")
test_submit <- merge(test, mean_PCA, all.x = TRUE, by = c("Producto_ID", "Cliente_ID", "Agencia_ID"))
test_submit <- merge(test_submit, mean_PR, all.x = TRUE, by = c("Producto_ID", "Ruta_SAK"))
test_submit <- merge(test_submit, mean_PA, all.x = TRUE, by = c("Producto_ID", "Agencia_ID"))
test_submit <- merge(test_submit, mean_C, all.x = TRUE, by = "Cliente_ID")
test_submit <- merge(test_submit, mean_P, all.x = TRUE, by = "Producto_ID")

#get pred for val
val_submit$Pred <- expm1(val_submit$MPCA)*0.717+expm1(val_submit$MPR)*0.1823+0.132
val_submit[is.na(Pred)]$Pred <- expm1(val_submit[is.na(Pred)]$MPR)*0.739+0.1926
val_submit[is.na(Pred)]$Pred <- expm1(val_submit[is.na(Pred)]$MC)*0.822+0.855
val_submit[is.na(Pred)]$Pred <- expm1(val_submit[is.na(Pred)]$MPA)*0.525+0.95
val_submit[is.na(Pred)]$Pred <- expm1(val_submit[is.na(Pred)]$MP)*0.48+1
val_submit[is.na(Pred)]$Pred <- expm1(mean_total)-0.92

#get pred for test_submit
test_submit$Pred <- expm1(test_submit$MPCA)*0.717+expm1(test_submit$MPR)*0.1823+0.132
test_submit[is.na(Pred)]$Pred <- expm1(test_submit[is.na(Pred)]$MPR)*0.739+0.1926
test_submit[is.na(Pred)]$Pred <- expm1(test_submit[is.na(Pred)]$MC)*0.822+0.855
test_submit[is.na(Pred)]$Pred <- expm1(test_submit[is.na(Pred)]$MPA)*0.525+0.95
test_submit[is.na(Pred)]$Pred <- expm1(test_submit[is.na(Pred)]$MP)*0.48+1
test_submit[is.na(Pred)]$Pred <- expm1(mean_total)-0.92

#fwrite(train,"train_new.csv")
fwrite(val_submit,"val.csv")
fwrite(test_submit,"test_new.csv")
#################
## Set up Cluster (H2O is a Java ML Platform, with R/Python/Web/Java APIs)
#################
print(paste("Set up Cluster",Sys.time()))
library(h2o) # R API is just a library
## start a cluster; default is 2 cores for CRAN reasons; -1 = all
h2o.init(nthreads=-1,min_mem_size='12G')

#train_hex<-h2o.importFile("/Users/navdeepgill/Desktop/Git/kaggle/bimbo/analysis/mlsolution/train_new.csv",destination_frame = "train.hex")
val_hex<-h2o.importFile("/Users/navdeepgill/Desktop/Git/kaggle/bimbo/analysis/mlsolution/val.csv",destination_frame = "val.hex")
test_hex<-h2o.importFile("/Users/navdeepgill/Desktop/Git/kaggle/bimbo/analysis/mlsolution/test_new.csv",destination_frame = "test.hex")
## train a GBM; use aggressive parameters to keep overall runtime within 20 minutes
## this model is fit on Semana 6 & 7, and evaluated on Semana 8.
#val_hex$target<-log(val_hex$Demanda_uni_equil+1)
predictors<-c("Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","MPCA","MPR","MPA","MC","MP")
val_hex$target<-log1p(val_hex$Demanda_uni_equil+1)
val_hex$Semana = as.factor(val_hex$Semana)
test_hex$Semana = as.factor(test_hex$Semana)

print("Building H2O GBM Model")
g2<-h2o.gbm(
  training_frame = val_hex,      ## H2O frame holding the training data
  x=predictors,                 ## this can be names or column numbers
  y="target",                   ## target: using the logged variable created earlier
  model_id="gbm2",              ## internal H2O name for model
  ntrees = 200,                  ## use fewer trees than default (50) to speed up training
  learn_rate = 0.05,             ## lower learn_rate is better, but use high rate to offset few trees
  score_tree_interval = 3,      ## score every 3 trees
  #sample_rate = 0.5,            ## use half the rows each scoring round
  #col_sample_rate = 0.8,        ## use 4/5 the columns to decide each split decision
  fold_column = "Semana"
  #offset_column = "Pred"
)

#Make predictions
p<-h2o.predict(g,test_hex)
p<-exp(p)-1
summary(p)
## Create Submission
print(paste("Create Submission",Sys.time()))
submissionFrame<-h2o.cbind(test_hex$id,p)
colnames(submissionFrame)<-c("id","Demanda_uni_equil")
h2o.exportFile(submissionFrame,path="h2o_gbmv3.csv")  ## export submission
