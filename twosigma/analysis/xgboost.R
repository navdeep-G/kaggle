# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 
# Load packages and data
packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

library(data.table)
library(xgboost)
library(caret)
library(stringr)
library(quanteda)
library(lubridate)
library(stringr)
library(Hmisc)
library(Matrix)

catNWayAvgCV <- function(data, varList, y, pred0, filter, k, f, g=1, lambda=NULL, r_k, cv=NULL){
  # It is probably best to sort your dataset first by filter and then by ID (or index)
  n <- length(varList)
  varNames <- paste0("v",seq(n))
  ind <- unlist(cv, use.names=FALSE)
  oof <- NULL
  if (length(cv) > 0){
    for (i in 1:length(cv)){
      sub1 <- data.table(v1=data[,varList,with=FALSE], y=data[,y,with=FALSE], pred0=data[,pred0,with=FALSE], filt=filter)
      sub1 <- sub1[sub1$filt==TRUE,]
      sub1[,filt:=NULL]
      colnames(sub1) <- c(varNames,"y","pred0")
      sub2 <- sub1[cv[[i]],]
      sub1 <- sub1[-cv[[i]],]
      sum1 <- sub1[,list(sumy=sum(y), avgY=mean(y), cnt=length(y)), by=varNames]
      tmp1 <- merge(sub2, sum1, by = varNames, all.x=TRUE, sort=FALSE)
      set(tmp1, i=which(is.na(tmp1[,cnt])), j="cnt", value=0)
      set(tmp1, i=which(is.na(tmp1[,sumy])), j="sumy", value=0)
      if(!is.null(lambda)) tmp1[beta:=lambda] else tmp1[,beta:= 1/(g+exp((tmp1[,cnt] - k)/f))]
      tmp1[,adj_avg:=((1-beta)*avgY+beta*pred0)]
      set(tmp1, i=which(is.na(tmp1[["avgY"]])), j="avgY", value=tmp1[is.na(tmp1[["avgY"]]), pred0])
      set(tmp1, i=which(is.na(tmp1[["adj_avg"]])), j="adj_avg", value=tmp1[is.na(tmp1[["adj_avg"]]), pred0])
      set(tmp1, i=NULL, j="adj_avg", value=tmp1$adj_avg*(1+(runif(nrow(sub2))-0.5)*r_k))
      oof <- c(oof, tmp1$adj_avg)
    }
  }
  oofInd <- data.frame(ind, oof)
  oofInd <- oofInd[order(oofInd$ind),]
  sub1 <- data.table(v1=data[,varList,with=FALSE], y=data[,y,with=FALSE], pred0=data[,pred0,with=FALSE], filt=filter)
  colnames(sub1) <- c(varNames,"y","pred0","filt")
  sub2 <- sub1[sub1$filt==F,]
  sub1 <- sub1[sub1$filt==T,]
  sum1 <- sub1[,list(sumy=sum(y), avgY=mean(y), cnt=length(y)), by=varNames]
  tmp1 <- merge(sub2, sum1, by = varNames, all.x=TRUE, sort=FALSE)
  tmp1$cnt[is.na(tmp1$cnt)] <- 0
  tmp1$sumy[is.na(tmp1$sumy)] <- 0
  if(!is.null(lambda)) tmp1$beta <- lambda else tmp1$beta <- 1/(g+exp((tmp1$cnt - k)/f))
  tmp1$adj_avg <- (1-tmp1$beta)*tmp1$avgY + tmp1$beta*tmp1$pred0
  tmp1$avgY[is.na(tmp1$avgY)] <- tmp1$pred0[is.na(tmp1$avgY)]
  tmp1$adj_avg[is.na(tmp1$adj_avg)] <- tmp1$pred0[is.na(tmp1$adj_avg)]
  # Combine train and test into one vector
  return(c(oofInd$oof, tmp1$adj_avg))
}


# Load training set
print("loading training set")
t1 <- fromJSON("../input/train.json")
t1_feats <- data.table(listing_id=rep(unlist(t1$listing_id), lapply(t1$features, length)), features=unlist(t1$features))
t1_photos <- data.table(listing_id=rep(unlist(t1$listing_id), lapply(t1$photos, length)), features=unlist(t1$photos))
vars <- setdiff(names(t1), c("photos", "features"))
t1<- map_at(t1, vars, unlist) %>% as.data.table(.)
t1[,":="(filter=0)]

# create 5 fold CV
set.seed(321)
cvFoldsList <- createFolds(t1$interest_level, k=5, list=TRUE, returnTrain=FALSE)

# Convert classes to integers for xgboost
class <- data.table(interest_level=c("low", "medium", "high"), class=c(0,1,2))
t1 <- merge(t1, class, by="interest_level", all.x=TRUE, sort=F)

# Load test set
print("loading test set")
s1 <- fromJSON("../input/test.json")
s1_feats <- data.table(listing_id=rep(unlist(s1$listing_id), lapply(s1$features, length)), features=unlist(s1$features))
s1_photos <- data.table(listing_id=rep(unlist(s1$listing_id), lapply(s1$photos, length)), features=unlist(s1$photos))
vars <- setdiff(names(s1), c("photos", "features"))
s1<- map_at(s1, vars, unlist) %>% as.data.table(.)
s1[,":="(interest_level="-1",
         class=-1,
         filter=2)]

ts1 <- rbind(t1, s1)
rm(t1, s1);gc()
ts1_feats <- rbind(t1_feats, s1_feats)
rm(t1_feats, s1_feats);gc()
ts1_photos <- rbind(t1_photos, s1_photos)
rm(t1_photos, s1_photos);gc()

ts1[,":="(created=as.POSIXct(created)
          ,dummy="A"
          ,low=as.integer(interest_level=="low")
          ,medium=as.integer(interest_level=="medium")
          ,high=as.integer(interest_level=="high")
          ,display_address=trimws(tolower(display_address))
          ,street_address=trimws(tolower(street_address)))]
ts1[, ":="(pred0_low=sum(interest_level=="low")/sum(filter==0),
           pred0_medium=sum(interest_level=="medium")/sum(filter==0),
           pred0_high=sum(interest_level=="high")/sum(filter==0))]

# merge Feature column
ts1_feats[,features:=gsub(" ", "_", paste0("feature_",trimws(char_tolower(features))))]
feats_summ <- ts1_feats[,.N, by=features]
ts1_feats_cast <- dcast.data.table(ts1_feats[!features %in% feats_summ[N<10, features]], listing_id ~ features, fun.aggregate = function(x) as.integer(length(x) > 0), value.var = "features")
ts1 <- merge(ts1, ts1_feats_cast, by="listing_id", all.x=TRUE, sort=FALSE)
rm(ts1_feats_cast);gc()

# Photo counts
ts1_photos_summ <- ts1_photos[,.(photo_count=.N), by=listing_id]
ts1 <- merge(ts1, ts1_photos_summ, by="listing_id", all.x=TRUE, sort=FALSE)
rm(ts1_photos, ts1_photos_summ);gc()

# Convert building_ids and manager_ids with only 1 observation into a separate group
build_count <- ts1[,.(.N), by=building_id]
manag_count <- ts1[,.(.N), by=manager_id]
add_count <- ts1[,.(.N), by=display_address]
set(ts1, i=which(ts1[["building_id"]] %in% build_count[N==1, building_id]), j="building_id", value="-1")
set(ts1, i=which(ts1[["manager_id"]] %in% manag_count[N==1, manager_id]), j="manager_id", value="-1")
set(ts1, i=which(ts1[["display_address"]] %in% add_count[N==1, display_address]), j="display_address", value="-1")

# Mean target encode high cardinality variables
print("target encoding")
highCard <- c(
  "building_id",
  "manager_id"
)
for (col in 1:length(highCard)){
  # ts1[,paste0(highCard[col],"_mean_low"):=catNWayAvgCV(ts1, varList=c("dummy",highCard[col]), y="low", pred0="pred0_low", filter=ts1[["filter"]]==0, k=10, f=2, r_k=0.02, cv=cvFoldsList)]
  ts1[,paste0(highCard[col],"_mean_med"):=catNWayAvgCV(ts1, varList=c("dummy",highCard[col]), y="medium", pred0="pred0_medium", filter=ts1$filter==0, k=5, f=1, r_k=0.01, cv=cvFoldsList)]
  ts1[,paste0(highCard[col],"_mean_high"):=catNWayAvgCV(ts1, varList=c("dummy",highCard[col]), y="high", pred0="pred0_high", filter=ts1$filter==0, k=5, f=1, r_k=0.01, cv=cvFoldsList)]
}

# Create some date and other features
print("creating some more features")
ts1[,":="(building_id=as.integer(as.factor(building_id))
          ,display_address=as.integer(as.factor(display_address))
          ,manager_id=as.integer(as.factor(manager_id))
          ,street_address=as.integer(as.factor(street_address))
          ,desc_wordcount=str_count(description)
          ,pricePerBed=ifelse(!is.finite(price/bedrooms),-1, price/bedrooms)
          ,pricePerBath=ifelse(!is.finite(price/bathrooms),-1, price/bathrooms)
          ,pricePerRoom=ifelse(!is.finite(price/(bedrooms+bathrooms)),-1, price/(bedrooms+bathrooms))
          ,bedPerBath=ifelse(!is.finite(bedrooms/bathrooms), -1, price/bathrooms)
          ,bedBathDiff=bedrooms-bathrooms
          ,bedBathSum=bedrooms+bathrooms
          ,bedsPerc=ifelse(!is.finite(bedrooms/(bedrooms+bathrooms)), -1, bedrooms/(bedrooms+bathrooms)))
    ]

# fill in missing values with -1
print("fill in missing values")
for (col in 1:ncol(ts1)){
  set(ts1, i=which(is.na(ts1[[col]])), j=col, value=-1)
}

print("get variable names")
varnames <- setdiff(colnames(ts1), c("photos","pred0_high", "pred0_low","pred0_medium","description", "features","interest_level","dummy","filter", "created", "class", "low","medium","high","street"))
# Convert dataset to sparse format
print("converting data to sparse format")
t1_sparse <- Matrix(as.matrix(ts1[filter==0, varnames, with=FALSE]), sparse=TRUE)
s1_sparse <- Matrix(as.matrix(ts1[filter==2, varnames, with=FALSE]), sparse=TRUE)
listing_id_test <- ts1[filter %in% c(2), listing_id]
labels <- ts1[filter %in% c(0), class]
rm(ts1);gc()

print("converting data into xgb format")
dtrain <- xgb.DMatrix(data=t1_sparse, label=labels)
dtest <- xgb.DMatrix(data=s1_sparse)

param <- list(booster="gbtree",
              objective="multi:softprob",
              eval_metric="mlogloss",
              nthread=13,
              num_class=3,
              eta = .02,
              gamma = 1,
              max_depth = 4,
              min_child_weight = 1,
              subsample = .7,
              colsample_bytree = .5
)

set.seed(201609)
(tme <- Sys.time())
xgb2cv <- xgb.cv(data = dtrain,
                 params = param,
                 nrounds = 50000,
                 maximize=FALSE,
                 prediction = TRUE,
                 folds = cvFoldsList,
                 # nfold = 5,
                 print_every_n = 50,
                 early_stopping_round=300)
Sys.time() - tme
watch <- list(dtrain=dtrain)
xgb2 <- xgb.train(data = dtrain,
                  params = param,
                  # watchlist=watch,
                  # nrounds = xgb2cv$best_ntreelimit
                  nrounds = 2710
)

sPreds <- as.data.table(t(matrix(predict(xgb2, dtest), nrow=3, ncol=nrow(dtest))))
colnames(sPreds) <- class$interest_level
fwrite(data.table(listing_id=listing_id_test, sPreds[,list(high,medium,low)]), "submission.csv")