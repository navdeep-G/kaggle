# dummy
d1 <- label1[,list(device_id,phone_brand)]
label1$phone_brand <- NULL
d2 <- label1[,list(device_id,device_model)]
label1$device_model <- NULL
d3 <- device_apps
rm(device_apps)
d1[,phone_brand:=paste0("phone_brand:",phone_brand)]
d2[,device_model:=paste0("device_model:",device_model)]
d3[,app_id:=paste0("app_id:",app_id)]
names(d1) <- names(d2) <- names(d3) <- c("device_id","feature_name")
dd <- rbind(d1,d2,d3)
rm(d1,d2,d3);gc()

require(Matrix)
ii <- unique(dd$device_id)
jj <- unique(dd$feature_name)
id_i <- match(dd$device_id,ii)
id_j <- match(dd$feature_name,jj)
id_ij <- cbind(id_i,id_j)
M <- Matrix(0,nrow=length(ii),ncol=length(jj),
            dimnames=list(ii,jj),sparse=T)
M[id_ij] <- 1
rm(ii,jj,id_i,id_j,id_ij,dd);gc()

x <- M[rownames(M) %in% label1$device_id,]
id <- label1$device_id[match(rownames(x),label1$device_id)]
y <- label1$group[match(rownames(x),label1$device_id)]
rm(M,label1)

# level reduction
x_train <- x[!is.na(y),]
tmp_cnt_train <- colSums(x_train)
x <- x[,tmp_cnt_train>0 & tmp_cnt_train<nrow(x_train)]
rm(x_train,tmp_cnt_train)

require(xgboost)
(group_name <- na.omit(unique(y)))
idx_train <- which(!is.na(y))
idx_test <- which(is.na(y))
train_data <- x[idx_train,]
test_data <- x[idx_test,]
train_label <- match(y[idx_train],group_name)-1
test_label <- match(y[idx_test],group_name)-1
dtrain <- xgb.DMatrix(train_data,label=train_label,missing=NA)
dtest <- xgb.DMatrix(test_data,label=test_label,missing=NA)

param <- list(booster="gblinear",
              num_class=length(group_name),
              objective="multi:softprob",
              eval_metric="mlogloss",
              eta=0.01,
              lambda=5,
              lambda_bias=0,
              alpha=2)
watchlist <- list(train=dtrain)
#set.seed(114)
#fit_cv <- xgb.cv(params=param,
#                  data=dtrain,
#                  nrounds=100000,
#                  watchlist=watchlist,
#                  nfold=5,
#                  early.stop.round=3,
#                  verbose=1)

ntree <- 280
set.seed(114)
fit_xgb <- xgb.train(params=param,
                     data=dtrain,
                     nrounds=ntree,
                     watchlist=watchlist,
                     verbose=1)
pred <- predict(fit_xgb,dtest)
pred_detail <- t(matrix(pred,nrow=length(group_name)))
res_submit <- cbind(id=id[idx_test],as.data.frame(pred_detail))
colnames(res_submit) <- c("device_id",group_name)
write.csv(res_submit,file="submit2.csv",row.names=F,quote=F)