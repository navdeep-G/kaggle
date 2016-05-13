#Read in data using readr
#train = read_csv("/home/scry/Desktop/Kaggle_SpringLeaf/train.csv")
#test = read_csv("/home/scry/Desktop/Kaggle_SpringLeaf/test.csv")
train.unique.count=lapply(train, function(x) length(unique(x)))
train.unique.count_1=unlist(train.unique.count[unlist(train.unique.count)==1])
train.unique.count_2=unlist(train.unique.count[unlist(train.unique.count)==2])
train.unique.count_2=train.unique.count_2[-which(names(train.unique.count_2)=='target')]

delete_const=names(train.unique.count_1)
delete_NA56=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==145175))
delete_NA89=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==145142))
delete_NA918=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==144313))

#VARS to delete
#safe to remove VARS with 56, 89 and 918 NA's as they are covered by other VARS
print(length(c(delete_const,delete_NA56,delete_NA89,delete_NA918)))

train=train[,!(names(train) %in% c(delete_const,delete_NA56,delete_NA89,delete_NA918))]
test=test[,!(names(test) %in% c(delete_const,delete_NA56,delete_NA89,delete_NA918))]

print(dim(train))
print(dim(test))

# From manual data analysis
datecolumns = c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", "VAR_0159", "VAR_0166", "VAR_0167", "VAR_0168", "VAR_0176", "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217")

train_cropped <- train[datecolumns]
train_cc <- data.frame(apply(train_cropped, 2, function(x) as.double(strptime(x, format='%d%b%y:%H:%M:%S', tz="UTC")))) #2 = columnwise

for (dc in datecolumns){
  train[dc] <- NULL
  train[dc] <- train_cc[dc]
}

train_cc <- NULL
train_cropped <- NULL
gc()

test_cropped <- test[datecolumns]
test_cc <- data.frame(apply(test_cropped, 2, function(x) as.double(strptime(x, format='%d%b%y:%H:%M:%S', tz="UTC")))) #2 = columnwise

for (dc in datecolumns){
  test[dc] <- NULL
  test[dc] <- test_cc[dc]
}

test_cc <- NULL
test_cropped <- NULL
gc()


# safe target and put it at the end again
train_target <- train$target
train$target <- NULL
train$target <- train_target

#Exclude data variables for now
train=train[,!(names(train) %in% c("VAR_0073", "VAR_0075", "VAR_0156", 
                                   "VAR_0157", "VAR_0158", "VAR_0159", "VAR_0166", 
                                   "VAR_0167", "VAR_0168", "VAR_0176", "VAR_0177", 
                                   "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217"))]
test=test[,!(names(test) %in% c("VAR_0073", "VAR_0075", "VAR_0156", 
                                "VAR_0157", "VAR_0158", "VAR_0159", "VAR_0166", 
                                "VAR_0167", "VAR_0168", "VAR_0176", "VAR_0177", 
                                "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217"))]

feature.names <- names(train)[2:ncol(train)-1]
feature.names = feature.names[!feature.names %in% c("ID")]


for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

cat("replacing missing values with -1\n")
train[is.na(train)] <- -1
test[is.na(test)]   <- -1

#Bind together newly set up train_char and train_numr and make into h2o frame
#Columns with only one unique value
col_ct = sapply(train, function(x) length(unique(x)))
cat("Constant feature count:", length(col_ct[col_ct==1]),"columns with one unique value: ",names(col_ct[col_ct==1]))

#Lets take a look at the columns with only one unique value as these probably don't give much info to predition as there is no variability:
names(col_ct[col_ct==1])
train_unique_one = train[, names(train) %in% names(col_ct[col_ct==1])]  

#Can we remove the previous variables from the train set?
train = train[, !names(train) %in% names(col_ct[col_ct==1])]

#Convert to h2o frame
train = as.h2o(localH2O,train,destination_frame="train")
