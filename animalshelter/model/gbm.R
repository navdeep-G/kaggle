library(lubridate)
library(gbm)

read.csv("../input/train.csv") -> train
read.csv("../input/test.csv") -> test

popularBreeds <- names(summary(train$Breed,maxsum=10L))
trainNameSummary <- summary(train$Name,maxsum=Inf)

clean <- function(x){
  x$Hour <- hour(x$DateTime)
  x$Weekday <- wday(x$DateTime)
  x$DateTime <- as.numeric(as.POSIXct(x$DateTime))
  x$OutcomeSubtype <- NULL
  x$NameLen <- nchar(as.character(x$Name))
  x$NameWeirdness <- trainNameSummary[match(x$Name,names(trainNameSummary))]
  x$Name <- NULL
  x$AgeuponOutcome <- gsub(" years?","0000",x$AgeuponOutcome)
  x$AgeuponOutcome <- gsub(" months?","00",x$AgeuponOutcome)
  x$AgeuponOutcome <- gsub(" weeks?","0",x$AgeuponOutcome)
  x$AgeuponOutcome <- gsub(" days?","",x$AgeuponOutcome)
  x$AgeuponOutcome <- as.numeric(paste0("0",x$AgeuponOutcome))
  x$AnimalID <- NULL
  for(i in c("Black","White","Brown","Blue","Orange","Calico","Chocolate","Gold","Red","Tan","Tortie","Yellow")) x[[paste0("col.",i)]] <- grepl(i,x$Color)
  x$Color <- NULL
  for(i in popularBreeds) x[[paste0("breed.",make.names(i))]] <- x$Breed == i
  x$Breed <- NULL
  x
}

train <- clean(train)
test <- clean(test)

for(i in names(train)) if(is.logical(train[[i]])) {train[[i]] <- as.numeric(train[[i]]); test[[i]] <- as.numeric(test[[i]])}

set.seed(20160324L)

gbm1 <- gbm(
  OutcomeType ~ .,
  data=train,
  distribution="multinomial",
  shrinkage=0.05,
  n.trees=500,
  interaction.depth=6L,
  train.fraction=0.8,
  keep.data=FALSE,
  verbose=TRUE
)

testPreds2 <- predict(gbm1,test,type="response")
dim(testPreds2) <- c(nrow(test),5)
colnames(testPreds2) <- levels(train$OutcomeType)

options(scipen=100)

ss <- data.frame(ID=test$ID)
ss <- cbind(ss,testPreds2)

write.csv(ss,file="gbm_one.csv",row.names=FALSE)