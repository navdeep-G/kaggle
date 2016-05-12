#Prudential EDA

library(ggplot2)
library(gridExtra)

#Read in data
train <- read.table("../../data/train.csv", sep=",", header=TRUE)
test <- read.table("../../data/test.csv", sep=",", header=TRUE)

#Separate categorical, continous and discrete variables
cat.var.names <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
                   paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
                   "Family_Hist_1", paste("Medical_History_", c(2:14, 16:23, 25:31, 33:41), sep=""))
cont.var.names <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
                    "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
                    "Family_Hist_5")
disc.var.names <- c("Medical_History_1", "Medical_History_15", "Medical_History_24", "Medical_History_32", 
                    paste("Medical_Keyword_", 1:48, sep=""))

train.cat <- train[, cat.var.names]
test.cat <- test[, cat.var.names]

train.cont <- train[, cont.var.names]
test.cont <- test[, cont.var.names]

train.disc <- train[, disc.var.names]
test.disc <- test[, disc.var.names]

train.cat <- as.data.frame(lapply(train.cat, factor))
test.cat <- as.data.frame(lapply(test.cat, factor))

#Look at structure of continous, categorical, and discrete vars in the train set
str(train.cont)
str(train.cat)
str(train.disc)

#Look at structure of continous, categorical, and discrete vars in the test set
str(test.cont)
str(test.cat)
str(test.disc)

#Look at summaries of train set
summary(train.cont)
summary(train.disc)
summary(train.cat)

#Look at summaries of test set
summary(test.cont)
summary(test.disc)
summary(test.cat)

#Dimensions of train set
cat("Train data has", nrow(train), "rows and", ncol(train), "columns \n")

#Dimensions of test set
cat("Test data has", nrow(test), "rows and", ncol(test), "columns \n")

#Missing data in train
sum(is.na(train)) / (nrow(train) * ncol(train))

#Missing data in test
sum(is.na(test)) / (nrow(test) * ncol(test))

#Missing data on a column by column basis for the train set
apply(train, 2, function(x) { sum(is.na(x)) })

#Missing data on a column by column basis for the test set
apply(test, 2, function(x) { sum(is.na(x)) })

#Can we see any different missing data structure depending on the response?
train.na.per.response <- sapply(sort(unique(train$Response)), function(x) { apply(train[train$Response == x, ], 2, function(y) { sum(is.na(y)) }) })
train.na.per.response

#Data with response equal 8 has the most and response equal 3 the least missing data.
round(colSums(train.na.per.response) / sum(train.na.per.response), digits=4)

#Plot the missingness structure (only for those features with missing data) where the feature with 
#the most missing data is on top and with the least missing data on bottom.
plotMissingnessStr <- function(data.in, title=NULL) {
  r <- as.data.frame(ifelse(is.na(data.in), 0, 1))
  r <- r[,order(colMeans(is.na(data.in)))]
  dat.tmp <- expand.grid(list(x=1:nrow(r), y=colnames(r)))
  dat.tmp$r <- as.vector(t(r))
  
  ggplot(dat.tmp) + geom_tile(aes(x=x, y=y, fill=factor(r))) + scale_fill_manual(values=c("black", "white"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

#Train set
plotMissingnessStr(data.in=train[,apply(train, 2, function(x) { sum(is.na(x)) > 0 })], title="Train data set")

#Test set
plotMissingnessStr(data.in=test[,apply(test, 2, function(x) { sum(is.na(x)) > 0 })], title="Test data set")

#Check for duplicate rows in train set
cat("Train data set - Number of duplicated rows:", nrow(train) - nrow(unique(train)), "\n")

#Check for duplicate rows in test set
cat("Test data set - Number of duplicated rows:", nrow(test) - nrow(unique(test)), "\n")

#Check for constant columns
train.const <- sapply(train, function(x) { length(unique(x)) == 1 })
test.const <- sapply(test, function(x) { length(unique(x)) == 1 })
cat("Train data set - Number of constant columns:", sum(train.const), "\n")
cat("Test data set - Number of constant columns:", sum(test.const), "\n")

#Plot histograms of categorical variables
plotHist <- function(data.in, i) {
  data <- data.frame(x=data.in[,i])
  p <- ggplot(data=data, aes(x=factor(x))) + geom_histogram() + xlab(colnames(data.in)[i]) + theme_light() + 
    theme(axis.text.x=element_text(size=8))
  return (p)
}

doPlots <- function(data.in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data.in=data.in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

doPlots(data.in=train.cat, fun=plotHist, ii=1:4, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=5:8, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=9:12, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=13:16, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=17:20, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=21:24, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=25:28, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=29:32, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=33:36, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=37:40, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=41:44, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=45:48, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=49:52, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=53:56, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=57:60, ncol=2)
doPlots(data.in=train.cat, fun=plotHist, ii=61, ncol=2)

#Densities of continous Features
train.cont <- data.frame(train.cont, Response=train$Response)
plotDensity <- function(data.in, i) {
  data <- data.frame(x=data.in[,i], Response=data.in$Response)
  p <- ggplot(data) + #geom_density(aes(x=x, colour=factor(Response))) + 
    geom_line(aes(x=x), stat="density", size=1, alpha=1.0) +
    xlab(colnames(data.in)[i]) + theme_light()
  return (p)
}

doPlots(data.in=train.cont, fun=plotDensity, ii=1:4, ncol=2)
doPlots(data.in=train.cont, fun=plotDensity, ii=5:8, ncol=2)
doPlots(data.in=train.cont, fun=plotDensity, ii=9:12, ncol=2)
doPlots(data.in=train.cont, fun=plotDensity, ii=13, ncol=2)

#Boxplots of continous Features depending on Response
plotBox <- function(data.in, i) {
  data <- data.frame(y=data.in[,i], Response=data.in$Response)
  p <- ggplot(data, aes(x=factor(Response), y=y)) + geom_boxplot() + ylab(colnames(data.in)[i]) + theme_light()
  return (p)
}

doPlots(data.in=train.cont, fun=plotBox, ii=1:4, ncol=2)
doPlots(data.in=train.cont, fun=plotBox, ii=5:8, ncol=2)
doPlots(data.in=train.cont, fun=plotBox, ii=9:12, ncol=2)
doPlots(data.in=train.cont, fun=plotBox, ii=13, ncol=2)

#Take a look at the response
ggplot(train) + geom_histogram(aes(factor(Response))) + xlab("Response") + theme_light()
