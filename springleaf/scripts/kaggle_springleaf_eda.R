#Might need readr,ggplot2,gridExtra,maps, and maptools
library(readr)
library(ggplot2)
library(gridExtra)
library(maps)
library(maptools)
library(dplyr)
library(stringdist)

#Bulk of EDA below:
#Had some trouble dealing with native R functions with H2o Frame. So, switching to data frame for now. 
#Surpisingly fast using read_csv from readr rather than converting from h2o frame.
train = read_csv("/Users/navdeepgill/Desktop/ToNewMac/Kaggle/SpringLeaf/train.csv")
test = read_csv("/Users/navdeepgill/Desktop/ToNewMac/Kaggle/SpringLeaf/test.csv")

#Get proportion of NA's in training dataset
length(train[is.na(train)])/(ncol(train)*nrow(train)) 

#About .006 % NA

#Columns with only one unique value
col_ct = sapply(train, function(x) length(unique(x)))
cat("Constant feature count:", length(col_ct[col_ct==1]),"columns with one unique value: ",names(col_ct[col_ct==1]))

#Lets take a look at the columns with only one unique value as these probably don't give much info to predition as there is no variability:
names(col_ct[col_ct==1])
train_unique_one = train[, names(train) %in% names(col_ct[col_ct==1])]  

#Can we remove the previous variables from the train set?
train = train[, !names(train) %in% names(col_ct[col_ct==1])]

#Clean up city names

cat("Nr of city names before cleanup:", length(unique(train$VAR_0200)), fill=T)

reviewDupes <- mutate(train, City = VAR_0200, State = VAR_0237, Zip=VAR_0241) %>% 
  select(City, State, Zip) %>%
  mutate(stateZip = paste(Zip, State, sep="_"),
         fullGeoID = paste(City, Zip, State, sep="_")) %>%
  distinct()

potentialDupes <- group_by(reviewDupes, stateZip) %>% 
  dplyr::summarise(n = n(), 
                   altName = first(City), # prettier: most common
                   altID = first(fullGeoID)) %>% 
  filter(n > 1)
dupes <- mutate(left_join(potentialDupes, reviewDupes, by="stateZip"), 
                dist=stringdist(altName, City)) %>% 
  filter(dist >= 1 & dist <= 2)

print("Preview:")
print(head(paste(dupes$City, dupes$State, "=>", dupes$altName), 20))

train <- mutate(train, fullGeoID = paste(VAR_0200, VAR_0241, VAR_0237, sep="_"))
train <- left_join(train, select(dupes, altName, fullGeoID), by="fullGeoID") %>%
  mutate(VAR_0200 = ifelse(is.na(altName), VAR_0200, altName)) %>%
  select(-fullGeoID, -altName)
# and do the same for the test set

cat("Nr of city names after cleansing:", length(unique(train$VAR_0200)), fill=T)

#Check what columns are numerics and characters
train_numr = train[, sapply(train, is.numeric)]
train_char = train[, sapply(train, is.character)]
cat("Numerical column count : ", dim(train_numr)[2], 
    "; Character column count : ", dim(train_char)[2])

#Look at character values
str(lapply(train_char, unique), vec.len = 4)

#Looks like several date values, some lettered categorical variables, boolean variables, state variables, city variables, and  two occupation type 
#variables. 

#Looks like NA values are represented by -1,"". and []. Thinks its best to make them NA to keep things constant.
train_char[train_char==-1] = NA
train_char[train_char==""] = NA
train_char[train_char=="[]"] = NA

#In depth look at the date columns
train_date = train_char[,grep("JAN1|FEB1|MAR1", train_char),]
train_char = train_char[, !colnames(train_char) %in% colnames(train_date)]
train_date = sapply(train_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
train_date = do.call(cbind.data.frame, train_date)

#Go through each date field to narrow down which ones are useful by how many NA's there are:
print("Looking at NA's per Date field...")
for(i in names(train_date)){
  cat("Var:",i, " => Number of NA:",sum(is.na(train_date[,i]))," => Proporition of NA:", sum(is.na(train_date[,i]))/nrow(train_date),"\n")
}

#Seems like a huge percentage of NA for most (>=.69) except VAR_0075,VAR_0204, & VAR_0217

#Subset train_date to only inlcude variables with the above threee
train_date_subset = subset(train_date,select=c("VAR_0075","VAR_0204","VAR_0217"))

#Seperate out time variables
train_time = train_date[,colnames(train_date) %in% c("VAR_0204","VAR_0217")]
train_time = data.frame(sapply(train_time, function(x) strftime(x, "%H:%M:%S")))
train_hour = as.data.frame(sapply(train_time, function(x) as.numeric(as.character(substr( x ,1, 2)))))

#Plot dates
par(mar=c(2,2,2,2),mfrow=c(4,4))
for(i in 1:3) hist(train_date_subset[,i], "weeks", format = "%d %b %y", main = colnames(train_date_subset)[i], xlab="", ylab="")

#VAR_0204 seems off, lets take a closer look
table(train_date_subset$VAR_0204)

#Date range is from 01/29-02/01. So limited data. 

#Plot times
par(mar=c(2,2,2,2),mfrow=c(1,2))
for(i in 1:2) hist(train_hour[,i], main = paste(colnames(train_hour)[i], "hourly"), breaks = c(0:24), xlab="", ylab="")

#VAR_0204 seems to show most activity at 12AM and from 3PM-8PM. VAR_0217 is strictly around 2AM

#Breakdown of state features
mapUSA <- map('state', fill=TRUE, plot=FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))

mapStates = function(df, feat){
  dat = data.frame(table(df[,feat]))
  names(dat) = c("state.abb", "value")
  dat$states <- tolower(state.name[match(dat$state.abb,  state.abb)])
  
  
  idx <- match(unique(nms),  dat$states)
  dat2 <- data.frame(value = dat$value[idx], state = unique(nms))
  row.names(dat2) <- unique(nms) 
  USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
  spplot(USAsp['value'], main=paste(feat, "value count"), col.regions=rev(heat.colors(21)))
}
grid.arrange(mapStates(train_char, "VAR_0274"), mapStates(train_char, "VAR_0237"),ncol=2)

#Unique values per feature
num_ct = sapply(train_numr, function(x) length(unique(x)))
char_ct = sapply(train_char, function(x) length(unique(x)))
date_ct = sapply(train_date, function(x) length(unique(x)))
all_ct = rbind(data.frame(count=num_ct, type="Numerical"), 
               data.frame(count=char_ct, type="Character"), 
               data.frame(count=date_ct, type="Date"))

g1 = ggplot(all_ct, aes(x = count, fill=type)) + 
  geom_histogram(binwidth = 1, alpha=0.7, position="identity") + 
  xlab("Unique values per feature (0-100)")+ theme(legend.position = "none") + 
  xlim(c(0,100)) +theme(axis.title.x=element_text(size=14, ,face="bold"))
g2 = ggplot(all_ct, aes(x = count, fill=type)) +  
  geom_histogram(binwidth = 100, alpha=0.7, position="identity") + 
  xlab("Unique values per feature(101+)")  + xlim(c(101,nrow(train))) +
  theme(axis.title.x=element_text(size=14, ,face="bold"))
grid.arrange(g1, g2, ncol=2)

#Lets look at the number of NA’s per feature type (Numeric, Character or String).
num_na = sapply(train_numr, function(x) sum(is.na(x)))
char_na = sapply(train_char, function(x) sum(is.na(x)))
date_na = sapply(train_date, function(x) sum(is.na(x)))
all_na = rbind(data.frame(count=num_na, type="Numerical"), 
               data.frame(count=char_na, type="Character"), 
               data.frame(count=date_na, type="Date"))
#table(all_na)
all_na = data.frame(all_na)
all_na = all_na[all_na$count>0,]

breaks <- c(5,10,50,100,500,1000,2000)

ggplot(all_na, aes(x = count, fill=type)) +  
  geom_histogram(alpha=0.7) + 
  #  scale_y_log10(limits=c(1,2000), breaks=breaks) + 
  scale_x_log10(limits=c(1,20000), breaks=c(breaks,5000,10000,20000)) + 
  labs(title="Histogram of feature count per NA count", size=24, face="bold") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  xlab("NA Count") + ylab("Feature Count")

#Look at numeric values
str(lapply(train_numr, unique), vec.len = 4)

#We shall impute -99999999 to the missing values and check for non-unique columns
#train_numr[is.na(train_numr)] = -99999999
length(colnames(train_numr[,sapply(train_numr, function(v) var(v, na.rm=TRUE)==0)]))

#Go through each numeric field and see which ones display a MV, i.e., 99999999,99999998,etc.
print("Looking at NA's per Numeric field...")
num_vec <- c("")
j=1
for(i in names(train_numr)){
    num_var = paste0("Var:",i, " => Number of NA:",sum((train_numr[,i])>=999999990)," => Proporition of NA:", sum((train_numr[,i])>=999999990)/nrow(train_numr))
    j = j+1
    num_vec[j] = num_var
}

num_df <- data.frame(Var=character(),
                      CountMV = as.numeric(), 
                 stringsAsFactors=FALSE) 
#Get numeric variables that have MV rate >.9
for(i in names(train_numr)){
  num_df[i,"Var"] = i
  num_df[i,"CountMV"] = sum((train_numr[,i])>=999999990)
}
num_df = subset(num_df,!is.na(CountMV))
num_df$propMv = num_df$CountMV/nrow(train_numr)
num_df = num_df[with(num_df, order(-propMv)), ]

num_bin <- cut(num_df$propMv, breaks = 10)
table(num_bin)

#Get columsn who have MV greater than or equal to .09
num_mv = subset(num_df,propMv>=.0999)

num_mv_names = c(num_mv$Var)

#Remove numeric vars with high amount of mv's from train_numr
train_numr = train_numr[, !names(train_numr) %in% num_mv_names]
#Get hour vars
colnames(train_hour) = c("hr204","hr217")
#Bind together train_char,train_numr, and train_date_subset
train= cbind(train_numr,train_char,train_date_subset,train_hour)

#Remove variables with high counts of NA
train.unique.count=lapply(train, function(x) length(unique(x)))
train.unique.count_1=unlist(train.unique.count[unlist(train.unique.count)==1])
train.unique.count_2=unlist(train.unique.count[unlist(train.unique.count)==2])
train.unique.count_2=train.unique.count_2[-which(names(train.unique.count_2)=='target')]

delete_const=names(train.unique.count_1)
#delete_NA56=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==145175))
delete_NA89=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==145142))
delete_NA918=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==144313))

#VARS to delete
#safe to remove VARS with 56, 89 and 918 NA's as they are covered by other VARS
print(length(c(delete_const,delete_NA89,delete_NA918)))

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
train=train[,!(names(train) %in% c("VAR_0156", 
                                   "VAR_0157", "VAR_0158", "VAR_0159", "VAR_0166", 
                                   "VAR_0167", "VAR_0168", "VAR_0176", "VAR_0177", 
                                   "VAR_0178", "VAR_0179"))]
test=test[,!(names(test) %in% c("VAR_0156", 
                                "VAR_0157", "VAR_0158", "VAR_0159", "VAR_0166", 
                                "VAR_0167", "VAR_0168", "VAR_0176", "VAR_0177", 
                                "VAR_0178", "VAR_0179"))]

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

#Make target variable a factor
train$target = as.factor(train$target)

#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Looking at response (target) by a few variables of interest:
nrow(train[train$target == 1, ]) #33773 rows

#Look at categorical data by target to see distribution of response
#Any binning we can do?

#Subset data to get values of interest, i.e., where target==1
train.hit = subset(train,target==1)

#Look at character vars again:
str(lapply(train_char, unique), vec.len = 4)

#Function to look character variable distn among train.hit
char_bin = function(var){
  #Break up by state variable 0237
  train.hit.state.237 = train.hit %>%
    group_by(train.hit[,var]) %>%
    summarise(n= n())
  arrange(train.hit.state.237,-n)
  
  train.hit.state.237 = as.data.frame(train.hit.state.237)
  colnames(train.hit.state.237) = c("Var","n")
  train.hit.state.237 = train.hit.state.237[with(train.hit.state.237, order(-n)), ]
  train.hit.state.237$Var = as.factor(train.hit.state.237$Var)
  
  #Sort for ggplot2
  train.hit.state.237 <- train.hit.state.237[with(train.hit.state.237, order(-n)), ]
  train.hit.state.237$Var <- ordered(train.hit.state.237$Var, levels=levels(train.hit.state.237$Var)[unclass(train.hit.state.237$Var)])
  plot.state = ggplot(data=train.hit.state.237, aes(x=Var, y=n),fill=factor(Var)) +
    geom_bar(stat="identity")
  
  plot.state + geom_bar() + coord_flip()
}

#Add some month & day values
train$day75 = as.factor(weekdays(train$VAR_0075))
train$day204 = as.factor(weekdays(train$VAR_0204))
train$day217 = as.factor(weekdays(train$VAR_0217))
train$month75 = as.factor(months(train$VAR_0075))
train$month204 = as.factor(months(train$VAR_0204))
train$month217 = as.factor(months(train$VAR_0217))

#Do not need data variables as the previous cover this
train = train[, !names(train) %in% c("VAR_0075","VAR_0204","VAR_0217")]
#Re assign feature.names
feature.names <- names(train)[2:ncol(train)-1]
feature.names = feature.names[!feature.names %in% c("ID","target")]


for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}
