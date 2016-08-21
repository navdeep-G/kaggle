# Scores 0.50758 on Public leaderboard

#Load data.table for fast reads and aggreagtions
library(data.table)

# Input data files are available in the "../input/" directory.

# Read in only required columns and force to numeric to ensure that subsequent 
# aggregation when calculating medians works
train <- fread('../data/train.csv', 
               select = c('Cliente_ID', 'Producto_ID', 'Demanda_uni_equil'),
               colClasses=c(Cliente_ID="numeric",Producto_ID="numeric",Demanda_uni_equil="numeric"))

#Print first 6 rows and show that conversion from integer has been successful
head(train)
sapply(train, class)


# In case the pair of product/client pair has a median available, we use that as the predicted value.
# If not, then the product median is checked. If a value is not found, then the global median is used.

# set a table key to enable fast aggregations
setkey(train, Producto_ID, Cliente_ID)

#calculate the overall median
median <- train[, median(Demanda_uni_equil)]

#calculate the product overall median; call it M2
median_Prod <- train[, median(Demanda_uni_equil), by = Producto_ID]
setnames(median_Prod,"V1","M2")

#calculate the client and product  median; call it M3
median_Client_Prod <- train[, median(Demanda_uni_equil),by = .(Producto_ID,Cliente_ID)]
setnames(median_Client_Prod,"V1","M3")

# That's the 'modeling' done now need to apply scoring to test set

# Read in Test data 
# Read in only required columns and force to numeric

test <- fread('../data/test.csv', 
              select = c('id','Cliente_ID', 'Producto_ID'),
              colClasses=c(Cliente_ID="numeric",Producto_ID="numeric"))


#Print first 6 rows and show that conversion from integer has been successful
head(test)
sapply(test, class)

# set a table key to enable fast joins to predictions
setkey(test, Producto_ID, Cliente_ID)

# Create table called submit that joins medians (in field M3) by Product and Client to test data set
submit <- merge(test, median_Client_Prod, all.x = TRUE)

# add column M2 that contains median by Product
submit$M2 <- merge(test, median_Prod, by = "Producto_ID", all.x = TRUE)$M2


# Now create Predictions column; intially set to be M3 which contains median by product and client
submit$Pred <- submit$M3

# where median by product and client is null use median by product (M2)
submit[is.na(M3)]$Pred <- submit[is.na(M3)]$M2

# where median by product is null use overall median
submit[is.na(Pred)]$Pred <- median

# now relabel columns ready for creating submission
setnames(submit,"Pred","Demanda_uni_equil")

# check all looks OK
head(submit)

# Write out submission file.
# Any results you write to the current directory are saved as output.
write.csv(submit[,.(id,Demanda_uni_equil)],"submit.csv", row.names = FALSE)