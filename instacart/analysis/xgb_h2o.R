library(data.table)
library(h2o)

h2o.init(nthreads = 15, max_mem_size = "16g")

print("Load datasets")
ais <- fread("../input/aisles.csv", key = "aisle_id")
dept <- fread("../input/departments.csv", key = "department_id")
prod <- fread("../input/products.csv", key = c("product_id","aisle_id", "department_id"))
opp <- fread("../input/order_products__prior.csv")
opt <- fread("../input/order_products__train.csv")
ord <- fread("../input/orders.csv")

print("Get product department and aisle names")
prod <- merge(prod, ais, by="aisle_id", all.x=TRUE, sort=FALSE)
prod <- merge(prod, dept, by="department_id", all.x=TRUE, sort=FALSE)

# For the prior orders get the associated product, aisle, departments, and users
opp <- merge(opp, prod, by="product_id", all.x=TRUE, sort=FALSE)
opp <- merge(opp, ord, by="order_id", all.x=TRUE, sort=FALSE)

opp[,":="(orders_ago=max(order_number) - order_number + 1), by=user_id]
# For each user get list of all prior products purchased
user_prod_list <- opp[ ,.(last_order_number=max(order_number),
                          purch_count=.N), keyby=.(user_id, product_id)]
print("Create a few simple features")
user_summ <- opp[,.(user_total_products_ordered_hist=.N,
                    uniq_prod=uniqueN(product_name),
                    uniq_aisle=uniqueN(aisle),
                    uniq_dept=uniqueN(department),
                    prior_orders=max(order_number)), 
                 by=user_id]
user_prior_prod_cnt <- opp[,.(prior_prod_cnt=.N,
                              last_purchased_orders_ago=min(orders_ago),
                              first_purchased_orders_ago=max(orders_ago)),
                           by=.(user_id, product_id)]

print("Merge datasets to create training frame")
opt_user <- merge(opt[reordered==1,.(order_id, product_id)], ord[,.(order_id, user_id)], by="order_id", all.x=TRUE, sort=FALSE)
dt_expanded  <- merge(user_prod_list[user_id %in% opt_user[["user_id"]],.(user_id, product_id)], opt_user, by=c("user_id", "product_id"), all.x=TRUE, sort=FALSE)
dt_expanded[,curr_prod_purchased:=ifelse(!is.na(order_id), 1, 0)]

train <- merge(dt_expanded, user_summ, by="user_id", all.x=TRUE, sort=FALSE)
train <- merge(train, user_prior_prod_cnt, by=c("user_id", "product_id"), all.x=TRUE, sort=FALSE)
varnames <- setdiff(colnames(train), c("user_id","order_id","curr_prod_purchased"))

print("Create the test frame")
test_orders <- ord[eval_set=="test"]
optest_user <- merge(test_orders[,.(order_id)], ord[,.(order_id, user_id)], by="order_id", all.x=TRUE, sort=FALSE)
dt_expanded  <- merge(user_prod_list[user_id %in% optest_user[["user_id"]],.(user_id, product_id)], optest_user, by=c("user_id"), all.x=TRUE, sort=FALSE)
# There is currently a bug which prevents predictions when the response is missing in the test set, so generating fake responses. This won't affect the predictions
dt_expanded[,curr_prod_purchased:=sample(c(0,1), nrow(dt_expanded), replace=TRUE)] 

test <- merge(dt_expanded, user_summ, by="user_id", all.x=TRUE, sort=FALSE)
test <- merge(test, user_prior_prod_cnt, by=c("user_id", "product_id"), all.x=TRUE, sort=FALSE)

print("Sample users for the validation set")
set.seed(200)
val_users <- sample(unique(train$user_id), size = 10000, replace = FALSE)

print("Convert response to factor")
train[,curr_prod_purchased:=as.factor(curr_prod_purchased)]
test[,curr_prod_purchased:=as.factor(curr_prod_purchased)]

print("Add data to H2O")
train.hex <- as.h2o(train[!user_id %in% val_users,c("curr_prod_purchased", varnames),with=FALSE], destination_frame = "train.hex")
val.hex <- as.h2o(train[user_id %in% val_users,c("curr_prod_purchased", varnames),with=FALSE], destination_frame = "val.hex")

print("Free up some memory")
rm(train, opp, opt, ord, prod, dept, ais, user_prod_list, user_summ);gc()

print("Train xgboost model")
xgb <- h2o.xgboost(x = varnames
                   ,y = "curr_prod_purchased"
                   ,training_frame = train.hex
                   ,validation_frame = val.hex
                   ,model_id = "xgb_model_1"
                   ,stopping_rounds = 3
                   ,stopping_metric = "logloss"
                   ,distribution = "bernoulli"
                   ,score_tree_interval = 1
                   ,learn_rate=0.1
                   ,ntrees=500
                   ,subsample = 0.75
                   ,colsample_bytree = 0.75
                   ,tree_method = "hist"
                   ,grow_policy = "lossguide"
                   ,booster = "gbtree"
                   ,gamma = 0.0
)

print("Make predictions")
test.hex <- as.h2o(test[,c("curr_prod_purchased", varnames),with=FALSE], destination_frame = "test.hex")

sPreds <- as.data.table(h2o.predict(xgb, test.hex))
sPreds <- data.table(order_id=test$order_id, product_id=test$product_id, testPreds=sPreds$C3)
testPreds <- sPreds[,.(products=paste0(product_id[testPreds>0.21], collapse=" ")), by=order_id]
set(testPreds, which(testPreds[["products"]]==""), "products", "None")
print("Create submission file")
fwrite(testPreds, "submission.csv")