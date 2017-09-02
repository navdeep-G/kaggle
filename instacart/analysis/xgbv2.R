library(data.table)
library(xgboost)

print("Mean F1 Score for use with XGBoost")
xgb_eval_f1 <- function (yhat, dtrain) {
  require(ModelMetrics)
  y = getinfo(dtrain, "label")
  dt <- data.table(user_id=train[user_id %in% val_users, user_id], purch=y, pred=yhat)
  f1 <- mean(dt[,.(f1score=f1Score(purch, pred, cutoff=0.2)), by=user_id]$f1score)
  return (list(metric = "f1", value = f1))
}

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

print("For the prior orders get the associated product, aisle, departments, and users")
opp <- merge(opp, prod, by="product_id", all.x=TRUE, sort=FALSE)
opp <- merge(opp, ord, by="order_id", all.x=TRUE, sort=FALSE)

opp[,":="(orders_ago=max(order_number) - order_number + 1), by=user_id]
print("For each user get list of all prior products purchased")
user_prod_list <- opp[ ,.(last_order_number=max(order_number),
                          purch_count=.N), keyby=.(user_id, product_id)]
print("A few features")
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

print("Sample users for the validation set")
set.seed(200)
val_users <- sample(unique(train$user_id), size = 10000, replace = FALSE)

dtrain <- xgb.DMatrix(data=data.matrix(train[!user_id %in% val_users,varnames,with=FALSE]), label=train[!user_id %in% val_users, curr_prod_purchased])
dval <- xgb.DMatrix(data=data.matrix(train[user_id %in% val_users,varnames,with=FALSE]), label=train[user_id %in% val_users, curr_prod_purchased])
watchlist <- list(dval=dval)

params <- list(booster="gbtree"
               ,objective="reg:logistic"
               ,eval_metric=xgb_eval_f1
               ,eta=0.1
               ,gamma=0
               ,max_depth=5
               ,subsample=1
               ,colsample_bytree=1
               ,base_score=0.2
               ,nthread=8
)

print("Train XGBoost")
set.seed(3000)
xgb1 <- xgb.train(params = params,
                  data = dtrain,
                  nrounds = 10,
                  watchlist = watchlist,
                  maximize = TRUE,
                  print_every_n = 1)

# Apply model
test <- fread("test.csv")
test <- as.data.frame(test)
test$user_id <- NULL
test$prior_orders <- NULL
test$last_purchased_orders_ago <- NULL
test$first_purchased_orders_ago <- NULL
test[] <- lapply(test, as.numeric)
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
test$reordered <- predict(xgb1, X)

test$reordered <- (test$reordered > 0.21) * 1

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = "submit.csv", row.names = F)

