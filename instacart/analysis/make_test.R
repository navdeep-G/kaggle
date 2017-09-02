library(data.table)
library(xgboost)

print("Load datasets")
ais <- fread("../input/aisles.csv", key = "aisle_id")
dept <- fread("../input/departments.csv", key = "department_id")
prod <- fread("../input/products.csv", key = c("product_id","aisle_id", "department_id"))
opp <- fread("../input/order_products__prior.csv")
opt <- fread("../input/order_products__train.csv")
ord <- fread("../input/orders.csv")
ord <- subset(ord,eval_set == "test")

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

test <- merge(dt_expanded, user_summ, by="user_id", all.x=TRUE, sort=FALSE)
test <- merge(test, user_prior_prod_cnt, by=c("user_id", "product_id"), all.x=TRUE, sort=FALSE)
fwrite(test,"./test.csv")
