#More EDA with less plotting and more data.table functionality

library(data.table)

print("Reading data")
train <- fread('../../data/train.csv')
test <- fread('../../data/test.csv')

#Data field definitions

#Semana — Week number (From Thursday to Wednesday)
#Agencia_ID — Sales Depot ID
#Canal_ID — Sales Channel ID
#Ruta_SAK — Route ID (Several routes = Sales Depot)
#Cliente_ID — Client ID
#NombreCliente — Client name
#Producto_ID — Product ID
#NombreProducto — Product Name
#Venta_uni_hoy — Sales unit this week (integer)
#Venta_hoy — Sales this week (unit: pesos)
#Dev_uni_proxima — Returns unit next week (integer)
#Dev_proxima — Returns next week (unit: pesos)
#Demanda_uni_equil — Adjusted Demand (integer) (This is the target you will predict)

#Looking into the data

#Look at Semana(week) frequencies
train[, .N ,by = Semana] #Frequencies seem to be stable across weeks. 

#Look at mean & median demand per week (Semana)
train[, .(mean_demand = mean(Demanda_uni_equil)), by = .(Semana)]
train[, .(median_demand = as.double(median(Demanda_uni_equil))), by = .(Semana)]

## train on the log of the target
train$target<-log(train$Demanda_uni_equil+1)

#Conduct group bys for target and merge back into train
#mean by product
mean_P <-  train[, .(MP = mean(target)), by = .(Producto_ID)]
#mean by cliente
mean_C <-  train[, .(MC = mean(target)), by = .(Cliente_ID)]
#mean by product and agencia
mean_PA <- train[, .(MPA = mean(target)), by = .(Producto_ID, Agencia_ID)] 
#mean by product and ruta
mean_PR <- train[, .(MPR = mean(target)), by = .(Producto_ID, Ruta_SAK)] 
#mean by product, client, agencia
mean_PCA <- train[, .(MPCA = mean(target)), by = .(Producto_ID, Cliente_ID, Agencia_ID)]
#mean by product, client, agencia, and week
mean_PCAS <- train[, .(MPCAS = mean(target)), by = .(Producto_ID, Cliente_ID, Agencia_ID, Semana)]

print("Merging means with train set")
train <- merge(train, mean_PCAS, all.x = TRUE, by = c("Producto_ID", "Cliente_ID", "Agencia_ID","Semana"))
train <- merge(train, mean_PCA, all.x = TRUE, by = c("Producto_ID", "Cliente_ID", "Agencia_ID"))
train <- merge(train, mean_PR, all.x = TRUE, by = c("Producto_ID", "Ruta_SAK"))
train <- merge(train, mean_PA, all.x = TRUE, by = c("Producto_ID", "Agencia_ID"))
train <- merge(train, mean_C, all.x = TRUE, by = "Cliente_ID")
train <- merge(train, mean_P, all.x = TRUE, by = "Producto_ID")

