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
#Conduct grouping across variables in test set, which are {Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID}

#mean by agencia
mean_agent <-  train[, .(MP = mean(target)), by = .(Agencia_ID)]
#mean by canal
mean_canal <-  train[, .(MC = mean(target)), by = .(Canal_ID)]
#mean by Ruta_SAK
mean_ruta <- train[, .(MPA = mean(target)), by = .(Ruta_SAK)] 
#mean by client
mean_client <- train[, .(MPR = mean(target)), by = .(Cliente_ID)] 
#mean by product
mean_product <- train[, .(MPCA = mean(target)), by = .(Producto_ID)]
#mean by agencia, canal, Ruta_SAK, client, and product
mean_acrcp <- train[, .(MPCAS = mean(target)), by = .(Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID)]

