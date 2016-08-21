library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(treemap)

train <- read_csv("../../data/train.csv")
test <- read_csv("../../data/test.csv")
client <- read_csv("../../data/cliente_tabla.csv")
product <- read_csv("../../data/producto_tabla.csv")
town <- read_csv("../../data/town_state.csv")

ggplot(train %>% sample_frac(0.005))+
  geom_histogram(aes(x=Semana), color="black", fill="red", alpha=0.5)+
  scale_x_continuous(breaks=1:10)+
  scale_y_continuous(name="Client / Product deliveries")+
  theme_bw()

# Agencies & States

agencias <- train %>%
  group_by(Agencia_ID) %>%
  summarise(Units = sum(Venta_uni_hoy),
            Pesos = sum(Venta_hoy),
            Return_Units = sum(Dev_uni_proxima),
            Return_Pesos = sum(Dev_proxima),
            Net = sum(Demanda_uni_equil)) %>%
  mutate(Net_Pesos = Pesos - Return_Pesos,
         Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Units)) %>%
  inner_join(town, by="Agencia_ID")

ggplot(agencias, aes(x=Units/7))+
  geom_histogram(fill="red", color="gray", binwidth=10000)+
  scale_x_continuous(name="Units / Week", labels=function(x)paste(x/1000, "k"))+
  scale_y_continuous(name="Agencias")+
  theme_bw()

treemap(agencias[1:100, ], 
        index=c("Agencia_ID"), vSize="Units", vColor="Return_Rate", 
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="Units return %", title="Top 100 agencias")

top30agencias <- agencias$Agencia_ID[1:30]
top100agencias <- agencias$Agencia_ID[1:100]
rm(agencias)

agencias.history <- train %>%
  group_by(Agencia_ID, Semana) %>%
  summarise(Units = sum(Venta_uni_hoy),
            Pesos = sum(Venta_hoy),
            Return_Units = sum(Dev_uni_proxima),
            Return_Pesos = sum(Dev_proxima),
            Net = sum(Demanda_uni_equil)) %>%
  mutate(Net_Pesos = Pesos - Return_Pesos,
         Avg_Pesos = Pesos / Units,
         Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(Agencia_ID, Semana) %>%
  inner_join(town, by="Agencia_ID")

ggplot(agencias.history %>% filter(Agencia_ID %in% top30agencias))+
  geom_bar(aes(x=Semana, y=Units, fill=Return_Rate), stat="identity", color="black")+
  facet_wrap(~Agencia_ID)+
  scale_y_continuous(labels=function(x)paste(x/1000, "k"))+
  scale_fill_gradient(name="Units\nReturn %", low="white", high="red")+
  ggtitle("Top 30 agencias")+
  theme_bw()

states <- agencias.history %>%
  group_by(State, Semana) %>%
  summarise(Units = sum(Units),
            Pesos = sum(Pesos),
            Return_Units = sum(Return_Units),
            Return_Pesos = sum(Return_Pesos),
            Net = sum(Net)) %>%
  mutate(Avg_Pesos = Pesos / Units,
         Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Units))

ggplot(states)+
  geom_bar(aes(x=Semana, y=Units, fill=Return_Rate), stat="identity", color="black")+
  scale_y_continuous(labels=function(x)paste(x/1e6, "m"))+
  scale_fill_gradient(name="Units\nReturn %", low="white", high="red")+
  facet_wrap(~State)+
  ggtitle("States")+
  theme_bw()



rm(states)
rm(agencias.history)


#Canals
#Canal 1 is the most present.
canals <- train %>%
  group_by(Canal_ID, Semana) %>%
  summarise(Units = sum(Venta_uni_hoy),
            Pesos = sum(Venta_hoy),
            Return_Units = sum(Dev_uni_proxima),
            Return_Pesos = sum(Dev_proxima),
            Net = sum(Demanda_uni_equil)) %>%
  mutate(Net_Pesos = Pesos - Return_Pesos,
         Avg_Pesos = Pesos / Units,
         Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Units))

treemap(canals, index=c("Canal_ID"), vSize="Units", type="index", title="Canals repartition")

ggplot(canals)+
  geom_bar(aes(x=Semana, y=Units, fill=Return_Rate), stat="identity", color="black")+
  scale_y_continuous(labels=function(x)paste(x/1e6, "m"))+
  scale_fill_gradient(name="Units\nReturn %", low="white", high="red")+
  facet_wrap(~Canal_ID, scale="free")+
  theme_bw()



rm(canals)

## Canals x Agencies
agencias.canals <- train %>%
  group_by(Agencia_ID) %>%
  summarise(n_canals = n_distinct(Canal_ID))

ggplot(agencias.canals)+
  geom_histogram(aes(x=n_canals), fill="red", color="black", alpha="0.3", binwidth=0.5)+
  scale_x_continuous(name="Number of canals", breaks=1:5)+
  scale_y_continuous(name="Number of agencies")+
  theme(axis.text.x=element_text(hjust=1))+
  theme_bw()
rm(agencias.canals)


# Routes
#It is not clear what Routes represent. More than 2/3 of the routes provide less than 10k products a week.

routes <- train %>% group_by(Ruta_SAK) %>%
  summarise(n_Agencias = n_distinct(Agencia_ID),
            n_Clients = n_distinct(Cliente_ID),
            Units=sum(Venta_uni_hoy),
            Return_Units = sum(Dev_uni_proxima)) %>%
  mutate(Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Units))

ggplot(routes, aes(x=Units/7))+
  geom_histogram(fill="red", color="gray", binwidth=5000)+
  scale_x_continuous(name="Units / Week", labels=function(x)paste(x/1000, "k"))+
  scale_y_continuous(name="Routes")+
  theme_bw()



top100routes <- routes$Ruta_SAK[1:100]
rm(routes)


## Routes x Agencies
#Even if there is no clear pattern, some routes seem to be working together with same agencies.

routes.agencias <- train %>% group_by(Ruta_SAK, Agencia_ID) %>%
  summarise(count=n(),
            n_Clients = n_distinct(Cliente_ID),
            Units=sum(Venta_uni_hoy),
            Return_Units = sum(Dev_uni_proxima)) %>%
  mutate(Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Units))

ggplot(routes.agencias %>% 
         filter(Ruta_SAK %in% top100routes, Agencia_ID %in% top100agencias))+
  geom_point(aes(x=as.character(Ruta_SAK), 
                 y=as.character(Agencia_ID), 
                 size=Units, color=Return_Rate))+
  scale_x_discrete(name="Routes")+
  scale_y_discrete(name="Agencies")+
  scale_color_gradient(name="Return Rate", low="blue", high="red")+
  ggtitle("Top 100 agencies & routes")+
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank())


rm(routes.agencias)


# Clients
#There is one big client, "Puebla Remision".
sales <- train %>%
  group_by(Cliente_ID) %>%
  summarise(Units = sum(Venta_uni_hoy),
            Pesos = sum(Venta_hoy),
            Return_Units = sum(Dev_uni_proxima),
            Return_Pesos = sum(Dev_proxima),
            Net = sum(Demanda_uni_equil)) %>%
  mutate(Return_Rate = Return_Units / (Units+Return_Units),
         Avg_Pesos = Pesos / Units) %>%
  mutate(Net_Pesos = Pesos - Return_Pesos) %>%
  inner_join(client, by="Cliente_ID") %>%
  arrange(desc(Pesos))

treemap(sales[1:100, ], 
        index=c("NombreCliente"), vSize="Units", vColor="Return_Rate", 
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="Units return %", title="Top 100 clients")


sales$Cum_Units <- cumsum(sales$Units) / sum(sales$Units)
s <- seq(1, 800000, 100)
ggplot()+geom_line(aes(x=s, y=sales$Cum_Units[s]))+
  scale_x_continuous(name="Clients", labels=function(x) paste(x/1000, "k"))+
  scale_y_continuous(name="Cumulative share (units)", labels=percent)+
  ggtitle("Clients repartition")+
  theme_bw()

sales$share <- sales$Units / sum(sales$Units)


#Herfindahl Index = `r round(1/sum(sales$share^2))` clients
rm(sales)

## Clients x Agencies
#The large majority of clients only work with one agency. Only the largest clients work with multiple agencies.

agencias.by.client <- train %>%
  group_by(Cliente_ID) %>%
  summarise(n_agencias = n_distinct(Agencia_ID)) %>%
  inner_join(client, by="Cliente_ID")

table(agencias.by.client$n_agencias)

agencias.by.client %>% filter(n_agencias %in% c(5, 9, 62))


rm(agencias.by.client)


## Clients x Canals
#Most clients only have one canal. Different agencies can use the same canal for one client. 

clients.canals <- train %>%
  group_by(Cliente_ID) %>%
  summarise(n_canals = n_distinct(Canal_ID))

table(clients.canals$n_canals)


rm(clients.canals)


#Few agencies have multiple channels for the same client.

clients.agencies.canals <- train %>%
  group_by(Cliente_ID, Agencia_ID) %>%
  summarise(n_canals = n_distinct(Canal_ID))

table(clients.agencies.canals$n_canals)


rm(clients.agencies.canals)


## Clients x Routes
#Most client only have deliveries from less than 5 depots, but more than 2500 clients work with 10 depots or more.

clients.routes <- train %>%
  group_by(Cliente_ID) %>%
  summarise(n_routes = n_distinct(Ruta_SAK))

ggplot(clients.routes)+
  geom_histogram(aes(x=n_routes), fill="red", color="black", alpha="0.3", binwidth=1)+
  scale_x_continuous(name="Number of clients")+
  scale_y_continuous(name="Number of routes", labels=function(x)paste(x/1000, "k"))+
  theme_bw()


rm(clients.routes)


# Products
products <- train %>% group_by(Producto_ID) %>%
  summarise(Units = sum(Venta_uni_hoy),
            Pesos = sum(Venta_hoy),
            Return_Units = sum(Dev_uni_proxima),
            Return_Pesos = sum(Dev_proxima),
            Net = sum(Demanda_uni_equil)) %>%
  mutate(Avg_Pesos = Pesos / Units,
         Return_Rate = Return_Units / (Units+Return_Units)) %>%
  filter(!is.nan(Avg_Pesos)) %>%
  inner_join(product, by="Producto_ID") %>%
  arrange(desc(Units))

products$NombreProducto <- factor(as.character(products$NombreProducto), levels=products$NombreProducto)

treemap(products[1:100, ], 
        index=c("NombreProducto"), vSize="Units", vColor="Return_Rate", 
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="Units return %", title="Top 100 products")


ggplot(products, aes(x=Avg_Pesos))+
  geom_histogram(aes(y=..density..), fill="gray", color="black", alpha="0.3")+
  geom_density(fill="red", alpha="0.3")+
  scale_x_continuous(name="Products average price", lim=c(0, 50))+
  scale_y_continuous(name="Density", labels=percent)+
  theme_bw()


top100products <- products$Producto_ID[1:100]
rm(products)


## Products x Agencies
#Agencies usually deal with between around 100 and 200 products.

agencias.products <- train %>% group_by(Agencia_ID) %>%
  summarise(n_products = n_distinct(Producto_ID))

ggplot(agencias.products)+
  geom_histogram(aes(x=n_products), fill="red", color="black", alpha="0.3", binwidth=10)+
  scale_x_continuous(name="Number of routes")+
  scale_y_continuous(name="Number of products")+
  theme_bw()

rm(agencias.products)


## Products x Canals
#Products can be delivered through multiple channels.
canals.products <- train %>% group_by(Producto_ID) %>%
  summarise(n_canals = n_distinct(Canal_ID))

ggplot(canals.products)+
  geom_histogram(aes(x=n_canals), fill="red", color="black", alpha="0.3", binwidth=1)+
  scale_x_continuous(name="Number of canals", breaks=1:10, lim=c(1, 10))+
  scale_y_continuous(name="Number of products")+
  theme_bw()

rm(canals.products)


## Products x Routes
#As expected, products can be in many depots, and depots can stock many products. 
#However, it seems than some products are usually kept together.
routes.products <- train %>% group_by(Producto_ID) %>%
  summarise(n_routes = n_distinct(Ruta_SAK))

ggplot(routes.products)+
  geom_histogram(aes(x=n_routes), fill="red", color="black", alpha="0.3", binwidth=10)+
  scale_x_continuous(name="Number of routes")+
  scale_y_continuous(name="Number of products")+
  theme_bw()

routes.products <- train %>% group_by(Ruta_SAK) %>%
  summarise(n_products = n_distinct(Producto_ID))

ggplot(routes.products)+
  geom_histogram(aes(x=n_products), fill="red", color="black", alpha="0.3", binwidth=10)+
  scale_x_continuous(name="Number of products")+
  scale_y_continuous(name="Number of routes")+
  theme_bw()

routes.products <- train %>% group_by(Ruta_SAK, Producto_ID) %>%
  summarise(count=n(),
            n_Agencias = n_distinct(Agencia_ID),
            n_Clients = n_distinct(Cliente_ID),
            Units=sum(Venta_uni_hoy),
            Return_Units = sum(Dev_uni_proxima)) %>%
  mutate(Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Units))

ggplot(routes.products %>% 
         filter(Ruta_SAK %in% top100routes, Producto_ID %in% top100products))+
  geom_point(aes(x=as.character(Ruta_SAK), 
                 y=as.character(Producto_ID), 
                 size=Units, color=Return_Rate))+
  scale_x_discrete(name="Ruta SAK")+
  scale_y_discrete(name="Product ID")+
  scale_color_gradient(name="Return Rate", low="blue", high="red")+
  ggtitle("Top 100 products & routes")+
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

rm(routes.products)


## Products x Clients

products.by.client <- train %>%
  group_by(Cliente_ID) %>%
  summarise(n_products = n_distinct(Producto_ID)) %>%
  inner_join(client, by="Cliente_ID")

ggplot(products.by.client)+
  geom_histogram(aes(x=n_products), fill="red", color="black", alpha="0.3", binwidth=2)+
  scale_x_continuous(name="Number of products by client", lim=c(0, 150))+
  scale_y_continuous(name="Number of clients", labels=function(x)paste(x/1000, "k"))+
  theme_bw()

rm(products.by.client)
