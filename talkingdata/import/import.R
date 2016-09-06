options(stringsAsFactors=F,scipen=99)
rm(list=ls());gc()
require(data.table)

train <- fread("../input/gender_age_train.csv",
                     colClasses=c("character","character",
                                  "integer","character"))
test <- fread("../input/gender_age_test.csv",
                    colClasses=c("character"))
test$gender <- test$age <- test$group <- NA
label <- rbind(train,test)
setkey(label,device_id)
rm(test,train);gc()

brand <- fread("../input/phone_brand_device_model.csv",
               colClasses=c("character","character","character"))
setkey(brand,device_id)
brand0 <- unique(brand,by=NULL)
brand0 <- brand0[sample(nrow(brand0)),]
brand2 <- brand0[-which(duplicated(brand0$device_id)),]
label1 <- merge(label,brand2,by="device_id",all.x=T)
rm(brand,brand0,brand2);gc()

# apps
events <- fread("../input/events.csv",
                colClasses=c("character","character","character",
                             "numeric","numeric"))
setkeyv(events,c("device_id","event_id"))
event_app <- fread("../input/app_events.csv",
                   colClasses=rep("character",4))
setkey(event_app,event_id)

events <- unique(events[,list(device_id,event_id)],by=NULL)
event_apps <- event_app[,list(apps=paste(unique(app_id),collapse=",")),by="event_id"]
device_event_apps <- merge(events,event_apps,by="event_id")
rm(events,event_app,event_apps);gc()

f_split_paste <- function(z){paste(unique(unlist(strsplit(z,","))),collapse=",")}
device_apps <- device_event_apps[,list(apps=f_split_paste(apps)),by="device_id"]
rm(device_event_apps,f_split_paste);gc()

tmp <- strsplit(device_apps$apps,",")
device_apps <- data.table(device_id=rep(device_apps$device_id,
                                        times=sapply(tmp,length)),
                          app_id=unlist(tmp))
rm(tmp)