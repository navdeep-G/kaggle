##Libraries required
#Data work 
require(data.table) #Working with large files
require(xlsx)       #Loading and saving .xlsx files 
require(plyr)   #Always load in this order plyr, dpply, lubridate - dpplyr overrides some of methods in plyr. 
require(dplyr) #Use require as it will give an error message if the package doesn't exist
require(lubridate) #used for working with data information. 
require(reshape2)  #used for melting 


#Formating and printing 
#install.packages("devtools")
#devtools::install_github("adletaw/captioner")   #Nice library for numbering and captioning tables in conjunction with knitr and pandoc
require(pander)   	#for creating nice output tables.
require(captioner)

#Set up the figure and table numbering
fig_nums<-captioner()
tab_nums<-captioner(prefix = "Table")

#Using pryr abbreviate how to call fig_nums function 
require(pryr)
citefig<-pryr::partial(fig_nums,display="cite")
citetab<-pryr::partial(tab_nums,display="cite")

#Turn off caption.prefix as allow captioner to handle this. 
panderOptions('table.caption.prefix', '')
panderOptions('big.mark', ",")
panderOptions('keep.trailing.zeros', TRUE)

#Load the data 
hdTrain<-data.table::fread("../input/train.csv",header=TRUE,encoding="Latin-1") 
hdTest<-data.table::fread("../input/test.csv",header=TRUE) 
hdAtt<-data.table::fread("../input/attributes.csv",header=TRUE)

#Not really need here
hdSS<-data.table::fread("../input/sample_submission.csv",header=TRUE) 
hdDesc<-data.table::fread("../input/product_descriptions.csv",header=TRUE)

# train.csv and test.csv Overlap. 

#Count unique *product_uid*, the PK across datasets.
pidTrain = unique(hdTrain[,product_uid])
pidTest = unique(hdTest[,product_uid])
pidAtt = unique(hdAtt[,product_uid])
pidDesc = unique(hdDesc[,product_uid])

#Create venn diagram to represent the relationship between Product IDs across both. 

#install.packages("VennDiagram")
library(VennDiagram)

grid.newpage()

venn.plot1<-draw.pairwise.venn(
  length(pidTrain),length(pidTest),length(intersect(pidTrain,pidTest)),
  category = c("Train product_uid", "Test product_uid"),
  lty = rep("blank",2),
  fill = c("light blue", "pink"),
  alpha = rep(0.5, 2),
  cat.pos = c(0,0),
  cat.dist = rep(0.025, 2))

grid.draw(venn.plot1)

grid.newpage()
venn.plot2<-draw.triple.venn(area1 = length(pidTest), area2 = length(pidTrain), area3 = length(pidAtt), 
                             n12 = length(intersect(pidTest,pidTrain)), 
                             n23 = length(intersect(pidTrain,pidAtt)),
                             n13 = length(intersect(pidTest,pidAtt)),
                             n123 =length(intersect(pidAtt,intersect(pidTest,pidTrain))),
                             category = c("test", "train", "attribute"),
                             lty = "blank",
                             fill = c("skyblue", "pink1", "mediumorchid"))
grid.draw(venn.plot2)
rm(venn.plot1,venn.plot2)
detach("package:VennDiagram",unload=TRUE)
#use search() to check if unloaded


# train.csv Data Exploration

library(ggplot2)
ggplot(hdTrain,aes(relevance))+geom_bar()+labs(title="Frequency of 13 individual relevance scores",x="relevance score",y="row count")

#Create frequency table for printing
mytable<-data.frame(table(hdTrain$relevance))
names(mytable)<-c("Rel. Score","Frequency")
tab_nums("relFreq","Count of frequency of the relevance score")

hdTrain %>% 
  group_by(product_uid) %>%
  summarize(count.PUID=.N,avg.Relevance=mean(relevance,na.rm=TRUE)) %>%
  ggplot(aes(x=count.PUID,y=avg.Relevance,group=1)) +
  geom_jitter(alpha=0.2) +
  scale_y_continuous(breaks = round(seq(1,3,by = 0.333),1))+
  labs(title="Average Relevance by count of product_uid")

#count the number of search terms
hdTrain<-hdTrain[,c.count_search_term:=sapply(gregexpr("\\W+", search_term), length) + 1,]

#Create averages for annotating the plot
avgST<-round(mean(hdTrain$c.count_search_term),2)
avgSTGroup<-hdTrain %>% group_by(as.factor(relevance)) %>% summarise(groupMean=round(mean(c.count_search_term),1))
names(avgSTGroup)=c("relevance","grpMean")

#Create plot, limit y-axis,add overlay boxplot, annotate with meanby group and text. 
plotText<-paste("The mean relevance is ",avgST,"\ny-axis has been reduced.\n max number of  search_terms =",max(hdTrain$c.count_search_term))

hdTrain %>% 
  ggplot(aes(x=factor(relevance),y=c.count_search_term)) +
  geom_jitter(alpha=0.05) +
  coord_cartesian(ylim=c(1,12)) +
  geom_boxplot(color = "red", outlier.colour = NA, fill = NA)+
  geom_text(data = avgSTGroup, aes(x = relevance, y = grpMean, label = grpMean), size = 5, vjust = -0.25,colour="green")+
  annotate("text",label=plotText,x=2,y=11,size=3,colour="black")+
  labs(title="Average relevance by number search_terms",x="relevance",y="Count of search_terms")

#Facetting code not used

ggplot(hdTrain, aes(x=c.count_search_term,colour=as.factor(relevance)))+geom_freqpoly(binwidth = 1)

ggplot(hdTrain, aes(x=c.count_search_term,colour=as.factor(relevance)))+geom_histogram(aes(group=as.factor(hdTrain$relevance),fill=as.factor(relevance)),binwidth = 1,alpha=0.3)

ggplot(dfs, aes(x=values)) + geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.3)

plot<-ggplot(hdTrain, aes(x=c.count_search_term,fill=as.factor(c.count_search_term)))+geom_histogram(binwidth = 1)+theme(legend.position='none')
plot+facet_wrap(~ relevance,nrow=1)

#Function for calculating JC
calcJacCoef<-function(A,B){
  
  #Using library(sets) for calculating Jacard Coefficent.
  ### Note it the warning messages: %>% is masked from package:dplyr, set is masked from package:data.table
  ### Deatch it after its use otherwise gets annoying. 
  require(sets)
  
  A<-gset(strsplit(A, " ")[[1]])
  B<-gset(strsplit(B, " ")[[1]])
  return(gset_similarity(A,B,"Jaccard"))
  
  detach("package:sets",unload=TRUE)
  #use search() to check if unloaded
}

#Calculate the Jaccard Coefficient between the *search_term&* and *product_title* for each row in hdTrain
#Takes time to calculate. 
hdTrain<-hdTrain[,search_terms:=tolower(search_term)]
hdTrain<-hdTrain[,product_title:=tolower(product_title)]
hdTrain<-hdTrain[,c.Jaccard.ST_PT := calcJacCoef(search_term,product_title),by = 1:nrow(hdTrain)]

detach("package:sets",unload=TRUE)
#use search() to check if unloaded

#Create averages for annotating the plot
avgJC<-round(mean(hdTrain[,c.Jaccard.ST_PT]),2)
avgJCGroup<-hdTrain %>% group_by(relevance) %>% summarise(groupMean=round(mean(c.Jaccard.ST_PT),2))
names(avgJCGroup)=c("relevance","grpMean")


hdTrain %>% 
  ggplot(aes(x=factor(relevance),y=c.Jaccard.ST_PT)) +
  geom_jitter(alpha=0.05) +
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)+
  geom_text(data = avgJCGroup, aes(x = factor(relevance), y = grpMean, label = grpMean), size = 5, vjust = 0,colour="green")+
  labs(title="Average Jaccard Coefficient by Relevance Score",x="relevance",y="Jaccard Coefficent")

hdTrain<-hdTrain[product_uid %in% unique(hdAtt[,product_uid]),c.hasAttribute:="Yes"]
hdTrain<-hdTrain[is.na(c.hasAttribute),c.hasAttribute:="No"]

hdTrain %>% 
  ggplot(aes(x=factor(c.hasAttribute),y=relevance)) +
  geom_jitter(alpha=0.05) +
  geom_boxplot(color = "red", outlier.colour = NA, fill = NA)+
  labs(title="Compare relevance by product_uid in Attribute file",x="Product has Attributes",y="relevance")

#Create frequency table for printing
mytable<-data.table(table(tolower(hdAtt$name)))
names(mytable)<-c("Attribute.name","Freq.")
mytable<-mytable[with(mytable,order(-Freq.)),]
tab_nums("attNameFreq","Count of Top 10 Freq. of categories in attribute.csv")

mytableColor<-mytable[Attribute.name %like% "colo*r",]
mytableBrand<-mytable[Attribute.name %like% "brand",]
tab_nums("attNameFreqColor","Count of Top 10 Freq. of Colour categories")
tab_nums("attNameFreqBrand","Count of Top 10 Freq. of Brand categories")

pander(mytableColor[1:10,],caption=tab_nums("attNameFreqColor"),split.table=60,justify=c("left"),table.alignment="center")
pander(mytableBrand[1:10,],caption=tab_nums("attNameFreqBrand"),split.table=60,justify=c("left"),table.alignment="center")

#First change hdAtt tolower
hdAtt<-hdAtt[,name:=tolower(name)]

hdTrain<-hdTrain[product_uid %in% unique(hdAtt[name %like% "colo*r",product_uid]),c.hasColorAtt:="Yes"]
hdTrain<-hdTrain[is.na(c.hasColorAtt),c.hasColorAtt:="No"]

hdTrain<-hdTrain[product_uid %in% unique(hdAtt[name == "mfg brand name",product_uid]),c.hasBrandAtt:="Yes"]
hdTrain<-hdTrain[is.na(c.hasBrandAtt),c.hasBrandAtt:="No"]

hdTrain[c.hasAttribute=="Yes",] %>%
  ggplot(aes(x=factor(c.hasBrandAtt),y=relevance)) +
  geom_jitter(alpha=0.05) +
  geom_boxplot(color = "blue", outlier.colour = NA, fill = NA)+
  labs(title="Compare relevance by product_uid in attributes file \n and has Attribute.name==mfg brand name",x="Product has mfg brand name in    attributes",y="relevance")

hdTrain[c.hasAttribute=="Yes",] %>%
  ggplot(aes(x=factor(c.hasColorAtt),y=relevance)) +
  geom_jitter(alpha=0.05) +
  geom_boxplot(color = "green", outlier.colour = NA, fill = NA)+
  labs(title="Compare relevance by product_uid in attributes file \n and has Attribute.name of type color",x="Product has color attributes",y="relevance")


