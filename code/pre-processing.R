#########################
##### preprocessing #####
#########################

setwd("D:/Github/Model-Based-Synthetic-Sampling/code")
source("Data information.R")

##### Pima data #####
DATA = Total[[1]]
setwd(DATA$data_path)
data = read.csv("pima-indians-diabetes.data",header=F)
data = data[c(9,1:8)]
names(data)[1] <- "Label"
data[,1] = as.factor(data[,1])
write.csv(data,paste(DATA$data_name,".csv",sep = ""),row.names = F)

#-------------------------------------------------------#

##### Haberman data #####
DATA = Total[[2]]
setwd(DATA$data_path)
data = read.csv("haberman.data",sep=",",header = FALSE)
data = data[c(4,1:3)]
names(data)[1] <- "Label"
data$Label = ifelse(data$Label==2,"yes","no")
data$Label = ifelse(data$Label=="yes",1,0)
data[,1] = as.factor(data[,1])
write.csv(data,paste(DATA$data_name,".csv",sep = ""),row.names = F)

#-------------------------------------------------------#

##### Satimage data #####
DATA = Total[[3]]
setwd(DATA$data_path)
trn = read.csv("sat.trn",sep=" ",header = FALSE)
tst = read.csv("sat.tst",sep=" ",header = FALSE)
data = rbind(trn,tst)
data = data[c(37,1:36)]
names(data)[1] <- "Label"
data$Label = ifelse(data$Label==4,"yes","no")
data$Label = ifelse(data$Label=="yes",1,0)
data[,1] = as.factor(data[,1])
write.csv(data,paste(DATA$data_name,".csv",sep = ""),row.names = F)

#-------------------------------------------------------#

##### Ecoli data #####
DATA = Total[[4]]
setwd(DATA$data_path)
raw = read.table("ecoli.data",sep="\t",header = FALSE)
raw$V1 = as.character(raw$V1)

data = matrix(NA,nrow=nrow(raw),ncol=9)
for(i in 1:nrow(data)){
  string = strsplit(raw[i,],split=" ")
  string = string[[1]][string[[1]]!=""]
  data[i,] = string
}
data = data[,-1]
data = as.data.frame(data)
data = na.omit(data)
for(i in 1:7){
  data[,i] = as.numeric(as.character(data[,i]))
}
data = data[c(8,1:7)]
names(data)[1] <- "Label"
data$Label = ifelse(data$Label=="imU",1,0)
write.csv(data,paste(DATA$data_name,".csv",sep = ""),row.names = F)

#-------------------------------------------------------#

##### shuttle data #####
DATA = Total[[5]]
setwd(DATA$data_path)
trn = read.csv("shuttle.trn",sep=" ",header = FALSE)
tst = read.csv("shuttle.tst",sep=" ",header = FALSE)
data = rbind(trn,tst)
data = data[c(10,1:9)]
names(data)[1] <- "Label"
data$Label = ifelse(data$Label==2,"yes","no")
data$Label = ifelse(data$Label=="yes",1,0)
data[,1] = as.factor(data[,1])
write.csv(data,paste(DATA$data_name,".csv",sep = ""),row.names = F)

#-------------------------------------------------------#

##### Ionosphere data #####
DATA = Total[[6]]
setwd(DATA$data_path)
data = read.csv("ionosphere.data",sep=",",header = FALSE)
data = data[c(35,1:34)]
names(data)[1] <- "Label"
data$Label = ifelse(data$Label=="b",1,0)
data[,1] = as.factor(data[,1])
write.csv(data,paste(DATA$data_name,".csv",sep = ""),row.names = F)

#-------------------------------------------------------#

##### vehicle data #####
DATA = Total[[7]]
setwd(DATA$data_path)
temp = list.files(path = DATA$data_path,pattern="[:.:]dat")
read.dat = function(n){
  data = read.csv(n,sep=" ",header = FALSE)
  data = data[,1:19]
  return(data)
}
Data_List = lapply(temp, read.dat)
data = Data_List[[1]]
for(i in 2:length(temp)){
  data = rbind(data,Data_List[[i]])
}
data = data[c(19,1:18)]
names(data)[1] <- "Label"
data$Label = ifelse(data$Label=="van",1,0)
data[,1] = as.factor(data[,1])
write.csv(data,paste(DATA$data_name,".csv",sep = ""),row.names = F)

#-------------------------------------------------------#

##### Credit data #####
DATA = Total[[8]]
setwd(DATA$data_path)
data = read.csv("cs-training.csv")
data = data[-1]
names(data)[1] <- "Label"
data[,1] = as.factor(data[,1])
write.csv(data,paste(DATA$data_name,".csv",sep = ""),row.names = F)

#-------------------------------------------------------#
