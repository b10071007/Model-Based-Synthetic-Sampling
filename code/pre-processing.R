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
#-------------------------------------------------------#
#-------------------------------------------------------#

CateToNumeric = function(feature, orig_list=c('no', 'yes'), new_list=c(0, 1)) {
  data_size = length(feature)
  feature_new = rep(NA, data_size)
  for (i in 1:data_size ) {
    feature_new[i] = new_list[which(feature[i]==orig_list)]
  }
  return(feature_new)
}

### diabetes data ### 
DATA = Total[[9]]
setwd(DATA$data_path)
data = read.csv("diabetic_data.csv",header=T)
data = data[,c(5,10,13:18,22,50)]
data$age = ifelse(data$age=="[0-10)",5,
  ifelse(data$age=="[10-20)",15,
  ifelse(data$age=="[20-30)",25,
  ifelse(data$age=="[30-40)",35,       
  ifelse(data$age=="[40-50)",45,
  ifelse(data$age=="[50-60)",55,
  ifelse(data$age=="[60-70)",65,
  ifelse(data$age=="[70-80)",75,       
  ifelse(data$age=="[80-90)",85,
  ifelse(data$age=="[90-100)",95,NA))))))))))

data = data[c(ncol(data),1:(ncol(data)-1))]
names(data)[1] <- "Label"
data$Label = ifelse(data$Label=="<30",1,0)
data[,1] = as.factor(data[,1])
write.csv(data,paste(DATA$data_name,".csv",sep = ""),row.names = F)

### hmeq data ### 
DATA = Total[[10]]
setwd(DATA$data_path)
data = read.csv("hmeq.csv",header=T)
data = data[,c(-5,-6)]
names(data)[1] <- "Label"
data[,1] = as.factor(data[,1])
write.csv(data,paste(DATA$data_name,".csv",sep = ""),row.names = F)

### promotion data ### 
DATA = Total[[11]]
setwd(DATA$data_path)
data = read.csv("promoted.csv",header=T)
data = data[,c(-1,-7,-8)]
names(data)[1] <- "Label"
data[,1] = as.factor(data[,1])
write.csv(data,paste(DATA$data_name,".csv",sep = ""),row.names = F)

##### Bank data #####
DATA = Total[[12]]
setwd(DATA$data_path)
data = read.csv("bank-full.csv",header=T, sep = ';')

data = data[c(-2,-3,-4,-9,-16)]
for (j in c(2,4,5,12)) {
  data[,j] = CateToNumeric(data[,j])
}
data[,7] = CateToNumeric(data[,7], 
                         orig_list=c('jan','feb','mar','apr','may','jun',
                                     'jul','aug','sep','oct','nov','dec'),
                         new_list=1:12)

names(data)[ncol(data)] <- "Label"
data$Label = as.factor(data$Label)
data = data[c(ncol(data) , 1:(ncol(data)-1))]

write.csv(data,paste(DATA$data_name,".csv",sep = ""),row.names = F)

#-------------------------------------------------------#

##### Spambase data #####
DATA = Total[[13]]
setwd(DATA$data_path)
data = read.csv("spambase.data",header=F)
names(data)[ncol(data)] <- "Label"
data[,ncol(data)] = as.factor(data[,ncol(data)])
data = data[c(ncol(data) , 1:(ncol(data)-1))]
write.csv(data,paste(DATA$data_name,".csv",sep = ""),row.names = F)

#-------------------------------------------------------#
