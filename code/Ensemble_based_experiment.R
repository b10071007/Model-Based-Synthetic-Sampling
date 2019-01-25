##### Ensemble_based method experiment #####
No = "Ensemble_exp_All_"

##### library #####
library(caret)
library(pROC)
library(mice)
library(caTools)
library(dplyr)
library(ebmc)

rootPath = "D:/Github/Model-Based-Synthetic-Sampling/code"
setwd(rootPath)
source("Data information.R")
source("Sampling methods.R")

#--------------------------------------------------------#

## parameter setting ###
split_times = 10
sample_times = 10
test_ratio = 0.4
imputation = list(m=5, method="mean")
over_rate = 1
under_rate = 0.5
num_tree = 20
name_index=c("Original","MBSBoost","RUSBoost","UnderBagging")

#--------------------------------------------------------#

## time calculation ##
start_time = format(Sys.time())
Time = rep(0,length(Total))
sampling_Time = as.data.frame(matrix(0,nrow=length(Total),ncol=length(name_index)))
names(sampling_Time) = name_index[-1]

#--------------------------------------------------------#

AUC_mean_total = data.frame(matrix(0,nrow=length(Total),ncol=length(name_index)))
colnames(AUC_mean_total) = name_index
AUC_std_total = AUC_mean_total
AUC_rank_total = AUC_mean_total
Result_total = list(AUC_mean_total, AUC_std_total, AUC_rank_total)
result_path_each = paste(result_path, No, sep='')
build_path(result_path_each)

for( Data_Index in 1:length(Total) ){
  Data = Total[[Data_Index]]
  cat(paste("[",Data_Index,"] ", sep=""))
  cat(format(Sys.time()))
  cat(paste(" start to run",Data$data_name))
  cat("\n")
  start = proc.time()
  
  ### read data
  setwd(Data$data_path)
  data = read.csv(paste(Data$data_name,".csv",sep = ""),header = TRUE)
  data$Label = as.factor(data$Label)
  
  ### build result data frame
  set.seed(1)
  split_seed = sample.int(2000,split_times,replace = FALSE)
  test_result = as.data.frame(matrix(nrow=split_times*sample_times,ncol=length(name_index)+2))
  colnames(test_result) = c("split_seed","sample_Seed",name_index)
  
  #--------------------------------------------------------#
  
  for (s in 1:length(split_seed)){
    ##### split data #####
    test_result[((s-1)*sample_times+1):((s)*sample_times) ,1] = split_seed[s]
    cat(paste("No.",s," splitting",sep=""))
    cat("\n")
    set.seed(split_seed[s])
    index_test <- createDataPartition(data$Label,p=test_ratio, list = FALSE, times = 1)
    train = data[-index_test,]
    test = data[index_test,]
    maj_size = max(table(train$Label))
    min_size = min(table(train$Label))
    IR = maj_size/min_size/(1+over_rate)
    m = maj_size/(min_size*(1+over_rate))
    B = min_size*over_rate/(maj_size-min_size)
    
    ##### pre-processing #####
    ### imputation for NA
    if(anyNA(data)){
      gc()
      imp = mice(train, m = imputation$m, method = imputation$method)
      train = mice::complete(imp)
      #summary(train)
      imp = mice(test[-1], m = imputation$m, method = imputation$method)
      test[-1] = mice::complete(imp)
      #summary(test)
      table(train$Label)
    }
    
    ### normalization
    train[-1] = apply(train[-1],2,scale)
    test[-1] = apply(test[-1],2,scale)
    train[is.na(train)] = 0
    test[is.na(test)] = 0
    
    gc()
    
    ##### set sample seed #####
    seedset = sample.int(500,sample_times,replace = FALSE)
    #--------------------------------------------------------#
    
    ##### sampling method #####
    for (k in 1:sample_times){
      test_result[(s-1)*sample_times+k , 2] = seedset[k]
      
      ### MBSBoost ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      m1 <- MBSBoost(Label~., data=train, size=20, "c50", over_rate=1)
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,1] = sampling_Time[Data_Index,1] + (sampling_end - sampling_start)
      
      ### RUSBoost ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      m2 <- rus(Label~., data=train, size=20, "c50", ir=IR)
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,2] = sampling_Time[Data_Index,2] + (sampling_end - sampling_start)
      
      ### UnderBagging ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      m3 <- ub(Label~., data=train, size=20, "c50", ir=IR)
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,3] = sampling_Time[Data_Index,3] + (sampling_end - sampling_start)
      
      gc()
      ############################################################################################
      
      Model_list = list(m1,m2,m3)
      ### scoring
      # test result
      for(i in 1:length(Model_list)){
        pred = predict(Model_list[i], newdata = test[-1])[[1]]
        test_result[(s-1)*sample_times+k,i+2] = colAUC(pred , test$Label)
        
      } # end of prediction loop
      
      #--------------------------------------------------------#
      
    } # end of sampling loop
  } # end of splitting loop
  
  ### performance ###
  setwd(result_path_each)
  test_result$split_seed = as.factor(test_result$split_seed)
  test_result$sample_Seed = as.factor(test_result$sample_Seed)
  AUC_mean = sapply(test_result[-1:-2],mean)
  split_result = split(test_result[-2],test_result$split_seed)
  AUC_std = lapply(split_result,function(x)apply(x[-1],2,sd)) 
  AUC_std = apply(t(as.data.frame(AUC_std)),2,mean)
  AUC_rank = rank(-AUC_mean,ties.method="min")
  
  performance = data.frame(Method=names(test_result)[-1:-2],
                           AUC_mean = AUC_mean,
                           AUC_std = AUC_std, 
                           AUC_rank = AUC_rank)
  Result_name = paste(No, Total[[Data_Index]]$data_name, 
                      "result.csv",sep="")
  write.csv(performance, Result_name, row.names = F)
  
  Result_total[[1]][Data_Index,] = AUC_mean
  Result_total[[2]][Data_Index,] = AUC_std
  Result_total[[3]][Data_Index,] = AUC_rank
  
  end = proc.time()
  Time[Data_Index] = end[3]-start[3]
} # end of Data loop

#--------------------------------------------------------#

setwd(result_path_each)
write.csv(sampling_Time/100,
          paste(No, "sample_time.csv", sep=""),
          row.names = FALSE)

paste(paste("Execution time is", Time/60, "minutes"))
cat(paste("Total execution time is",sum(Time)/60, "minutes") )

Data_name=''
for( Data_Index in 1:length(Total) ){
  Data = Total[[Data_Index]]
  Data_name = c(Data_name, Data$data_name )
}
Data_name = Data_name[-1]

execution_time = data.frame(Data = Data_name, Execution_time = Time/60)
write.csv(execution_time,
          paste(No,"execution_time.csv",sep=""),
          row.names = FALSE)

#--------------------------------------------------------#

setwd(result_path)
write.csv(Result_total[[1]], paste(No, "AUC_mean_total.csv",sep=""), row.names = F)
write.csv(Result_total[[2]], paste(No, "AUC_std_total.csv",sep=""), row.names = F)
write.csv(Result_total[[3]], paste(No, "AUC_rank_total.csv",sep=""), row.names = F)

