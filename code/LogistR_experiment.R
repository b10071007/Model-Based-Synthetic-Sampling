##### Logistic regression experiment #####
No = "LogistR_exp_All_"

##### library #####
library(caret)
library(pROC)
library(mice)
library(caTools)
library(dplyr)

rootPath = "D:/Github/Model-Based-Synthetic-Sampling/code"
setwd(rootPath)
source("Data information.R")
source("Sampling methods.R")

#--------------------------------------------------------#

## parameter setting ###
split_times = 10
sample_times = 10
test_ratio = 0.4
over_rate = 1
under_rate = 0.5
name_index=c("Original","Over","Under","CBO","SBC",
             "Smote","B_Smote1","B_Smote2","Safe_Smote","ADASYN",
             "MBS_linear","MBS_CART","MBS_SVR")

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
    cat(paste("No.",s," splitting",sep=""))
    cat("\n")
    set.seed(split_seed[s])
    index_test <- createDataPartition(data$Label,p=test_ratio, list = FALSE, times = 1)
    train = data[-index_test,]
    test = data[index_test,]
    maj_size = max(table(train$Label))
    min_size = min(table(train$Label))
    IR = min_size/maj_size*(1+over_rate)
    m = maj_size/(min_size*(1+over_rate))
    B = min_size*over_rate/(maj_size-min_size)
    
    ##### pre-processing #####
    ### imputation for NA
    if(anyNA(data)){
      gc()
      imp = mice(train, m = 5, method = "mean")
      train = mice::complete(imp)
      imp = mice(test[-1], m = 5, method = "mean")
      test[-1] = mice::complete(imp)
    }
    
    ### normalization
    train[-1] = apply(train[-1],2,scale)
    test[-1] = apply(test[-1],2,scale)
    train[is.na(train)] = 0
    test[is.na(test)] = 0
    
    ##### train model of original data #####
    train_LR = function(data){
      model = glm(Label~., data = data, family = "binomial")
      return(model)
    }
    LR_original <- train_LR(train)
    test_pred = predict(LR_original, newdata = test[-1], type = "response")
    Original_AUC = colAUC(test_pred , test$Label)
    test_result[((s-1)*sample_times+1):((s)*sample_times) ,1] = split_seed[s]
    test_result[((s-1)*sample_times+1):((s)*sample_times) ,3] = as.numeric(Original_AUC)
    gc()
    
    ##### set sample seed #####
    seedset = sample.int(500,sample_times,replace = FALSE)
    
    #--------------------------------------------------------#
    
    ##### sampling method #####
    for (k in 1:sample_times){
      test_result[(s-1)*sample_times+k , 2] = seedset[k]
      
      ### Over_train ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      train1 <- overSample(feature=train[-1],label=train$Label,
                           over_rate=eval(over_rate))
      names(train1)[length(train1)] = "Label"
      train1$Label = as.numeric(train1$Label)-1
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,1] = sampling_Time[Data_Index,1] + (sampling_end - sampling_start)
      
      ### Under_train ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      train2 <- underSample(feature=train[-1],label=train$Label,
                            under_rate=eval(under_rate))  
      names(train2)[length(train2)] = "Label"
      train2$Label = as.numeric(train2$Label)-1
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,2] = sampling_Time[Data_Index,2] + (sampling_end - sampling_start)
      
      ### CBO_train ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      train3 = CBO(feature=train[-1],label=train$Label,IR=IR,Nclusters=5)
      train3$Label = as.numeric(train3$Label)-1
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,3] = sampling_Time[Data_Index,3] + (sampling_end - sampling_start)
      
      ### SBC_train ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      train4 = SBC(feature=train[-1],label=train$Label,m=m,Nclusters=5)
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,4] = sampling_Time[Data_Index,4] + (sampling_end - sampling_start)
      
      ### Smote_train ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      train5 <- smote(feature=train[-1], label=train$Label,
                      N=over_rate, K=5)
      train5$Label = as.numeric(train5$Label)-1
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,5] = sampling_Time[Data_Index,5] + (sampling_end - sampling_start)
      
      ### B_Smote1_train ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      train6 <- border_smote1(feature=train[-1], label=train$Label,
                              N=over_rate, K=5)
      train6$Label = as.numeric(train6$Label)-1
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,6] = sampling_Time[Data_Index,6] + (sampling_end - sampling_start)
      
      ### B_Smote2_train ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      train7 <- border_smote2(feature=train[-1], label=train$Label,
                              N=over_rate, K=5)
      train7$Label = as.numeric(train7$Label)-1
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,7] = sampling_Time[Data_Index,7] + (sampling_end - sampling_start)
      
      ### safe_Smote_train ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      train8 <- safe_smote(feature=train[-1], label=train$Label,
                           N=over_rate, K=5)
      train8$Label = as.numeric(train8$Label)-1
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,8] = sampling_Time[Data_Index,8] + (sampling_end - sampling_start)
      
      ### ADASYN_train ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      train9 <- ADASYN(feature=train[-1], label=train$Label,
                       B=IR, K=5)
      train9$Label = as.numeric(train9$Label)-1
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,9] = sampling_Time[Data_Index,9] + (sampling_end - sampling_start)
      
      ### MBS_train ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      train10 = MBSFast(Label~., train, over_rate=eval(over_rate),iteration=5)
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,10] = sampling_Time[Data_Index,10] + (sampling_end - sampling_start)
      
      ### MBS_CART ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      train11 = MBS_CART(feature=train[-1],label=train$Label,
                         over_rate=eval(over_rate),iteration=5)
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,11] = sampling_Time[Data_Index,11] + (sampling_end - sampling_start)
      
      ### MBS_SVR ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      train12 = MBS_SVR(feature=train[-1],label=train$Label,
                        over_rate=eval(over_rate),iteration=5)
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,12] = sampling_Time[Data_Index,12] + (sampling_end - sampling_start)
      
      ### MBS_Fast ###
      sampling_start = proc.time()[3]
      set.seed(seedset[k])
      train13 = MBSFast(Label~., train, over_rate=eval(over_rate),iteration=5)
      sampling_end = proc.time()[3]
      sampling_Time[Data_Index,13] = sampling_Time[Data_Index,13] + (sampling_end - sampling_start)
      
      gc()
      #--------------------------------------------------------#
      
      ### Training ###
      m1 = train_LR(train1)
      m2 = train_LR(train2)
      m3 = train_LR(train3)
      m4 = train_LR(train4)
      m5 = train_LR(train5)
      m6 = train_LR(train6)
      m7 = train_LR(train7)
      m8 = train_LR(train8)
      m9 = train_LR(train9)
      m10 = train_LR(train10)
      m11 = train_LR(train11)
      m12 = train_LR(train12)
      m13 = train_LR(train13)
      
      LR_list = list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13)
      
      ### Testing ###
      for(i in 1:length(LR_list)){
        pred = as.data.frame(predict(LR_list[i], newdata = test[-1], type = "response"))
        test_result[(s-1)*sample_times+k,i+3] = colAUC(pred , test$Label)
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
  Result_name = paste(No, Total[[Data_Index]]$data_name, "result.csv",sep="")
  write.csv(performance, Result_name,row.names = F)
  
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

