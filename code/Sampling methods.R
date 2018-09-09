library(tidyr)
library(dplyr)
library(ggplot2)
library(FNN)
library(stats)
library(rpart)
library(MASS)
library(e1071)
library(kknn)
library(ebmc)

## test ##
if(FALSE){
  feature = train[-1]
  label = train$Label
  over_rate = 1
  N=1
  K=5
  Nclusters=5
  iteration=5
}
shuffle = function(train_data){
  n_row = nrow(train_data)
  train_data = train_data[sample(n_row,n_row),]
  return(train_data)
}
##########################################

####################################################
##### original method and cluster-based method #####
####################################################
if(TRUE){
##### Random Over-sampling #####
overSample = function(feature,label,over_rate=1){ # over_rate: over-sampling rate
  ## find minority class and get its subset
  minClass = levels(label)[which.min(table(label))]
  minIndex = which(label == minClass)
  minority = subset(feature,label==minClass)
  
  ## resample minority
  over_size = round( nrow(minority)*(over_rate) ) 
  sample_index = sample.int(nrow(minority),over_size,replace = TRUE)
  over_sample = minority[sample_index,]
  over_sample$Label = minClass
  
  ## merge sampled examples into whole data
  over_train = feature
  over_train$Label = label
  over_train = rbind(over_train,over_sample)
  over_train = shuffle(over_train)
  return(over_train)
}
####################################################

##### Random Under-sampling #####
underSample = function(feature,label,under_rate=0.5){ # under_rate: under-sampling rate
  ## split data into minority and majority
  minClass = levels(label)[which.min(table(label))]
  majClass = levels(label)[which.max(table(label))]
  minIndex = which(label == minClass)
  minority = subset(feature,label==minClass)
  majority = subset(feature,label!=minClass)
  
  ## remove majority
  remove_size = round( nrow(majority)*(1-under_rate) )
  remove_index = sample.int(nrow(majority),remove_size,replace = FALSE)
  majority = majority[-1*remove_index,]
  
  ## merge sampled examples into whole data
  minority$Label = minClass
  majority$Label = majClass
  under_train = rbind(minority,majority)
  under_train$Label = as.factor(under_train$Label)
  under_train = shuffle(under_train)
  return(under_train)
}
####################################################


##### cluster-based over-sampling(CBO) #####
CBO = function(feature,label,IR=1,Nclusters=4){ 
      # IR: Imbalanced Ratio 
      # Nclusters: number of clusters
  ## split data into minority and majority
  minClass = levels(label)[which.min(table(label))]
  majClass = levels(label)[which.max(table(label))]
  minIndex = which(label == minClass)
  minority = subset(feature,label==minClass)
  majority = subset(feature,label!=minClass)
  
  ## do clustering on two classes separately
  row.names(minority) = 1:nrow(minority)
  cluster_s = kmeans(minority,Nclusters) # cluster of 'minority'
  row.names(majority) = 1:nrow(majority)
  cluster_l = kmeans(majority,Nclusters) # cluster of 'majority'
  
  max_size = max(table(cluster_l$cluster))
  size_s = table(cluster_s$cluster) # cluster sizes of 'minority' 
  size_l = table(cluster_l$cluster) # cluster sizes of 'majority'
  
  ## create sampling set for 'minority'
  n_col = ncol(feature)
  sampling_set_s = as.data.frame(matrix(NA, nrow = 1, ncol = n_col))
  names(sampling_set_s) = names(minority)
  
  for(k in 1:Nclusters){
    subgroup = cluster_s$cluster[cluster_s$cluster==k]
    index = sample(1:size_s[k],max((IR*max_size-size_s[k]),0),replace=TRUE)
    sampling_set_s = rbind(sampling_set_s,minority[index,])
  }
  sampling_set_s = sampling_set_s[-1,]
  sampling_set_s$Label = minClass
  
  ## create sampling set for 'majority'
  sampling_set_l = as.data.frame(matrix(NA, nrow = 1, ncol = n_col))
  names(sampling_set_l) = names(minority)
  
  for(k in 1:Nclusters){
    subgroup = cluster_l$cluster[cluster_l$cluster==k]
    index = sample(1:size_l[k],(max_size-size_l[k]),replace=TRUE)
    sampling_set_l = rbind(sampling_set_l,minority[index,])
  }
  sampling_set_l = sampling_set_l[-1,]
  sampling_set_l$Label = majClass
  
  ## merge sampled examples into whole data
  CBO_train = feature
  CBO_train$Label = label
  CBO_train = rbind(CBO_train,sampling_set_s,sampling_set_l)
  CBO_train = shuffle(CBO_train)
  return(CBO_train)
}
####################################################

##### under-sampling based on clustering(SBC) #####
SBC = function(feature,label,m=1,Nclusters=5){
      # m: a hyperparameter to adjust imbalanced ratio
      # Nclusters: number of clusters
  ## find minority class and get its subset
  total = feature
  row.names(total) = 1:nrow(total)
  minClass = levels(label)[which.min(table(label))]
  majClass = levels(label)[which.max(table(label))]
  minIndex = which(label == minClass)
  majIndex = which(label == majClass)
  size_l = length(majIndex)
  size_s = length(minIndex)
  minority = subset(feature,label==minClass)
  
  ## do clustering and calculate size for each cluster
  cluster = kmeans(total,Nclusters)
  cluster_size = matrix(NA,ncol=Nclusters,nrow=1)
  for(k in 1:Nclusters){
    subgroup = cluster$cluster[cluster$cluster==k]
    index_subgroup = as.numeric(names(subgroup))
    s_l = sum(label[index_subgroup]==majClass) # number of majority esamples in cluster
    s_s = sum(label[index_subgroup]==minClass) # number of minority esamples in cluster
    if(s_s==0){
      s_s = 1
    }
    cluster_size[1,k] = s_l/s_s
  }
  each_size = (m*size_s)*cluster_size/sum(cluster_size)
  
  if(sum(each_size<0.5)==Nclusters){
    each_size[which.max(each_size)]=0.51 # avoid no example in cluster
  }
  each_size = round(each_size)
  
  ## sampling for each cluster
  total_index_l = NA # index of sampled majority 
  for(k in 1:Nclusters){
    subgroup = cluster$cluster[cluster$cluster==k]
    index_subgroup = as.numeric(names(subgroup))
    index_subgroup = index_subgroup[label[index_subgroup]==majClass]
    index_sampled = sample(index_subgroup,each_size[k],replace = TRUE)
    total_index_l = c(total_index_l,index_sampled)
  }
  total_index_l = total_index_l[-1]
  sampled_majority = total[total_index_l,]

  ## merge sampled examples into whole data
  sampled_majority$Label = majClass
  minority$Label = minClass
  SBC_train = rbind(minority,sampled_majority)
  SBC_train = SBC_train[sample(nrow(SBC_train),nrow(SBC_train)),]
  SBC_train$Label = as.factor(SBC_train$Label)
  SBC_train = shuffle(SBC_train)
  return(SBC_train)
}
################################################################
}
################################################################


#################################### 
##########  SMOTE family  ########## 
#################################### 
if(TRUE){
### SMOTE ###
smote = function(feature,label,N=1,K=5){
      # N: over-sampling rate (+ N times minority)
      # K: the number of nearest neighbors to do SMOTE
  ## find minority class and get its subset
  minClass = levels(label)[which.min(table(label))]
  TF = label == minClass
  minIndex = which(TF)
  minSize = length(minIndex)
  minority = as.matrix(subset(feature,TF))
  n_col = ncol(feature)
  
  KNN = knn.index(minority,k=K,algorithm="kd_tree") # find nearest neighbors
  
  ## create Synthetics set
  size = round(N*minSize)
  Syn = matrix(ncol=n_col)[-1,]
  while(nrow(Syn)<size){
    for(i in 1:minSize) {
      # select randomly 1 of the kNNs
      n <- sample(1:K,1,replace = TRUE)
      # the attribute values of the generated case
      index = KNN[i,n]
      dif <- minority[index,]-minority[i,]
      gap <-  matrix(round(runif(1*n_col,0,1),4),ncol=n_col)
      Syn = rbind(Syn,minority[i,] + dif*gap)
      if(nrow(Syn)==size){
        break
      }
    }
  }
  Syn = as.data.frame(Syn)
  names(Syn) = names(feature)
  Syn$Label = minClass
  
  ## merge data
  smote_train = feature
  smote_train$Label = label
  smote_train = rbind(smote_train,Syn)
  smote_train = shuffle(smote_train)
  return(smote_train)
}
####################################################

### Borderline-SMOTE 1 ###
border_smote1 = function(feature,label,N=1 ,K=5){
      # N: over-sampling rate (+ N times minority)
      # K: the number of nearest neighbors to do SMOTE 
      #    and to find danger examples
  ## find minority class and get its subset
  row.names(feature) = 1:nrow(feature)
  minClass = levels(label)[which.min(table(label))]
  TF = label == minClass
  minIndex = which(TF)
  minSize = length(minIndex)
  minority = as.matrix(subset(feature,TF))
  n_col = ncol(feature)
  size = round(N*minSize) # +N times minority
  ## find danger minority examples
  KNN = knn.index(feature,k=K,algorithm="kd_tree")
  Danger_index = NA
  Danger_index = Danger_index[-1]
  for(i in minIndex){
    m = sum(label[KNN[i,]]!= minClass)
    if(m<K & m>=K/2){ # Danger criteria
      Danger_index = c(Danger_index,i)
    }
  }
  if(length(Danger_index)==0){
    cat("no borderline example")
    ## merge data
    smote_train = feature
    smote_train$Label = label
    smote_train = shuffle(smote_train)
    return(smote_train)
  }
  
  ## index transformation(based on total data -> minority)
  Danger_index_mi = NA
  Danger_index_mi = Danger_index_mi[-1]
  for(i in Danger_index){
    D_index = which(minIndex==i)
    Danger_index_mi = c(Danger_index_mi,D_index)
  }
  
  ## do SMOTE for danger data (KNN only minority)
  KNN_min = knn.index(minority,k=K,algorithm="kd_tree")
  Syn = matrix(ncol=n_col)
  Syn = Syn[-1,]
  while(nrow(Syn)<size){ # sampling until selected size
    for(i in Danger_index_mi) {
      # randomly select 1 of the k NNs
      n <- sample(1:K,1,replace = TRUE)
      # the feature values of the generated case
      index = KNN_min[i,n]
      dif <- minority[index,]-minority[i,]
      gap <-  matrix(round(runif(1*n_col,0,1),4),ncol=n_col)
      Syn = rbind(Syn,minority[i,] + dif*gap)
      if(nrow(Syn)==size){
        break
      }
    }
  }
  Syn = as.data.frame(Syn)
  names(Syn) = names(feature)
  Syn$Label = minClass
  ## merge data
  smote_train = feature
  smote_train$Label = label
  smote_train = rbind(smote_train,Syn)
  smote_train = shuffle(smote_train)
  return(smote_train)
}
####################################################

### Borderline-SMOTE 2 ###
border_smote2 = function(feature,label,N=1 ,K=5){
      # N: over-sampling rate (+ N times minority)
      # K: the number of nearest neighbors to do SMOTE 
      #    and to find danger examples
  ## find minority class and get its subset
  minClass = levels(label)[which.min(table(label))]
  TF = label == minClass
  minIndex = which(TF)
  minSize = length(minIndex)
  minority = as.matrix(subset(feature,TF))
  n_col = ncol(feature)
  
  ## find danger minority examples
  KNN = knn.index(feature,k=K,algorithm="kd_tree")
  Danger_index = NA
  Danger_index = Danger_index[-1]
  for(i in minIndex){
    m = sum(label[KNN[i,]]!= minClass)
    if(m<K & m>=K/2){ # Danger criteria
      Danger_index = c(Danger_index,i)
    }
  }
  if(length(Danger_index)==0){
    cat("no borderline example")
    ## merge data
    smote_train = feature
    smote_train$Label = label
    smote_train = shuffle(smote_train)
    return(smote_train)
  }
  
  ## create the number_generate and generate_index 
  
  l_danger = length(Danger_index)
  size = round(N*minSize) # +N times minority
  temp = size/l_danger
  times = floor(temp)
  r = temp-times
  generate_index = c(rep(Danger_index,times),
                     Danger_index[1:(l_danger*r)])
  generate_table = table(generate_index)
  number_generate = as.numeric(generate_table) # the sampling size for each example
  generate_index = as.numeric(names(generate_table))
  
  ## do SMOTE for danger data
  Syn = matrix(ncol=n_col)
  Syn = Syn[-1,]
  for(i in 1:length(generate_index)) {
    # select randomly N of the k NNs
    number = number_generate[i]
    n <- sample(1:K,number,replace = TRUE)
    # the attribute values of the generated case
    real_i = generate_index[i]
    index = KNN[real_i,n]
    TF = label[index] == minClass
    index_s = index[TF]
    index_l = index[!TF]
    
    # for minority neighbor
    if(length(index_s)>=1){
      dif = feature[index_s,]-feature[rep(real_i,length(index_s)),]
      gap = matrix(round(runif(length(index_s)*n_col,0,1),4),ncol=n_col)
      Syn = rbind(Syn,feature[rep(real_i,length(index_s)),] + dif*gap)
    }
    # for majority neighbor
    if(length(index_l)>=1){
      dif = feature[index_l,]-feature[rep(real_i,length(index_l)),]
      gap = matrix(round(runif(length(index_l)*n_col,0,0.5),4),ncol=n_col)
      Syn = rbind(Syn,feature[rep(real_i,length(index_l)),] + dif*gap)
    }
  }
  
  Syn = as.data.frame(Syn)
  names(Syn) = names(feature)
  Syn$Label = minClass
  ## merge data
  smote_train = feature
  smote_train$Label = label
  smote_train = rbind(smote_train,Syn)
  smote_train = shuffle(smote_train)
  return(smote_train)
}
####################################################

### Safe-level-SMOTE ###
safe_smote = function(feature,label,N=1 ,K=5){
      # N: over-sampling rate (+ N times minority)
      # K: the number of nearest neighbors to do SMOTE 
      #    and to estimate safe level
  ## find minority class and get its subset
  minClass = levels(label)[which.min(table(label))]
  TF = label == minClass
  minIndex = which(TF)
  minSize = length(minIndex)
  minority = as.matrix(subset(feature,TF))
  n_col = ncol(feature)
  size = round(N*minSize)
  
  ## find KNN and calculate safe level
  KNN = knn.index(feature,k=K,algorithm="kd_tree")
  safe_level = NA
  for(i in minIndex){
    sl = sum(label[KNN[i,]]==minClass)
    safe_level = c(safe_level,sl)
  }
  safe_level = safe_level[-1]
  
  ## find KNN for minority and generate synthetic data
  KNN_min = knn.index(minority,k=K,algorithm="kd_tree")
  
  Syn = matrix(ncol=n_col)[-1,]
  while(nrow(Syn) < size){
    for(i in 1:nrow(minority)){
      # get sl_p
      p_KNN = KNN_min[i,]
      sl_p = safe_level[i]
      # get sl_n (n is one of the KNN of p)
      n_index = sample(p_KNN,1)
      sl_n = safe_level[n_index]
      # 4 case
      if(sl_n==0 & sl_p==0){ # 1st case
        next
      }else if(sl_n==0 & sl_p!=0){ # 2nd case
        gap = rep(0,ncol(minority))
      }else if(sl_n==sl_p){ # 3rd case
        gap = runif(ncol(minority),0,1)
      }else if(sl_n < sl_p){ # 4th case
        gap = runif(ncol(minority),0,sl_n/sl_p)
      }else if(sl_n > sl_p){ # 5th case
        gap = runif(ncol(minority),1-(sl_p/sl_n),1)
      }
      p_point = minority[i,]
      n_point = minority[n_index,]
      dif = n_point - p_point
      Syn = rbind(Syn, p_point + dif*gap)
      if(nrow(Syn) == size){
        break # generate data until the amount we want
      }
    }
  }
  Syn = as.data.frame(Syn)
  names(Syn) = names(feature)
  Syn$Label = minClass
  ## merge data
  smote_train = feature
  smote_train$Label = label
  smote_train = rbind(smote_train,Syn)
  smote_train = shuffle(smote_train)
  return(smote_train)
}
####################################################

### ADASYN ###
ADASYN = function(feature,label,B=1,K=5){
      # B: a hyperparameter to specify balance level
      # K: the number of nearest neighbors to do SMOTE 
      #    and to estimate safe level
  ## find minority class and get its subset
  minClass = levels(label)[which.min(table(label))]
  TF = label == minClass
  minIndex = which(TF)
  majIndex = which(!TF)
  minSize = length(minIndex)
  majSize = length(label)-minSize
  minority = as.matrix(subset(feature,TF))
  n_col = ncol(feature)
  
  G = round((majSize - minSize)*B)
  
  ## calculate the number of synthetic examples
  KNN = knn.index(feature,k=K,algorithm="kd_tree")
  ratio = NA
  for(i in minIndex){
    ratio_i = sum(label[KNN[i,]]!= minClass)/K
    ratio = c(ratio,ratio_i)
  }
  ratio = ratio[-1]
  ratio = ratio/sum(ratio)
  g_i  = round(ratio*G)
  
  ## do SMOTE
  KNN = knn.index(minority,k=K,algorithm="kd_tree")
  Syn = matrix(ncol=n_col)[-1,]
  for(i in 1:minSize){
    if(g_i[i]!=0){
      # select randomly N of the k NNs
      n <- sample(1:K,g_i[i],replace = TRUE)
      # the attribute values of the generated case
      index = KNN[i,n]
      dif <- minority[index,]-minority[rep(i,g_i[i]),]
      gap <-  matrix(round(runif(g_i[i]*n_col,0,1),4),ncol=n_col)
      Syn = rbind(Syn,minority[rep(i,g_i[i]),] + dif*gap)
    }
  }
  #row.names(Syn) = 1:nrow(Syn)
  Syn = as.data.frame(Syn)
  names(Syn) = names(feature)
  Syn$Label = minClass
  ## merge data
  smote_train = feature
  smote_train$Label = label
  smote_train = rbind(smote_train,Syn)
  smote_train = shuffle(smote_train)
  return(smote_train)
}
################################################################
}
################################################################


##################################
##########  MBS family  ##########
##################################
if(TRUE){
##### Model-Based Synthetic sampling #####
MBS = function(feature,label,over_rate,iteration=5){
      # over_rate: over-sampling rate
      # iteration: iteration for step 3 of MBS
  ## get minority 
  minClass = levels(label)[which.min(table(label))]
  minIndex = which(label == minClass)
  minority = subset(feature,label==minClass)
  ## create random_set
  n_col = ncol(feature)
  size = round(length(minIndex) * over_rate)
  Rset = as.data.frame(matrix(NA, nrow = size, ncol = n_col))
  names(Rset) = names(minority)
  for(k in 1:n_col){
    index = sample.int(nrow(minority), size, replace=TRUE)
    Rset[,k] = minority[index,k]
  }
  
  ## train feature model and predict
  for(it in 1:iteration){
    predict_set = as.data.frame(matrix(NA, nrow=size, ncol=ncol(feature)))
    names(predict_set) = names(minority)
    for(i in 1:n_col){
      # train feature model
      fo = as.formula( paste(names(feature)[i]," ~ .",sep="") )
      model = lm(fo,minority)
      # predict feature
      predict_set[,i] = predict( model, Rset[,-i] )
    }
    Rset = predict_set
  }
  predict_set$Label = minClass
  ## merge data
  MBS_train = feature
  MBS_train$Label = label
  MBS_train = rbind(MBS_train,predict_set)
  MBS_train = shuffle(MBS_train)
  return(MBS_train)
}
####################################################### 

### MBS with CART ###
MBS_CART = function(feature,label,over_rate,iteration){
      # over_rate: over-sampling rate
      # iteration: iteration for step 3 of MBS
  ## get minority 
  minClass = levels(label)[which.min(table(label))]
  minIndex = which(label == minClass)
  minority = subset(feature,label==minClass)
  ## create random_set
  n_col = ncol(feature)
  size = round(length(minIndex) * over_rate)
  Rset = as.data.frame(matrix(NA, nrow = size, ncol = n_col))
  names(Rset) = names(minority)
  for(k in 1:n_col){
    index = sample.int(nrow(minority), size, replace=TRUE)
    Rset[,k] = minority[index,k]
  }
  ## train feature model and predict
  for(it in 1:iteration){
    predict_set = as.data.frame(matrix(NA, nrow=size, ncol=ncol(feature)))
    names(predict_set) = names(minority)
    for(i in 1:n_col){
      # train feature model
      fo = as.formula( paste(names(feature)[i]," ~ .",sep="") )
      model = rpart(fo,minority, cp=0.001)
      # predict feature
      predict_set[,i] = predict( model, Rset[,-i] )
    }
    Rset = predict_set
  }
  predict_set$Label = minClass
  ## merge data
  MBS_train = feature
  MBS_train$Label = label
  MBS_train = rbind(MBS_train,predict_set)
  MBS_train = shuffle(MBS_train)
  return(MBS_train)
}
####################################################### 

##### MBS with SVR #####
MBS_SVR = function(feature,label,over_rate,iteration){
  ## get minority 
  minClass = levels(label)[which.min(table(label))]
  minIndex = which(label == minClass)
  minority = subset(feature,label==minClass)
  ## create random_set
  n_col = ncol(feature)
  size = round(length(minIndex) * over_rate)
  Rset = as.data.frame(matrix(NA, nrow = size, ncol = n_col))
  names(Rset) = names(minority)
  for(k in 1:n_col){
    index = sample.int(nrow(minority), size, replace=TRUE)
    Rset[,k] = minority[index,k]
  }
  
  ## train feature model and predict
  for(it in 1:iteration){
    predict_set = as.data.frame(matrix(NA, nrow=size, ncol=ncol(feature)))
    names(predict_set) = names(minority)
    for(i in 1:n_col){
      # train feature model
      fo = as.formula( paste(names(feature)[i]," ~ .",sep="") )
      if(sd(minority[,i])==0){
        predict_set[,i] = minority[,i]
        next
      }else{
        model = svm(fo,minority,scale=FALSE,kernel="linear",
                    type="eps-regression",epsilon=0.001)
        # predict feature
        predict_set[,i] = predict( model, Rset[,-i] )
      }
    }
    Rset = predict_set
  }
  predict_set$Label = minClass
  ## merge data
  MBS_train = feature
  MBS_train$Label = label
  MBS_train = rbind(MBS_train,predict_set)
  MBS_train = shuffle(MBS_train)
  return(MBS_train)
}
####################################################### 

# Weight update/ pseudo-loss calculation for AdaBoost.M2
.wt.update <- function(probability, prediction, actual, wt, smooth)
{
  fp <- which(ifelse(prediction == "1" & actual == "0", TRUE, FALSE) == TRUE)
  fn <- which(ifelse(prediction == "0" & actual == "1", TRUE, FALSE) == TRUE)
  p_loss <- 0.5 * sum( wt[fp] * (1 - probability[fp, ][ ,"0"] + probability[fp, ][ ,"1"]),  # pseudo-loss
                       wt[fn] * (1 - probability[fn, ][ ,"1"] + probability[fn, ][ ,"0"]) )
  a <- (p_loss + smooth) / (1 - p_loss + smooth) # weight updater with prediction smoothing, dealing with a == 0
  wt[c(fp, fn)] <- rep(1/(length(fp) + length(fn)), (length(fp) + length(fn)))
  wt[fn] <- wt[fn] * a^(0.5 * (1 + probability[fn, ][ ,"1"] - probability[fn, ][ ,"0"]))
  wt[fp] <- wt[fp] * a^(0.5 * (1 + probability[fp, ][ ,"0"] - probability[fp, ][ ,"1"]))
  wt <- wt / sum(wt)
  result <- list()
  result[[1]] <- wt
  result[[2]] <- a
  return(result)
}

##### MBSBoost #####
MBS_forBoost = function(feature,label,over_rate,iteration=5){
  # over_rate: over-sampling rate
  # iteration: iteration for step 3 of MBS
  ## get minority 
  minClass = levels(label)[which.min(table(label))]
  minIndex = which(label == minClass)
  minority = subset(feature,label==minClass)
  ## create random_set
  n_col = ncol(feature)
  size = round(length(minIndex) * over_rate)
  Rset = as.data.frame(matrix(NA, nrow = size, ncol = n_col))
  names(Rset) = names(minority)
  for(k in 1:n_col){
    index = sample.int(nrow(minority), size, replace=TRUE)
    Rset[,k] = minority[index,k]
  }
  
  ## train feature model and predict
  for(it in 1:iteration){
    predict_set = as.data.frame(matrix(NA, nrow=size, ncol=ncol(feature)))
    names(predict_set) = names(minority)
    for(i in 1:n_col){
      # train feature model
      fo = as.formula( paste(names(feature)[i]," ~ .",sep="") )
      model = lm(fo,minority)
      # predict feature
      predict_set[,i] = predict( model, Rset[,-i] )
    }
    Rset = predict_set
  }
  predict_set$w[predict_set$w<0] = 0
  ## merge data
  MBS_train = rbind(minority,predict_set)
  MBS_train$Label = minClass
  return(MBS_train)
}

MBSBoost = function (formula, data, size, alg, over_rate = 1, rf.ntree = 50, 
                     svm.ker = "radial") {
  target <- gsub(" ", "", unlist(strsplit(format(formula), 
                                          split = "~"))[1])
  list_model <- list()
  a <- 0
  n <- data[which(data[, target] == "0"), ]
  p <- data[which(data[, target] == "1"), ]
  data$w <- rep(1/nrow(data), nrow(data))
  label <- data[, target]
  for (i in 1:size) {
    n <- data[which(data[, target] == "0"), ]
    f <- reformulate(paste(colnames(data)[which(colnames(data) != 
                                                  target & colnames(data) != "w")], collapse = "+"), 
                     response = target)
    feature = data[, colnames(data) != target]
    
    MBS_set = MBS_forBoost(feature, label, over_rate=eval(over_rate),iteration=5)
    
    train <- rbind(n, MBS_set)
    train$w <- train$w/sum(train$w)
    train <- train[sample(nrow(train), nrow(train), replace = TRUE, 
                          prob = train$w), ]
    train$w <- NULL
    if (alg == "svm") {
      list_model[[i]] <- e1071::svm(formula, data = train, 
                                    kernel = svm.ker, probability = TRUE)
      prob <- as.data.frame(attr(predict(list_model[[i]], 
                                         data, probability = TRUE), "prob"))
    }
    else if (alg == "cart") {
      list_model[[i]] <- rpart::rpart(formula, data = train)
      prob <- as.data.frame(predict(list_model[[i]], data, 
                                    type = "prob"))
    }
    else if (alg == "c50") {
      list_model[[i]] <- C50::C5.0(formula, data = train)
      prob <- as.data.frame(predict(list_model[[i]], data, 
                                    type = "prob"))
    }
    else if (alg == "nb") {
      list_model[[i]] <- e1071::naiveBayes(formula, data = train)
      prob <- as.data.frame(predict(list_model[[i]], data, 
                                    type = "raw"))
    }
    else if (alg == "rf") {
      list_model[[i]] <- randomForest::randomForest(formula, 
                                                    data = train, ntree = rf.ntree)
      prob <- as.data.frame(predict(list_model[[i]], data, 
                                    type = "prob"))
    }
    pred <- as.factor(ifelse(prob[, "1"] >= 0.5, 1, 0))
    new <- .wt.update(probability = prob, prediction = pred, 
                      actual = label, wt = data$w, smooth = 1/nrow(data))
    data$w <- new[[1]]
    a[i] <- new[[2]]
  }
  result <- list(weakLearners = list_model, errorEstimation = a)
  attr(result, "class") <- "modelBst"
  return(result)
}

################################################################
}
################################################################


##############################
##########  others  ##########
##############################
if(FALSE){
### No_Modeling ###
No_Modeling = function(feature,label,over_rate){
  ## get minority 
  minClass = levels(label)[which.min(table(label))]
  minIndex = which(label == minClass)
  minority = subset(feature,label==minClass)
  ## create random_set
  n_col = ncol(feature)
  size = round(length(minIndex) * over_rate)
  Rset = as.data.frame(matrix(NA, nrow = size, ncol = n_col))
  names(Rset) = names(minority)
  for(k in 1:n_col){
    index = sample.int(nrow(minority), size, replace=TRUE)
    Rset[,k] = minority[index,k]
  }
  
  Rset$Label = minClass
  ## merge data
  MBS_train = feature
  MBS_train$Label = label
  MBS_train = rbind(MBS_train,Rset)
  MBS_train = shuffle(MBS_train)
  return(MBS_train)
}
####################################################### 

### MBS_each_sample ###
MBS_each_sample = function(feature,label,over_rate,iteration){
  ## get minority 
  minClass = levels(label)[which.min(table(label))]
  minIndex = which(label == minClass)
  minority = subset(feature,label==minClass)
  ## create random_set
  n_col = ncol(feature)
  size = round(length(minIndex) * over_rate)
  Rset = as.data.frame(matrix(NA, nrow = size, ncol = n_col))
  names(Rset) = names(minority)
  index = sample.int(nrow(minority), size, replace=TRUE)
  Rset = minority[index,]
  ## train feature model and predict
  for(it in 1:iteration){
    predict_set = as.data.frame(matrix(NA, nrow=size, ncol=ncol(feature)))
    names(predict_set) = names(minority)
    for(i in 1:n_col){
      # train feature model
      fo = as.formula( paste(names(feature)[i]," ~ .",sep="") )
      model = lm(fo,minority)
      # predict feature
      predict_set[,i] = predict( model, Rset[,-i] )
    }
    Rset = predict_set
  }
  predict_set$Label = minClass
  ## merge data
  MBS_train = feature
  MBS_train$Label = label
  MBS_train = rbind(MBS_train,predict_set)
  MBS_train = shuffle(MBS_train)
  return(MBS_train)
}
####################################################### 

##### MBS random generator #####
MBS_RG = function(feature,label,over_rate,iteration){
  ## get minority 
  minClass = levels(label)[which.min(table(label))]
  minIndex = which(label == minClass)
  minority = subset(feature,label==minClass)
  ## create random_set
  n_col = ncol(feature)
  size = round(length(minIndex) * over_rate)
  Rset = as.data.frame(matrix(rnorm(size*n_col,0,1), nrow = size, ncol = n_col))
  names(Rset) = names(minority)
  
  ## train feature model and predict
  for(it in 1:iteration){
    predict_set = as.data.frame(matrix(NA, nrow=size, ncol=ncol(feature)))
    names(predict_set) = names(minority)
    for(i in 1:n_col){
      # train feature model
      fo = as.formula( paste(names(feature)[i]," ~ .",sep="") )
      model = lm(fo,minority)
      # predict feature
      predict_set[,i] = predict( model, Rset[,-i] )
    }
    Rset = predict_set
  }
  predict_set$Label = minClass
  ## merge data
  MBS_train = feature
  MBS_train$Label = label
  MBS_train = rbind(MBS_train,predict_set)
  MBS_train = shuffle(MBS_train)
  return(MBS_train)
}
####################################################### 

##### MBS_no_sample #####
MBS_no_sample = function(feature,label,over_rate,iteration){
  ## get minority 
  minClass = levels(label)[which.min(table(label))]
  minIndex = which(label == minClass)
  minority = subset(feature,label==minClass)
  
  n_col = ncol(feature)
  size = round(length(minIndex) * over_rate)
  
  NRSet = minority
  ## train feature model and predict
  predict_set = as.data.frame(matrix(NA, nrow=size, ncol=ncol(feature)))
  names(predict_set) = names(minority)
  for(it in 1:iteration){
    for(i in 1:n_col){
      # train feature model
      fo = as.formula( paste(names(feature)[i]," ~ .",sep="") )
      model = lm(fo,minority)
      # predict feature
      predict_set[,i] = predict( model, NRSet[,-i] )
    }
    NRSet = predict_set
  }
  predict_set$Label = minClass
  ## merge data
  MBS_train = feature
  MBS_train$Label = label
  MBS_train = rbind(MBS_train,predict_set)
  MBS_train = shuffle(MBS_train)
  return(MBS_train)
}
################################################################
}
################################################################