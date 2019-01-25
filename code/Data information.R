########################################
##### Data information integration #####
########################################

##### Path setting #####
Root_folder = "D:/Github/Model-Based-Synthetic-Sampling/"
basic_path = paste(Root_folder,"data/",sep="")
result_path = paste(Root_folder,"result/",sep="")

data_names = c("Pima_data", "Haberman_data","Satimage_data", 
               "Ecoli_data", "Shuttle_data", "Ionosphere_data",
               "Vehicle_data", "Credit_data","Diabetes_data", 
               "Hmeq_data", "Promotion_data", "Bank_data", "Spambase_data")

folder_names = c("Pima", "Haberman", "Satimage", 
                 "Ecoli", "Shuttle", "Ionosphere Data Set", 
                 "Statlog (Vehicle Silhouettes) Data Set", "Give Me Some Credit", 
                 "diabetes", "hmeq", "promotion", "bank","spambase")

#-------------------------------------------------------------------------#

build_path = function(path){if(!dir.exists(path)){dir.create(path)}}
build_path(result_path)

Data_information = function(Folder_name, Data_name){
  data_path = paste(basic_path,Folder_name,sep="")
  info = list(data_path=data_path,data_name=Data_name)
  return(info)
}

numData = length(data_names)
if (numData != length(folder_names)) {
  log("The number of data_names do not correspond to the number of folder_names")
}

Total = list()
for (i in 1:numData) {
  Data = Data_information(folder_names[i],data_names[i])
  Total[[i]] = Data
}

#-------------------------------------------------------------------------#


