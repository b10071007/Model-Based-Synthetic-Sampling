########################################
##### data information integration #####
########################################

##### Data imformation #####
Root_folder = "D:/Github/Model-Based-Synthetic-Sampling/"
basic_path = paste(Root_folder,"data/",sep="")
result_path = paste(Root_folder,"result/",sep="")
Data_name  = c("Pima","Haberman","Satimage","Ecoli",
               "shuttle","Ionosphere","Vehicle","Credit")

build_path = function(path){if(!dir.exists(path)){dir.create(path)}}
build_path(result_path)

Data_information = function(Folder_name,Data_name){
  data_path = paste(basic_path,folder_name,sep="")
  info = list(data_path=data_path,data_name=Data_name)
  return(info)
}

### Pima data ### 
folder_name = "Pima"
data_name = "Pima_data"
Data1 = Data_information(folder_name,data_name)

### Haberman data ### 
folder_name = "Haberman"
data_name = "Haberman_data"
Data2 = Data_information(folder_name,data_name)

### Satimage data ### 
folder_name = "Satimage"
data_name = "Satimage_data"
Data3 = Data_information(folder_name,data_name)

### Ecoli data ### 
folder_name = "Ecoli"
data_name = "Ecoli_data"
Data4 = Data_information(folder_name,data_name)

### shuttle data ### 
folder_name = "Shuttle"
data_name = "Shuttle_data"
Data5 = Data_information(folder_name,data_name)

### Ionosphere data ### 
folder_name = "Ionosphere Data Set"
data_name = "Ionosphere_data"
Data6 = Data_information(folder_name,data_name)

### Vehicle Data ### 
folder_name = "Statlog (Vehicle Silhouettes) Data Set"
data_name = "Vehicle_data"
Data7 = Data_information(folder_name,data_name)

### Credit data ### 
folder_name = "Give Me Some Credit"
data_name = "Credit_data"
Data8 = Data_information(folder_name,data_name)

#---------------------------------------------------#

Total = list(Data1,Data2,Data3,Data4,Data5,Data6,Data7,Data8)

#---------------------------------------------------#


