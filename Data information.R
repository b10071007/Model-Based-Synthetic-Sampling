########################################
##### data information integration #####
########################################

##### Data imformation #####
folder = "D://Research(MBS)//"
basic_path = paste(folder,"data//",sep="")
plot_path = paste(folder,"plot//",sep="")
time_path = paste(folder,"time//",sep="")

build_path = function(path){
  if(!dir.exists(path)){
    dir.create(path)
  }
}
build_path(plot_path)
build_path(time_path)

Data_information = function(Folder_name,Data_name){
  data_path = paste(basic_path,folder_name,sep="")
  data_name = Data_name
  result_path = paste(data_path,"/result",sep="")
  build_path(result_path)
  infor=list(data_path=data_path,data_name=data_name,
             result_path=result_path)
  return(infor)
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

Total = list(Data1,Data2,Data3,Data4,
             Data5,Data6,Data7,Data8)

#---------------------------------------------------#


