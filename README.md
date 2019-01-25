# Model-Based-Synthetic-Sampling

### Dataset description

+ **Pima Indians Diabetes** : 
    This dataset is originated from the National Institutes of Health, and it is used to identify whether people suffer from diabetes or not based on personal information and physiological indices. The percentage of patients with diabetes is 34.9\%.
    
+ **Haberman's survival** : 
    This dataset is originated from the Hospital of the University of Chicago, and it is used to predict whether a patient died within 5 years after surgical operation. The proportion of death cases is about 26.5\%.
    
+ **Satimage** : 
    This dataset contains satellite images from the NASA database. It is used to identify the surface of the earth by pixels of different spectrum. We selected the smallest class "damp gray soil" as the minority data and merged other classes together to form the majority data according to the settings described in \cite{bunkhumpornpat2009safe}.
    
+ **E.coli** : 
    This dataset is from a study by Nakai and Kanehisa. They predicted the position of protein in a cell via various biochemistry indicators. We selected the "imU" as the minority class and the other classes as the majority class as used in \cite{batista2004study}.%, {\color{red}on inner membrane with uncleavable signal sequence},
    
+ **Shuttle** : 
    This dataset is from the UCI machine learning repository. According to the setting used in \cite{chen2010ramoboost}, we selected "Fpv Close" as the minority class and combined other classes to form the majority class. Note that the minority data accounts for only 0.08\%. 
    
+ **Ionosphere** : 
    The radar data was collected by a system in Goose Bay, Labrador. The goal is to identify "Good" radar, which means showing evidence of some type of structure in the ionosphere, via 16 high-frequency antennas.

+ **Vehicle** : 
    The data was collected by JP Siebert, and the purpose of this data is to identify the type of vehicle via some features, which were extracted from the silhouette. We selected the "Van" as the minority class and the remaining classes as the majority class according to the setting used in \cite{chen2010ramoboost}.
    
+ **"Give me some credit"** : 
    This is a competition about credit scoring on Kaggle. Based on historical credit records and loan conditions, the goal is to predict whether the clients will encounter financial problem within two years.
