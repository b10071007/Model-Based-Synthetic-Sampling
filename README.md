# Model-Based-Synthetic-Sampling

### Dataset description

+ **Pima Indians Diabetes** : 
    This dataset is originated from the National Institutes of Health, and it is used to identify whether people suffer from diabetes or not based on personal information and physiological indices. The percentage of patients with diabetes is 34.9\%.
    http://ftp.ics.uci.edu/pub/machine-learning-databases/pima-indians-diabetes/
    
+ **Haberman's survival** : 
    This dataset is originated from the Hospital of the University of Chicago, and it is used to predict whether a patient died within 5 years after surgical operation. The proportion of death cases is about 26.5\%.
    https://archive.ics.uci.edu/ml/datasets/Haberman's+Survival

+ **Satimage** : 
    This dataset contains satellite images from the NASA database. It is used to identify the surface of the earth by pixels of different spectrum. We selected the smallest class "damp gray soil" as the minority data and merged other classes together to form the majority data according to the settings described in [1].
    https://archive.ics.uci.edu/ml/datasets/Statlog+(Landsat+Satellite)

+ **E.coli** : 
    This dataset is from a study by Nakai and Kanehisa. They predicted the position of protein in a cell via various biochemistry indicators. We selected the "imU" as the minority class and the other classes as the majority class as used in [2].
    https://archive.ics.uci.edu/ml/datasets/ecoli
    

+ **Shuttle** : 
    This dataset is from the UCI machine learning repository. According to the setting used in [3], we selected "Fpv Close" as the minority class and combined other classes to form the majority class. Note that the minority data accounts for only 0.08\%. 
    https://archive.ics.uci.edu/ml/datasets/Statlog+(Shuttle)

+ **Ionosphere** : 
    The radar data was collected by a system in Goose Bay, Labrador. The goal is to identify "Good" radar, which means showing evidence of some type of structure in the ionosphere, via 16 high-frequency antennas.
	https://archive.ics.uci.edu/ml/datasets/ionosphere

+ **Vehicle** : 
    The data was collected by JP Siebert, and the purpose of this data is to identify the type of vehicle via some features, which were extracted from the silhouette. We selected the "Van" as the minority class and the remaining classes as the majority class according to the setting used in [3].
    https://archive.ics.uci.edu/ml/datasets/Statlog+(Vehicle+Silhouettes)

+ **"Give me some credit"** : 
    This is a competition about credit scoring on Kaggle. Based on historical credit records and loan conditions, the goal is to predict whether the clients will encounter financial problem within two years.
    https://www.kaggle.com/c/GiveMeSomeCredit

+ **"Diabetes"** : 
	The dataset, released on Kaggle, represents 10 years (1999-2008) of clinical care at 130 US hospitals and integrated delivery networks. The objective is to know if a patient will be readmitted in some hospital. We choosed "readmission in less than 30 days" as the minority class and the remainder as the majority class.
	https://www.kaggle.com/brandao/diabetes/home

+ **"Hmeq"** : 
	The "Home Equity dataset" (Hmeq) on Kaggle contains baseline and loan performance information for 5,960 recent home equity loans. The target is to predict clients who eventually default on their loans.
	https://www.kaggle.com/ajay1735/hmeq-data/home

+ **"Promotion"** : 
	The "Promotion response and target datasets" is available ob Kaggle. It is used to build a model to select the best customers for second round of promotion from the pool of customers not contacted.
	https://www.kaggle.com/regivm/promotion-response-and-target-datasets/home

+ **"Bank"** : 
	The "Bank Marketing Data Set" on UCI is related to direct marketing campaigns (phone calls) of a Portuguese banking institution. The goal is to predict whether the client will subscribe a term deposit or not.
	https://archive.ics.uci.edu/ml/datasets/Bank+Marketing

+ **"Spambase"** :
	This dataset from UCI is related to the spam mail filter. The objective is to classify email as spam or non-spam according to the content of the mail.
	https://archive.ics.uci.edu/ml/datasets/Spambase


### Reference
[1] Bunkhumpornpat, C., Sinapiromsaran, K., & Lursinsap, C. (2009, April). Safe-level-smote: Safe-level-synthetic minority over-sampling technique for handling the class imbalanced problem. *In Pacific-Asia conference on knowledge discovery and data mining* (pp. 475-482). Springer, Berlin, Heidelberg.

[2] Batista, G. E., Prati, R. C., & Monard, M. C. (2004). A study of the behavior of several methods for balancing machine learning training data. *ACM SIGKDD explorations newsletter*, 6(1), 20-29.

[3] Chen, S., He, H., & Garcia, E. A. (2010). RAMOBoost: ranked minority oversampling in boosting. *IEEE Transactions on Neural Networks*, 21(10), 1624-1642.