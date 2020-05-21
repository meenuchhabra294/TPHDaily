#Importing the libraries

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.cluster import DBSCAN
#from collections import Counter
import pickle

dataset=pd.read_excel("daily data fillna 02042020.xlsx")
dataset.columns
dataset.isna().sum()
#dataset['FA_Blaine_n']=dataset['FA_Blaine'].ffill()
#dataset['FA_Blaine_n']=dataset['FA_Blaine_n'].bfill()
#dataset['FA_Blaine_n'].mean()
dataset = dataset.apply(pd.to_numeric, errors='coerce')

dataset1=dataset.loc[:,["RP_power","Ball_Mill","RP_Bin_Level","Clinker","BM_Frate","SFbyS","PA_prop","S_ByDamper","TPH"]]
dataset1.head()

dataset1.skew()

def stand(x):
    return((x-np.mean(x))/np.std(x))

#np.mean(dataset1['Cement_So3'])
dataset3=dataset1.apply(lambda x: stand(x))
dataset3.describe()

# DBSCAN model with parameters
mod1=DBSCAN(eps=2.9,min_samples=4).fit(dataset3)





# Creating Panda DataFrame with Labels for Outlier Detection
outlier_df = pd.DataFrame(dataset3)

# Printing total number of values for each label
#print(Counter(mod1.labels_))


# Printing DataFrame being considered as Outliers -1
print(outlier_df[mod1.labels_ == -1])

# Printing and Indicating which type of object outlier_df is
print(type(outlier_df))

x1=np.mean(dataset3)
x2=np.std(dataset3) 
dataset4=dataset3*dataset1.std()+dataset1.mean()
round(dataset4.head(),2)
dataset1.head()
dataset4=dataset4[mod1.labels_ != -1]

X = dataset4.iloc[:, 0:8].values
y = dataset4.iloc[:, 8].values


# Splitting the dataset into the Training set and Test set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 123)
from sklearn.ensemble import RandomForestRegressor
regressor = RandomForestRegressor(n_estimators = 500, max_features=3, random_state = 123) 
 
# fit the regressor with x and y data 
regressor.fit(X_train, y_train) 
importance=list(np.array(regressor.feature_importances_))
importance
X_test[0]
pickle.dump(regressor, open('model.pkl','wb'))

# Loading model to compare the results
model = pickle.load(open('model.pkl','rb'))
#print(model.predict(temp))
print(model.predict([[2200,2150,88,61,210,3.8,2,67]]))
print(model.predict([X_test[0]]))
predictions=regressor.predict(X_test)
act=y_test

rss1 = sum((predictions - act)**2)
tss1 = sum((act - np.mean(act))**2)
rsq1 =  1 - rss1/tss1
rsq1
