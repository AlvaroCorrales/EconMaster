# -*- coding: utf-8 -*-
"""
Created on Mon Feb 12 09:40:33 2018

@author: Jordi
"""

#Clean project
import numpy as np
import pandas as pd
import os

os.chdir('C:/Users/Jordi/Google Drive/BGSE/2nd Term/CML/Kaggle/')


data_X = pd.read_csv('X_train.dat', header=0,)
data_y = pd.read_csv('y_train.dat', header=None, names=['Particle', 'Type'])
y_train = pd.read_csv('y_train.dat', header=None)
sample = pd.read_csv('sample.dat')


data_X.rename(columns={'Unnamed: 0':'Particle'}, inplace=True)
data_all = pd.merge(data_X, data_y, on='Particle', how='right')


df = pd.DataFrame(data_all)

df= df.drop([27610,14910])

#df = df[df.Particle != 27610]
#df = df[df.Particle != 14910]

df = df.drop(df.columns[[18,21,22,44,45,47,48,49,50,51]],axis=1) #Pandas starts counting at 0

#replace 999's
#np.nan = (df.values[:,1:69] == 999)
#df[:, 1:69].replace(999,np.nan)
#df.replace(999,np.nan)



data = np.array(df)

target = pd.DataFrame(np.column_stack((df.values[:,0],df.values[:,69])), columns=("Id","Type"))
X = df.drop("Type",axis=1)

from sklearn.ensemble import GradientBoostingRegressor

clf = GradientBoostingRegressor(n_estimators=600, max_depth = 5, learning_rate = 0.0015, loss='ls', verbose =1)

clf.fit(X, target.values[:,1])

data_X_test = pd.read_csv('X_test.dat', header=0,)
data_X_test.rename(columns={'Unnamed: 0':'Particle'}, inplace=True)
X_test = pd.DataFrame(data_X_test)
X_test = X_test.drop(X_test.columns[[18,21,22,44,45,47,48,49,50,51]],axis=1) #Pandas starts counting at 0


result = clf.predict(X_test)


#result[result>=0.5] = 1
#result[result<0.5] = 0

send = pd.DataFrame(np.column_stack((X_test.values[:,0],result)), columns=("Id","Prediction"))

send.to_csv('python.csv', index=False)

check = pd.read_csv('python.csv')

