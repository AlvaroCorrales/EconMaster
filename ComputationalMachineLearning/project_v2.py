# -*- coding: utf-8 -*-
"""
Created on Sat Feb 10 12:54:35 2018

@author: Jordi
"""
"""


import matplotlib.pyplot as plt
"""
import sklearn
import seaborn
import numpy as np
import pandas as pd
import os

os.chdir('C:/Users/Jordi/Google Drive/BGSE/2nd Term/CML/Kaggle/')

X_train = pd.read_csv('X_train.dat')
X_test = pd.read_csv('X_test.dat')
y_train = pd.read_csv('y_train.dat', header=None)
sample = pd.read_csv('sample.dat')


X_train.head(6)

# Initialize linear regression object
from sklearn import datasets, linear_model

regr  = linear_model.LinearRegression()

# Train the model

regr.fit(X_train, y_train)

print ('Coefficients: \n', regr.coef_)

#plt.scatter(X_train, y_train, color='black')

#Ridge Regression
from sklearn.linear_model import Ridge
rreg = Ridge(alpha = 0.001)
rreg.fit(X_train, y_train)

print ('Coefficients: \n', rreg.coef_)

#Lasso
from sklearn.linear_model import Lasso

lreg = Lasso(alpha = 0.1)

lreg.fit(X_train, y_train)

print ('Coefficients: \n', lreg.coef_)  #many coefs set to 0

# Training Logistic regression

from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(X_train, y_train, test_size=0.2,random_state=0)


logistic_classifier = linear_model.LogisticRegression(C=100.0)
logistic_classifier.fit(X_train, y_train.values[:,1])

#Cross-Validation
from sklearn.model_selection import cross_val_score
scores = cross_val_score(logistic_classifier, X_train, y_train.values[:,1], cv=10)
scores

print("Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))

#Download again data - Alvaro style
import os
import sklearn
import seaborn
import numpy as np
import pandas as pd
import scipy
from sklearn.model_selection import train_test_split
from sklearn import datasets, linear_model
import arff
import matplotlib.pyplot as plt

os.chdir('C:/Users/USUARIO/Google Drive/Kaggle/')

data_X = pd.read_csv('X_train.dat', header=0,)
data_y = pd.read_csv('y_train.dat', header=None, names=['Particle', 'Type'])
data_X.rename(columns={'Unnamed: 0':'Particle'}, inplace=True)
data_all = pd.merge(data_X, data_y, on='Particle', how='right')


df = pd.DataFrame(data_all)

df= df.drop([27610,14910])

#df = df[df.Particle != 27610]
#df = df[df.Particle != 14910]

df = df.drop(df.columns[[18,21,22,44,45,47,48,49,50,51]],axis=1) #Pandas starts counting at 0

data = np.array(df)

target = pd.DataFrame(np.column_stack((df.values[:,0],df.values[:,69])), columns=("Id","Type"))
X = df.drop("Type",axis=1)

#target = data[:,69]

#X = df[:,0:68]

#Lasso
from sklearn.linear_model import Lasso

lreg = Lasso(alpha = 0.001)

lreg.fit(X, target)

print ('Coefficients: \n', lreg.coef_)  #many coefs set to 0

#Select variables different than 0.0
vars = lreg.coef_ != 0.0
X_clean = X[:,vars]

X_train, X_test, y_train, y_test= train_test_split(X, target, test_size=0.2,random_state=0)


logistic_binary = linear_model.LogisticRegression(C=100.0)

#Run normal logit
X_train = pd.read_csv('X_train.dat')
X_test = pd.read_csv('X_test.dat')
y_train = pd.read_csv('y_train.dat', header=None)
sample = pd.read_csv('sample.dat')

lg2 = pd.read_csv('lg.csv')
logistic_binary.fit(X_train, y_train)

result = logistic_binary.predict(X_test.astype(int))

send = pd.DataFrame(np.column_stack((X_test.values[:,0],result)), columns=("Id","Prediction"))

import csv

with writer('result.csv') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
         print(row['first_name'], row['last_name'])

fpr, tpr, thresholds = sklearn.metrics.roc_curve(y_test, logistic_binary.predict(X_test), pos_label=1)
auc = sklearn.metrics.auc(fpr, tpr)
auc

result = logistic_binary.predict(X_test)
result[result>=0.5] = 1
result[result<0.5] = 0

send = pd.DataFrame(np.column_stack((X_test.values[:,0],result)), columns=("Id","Prediction"))

plt.figure()

plt.plot(fpr, tpr, color = "red", lw = 2, label='ROC curve (area = %0.2f)' % auc)

plt.plot([0, 1], [0, 1], color='navy', lw = 2, linestyle='--')

plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Receiver operating characteristic example')
plt.legend(loc="lower right")
plt.show()


from sklearn.metrics import confusion_matrix
import seaborn as sns
mat = confusion_matrix(y_test, logistic_binary.predict(X_test))
sns.heatmap(mat.T, square=True, annot=True, fmt='d', cbar=False, xticklabels=['-1','1'], yticklabels=['-1','1'])
plt.xlabel('true label')
plt.ylabel('predicted label');
plt.show()

#more 0's predicted as 1's than 1's predicted as 0's, but more or less balanced

# Random Forest
from sklearn.tree import DecisionTreeRegressor
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score

random_forest = RandomForestClassifier(max_depth=100, n_estimators=20)
random_forest.fit(X_train, y_train)

scores_rf = cross_val_score(random_forest, X_train, y_train, cv=10)
scores_rf

# Kneighbors
from sklearn.neighbors import KNeighborsClassifier
k_neighbor_class = KNeighborsClassifier(10)
scores_k = cross_val_score(k_neighbor_class, X_train, y_train, cv=10)
scores_k

# Naive Bayes
from sklearn.naive_bayes import GaussianNB
naive_class=GaussianNB()
scores_nv = cross_val_score(naive_class, X_clean, target, cv=10)
scores_nv


#Gradient Boosting regression
import numpy as np

import matplotlib.pyplot as plt
from sklearn.ensemble import GradientBoostingRegressor


#Given values: n_estimators=500, max_depth = 4, learning_rate = 0.01, loss="ls"
clf = GradientBoostingRegressor(n_estimators=600, max_depth = 5, learning_rate = 0.01, loss='ls')

clf.fit(X, target.values[:,1])
    

fpr, tpr, thresholds = sklearn.metrics.roc_curve(y_test, clf.predict(X_test), pos_label=1)
auc_gbr = sklearn.metrics.auc(fpr, tpr)
auc_gbr

data_X_test = pd.read_csv('X_test.dat', header=0,)
data_X_test.rename(columns={'Unnamed: 0':'Particle'}, inplace=True)
X_test = pd.DataFrame(data_X_test)
X_test = X_test.drop(X_test.columns[[18,21,22,44,45,47,48,49,50,51]],axis=1) #Pandas starts counting at 0


result = clf.predict(X_test)


result[result>=0.5] = 1
result[result<0.5] = 0

send = pd.DataFrame(np.column_stack((X_test.values[:,0],result)), columns=("Id","Prediction"))
