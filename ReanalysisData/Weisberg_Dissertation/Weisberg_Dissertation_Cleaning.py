# -*- coding: utf-8 -*-
"""
Created on Mon May 17 14:20:11 2021

@author: Steven Weisberg
"""

#%%
import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt

baseDir = 'C:\\Users\\smwei\\Dropbox (UFL)\\Virtual_Ambler\\\Fixing_Silcton\\OSF\\ReanalysisData'

# Load in dataset part 1
df_1 = pd.read_spss(os.path.join(baseDir,'Weisberg_Dissertation','Dissertation_Part_1_Master_Spring_1.sav'))
df_1 = df_1[df_1['Within_Pointing'].notna()]
df_1 = df_1[df_1['Between_Pointing'].notna()]
# Load in dataset part 2
df_2 = pd.read_spss(os.path.join(baseDir,'Weisberg_Dissertation','Dissertation_Part_2_Master_1.sav'))
df_2 = df_2[df_2['Within_Pointing'].notna()]
df_2 = df_2[df_2['Between_Pointing'].notna()]

#%% Replicate a few analyses from JEPLMC 2016 paper

# Correlation between OSpan and SymSpan
print(np.ma.corrcoef(np.ma.masked_invalid(df_1.OSpan_Total),np.ma.masked_invalid(df_1.SymSpan_Total)))

# Group means and SDs for route membership task
print(df_1.groupby('Pointing_Groups_Intrinsic')['Num_Correct'].mean()/64)
print(df_1.groupby('Pointing_Groups_Intrinsic')['Num_Correct'].std()/64)

print(df_2.groupby('Pointing_Clusters_Intrinsic')['Shortcut_Goals_Found'].mean())
print(df_2.groupby('Pointing_Clusters_Intrinsic')['Shortcut_Goals_Found'].std())

#%% Interpolate pointing data

from sklearn import linear_model

# Training_Data.csv is the data used to train the model.
# These data were coded to have both buggy data AND corrected data, and are used to train the regression model. 
# The data are stored in the repo with this notebook, so you do not need to change this. 
df_train_path = os.path.join(baseDir,'Training_Data.csv')

df_train = pd.read_csv(df_train_path, encoding = 'unicode_escape', sep = ",")

# We train only on new data
df_train = df_train[df_train.New_or_Original=='New']

# Separate models for within and between 
X_train_within = df_train[['bad_pointing_coding_within']]
y_train_within = df_train[['good_pointing_coding_within']]


within_model = linear_model.LinearRegression()
within_model.fit(X_train_within,y_train_within)

X_train_between = df_train[['bad_pointing_coding_between']]
y_train_between = df_train[['good_pointing_coding_between']]

between_model = linear_model.LinearRegression()
between_model.fit(X_train_between,y_train_between)

for df in df_1,df_2:
    
    # Now calculate participant and within/between averages. 
    grouped_within = df[['Within_Pointing']]
    grouped_between = df[['Between_Pointing']]
    
    # we store in an array all predicted (corrected) angles
    df['Within_Pointing_Corrected'] = within_model.predict(grouped_within)
    df['Between_Pointing_Corrected'] = between_model.predict(grouped_between)
    
    
    
    
    fig, ax = plt.subplots()
    ax.scatter(df['Within_Pointing_Corrected'],df['Within_Pointing'])
    ax.set_title('Within Route Error')
    ax.set_xlabel('Corrected Average Error')
    ax.set_ylabel('Bug Average Error')
    
    fig, ax = plt.subplots()
    ax.scatter(df['Between_Pointing_Corrected'],df['Between_Pointing'])
    ax.set_title('Within Route Error')
    ax.set_xlabel('Corrected Average Error')
    ax.set_ylabel('Bug Average Error')
    ax.set_title('Between Route Error')
    ax.set_xlabel('Corrected Average Error')
    ax.set_ylabel('Bug Average Error')
    
    fig, ax = plt.subplots()
    ax.scatter(df['Between_Pointing_Corrected'],df['Within_Pointing_Corrected'], 
               c='blue',label='Corrected Average Error')
    ax.scatter(df['Between_Pointing'],df['Within_Pointing'],
               c='red',label='Bug Average Error')
    
    plt.legend()
    
    plt.show()


#%% Table 2 is unchanged (a linear adjustment)

table2_data = df_2[['Within_Pointing','Between_Pointing','Goals_Using_Shortcut','Goals_Using_Route','Goals_Found','Model_Building_Within_average','Model_Building_Total']]


table2_original = np.ma.corrcoef(np.ma.masked_invalid(table2_data.T))

table2_data_corrected = df_2[['Within_Pointing_Corrected','Between_Pointing_Corrected','Goals_Using_Shortcut','Goals_Using_Route','Goals_Found','Model_Building_Within_average','Model_Building_Total']]

table2_corrected = np.ma.corrcoef(np.ma.masked_invalid(table2_data_corrected.T))


#%% Cluster analysis won't change either.

from sklearn.cluster import KMeans

X = df_2[['Between_Pointing','Within_Pointing']]
kmeans = KMeans(n_clusters=3)
kmeans.fit(X)
y_kmeans = kmeans.predict(X)

fig, ax = plt.subplots()
ax.scatter(X.iloc[:, 0], X.iloc[:, 1], c=y_kmeans, s=50, cmap='viridis')

centers = kmeans.cluster_centers_
ax.scatter(centers[:, 0], centers[:, 1], c='black', s=200, alpha=0.5)

fig, ax = plt.subplots()
X = df_2[['Between_Pointing_Corrected','Within_Pointing_Corrected']]
kmeans = KMeans(n_clusters=3)
kmeans.fit(X)
y_kmeans = kmeans.predict(X)

ax.scatter(X.iloc[:, 0], X.iloc[:, 1], c=y_kmeans, s=50, cmap='viridis')

centers = kmeans.cluster_centers_
ax.scatter(centers[:, 0], centers[:, 1], c='black', s=200, alpha=0.5)