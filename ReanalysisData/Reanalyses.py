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
from sklearn import linear_model

baseDir = os.path.dirname(os.path.realpath(__file__))


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

def applyCorrection(df,within,between,plot=True,within_model=within_model,between_model=between_model):

    # Now calculate participant and within/between averages. 
    within_buggy = df[[within]]
    between_buggy = df[[between]]
    
    # Filter out the nans
    within_buggy_filter = within_buggy.isna()
    between_buggy_filter = between_buggy.isna()
    
    # Create a copy of each of these
    within_corrected = within_buggy.copy()
    between_corrected = between_buggy.copy()
    
    # we store in an array all predicted (corrected) angles
    within_corrected[~within_buggy_filter[within]] = within_model.predict(within_buggy[~within_buggy_filter[within]])
    between_corrected.loc[~between_buggy_filter[between]] = between_model.predict(between_buggy[~between_buggy_filter[between]])
    
    
    if plot:
        
        fig, ax = plt.subplots()
        ax.scatter(within_corrected,within_buggy)
        ax.set_title('Within Route Error')
        ax.set_xlabel('Corrected Average Error')
        ax.set_ylabel('Bug Average Error')
        plt.show()
    
        fig, ax = plt.subplots()
        ax.scatter(between_corrected,between_buggy)
        ax.set_title('Within Route Error')
        ax.set_xlabel('Corrected Average Error')
        ax.set_ylabel('Bug Average Error')
        ax.set_title('Between Route Error')
        ax.set_xlabel('Corrected Average Error')
        ax.set_ylabel('Bug Average Error')
        
        fig, ax = plt.subplots()
        ax.scatter(between_corrected,within_corrected, 
                   c='blue',label='Corrected Average Error')
        ax.scatter(between_buggy,within_buggy,
                   c='red',label='Bug Average Error')
        
        plt.legend()
        
        plt.show()
        
    return within_corrected,between_corrected

#%% Weisberg data reanalysis

# Load in dataset part 1
df_1 = pd.read_spss(os.path.join(baseDir,'Weisberg_Dissertation','Dissertation_Part_1_Master_Spring_1.sav'))
df_1 = df_1[df_1['Within_Pointing'].notna()]
df_1 = df_1[df_1['Between_Pointing'].notna()]
# Load in dataset part 2
df_2 = pd.read_spss(os.path.join(baseDir,'Weisberg_Dissertation','Dissertation_Part_2_Master_1.sav'))
df_2 = df_2[df_2['Within_Pointing'].notna()]
df_2 = df_2[df_2['Between_Pointing'].notna()]


# Replicate a few analyses from JEPLMC 2016 paper

# Correlation between OSpan and SymSpan
print(np.ma.corrcoef(np.ma.masked_invalid(df_1.OSpan_Total),np.ma.masked_invalid(df_1.SymSpan_Total)))

# Group means and SDs for route membership task
print(df_1.groupby('Pointing_Groups_Intrinsic')['Num_Correct'].mean()/64)
print(df_1.groupby('Pointing_Groups_Intrinsic')['Num_Correct'].std()/64)

print(df_2.groupby('Pointing_Clusters_Intrinsic')['Shortcut_Goals_Found'].mean())
print(df_2.groupby('Pointing_Clusters_Intrinsic')['Shortcut_Goals_Found'].std())

# Interpolate pointing data for Weisberg dissertation

for df in df_1,df_2:
    df['Within_Pointing_Corrected'],df['Between_Pointing_Corrected'] = applyCorrection(df,'Within_Pointing','Between_Pointing')

#%% Table 2 is unchanged (a linear adjustment)

table2_data = df_2[['Within_Pointing','Between_Pointing','Goals_Using_Shortcut','Goals_Using_Route','Goals_Found','Model_Building_Within_average','Model_Building_Total']]


table2_original = np.ma.corrcoef(np.ma.masked_invalid(table2_data.T))

table2_data_corrected = df_2[['Within_Pointing_Corrected','Between_Pointing_Corrected','Goals_Using_Shortcut','Goals_Using_Route','Goals_Found','Model_Building_Within_average','Model_Building_Total']]

table2_corrected = np.ma.corrcoef(np.ma.masked_invalid(table2_data_corrected.T))


# Cluster analysis won't change either.

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

#%% Ruginski reanalysis

# Load in Ruginski dataset
df_ruginski = pd.read_csv(os.path.join(baseDir,'Ruginski','Data','ian_diss_cleaneddata.csv'))

# Interpolate pointing data for Ruginski analyses
df_ruginski['WithinRtPt'],df_ruginski['BetweenRtPt'] = applyCorrection(df_ruginski,'WithinRtPt','BetweenRtPt')
df_ruginski.to_csv(os.path.join(baseDir,'Ruginski','Data','ian_diss_cleaneddata_reanalysis.csv'))
