# -*- coding: utf-8 -*-
"""
Created on Fri Dec 11 10:49:12 2020

This script imports the data (lightly cleaned by hand), then merges and cleans
those files for easy plotting and analysis.

@author: Steven Weisberg
"""
#%% Load and do some initial data processing and cleaning.
# Import necessary packages
import pandas as pd
import os


base_dir = os.path.dirname(os.path.abspath(__file__))

# Load up UF/Temple Data, which needs some merging and cleaning

uf_tu_pointing = pd.read_csv(os.path.join(base_dir,'..','CleanedData','UF_Temple_VirtualSilcton_Pointing.csv'))
uf_tu_meta = pd.read_csv(os.path.join(base_dir,'..','CleanedData','UF_Temple_VirtualSilcton_Metadata.csv'))
uf_tu_qualtrics = pd.read_csv(os.path.join(base_dir,'..','CleanedData','UF_Temple_VirtualSilcton_Qualtrics.csv'))
uf_tu_dems = pd.read_csv(os.path.join(base_dir,'..','CleanedData','UF_Temple_VirtualSilcton_Dems.csv'))
uf_tu_model_building = pd.read_csv(os.path.join(base_dir,'..','CleanedData','UF_Temple_VirtualSilcton_ModelBuilding.csv'))
uf_tu_model_building['participant'] = uf_tu_model_building['participant'].astype(str)



# We are taking a CONSERVATIVE APPROACH at the moment. Only analyzing data we can fully account for.
# These datasets have been somewhat cleaned by hand, eliminating rows without UUIDs.
# We are also doing the merge on the truncated UUIDs
uf_tu_df = uf_tu_meta.merge(uf_tu_qualtrics,
                            left_on='Participant_UUID_Meta_Truncated',
                            right_on='Participant_UUID_Qualtrics_Truncated',
                            how='inner', validate='one_to_one')

uf_tu_df = uf_tu_df.merge(uf_tu_dems,
                          left_on='Participant_UUID_Meta_Truncated',
                          right_on='Participant_UUID_Silcton_Truncated',
                          how='inner',validate='one_to_one')



# Cast the participant variable as a string
uf_tu_df['participant'] = uf_tu_df['participant'].astype(str)

# Define this column as new to designate it as new data
uf_tu_df['New_or_Original'] = 'New'

# DROP anyone who has been manually coded as DROP from the dataset
uf_tu_df = uf_tu_df[uf_tu_df['DROP'] !='DROP']

# Trim pointing values to ONLY use fully accounted for subjects
uf_tu_pointing_trimmed = uf_tu_pointing.copy()
uf_tu_pointing_trimmed = uf_tu_pointing_trimmed[uf_tu_pointing_trimmed.participant.isin(uf_tu_df['participant'])]

# Load and merge the Australia data
au_pointing = pd.read_csv(os.path.join(base_dir,'..','CleanedData','AU_VirtualSilcton_Pointing.csv'))
au_dems = pd.read_csv(os.path.join(base_dir,'..','CleanedData','AU_VirtualSilcton_Dems.csv'))
au_model_building = pd.read_csv(os.path.join(base_dir,'..','CleanedData','AU_VirtualSilcton_ModelBuilding.csv'))

au_model_building['participant'] = au_model_building['participant'].astype(str)
new_model_building = pd.concat([au_model_building,uf_tu_model_building])

# Define the site for Australia
au_dems['Site'] = 'AU'

# Combine all new dems
new_dems = pd.concat([uf_tu_df, au_dems])
new_dems['SBSOD'] = pd.to_numeric(new_dems['SBSOD'], errors='coerce')

# Define Original or New column in pointing data.
uf_tu_pointing_trimmed['New_or_Original'] = 'New'
au_pointing['New_or_Original'] = 'New'

uf_tu_pointing_trimmed['good_pointing'] = uf_tu_pointing_trimmed['abs error']
au_pointing['good_pointing'] = au_pointing['abs error']

new_pointing = pd.concat([uf_tu_pointing_trimmed,au_pointing])
new_pointing['participant'] = new_pointing['participant'].astype(str)





# This function will recalculate the INCORRECT pointing angles
def pointingCalculationBad(actual,guess):
    ## Correct the guess and actual, just in case, which were never negative.
    guess = abs(guess)
    actual = abs(actual)
    ## Take the difference between the actual angle and the guess, taking the absolute value.
    diff = abs(actual - guess)
    if diff > 180:
        diff = 360-diff
    return diff


# Recalculate the bad pointing angles for the participant data
new_pointing['bad_pointing'] = [pointingCalculationBad(x,y) for x, y in zip(new_pointing['actual direction'], new_pointing['bearing'])]

# Create a new dataset that is the averaged pointing data
grouped_new_pointing = new_pointing.groupby(['participant','same or different route'])[['abs error','bad_pointing']].mean().reset_index()
grouped_new_pointing = grouped_new_pointing.pivot_table(index=['participant'],columns='same or different route',values=['abs error','bad_pointing']).reset_index()
grouped_new_pointing.columns = list(map("_".join, grouped_new_pointing.columns))

# Create master dataset
new_merged = grouped_new_pointing.merge(new_dems, left_on='participant_', right_on='participant', how='outer',validate='one_to_one')

new_merged = pd.merge(new_merged,new_model_building,left_on='participant_',right_on='participant',how='outer')

new_merged['scratch1'] = new_merged['bad_pointing_different']*32
new_merged['scratch2'] = new_merged['bad_pointing_same']*24
new_merged['scratch3'] = new_merged['abs error_same']*24
new_merged['scratch4'] = new_merged['abs error_different']*32

new_merged['good_pointing_coding_total'] = new_merged[['scratch3', 'scratch4']].sum(axis=1)/56
new_merged['bad_pointing_coding_total'] = new_merged[['scratch1', 'scratch2']].sum(axis=1)/56


# Load up original data (from Weisberg et al. 2014 and 2016)
original_dems = pd.read_csv(os.path.join(base_dir,'..','CleanedData','Original_VirtualSilcton_All.csv'))
original_dems['participant'] = original_dems['Participant_ID'].astype(str)
# Rename the participants to have consistent labels
original_dems['participant'] = 'Original_' + original_dems['participant'].str.split('_').str[-1]


# Combine the model building scores
original_dems['Model_Building_A'] = pd.to_numeric(original_dems['Model_Building_A'],errors='coerce')
original_dems['Model_Building_B'] = pd.to_numeric(original_dems['Model_Building_B'],errors='coerce')
original_dems['Model_Building'] = pd.to_numeric(original_dems['Model_Building'],errors='coerce')

# Rename some other columns
original_dems.rename(columns={"Within_Pointing": "bad_pointing_coding_within",
                              "Between_Pointing": "bad_pointing_coding_between",
                              "Model_Building": "Overall_rsquared",
                              "Model_Building_A": "Batty_rsquared",
                              "Model_Building_B": "Golledge_rsquared"}
                     ,inplace=True)

original_dems.drop(['Participant_ID','Seen_Within','Unseen_All','Unseen_Within','Unseen_Between',
                    'Ints_Others_Clusters','Non_Int_Imps_Only_Clusters','Two_Step_Within_0',
                    'Two_Step_Within_1','Two_Step_Within_2','Two_Step_Within_3','Two_Step_Across_All',
                    'Two_Step_Within_All','K_Means_Within_0','K_Means_Within_1','K_Means_Within_2',
                    'K_Means_Within_3','K_Means_Within_All','Model_Building_Average_Within',
                    'Seen_Unseen','UnseenB_UnseenW','Pointing_mean'],axis=1,inplace=True)

original_dems['New_or_Original'] = 'Original'

# Drop some unnecessary columns
new_merged.drop(['scratch1','scratch2','scratch3','scratch4','Participant_UUID_Meta_Raw','Participant_UUID_Alternate_Meta','DATE RUN','TIME RUN','Silcton load time','Progress','Duration_seconds','Finished','Recorded_date','PIN_TU_Only','pilot','data_share','participant_','ID_Meta','DROP','Participant_UUID_Meta','Participant_UUID_Meta_Truncated','Participant_UUID_Qualtrics','Participant_UUID_Qualtrics_Truncated','Participant_UUID_Silcton_Truncated','P_Number','Condition','Start_date','End_date','Institution','participant_y'],axis=1,inplace=True)

# Rename some other columns
new_merged.rename(columns={"Study": "Original_Study_Number",
                            "Education": "Education_Numeric",
                            "abs error_different":"good_pointing_coding_between",
                            "bad_pointing_same":"bad_pointing_coding_within",
                            "abs error_same":"good_pointing_coding_within",
                            "bad_pointing_different":"bad_pointing_coding_between",
                            "participant_x":"participant"}
                   ,inplace=True)


all_merged = pd.concat([new_merged,original_dems])

all_merged['Within_rsquared_average'] = (all_merged['Batty_rsquared']+ all_merged['Golledge_rsquared'])/2

# A fair number of participants have model building data but nothing else. We drop them here.
all_merged = all_merged[all_merged['participant'].notna()]

# Drop anyone without pointing data

all_merged = all_merged[all_merged['bad_pointing_coding_within'].notna()]

# Save the CSV
all_merged.to_csv(os.path.join(base_dir,'..','CleanedData','masterDF.csv'),index=False)
new_pointing.to_csv(os.path.join(base_dir,'..','CleanedData','masterPointing.csv'),index=False)
