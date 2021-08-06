# -*- coding: utf-8 -*-
"""
Created on Mon May 11 15:01:09 2020

@author: smwei

This script explores the bug in the Virtual Silcton code in various ways. 


"""

#%% Setup

import pandas as pd
import numpy as np
import random
from collections import namedtuple
import matplotlib.pyplot as plt
from scipy import stats


####
# This section of code will re-calculate the angles from the original coordinates
landmark = namedtuple('landmark','visit_order front_door_pixel_x front_door_pixel_y pointing_location_pixel_x pointing_location_pixel_y')
landmark_columns = ['visit_order','front_door_pixel_x','front_door_pixel_y','pointing_location_pixel_x','pointing_location_pixel_y']
buildingNamesOnce = ['Batty House','Lynch Station','Harris Hall','Harvey House','Golledge Hall','Snow Church','Sauer Center','Tobler Museum']

# from seeds.rb on website (original coordinates, negative Y)
landmarks_db = {'Batty House':landmark(0,63,-292,91,-309),
               'Lynch Station':landmark(1,67,-169,158,-222),
               'Harris Hall':landmark(2,268,-262,284,-251),
               'Harvey House':landmark(3,310,-498,300,-488),
               'Golledge Hall':landmark(4,635,-303,628,-255),
               'Snow Church':landmark(5,731,-262,692,-258),
               'Sauer Center':landmark(6,683,-69,687,-141),
               'Tobler Museum':landmark(7,536,-189,536,-197)}

# Create dataframe to make things easier to work with
landmarks_df = pd.DataFrame(landmarks_db,index=landmark_columns)
landmarks_df = landmarks_df.T


def vectorSubtraction(point_from,target,facing):
    #vector subtraction logic from here:
    #https://stackoverflow.com/questions/21483999/using-atan2-to-find-angle-between-two-vectors
    vector1 = np.subtract(facing,point_from) # center the facing and target points
    vector2 = np.subtract(target,point_from)
    # Now calculate the signed angle between them as the difference between arctans (y,x)
    angle = np.arctan2(vector1[1],vector1[0]) - np.arctan2(vector2[1],vector2[0])
    angle = np.rad2deg(angle)

    # Correct for being the wrong side of 180
    if angle > 180:
        correctedAngle = angle - 360
    elif angle < -180:
        correctedAngle = 360 + angle
    else:
        correctedAngle = angle

    # This helps calculate what direction that angle is from "north"
    from_zero_helper = [point_from[0],point_from[1]+1]
    from_zero_vec = np.subtract(from_zero_helper,point_from)
    from_zero = np.arctan2(from_zero_vec[1],from_zero_vec[0]) - np.arctan2(vector1[1],vector1[0])
    from_zero = np.rad2deg(from_zero)

    return correctedAngle,from_zero


recalculatedAnswers = []


for i,j in enumerate(landmarks_df.index):
    if i in (3,7): # if the point_from location is at the end of the route
        face = i - 1 # the facing direction is toward the previous diamond
    else: # otherwise it's toward the next diamond
        face = i + 1
    
    point_from = [landmarks_df['pointing_location_pixel_x'][j],landmarks_df['pointing_location_pixel_y'][j]]
    facing = [landmarks_df['pointing_location_pixel_x'][face],landmarks_df['pointing_location_pixel_y'][face]]

    for x,y in enumerate(landmarks_df.index):
        if x == i:
            value = np.nan
        else:
            target = [landmarks_df['front_door_pixel_x'][y],landmarks_df['front_door_pixel_y'][y]]
            [value,from_zero] = vectorSubtraction(point_from,target,facing)

        recalculatedAnswers.append(value)
    


buildingNames = [['Batty House','Lynch Station','Harris Hall','Harvey House','Golledge Hall','Snow Church','Sauer Center','Tobler Museum'],
                 ['Batty House','Lynch Station','Harris Hall','Harvey House','Golledge Hall','Snow Church','Sauer Center','Tobler Museum']]


index = pd.MultiIndex.from_product(buildingNames,names=['Pointing_From','Pointing_To'])


unsignedAnswers = [np.nan,47.32798389,37.5286373,93.19423766,51.76768154,48.19947553,30.3316951,37.30802198,
		   130.6542463,np.nan,7.021704671,48.19593801,3.323863738,8.968174613,29.20903342,17.95076442,
		   83.35216568,114.5627943,np.nan,2.146796904,77.71082192,84.7281122,110.6574336,99.95984558,
		   46.54689912,32.28246429,4.196894849,np.nan,64.9530254,66.19140238,46.29205396,42.14615863,
		   173.5694617,173.9683139,176.2022806,139.9309368,np.nan,1.204134401,76.21088344,147.0284471,
		   90.64700942,79.44851915,88.09346154,119.6929579,125.8431145,np.nan,0.279262352,63.69277617,
		   6.744508622,17.76205492,4.240040564,23.09134403,51.85616181,89.63525726,np.nan,2.713388186,
		   171.0086651,156.2355625,173.2852643,147.248191,67.30353066,38.78279809,20.69981416,np.nan]

df = pd.DataFrame(unsignedAnswers, index=index, columns=['unsignedAnswer'])

df['recalculatedAnswer'] = recalculatedAnswers
df['participantAngle'] = np.nan
df.dropna(how='all',inplace=True)
df.sort_index(inplace=True)







def pointingCalculationGood(actual,guess):
    ## Take the difference between the actual angle and the guess, taking the absolute value. 
    diff = abs(actual - guess)
    if diff > 180:
        diff = 360-diff
    return diff

def pointingCalculationBad(actual,guess):
    ## Correct the guess and actual, just in case, which were never negative.
    guess = abs(guess)
    actual = abs(actual)
    ## Take the difference between the actual angle and the guess, taking the absolute value. 
    diff = abs(actual - guess)
    if diff > 180:
        diff = 360-diff
    return diff









def simulatePointing(df,pointingSimulator,avgError,stdError):
    for idx,row in df.iterrows():
        df.loc[idx,'participantAngle'] = pointingSimulator(avgError,stdError,df.loc[idx,'recalculatedAnswer'])
    df['correctAngularError'] = [pointingCalculationGood(x,y) for x, y in zip(df['recalculatedAnswer'], df['participantAngle'])]
    df['DO_NOT_USE_incorrectAngularError'] = [pointingCalculationBad(x,y) for x, y in zip(df['unsignedAnswer'], df['participantAngle'])]
    return df



# We'll also add some real participant data
participantPointing = pd.read_csv('Raw_Pointing_Data_Weisberg_Studies.csv').dropna() # import participant values
# sort by participant id, building order
participantPointing.sort_values(by=['participant', 'start landmark','target landmark'],inplace=True) 
participants = participantPointing.participant.unique() # grab unique participant IDs

# Grab the same/different route tags for each pointing judgment to apply to DF
oneParticipant = participantPointing.iloc[0:56]
df = df.assign(**{c: oneParticipant[c].to_numpy() for c in ('same or different route', 'start landmark', 'target landmark')})


#%%
# This cell will simulate random responses and evaluate them using the good and bad coding schemes.

# Define whatever function you like
def pointingSimulator(avgError,stdError,actualAngle):
    if np.random.randint(2) > 0:
        direction = -1
    else:
        direction = 1
    difference = direction*avgError
    simPointingJudgment = np.random.normal(actualAngle+difference, stdError, 1)
   #print(simPointingJudgment)
    return simPointingJudgment

# How many simulations to run?
nsims = 20
stdError= 30




# Initialize the simulations database
outcomes = pd.DataFrame(index=list(range(0,nsims)), columns=['goodCoding','badCoding'])
outcomes = outcomes.fillna(0)
dfSim = df.copy()


outcomesToPlot = pd.DataFrame(index=list(range(0,9)), columns=['goodCoding','badCoding','avgError'])

for i in range(9):
    avgError = i*10
    
    # Each simulation here will create a row of outcomes that is kind of like one Silcton participant. 
    # That participant will have an average simulated pointing error = avgError
    for index,row in outcomes.iterrows():
        dfSim = simulatePointing(dfSim,pointingSimulator,avgError,stdError)
        dfSim = dfSim.assign(**{c: oneParticipant[c].to_numpy() for c in ('same or different route', 'start landmark', 'target landmark')})
        outcomes.loc[index,'goodCoding'] = dfSim.correctAngularError.mean()
        outcomes.loc[index,'badCoding'] = dfSim.DO_NOT_USE_incorrectAngularError.mean()
        temp = dfSim.groupby(['same or different route']).mean()
        outcomes.loc[index,'badSame'] = temp.loc['same']['DO_NOT_USE_incorrectAngularError']
        outcomes.loc[index,'badDiff'] = temp.loc['different']['DO_NOT_USE_incorrectAngularError']
    
    # Average our 100 simulations to give us one value per avgError
    outcomesToPlot.loc[i,'goodCoding'] = outcomes.goodCoding.mean()
    outcomesToPlot.loc[i,'badCoding'] = outcomes.badCoding.mean()
    outcomesToPlot.loc[i,'avgError'] = avgError
    
    
        
print('good coding mean: '+str(outcomes.goodCoding.mean()) + '\nbad coding mean: '+str(outcomes.badCoding.mean()))
#print('good coding std: '+str(outcomes.goodCoding.std()) + '\nbad coding std: '+str(outcomes.badCoding.std()))
print('bad same Route mean: '+str(outcomes.badSame.mean()) + '\nbad diff Route mean: '+str(outcomes.badDiff.mean()))


# scatterplot of outcomesToPlot['goodCoding'] vs outcomesToPlot['avgError']
# scatterplot of outcomesToPlot['badCoding'] vs outcomesToPlot['avgError']
#fig = plt.figure()
#ax1 = fig.add_subplot(111)
#ax1.scatter(x[:4], y[:4], s=10, c='b', marker="s", label='first')
#ax1.scatter(x[40:],y[40:], s=10, c='r', marker="o", label='second')
#plt.legend(loc='upper left');
#plt.show()







# Ok, so there's a VERY SLIGHT tendency for random data to result in a better outcome for within compared to between trials
print(stats.ttest_rel(outcomes['badDiff'],outcomes['badSame']))




















# Next we want to what the mean error is for each pointing judgment for all possible angles. 
byAngleGood = pd.DataFrame()
byAngleBad = pd.DataFrame()
for index,row in df.iterrows():
    a = []
    b = []
    for i in list(range(-180,180)):
        a.append(pointingCalculationGood(row['recalculatedAnswer'],i))
        b.append(pointingCalculationBad(row['unsignedAnswer'],i))
    byAngleGood[index] = a
    byAngleBad[index] = b


# Here we'll process the by-pointing-judgment data a bit
df['goodAllAnglesAverage'] = np.mean(byAngleGood)
df['badAllAnglesAverage'] = np.mean(byAngleBad)
df['goodAllAnglesMax'] = np.max(byAngleGood)
df['badAllAnglesMax'] = np.max(byAngleBad)
temp = df.groupby(['same or different route']).mean()


# And again, there's a slight bias toward the same route being slightly easier for all possible pointing judgments
print('Same Route mean: '+str(temp['badAllAnglesAverage']['same']) + '\nDiff Route mean: '+str(temp['badAllAnglesAverage']['different']))
print('Same Route max: '+str(temp['badAllAnglesMax']['same']) + '\nDiff Route max: '+str(temp['badAllAnglesMax']['different']))





#%% Now we'd like to randomly sample from real participants judgments. First, we'll do so without regard to whether they were integrators or not

# Step 1 - pull in the participant data as participant angles (e.g., for each pointing judgment, randomly sample a participant's answer)
# Step 2 - randomly assign them as positive or negative
# Step 3 - Calculate the difference for each "participant"
# Step 4 - Do Steps 1-3 but within Integrators/non-ints/imprecise 
# Step 5 - 

participantPointingByTrialAverage = participantPointing.groupby(['start landmark','target landmark']).mean()
np.corrcoef(participantPointingByTrialAverage['abs error'],df['badAllAnglesAverage'])

fig, ax1 = plt.subplots(figsize=(10,10))
plt.scatter(participantPointingByTrialAverage['abs error'],df['badAllAnglesMax'])
ax1.set_xlabel('Average Participant Error')
ax1.set_ylabel('Mean of all possible responses (should be 90)')
#ax1.scatter(x['abs error']-df['badAllAnglesAverage'],df['abs error'],color='r')


participantPointingBySameDiff = participantPointing.groupby(['participant','same or different route']).mean()['abs error'].unstack()

# One outlier needs removing
participantPointingBySameDiff.drop(participantPointingBySameDiff[participantPointingBySameDiff.same > 70].index, inplace=True)
fig, ax = plt.subplots(figsize=(14,12))
plt.scatter(participantPointingBySameDiff['different'],participantPointingBySameDiff['same'])


# Set the participant pointing index so we can refer to each participant's pointing trials
participantPointing.set_index(['participant','start landmark','target landmark'],inplace=True)
dfParticipantSim = df.copy()
#p = (participantPointingpa)
#for index,row in participantPointing.iterrows():
    #dfParticipantSim.loc[index[0],index[1]] = participantPointing.loc[random.choice(participants),'Tobler Museum','Batty House']['bearing']
    #print(index[0])
df = participantPointing[participantPointing.index.get_level_values('start landmark') == 'Lynch Station']
df = df[df.index.get_level_values('target landmark') == 'Sauer Center']
plt.hist(df['bearing'], 50, density=True, facecolor='g', alpha=0.75)
df['actual direction'][0]
