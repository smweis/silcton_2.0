This readme file contains documentation (data dictionary) for the two main analysis files for the Silcton replication project.

The purpose of the replication project is to determine (to the extent possible) the effects of a bug in calculating the pointing judgments of the Virtual Silcton environment. To that end, we collected a new dataset (across 3 different sites - the University of Florida, Temple University, and Bond University).

The dataset was collected over the course of 5 experiments. The original experiments are published in Weisberg & Newcombe (2016) and Weisberg, Schinazi, Shipley, Newcombe, & Epstein (2014).

Due to changes in data collection and coding, these data sets have been merged but are not completely identical. See notes below.  

# Participant-level data:
  **filename: master_DF.csv**

  **variables:**
  - 'good_pointing_coding_between'
    - Average pointing error between main Silcton routes, coded correctly.
    - DISCREPANCY NOTE: For original study participants good and bad coding is the same.
  - 'good_pointing_coding_within'
    - Average pointing error within main Silcton routes, coded correctly.
    - DISCREPANCY NOTE: For original study participants good and bad coding is the same.
  - 'bad_pointing_coding_between'
    - Average pointing error between main Silcton routes, coded INCORRECTLY.
    - DISCREPANCY NOTE: For original study participants good and bad coding is the same.
  - 'bad_pointing_coding_within'
    - Average pointing error within main Silcton routes, coded INCORRECTLY.
    - DISCREPANCY NOTE: For original study participants good and bad coding is the same.
  - 'participant'
    - Unique Participant ID
  - 'Original_Study_Number'
    - If the participant was in one of the original studies, that study number is listed here.
    - 1000s = Weisberg et al. (2014) dataset
    - 2000s = Study 1, Weisberg & Newcombe (2016)
    - 3000s = Study 2, Weisberg & Newcombe (2016)
    - 4000s = Study 3, Weisberg & Newcombe (2016)
  - 'SBSOD'
    - Santa Barbara Sense of Direction score (between 1-7, 7 is highest self-reported sense of direction)
  - 'gender'
    - self-identified Gender
  - 'WRAT'
    - WRAT-IV reading subscale score (higher is better)
  - 'Model_Building'
    - Bidimensional regression score on the model building task (participants create a map of Silcton)
  - 'Model_Building_A'
    - Bidimensional regression score on the model building task just for the Silcton route beginning with Batty House
  - 'Model_Building_B'
    - Bidimensional regression score on the model building task just for the Silcton route beginning with Golledge Hall
  - 'K_Means_Across_All'
    - Solution for a K-means cluster analysis across all studies. Group number indicates original group membership (Integrator, non-Integrator, or Imprecise Navigator)
  - 'Model_Building_Average_Within'
    - Bidimensional regression score on the model building task for both within-route scores averaged together (e.g., average of Model_Building_A and Model_Building_B)
  - 'Education_Numeric'
    - Education level
  - 'New_or_Original'
    - New: Participant was a member of the NEW study with correct coding possible.
    - Original: Participant was a member of the ORIGINAL studies with correct coding impossible.
  - 'trackpad/mouse'
    - Whether the participant used a trackpad or the mouse.
  - 'Site'
    - Where the participant took part in the study through
  - 'Experimenter '
    - Who administered the experiment.
  - 'NOTES'
    - Any notes about the participant from the experimenter.
  - 'Age'
    - Participant age in years.
  - 'Hispanic'
    - Whether participant self-identifies as Hispanic or not)
  - 'Racial_Category'
    - Racial category participant self-reports belonging to.
  - 'Racial_Category_Text'
    - Racial category participant self-reports belonging to.
  - 'Education_Level'
    - Education level
  - 'Education_Level_Text'
    - Education level
  - 'Handedness'
    - Right or left-handed (self-reported)
  - 'First_Language'
    - Whether participant reports English as first language
  - 'Age_started_speaking_English'
    - Age at which participant began speaking English
  - 'Participant_UUID_Silcton'
    - Unique identifier assigned by the Virtual Silcton website (NEW participants only)
  - 'date'
    - Date participant participated in study (NEW participants only)
  - 'MRT'
    - Mental rotation test score (NEW participants only)
  - 'good_pointing_coding_total'
    - Average pointing error all judgments, coded correctly.
    - DISCREPANCY NOTE: For original study participants good and bad coding is the same.
  - 'bad_pointing_coding_total'
    - Average pointing error all judgments, coded INCORRECTLY.
    - DISCREPANCY NOTE: For original study participants good and bad coding is the same.
  - 'Color'
    - Just for plotting. Can be ignored.

# Trial-level data:
    **filename: masterPointing.csv**

    **variables:**
      - 'participant'
        - Participant ID
      - 'date'
        - Date data collected
      - 'direction test'
        - Ignore
      - 'start landmark'
        - Name of landmark participant was standing next to for the current trial.
      - 'facing landmark'
        - Landmark used as the reference direction (e.g., if Start landmark = Batty House, Facing landmark = Lynch Station). The facing landmark is the same for all start landmark trials.
      - 'target landmark'
        - Name of landmark participant was pointing to for the current trial.
      - 'bearing'
        - Participant response (varies from -180 to 180 for NEW participants but from 0-180 for ORIGINAL participants);
      - 'actual direction'
        - Correct angle (-180 to 180 for NEW participants but 0-180 for ORIGINAL participants)
      - 'abs error'
        - The angular distance between 'bearing' and 'actual direction' correcting for being > 180.
      - 'same or different route'
        - Whether the pointing judgment involved a start landmark and facing landmark that were on the same initial route or different initial routes. (Same also labeled as "Within" and Different also labeled as "Between")
      - 'Original_or_New'
        - Whether participant was collected as part of the original (buggy) data or new dataset.
      - 'good_pointing'
        - A column of JUST the good pointing judgments
      - 'error in pointing task'
        - This column contains FALSE if there is NO error in the pointing judgment and 'nan' if there IS potentially an error. (Best to ignore this column)
      - 'UUID'
        - Unique participant ID from Silcton website.
      - 'condition'
        - Condition (from the Bond University sample). Can be ignored.
      - 'bad_pointing'
        - Pointing judgments coded AS IF they were recorded the buggy way.
      - 'partic_and_between_within_mean'
        - Participant-level variable which is the average BAD between/within pointing error for that participant (and set of judgments)
      - 'pointing_code_error'
        - error: EITHER an original participant OR a new participant with a discrepancy between good_pointing and bad_pointing
        - no error: a NEW participant with NO discrepancy between good_pointing and bad_pointing
      - 'bad_good_orig_new'
        - Combination of pointing_code_error and whether the participant is original or new.
      - 'good_bad_diff'
        - For NEW subjects only, the actual difference between good and bad pointing coding.
