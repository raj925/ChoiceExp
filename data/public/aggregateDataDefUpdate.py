# aggregateData.py

# This script looks for all the inidividual participant data (csv) files generated by jsonConvert.py
# and then creates (and subsequently updates) an aggregate csv file with average data across participants
# This csv file can then be used for overall analysis such as ANOVAs.
# You can update the below code to add new fields to aggregate file derived from the individual data.
# If you need to add new inidividual participant data points, you will need to update jsonConvert.py.
# This only deals with aggregate analysis.
# If scheduling a cronjob to run both of these scripts, run this AFTER jonConvert.

# Author: Sriraj Aiyer

import json
import pandas as pd
import csv
import glob, os
import numpy as np
import fnmatch
import sys

# Set directory and filename to save csv file to.
currentDir = os.path.dirname(sys.argv[0])
currentDir = currentDir + "/Trials"
os.chdir(currentDir)
aggregateFilename = "../allSubjects.csv"

logf = open("aggregateDataLog.txt", "w+")

with open(aggregateFilename, mode='w') as dataOut:
    
    csv_writer = csv.writer(dataOut, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    
    # Update the below row to change the column headers for aggregate data file.
    # Make sure the order and length matches the variables added on each row in the last line of this script (ie csv_writer.writerow([...]))
    allHeads = ['Participant ID', 'Final Dot Difference', 'Num Of Trials', 'Choice of Human', 'Choice of Algorithm', 'Human Trials Changed to Algor', 'Algor Trials Changed to Human', 'Algor Default Trials Not Changed', 'Human Default Trials Not Changed', 'Block 4 Human Acc', 'Block 4 Algor Acc', 'Human Choice After Block 4', 'Algor Choice After Block 4', 'Advisor Changed', 'Left Box Chosen', 'Preference Strength', 'Max Cj', 'Min Cj', 'Cj Range', 'Num of Unique Cj Values', 'Advice Ignored Trials', 'Mean Cj Change', 'Cj1 Mean Resolution', 'Cj2 Mean Resolution', 'Mean Cj1 Accuracy', 'Mean Cj2 Accuracy', 'Sway of Human Advice', 'Sway of Algor Advice', 'Mean RT1', 'Mean RT2', 'Mean CTC', 'AccQuant1', 'AccQuant2', 'AccQuant3', 'AccQuant4', 'AdvQuant1', 'AdvQuant2', 'AdvQuant3', 'AdvQuant4', 'CjQuant1', 'CjQuant2', 'CjQuant3', 'CjQuant4', 'Mean Cj1', 'Mean Cj2', 'St Dev Cj1', 'St Dev Cj2', 'Human Agreed %', 'Algor Agreed %', 'Algor Agreed % Diff', 'Human Agreed Conf Diff', 'Algor Agreed Conf Diff', 'Human Disagreed Conf Diff', 'Algor Disagreed Conf Diff', 'Algor Relative Influence', 'dd Block 1 End', 'dd Block 2 End', 'Block 1 Accuracy', 'Block 2 Accuracy']
    count = 0;
    # For all individual participant files (which jsonConvert names in the form TRIALS.csv)
    for file in glob.glob("*TRIALS.csv"):
        estimateVars = []
        estimateHeads = []
        surveyVars = []
        surveyHeads = []
        miscVars = []
        miscHeads = []
        with open(file) as dataIn:
            #try:
            print(file)
            filenameSplit = file.split("_")
            pid = filenameSplit[0]
            
            # Retrieve the data under the columns listed below. Refer to the trials file to see which columns are there (currently all listed below).
            df = pd.read_csv(dataIn, usecols=['trialNumber', 'block', 'staircase', 'wherelarger', 'dotdifference', 'int1', 'cj1', 'cor1', 'int2', 'cj2', 'cor2', 'trialType', 'whichAdvisor', 'defaultAdvisor', 'advisorChanged', 'advAnswer', 'advCorrect', 'advConfidence', 'rt1', 'rt2', 'ctcTime'])

                            
            # We only want to pull trials after the practice/staricase trials for this script.
            # Remove this if you want to include these practice trials in analysis.

            # Staircasing analysis
            trialNum = df["trialNumber"]
            blockNum= df["block"]
            cor1 = df["cor1"]
            dd = df["dotdifference"]
            try:
                blk1dd = dd.iloc[30]
            except:
                blk1dd = 0
            try:
                blk2dd = dd.iloc[60]
            except:
                blk2dd = 0
            blk1acc = np.sum(cor1.loc[df["block"] == 1])/30
            blk2acc = np.sum(cor1.loc[df["block"] == 2])/30

            # All subsqeuent analysis just on experimental trials
            df = df.loc[df["block"] > 3]
            df = df.loc[df["trialType"] != "forceblk4"]

            forcedTrials = df.loc[df["trialType"] == "force"]
            changeTrials = df.loc[df["trialType"] == "change"]
            forceNum = len(forcedTrials)
            changeNum = len(changeTrials)

            # Save the fields under each column as a sepearate dataframe variable (basically, a vector).
            int1 = df["int1"]
            int2 = df["int2"]
            cj1 = df["cj1"]
            cor1 = df["cor1"]
            cj2 = df["cj2"]
            cor2 = df["cor2"]
            whichAdvisor = df["whichAdvisor"]
            advCorrect = df["advCorrect"]
            rt1 = df["rt1"]
            rt2 = df["rt2"]
            ctcTime = df["ctcTime"]
            numOfTrials = np.count_nonzero(~np.isnan(int1))
            dotdifference = df["dotdifference"]
            try:
                finalDD = dotdifference.iloc[numOfTrials-1]
            except:
                finalDD = 0
            whereLarger = df["wherelarger"]
            defaultAdvisor = df["defaultAdvisor"]
            advisorChanged = df["advisorChanged"]
            block = df["block"]

            advisorChangedPercent = len(df.loc[(df["trialType"] == "change") & (df["advisorChanged"]==1)])/changeNum
            leftBoxChosen = len(df.loc[df["int1"]==0])/numOfTrials
            rightBoxChosen = 1 - leftBoxChosen

            cj1Orig = cj1
            cj2Orig = cj2
            cjDiff = abs(cj2 - cj1)
            mask = (int1 == 0)
            mask2 = (int2 == 0)
            cj1Orig[mask] = (cj1Orig[mask])*-1
            cj2Orig[mask] = (cj2Orig[mask2])*-1

            # loc is the method in Python Pandas that allows you to pull a certain portion of data based on some filter.
            # So for below, we want to find the mean of advisor used only for choice trials in order to look at proportion of advisor chosen.
            # We subtract one here because the advisor ids in the trials files are 1 and 2 rather than 0 and 1.
            algorChoice = len(df.loc[(df["trialType"] == "change") & (df["whichAdvisor"] == 1)])/changeNum
            humanChoice = 1 - algorChoice
            algorChangedTo = len(df.loc[(df["trialType"] == "change") & (df["whichAdvisor"] == 2) & (df["defaultAdvisor"] == 1)])/len(df.loc[(df["trialType"] == "change") & (df["defaultAdvisor"] == 1)])
            humanChangedTo = len(df.loc[(df["trialType"] == "change") & (df["whichAdvisor"] == 1) & (df["defaultAdvisor"] == 2)])/len(df.loc[(df["trialType"] == "change") & (df["defaultAdvisor"] == 2)])
            algorKept = len(df.loc[(df["trialType"] == "change") & (df["whichAdvisor"] == 2) & (df["defaultAdvisor"] == 2)])/len(df.loc[(df["trialType"] == "change") & (df["defaultAdvisor"] == 2)])
            humanKept = len(df.loc[(df["trialType"] == "change") & (df["whichAdvisor"] == 1) & (df["defaultAdvisor"] == 1)])/len(df.loc[(df["trialType"] == "change") & (df["defaultAdvisor"] == 1)])

            maxCj = np.max(abs(cj1));
            minCj = np.min(abs(cj1));
            cjRange = maxCj - minCj;
            numOfCjVals = len(np.unique(abs(cj1[~np.isnan(cj1)])))
            adviceIgnoredTrials = len(df.loc[(df["cj1"] == df["cj2"]) & (df["int1"] == df["int2"])])
            meanCjDiff = np.mean(cjDiff);

            # Early experience separation from later choice (looking at block 4)
            earlyHumanAcc = len(df.loc[(df["advCorrect"] == 1) & (df["block"] == 4) & df["whichAdvisor"] == 1])/len(df.loc[(df["block"] == 4) & (df["whichAdvisor"] == 1)])
            earlyAlgorAcc = len(df.loc[(df["advCorrect"] == 1) & (df["block"] == 4) & (df["whichAdvisor"] != 1)])/len(df.loc[(df["block"] == 4) & (df["whichAdvisor"] != 1)])
            laterHumanChoice = len(df.loc[(df["trialType"] == "change") & (df["whichAdvisor"] == 1) & (df["block"] > 4)])/len(df.loc[(df["trialType"] == "change")  & (df["block"] > 4)])
            laterAlgorChoice = 1 - laterHumanChoice
            
            # Resolution is the difference in average confidence during correct and error trials.
            # Computer separately pre and post advice.
            resolution = np.mean(cj1.loc[df["cor1"] == 1]) - np.mean(cj1.loc[df["cor1"] == 0])
            resolution2 = np.mean(cj2.loc[df["cor2"] == 1]) - np.mean(cj2.loc[df["cor2"] == 0])
            meanCor1 = np.sum(cor1)/numOfTrials
            meanCor2 = np.sum(cor2)/numOfTrials
            humanSway = np.mean(cj2.loc[(df["whichAdvisor"] == 1) & (df["trialType"] == "force")] - cj1.loc[(df["whichAdvisor" ] == 1) & (df["trialType"] == "force")])
            algorSway = np.mean(cj2.loc[(df["whichAdvisor"] == 2) & (df["trialType"] == "force")] - cj1.loc[(df["whichAdvisor"] == 2) & (df["trialType"] == "force")])
            meanRt1 = np.mean(rt1)
            meanRt2 = np.mean(rt2)
            meanCtc = np.mean(ctcTime)
            meanCj1 = abs(np.mean(abs(cj1)))
            meanCj2 = abs(np.mean(abs(cj2)))
            stDevCj1 = abs(np.std(abs(cj1)))
            stDevCj2 = abs(np.std(abs(cj2)))
            
            humanAgreedPercent = len(df.loc[(df["int2"] == df["advAnswer"]) & (df["whichAdvisor"] == 1) & (df["trialType"] == "force")])/len(df.loc[(df["whichAdvisor"] == 1) & (df["trialType"] == "force")])
            algorAgreedPercent = len(df.loc[(df["int2"] == df["advAnswer"]) & (df["whichAdvisor"] == 2) & (df["trialType"] == "force")])/len(df.loc[(df["whichAdvisor"] == 2) & (df["trialType"] == "force")])
            agreedDiff = algorAgreedPercent - humanAgreedPercent
            humanAgreedConfDiff = np.mean(cj2Orig.loc[(df["int2"] == df["advAnswer"]) & (df["whichAdvisor"] == 1) & (df["trialType"] == "force")] - cj1Orig.loc[(df["int2"] == df["advAnswer"]) & (df["whichAdvisor"] == 1) & (df["trialType"] == "force")])
            algorAgreedConfDiff = np.mean(cj2Orig.loc[(df["int2"] == df["advAnswer"]) & (df["whichAdvisor"] == 2) & (df["trialType"] == "force")] - cj1Orig.loc[(df["int2"] == df["advAnswer"]) & (df["whichAdvisor"] == 2) & (df["trialType"] == "force")])
            humanDisagreedConfDiff = np.mean(cj2Orig.loc[(df["int2"] != df["advAnswer"]) & (df["whichAdvisor"] == 1) & (df["trialType"] == "force")] - cj1Orig.loc[(df["int2"] != df["advAnswer"]) & (df["whichAdvisor"] == 1) & (df["trialType"] == "force")])
            algorDisagreedConfDiff = np.mean(cj2Orig.loc[(df["int2"] != df["advAnswer"]) & (df["whichAdvisor"] == 2) & (df["trialType"] == "force")] - cj1Orig.loc[(df["int2"] != df["advAnswer"]) & (df["whichAdvisor"] == 2) & (df["trialType"] == "force")])
            algorRelativeInfluence = (algorAgreedConfDiff-algorDisagreedConfDiff) - (humanAgreedConfDiff-humanDisagreedConfDiff)

            # Quantiles below created using post-advice confidence.
            # We can see how these quantiles relate to accuracy and advisor choice.
            cj1Quant = df.cj1.quantile([0.25,0.5,0.75])

            accQuant1 = df.loc[(df["cj1"] < cj1Quant[0.25]), "cor1"]
            accQuant2 = df.loc[(df["cj1"] > cj1Quant[0.25]) & (df["cj1"] < cj1Quant[0.5]), "cor1"]
            accQuant3 = df.loc[(df["cj1"] > cj1Quant[0.5]) & (df["cj1"] < cj1Quant[0.75]), "cor1"]
            accQuant4 = df.loc[(df["cj1"] > cj1Quant[0.75]), "cor1"]
            
            advQuant1 = df.loc[(df["cj1"] < cj1Quant[0.25]), "whichAdvisor"]-1
            advQuant2 = df.loc[(df["cj1"] > cj1Quant[0.25]) & (df["cj1"] < cj1Quant[0.5]), "whichAdvisor"]-1
            advQuant3 = df.loc[(df["cj1"] > cj1Quant[0.5]) & (df["cj1"] < cj1Quant[0.75]), "whichAdvisor"]-1
            advQuant4 = df.loc[(df["cj1"] > cj1Quant[0.75]), "whichAdvisor"]-1

            # This gives you the distribution of cj2 indicated, so you can see how participants use the confidence scale.
            cjQuant1 = df.loc[(df["cj1"] < cj1Quant[0.25]), "cj1"]
            cjQuant2 = df.loc[(df["cj1"] > cj1Quant[0.25]) & (df["cj1"] < cj1Quant[0.5]), "cj1"]
            cjQuant3 = df.loc[(df["cj1"] > cj1Quant[0.5]) & (df["cj1"] < cj1Quant[0.75]), "cj1"]
            cjQuant4 = df.loc[(df["cj1"] > cj1Quant[0.75]), "cj1"]
            
            # Annoying bit below where we have to check if each quantile is empty, otherwise this messes up the table...
            if accQuant1.empty:
                accQuant1 = 0
            else:
                accQuant1 = np.nanmean(accQuant1)

            if accQuant2.empty:
                accQuant2 = 0
            else:
                accQuant2 = np.nanmean(accQuant2)

            if accQuant3.empty:
                accQuant3 = 0
            else:
                accQuant3 = np.nanmean(accQuant3)

            if accQuant4.empty:
                accQuant4 = 0
            else:
                accQuant4 = np.nanmean(accQuant4)

            if advQuant1.empty:
                advQuant1 = 0
            else:
                advQuant1 = np.nanmean(advQuant1)

            if advQuant2.empty:
                advQuant2 = 0
            else:
                advQuant2 = np.nanmean(advQuant2)

            if advQuant3.empty:
                advQuant3 = 0
            else:
                advQuant3 = np.nanmean(advQuant3)

            if advQuant4.empty:
                advQuant4 = 0
            else:
                advQuant4 = np.nanmean(advQuant4)

            if cjQuant1.empty:
                cjQuant1 = 0
            else:
                cjQuant1 = (cjQuant1.size)/numOfTrials

            if cjQuant2.empty:
                cjQuant2 = 0
            else:
                cjQuant2 = (cjQuant2.size)/numOfTrials

            if cjQuant3.empty:
                cjQuant3 = 0
            else:
                cjQuant3 = (cjQuant3.size)/numOfTrials

            if cjQuant4.empty:
                cjQuant4 = 0
            else:
                cjQuant4 = (cjQuant4.size)/numOfTrials

            # All of the above fields are rounded to 3 decimal places.
            algorChoice = round(algorChoice, 3)
            humanChoice = round(humanChoice, 3)
            resolution = abs(round(resolution, 3))
            resolution2 = abs(round(resolution2, 3))
            meanCor1 = round(meanCor1, 3)
            meanCor2 = round(meanCor2, 3)
            humanSway = abs(round(humanSway, 3))
            algorSway = abs(round(algorSway, 3))
            meanRt1 = round(meanRt1, 3)
            meanRt2 = round(meanRt2, 3)
            meanCtc = round(meanCtc, 3)
            accQuant1 = round(accQuant1, 3)
            accQuant2 = round(accQuant2, 3)
            accQuant3 = round(accQuant3, 3)
            accQuant4 = round(accQuant4, 3)
            advQuant1 = round(advQuant1, 3)
            advQuant2 = round(advQuant2, 3)
            advQuant3 = round(advQuant3, 3)
            advQuant4 = round(advQuant4, 3)
            cjQuant1 = round(cjQuant1, 3)
            cjQuant2 = round(cjQuant2, 3)
            cjQuant3 = round(cjQuant3, 3)
            cjQuant4 = round(cjQuant4, 3)
            preferenceStrength = round(abs(0.5 - algorChoice), 3)
            meanCj1 = round(meanCj1, 3)
            meanCj2 = round(meanCj2, 3)
            stDevCj1 = round(stDevCj1, 3)
            stDevCj2 = round(stDevCj2, 3)
            humanAgreedPercent = round(humanAgreedPercent, 3)
            algorAgreedPercent = round(algorAgreedPercent, 3)
            agreedDiff = round(agreedDiff, 3)
            humanAgreedConfDiff = round(humanAgreedConfDiff, 3)
            algorAgreedConfDiff = round(algorAgreedConfDiff, 3)
            humanDisagreedConfDiff = round(humanDisagreedConfDiff, 3)
            algorDisagreedConfDiff = round(algorDisagreedConfDiff, 3)
            algorRelativeInfluence = round(algorRelativeInfluence, 3)
            leftBoxChosen = round(leftBoxChosen, 3)
            meanCjDiff = round(meanCjDiff, 3)
            earlyHumanAcc = round(earlyHumanAcc, 3)
            earlyAlgorAcc = round(earlyAlgorAcc, 3)
            laterHumanChoice = round(laterHumanChoice, 3)
            laterAlgorChoice = round(laterAlgorChoice, 3)
            blk1acc = round(blk1acc, 3)
            blk2acc = round(blk2acc, 3)

            if (count == 0):
                allHeads.extend(estimateHeads)
                allHeads.extend(miscHeads)
                allHeads.extend(surveyHeads)
                csv_writer.writerow(allHeads)
                count = 1

            allVars = [pid, finalDD, numOfTrials, algorChoice, humanChoice, algorChangedTo, humanChangedTo, algorKept, humanKept, earlyHumanAcc, earlyAlgorAcc, laterHumanChoice, laterAlgorChoice, advisorChangedPercent, leftBoxChosen, preferenceStrength, maxCj, minCj, cjRange, numOfCjVals, adviceIgnoredTrials, meanCjDiff, resolution, resolution2, meanCor1, meanCor2, humanSway, algorSway, meanRt1, meanRt2, meanCtc, accQuant1, accQuant2, accQuant3, accQuant4, advQuant1, advQuant2, advQuant3, advQuant4, cjQuant1, cjQuant2, cjQuant3, cjQuant4, meanCj1, meanCj2, stDevCj1, stDevCj2, humanAgreedPercent, algorAgreedPercent, agreedDiff, humanAgreedConfDiff, algorAgreedConfDiff, humanDisagreedConfDiff, algorDisagreedConfDiff, algorRelativeInfluence, blk1dd, blk2dd, blk1acc, blk2acc]
                       
            if (count == 1):
                allVars.extend(estimateVars)
                allVars.extend(miscVars)
                allVars.extend(surveyVars)
                                                                                                                      
            csv_writer.writerow(allVars)

##            except Exception as e:
##                print(str(e))
##                logf.write(str(e))
 
