# jsonConvert.py
# This script takes the JSON output from the web experiment and writes two CSVs of data for that participants
# The subject CSV is for information related to the participant.
# This includes age/gender and thus is stored in the private folder.
# The trials CSV is the data related to the experiment.
# After this script is run for incoming participant JSON data, aggregateData.py is run to combine CSVs.

# Author: Sriraj Aiyer

import json
import csv
import glob, os
import sys

# Stay in the current directory
currentDir = os.path.dirname(sys.argv[0])
currentDir = currentDir + "/JSONs"
os.chdir(currentDir)

# Anything pre-dots task is contained in the JSON object miscTrials
# These are flags to say what data we should be looking for
demoFlag = 0
surveyFlag = 1
estimateFlag = 0
miscFlag = 0
surveyFilename = ""

logf = open("jsonConvertLog.txt", "w+")

# For all json files in the current directory
# No need to worry about the same json file being processed twice.
recentFiles = []
for subdir, dirs, files in os.walk("./"):
    for di in dirs:
        if (di[0] != "_"):
            lookin = "./" + di
            for jsonsubdir, jsondirs, jsonfiles in os.walk(lookin):
                dirTimes = []
                for jsonfile in jsonfiles:
                    jsonfilename = jsonfile.split("_")
                    if (jsonfilename[0] == '.DS'):
                        continue
                    time = jsonfilename[3] + jsonfilename[4] + jsonfilename[5]
##                    if (time[0] == "0"):
##                        offset = "0"
##                    else:
                    offset = ""
                    time = int(float(time))
                    dirTimes.append(time)
            recentfile = offset + str(max(dirTimes))
            recentfile = "./" + di + "/" + jsonfilename[0] + "_" + jsonfilename[1] + "_" + jsonfilename[2] + "_" + recentfile[0:2] + "_" + recentfile[2:4] + "_" + recentfile[4:6] + "_" + jsonfilename[6]
            recentFiles.append(recentfile)


for file in recentFiles:
    print(file)
    surveyData = []
    questions = []
    #try:
    filename = file.split("_")
    filename[0] = ((filename[0]).split("/"))[2]
    
    # Get the data of this participant's experiment from the filename (DD/MM/YYYY)
    date = filename[2] + "/" + filename[1] + "/" + filename[0]

    # Read the JSON file
    txt = open(file).read()

    # Some weird formatting in the JSON file we need to deal with so it can be parsed properly.
    txt = txt.replace('}"}','}}')
    txt = txt.replace('"{','{')
    txt = txt.replace('}",','},')

    # We change the JSON as above and then rewrite the JSON file with the cleaned-up version.
    with open(file, mode='w') as json_file:
        json_file.write(txt)

    # Main python function for intepreting JSON data. Won't work without the cleanup above.
    dataJson = json.loads(txt)

    ID = dataJson["rawData"]["participantId"]
    miscTrials = dataJson["rawData"]["miscTrials"]
    estimates = dataJson["rawData"]["estimates"]
    surveys = dataJson["rawData"]["radio"]

    if (demoFlag == 1):
        # Get the demographic data from the JSON
        try:
            gender = dataJson["rawData"]["miscTrials"][0]["0"]["answer"]
        except:
            gender = dataJson["rawData"]["demo"][0]["answer"]
        try:
            deviceUse = dataJson["rawData"]["miscTrials"][0]["1"]["answer"]
            age = dataJson["rawData"]["miscTrials"][0]["2"]["answer"]
            # Subject data filename is made up of date and participant ID and is saved in the private folder.
            subjectFilename = '../../private/Subjects/' + filename[2] + filename[1] + filename[0] + '_' + ID + '_SUBJECT.csv'
    
            # Open subject file to write. Creates new file if it doesn't already exist, otherwise it truncates the current file.
            with open(subjectFilename, mode='w') as subject_file:
                subject_writer = csv.writer(subject_file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                # Write the column headers first.
                subject_writer.writerow(['ID', 'date', 'gender', 'age', 'deviceUse'])
                # Write the corresponding data under each column header.
                subject_writer.writerow([ID, date, gender, age, deviceUse])

        except:
            print("couldn't retrive demo info")

    if (miscFlag == 1):
        dotReward = dataJson["rawData"]["dotReward"]
        estReward = dataJson["rawData"]["estimateReward"]
        totReward = dataJson["rawData"]["totalReward"]
        condition = dataJson["rawData"]["CBcondition"]
        # Subject data filename is made up of date and participant ID and is saved in the private folder.
        subjectFilename = '../../private/Subjects/' + filename[2] + filename[1] + filename[0] + '_' + ID + '_SUBJECT.csv'

        # Open subject file to write. Creates new file if it doesn't already exist, otherwise it truncates the current file.
        with open(subjectFilename, mode='w') as subject_file:
            subject_writer = csv.writer(subject_file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            # Write the column headers first.
            subject_writer.writerow(['dotReward', 'estimateReward', 'totalReward', 'CBcondition'])
            # Write the corresponding data under each column header.
            subject_writer.writerow([dotReward, estReward, totReward, condition])

    if (surveyFlag == 1):
        surveysLen = len(surveys)
        count = 0
        for x in range(0,surveysLen):
            if 'answer' not in surveys[x]:
                surveyData.append(surveys[x][1]["answer"])
                questions.append(surveys[x][0]["question"])
            else:
                surveyData.append(surveys[x]["answer"])
                questions.append(surveys[x]["question"])

        surveyData = list(filter(None, surveyData))
        questions = list(filter(None, questions))

        if len(surveyData) > 0:
            surveyFilename = '../Surveys/' + ID + '_SURVEY.csv'
            with open(surveyFilename, mode='w') as survey_file:
                survey_writer = csv.writer(survey_file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                survey_writer.writerow(questions)
                survey_writer.writerow(surveyData)

        else:
            print("No survey data for" + file)

    if (estimateFlag == 1):
        #try:
        estimatesLen = len(estimates)
        count = 0
        estimateQuestions = ['reward choice', 'model guess', 'own guess']
        estimateFilename = '../EstimateData/' + ID + '_ESTIMATE.csv'
        with open(estimateFilename, mode='w+') as dataOut:
            estimate_writer = csv.writer(dataOut, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            factorNames = estimates[x][0]["question"]["factorNames"]
            headings = ['Participant Estimate', 'Advisor Estimate', 'True Answer', 'Advisor Error', 'Participant Error', 'Trial Type'] + factorNames
            estimate_writer.writerow(headings)
            for x in range(1,estimatesLen):
                # Need to add a check for trial type and to add data differently
                # Training, adjust, influence, reward
                factorNames = estimates[x][0]["question"]
                pptEst = estimates[x][1]["answer"]
                advEst = estimates[x][0]["question"]["advEstimate"]
                trueAns = estimates[x][0]["question"]["trueAnswer"]
                advErr = estimates[x][0]["question"]["advErr"]
                pptErr = abs(int(trueAns) - int(pptEst))
                trialType = estimates[x][2]["taskType"]
                factorVals = estimates[x][0]["question"]["factorValues"]
                rowData = [pptEst, advEst, trueAns, advErr, pptErr, trialType] + factorVals
                estimate_writer.writerow(rowData)


        #except:
        #    print("No estimate data")


    # Get trials data from the JSON.
    trials = dataJson["processedData"]["trials"]
    
    # Trials data filename is made up of date and participant ID and is saved in the public folder.
    trialsFilename = '../Trials/' + ID + '_TRIALS.csv'



    

    # Open trials file to write.
    with open(trialsFilename, mode='w') as trials_file:
        subject_writer = csv.writer(trials_file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)

        # Write column headers in order. Make sure this matches the order of data below in subject_writer.writerow([currentTrial["id"]....
        subject_writer.writerow(['trialNumber', 'block', 'staircase', 'wherelarger', 'dotdifference', 'int1', 'cj1', 'cor1', 'int2', 'cj2', 'cor2', 'trialType', 'whichAdvisor', 'defaultAdvisor', 'advisorChanged', 'advAnswer', 'advCorrect', 'advConfidence', 'rt1', 'rt2', 'ctcTime'])
        for f in range(0,len(trials)):
            currentTrial = trials[f]

            left = currentTrial["leftGrid"]
            right = currentTrial["rightGrid"]

            if left is None:
                larger = currentTrial["correctAnswer"]
            else:
                totalL = sum(left)
                totalR = sum(right)

                if totalL > totalR:
                    larger = 0
                else:
                    larger = 1

            # We check the correctness of each trial manually by comparing ppt answer with correct answer.
            #if currentTrial["initialAnswer"] is not None:
            
            #if currentTrial["initialAnswer"] == currentTrial["correctAnswer"]:
            if currentTrial["initialAnswer"] == larger:
                cor1 = 1
            else:
                cor1 = 0

            # We don't have cor2 for the first few blocks where no advice is given.
            if currentTrial["finalAnswer"] is None:
                cor2 = None
            #elif currentTrial["finalAnswer"] == currentTrial["correctAnswer"]:
            elif currentTrial["finalAnswer"] == larger:
                cor2 = 1
            else:
                cor2 = 0

            trialType = currentTrial["typeName"]

            # Check for default advisor
            whichAdvisor = currentTrial["advisorId"]
            defaultAdvisor = currentTrial["defaultAdvisor"]
            if trialType == "force":
                defaultAdvisor = whichAdvisor
                advisorChanged = 0
            elif trialType == "forceblk4":
                defaultAdvisor = whichAdvisor
                advisorChanged = 0
            elif whichAdvisor != defaultAdvisor:
                advisorChanged = 1
            else:
                advisorChanged = 0
            

            # Response time calculations (from ms to seconds)
            try:
                rt1 = currentTrial["timeInitialResponse"]-currentTrial["timeInitialStimOff"]
                rt1 = round((rt1/1000), 2)
                rt2 = currentTrial["timeFinalResponse"]-currentTrial["timeFinalStart"]
                rt2 = round((rt2/1000), 2)
                ctcTime = currentTrial["timeFinalResponse"] - currentTrial["timeInitialResponse"]
                ctcTime = round((ctcTime/1000), 2)
            except:
                rt2 = None
                ctcTime = None

            if currentTrial["advisorAnswer"] == larger:
                advCorrect = 1
            else:
                advCorrect = 0

            # Write our data one trial at a time to the row.
            subject_writer.writerow([currentTrial["id"], currentTrial["block"]+1, currentTrial["practice"], currentTrial["correctAnswer"], currentTrial["dotDifference"], currentTrial["initialAnswer"], currentTrial["initialConfidence"], cor1, currentTrial["finalAnswer"], currentTrial["finalConfidence"], cor2, currentTrial["typeName"], whichAdvisor, defaultAdvisor, advisorChanged, currentTrial["advisorAnswer"], advCorrect, currentTrial["advisorConfidence"], rt1, rt2, ctcTime])

##    except Exception as e:
##        error = "Error in File: " + file
##        logf.write(error)
##        logf.write(str(e))
