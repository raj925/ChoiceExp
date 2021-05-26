# combineSurveys.py

# This script allows you to combine several csvs with participant
# survey responses into one file.
# This is needed because the questions are presented to participants
# in randomised order and then saved to files in this order.
# Hence, we need to combine responses together such that responses under
# each question is collated.

# Author: Sriraj Aiyer

import json
import pandas as pd
import csv
import glob, os
import numpy as np
import fnmatch

os.chdir("./Surveys")

for n in range(1,5):
    questions = []
    responses = []
    check = "*SURVEY" + str(n) + ".csv"
    ppts = 0;
    for file in glob.glob(check):
        aggFilename = "../allSurveys" + str(n) + ".csv"
        with open(aggFilename, mode = "w") as dataOut:
            with open(file) as dataIn:
                csv_writer = csv.writer(dataOut, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                df = pd.read_csv(dataIn)
                if len(questions) < 1:
                    for y in range(len(df.columns)):
                        questions.append(df.columns[y])
                add = []
                for x in range (0,len(questions)):
                    answer = df[questions[x]]
                    add.append(answer[0])
                responses.append(add)
                ppts = ppts + 1
                csv_writer.writerow(questions)
                for r in range (0,len(responses)):
                    csv_writer.writerow(responses[r])
                
                    
                
                        
