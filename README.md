Human vs Algor testing

See Social Metacogntion repository for foundation code, which this is built on top of.
This adaptation is for running dots task experiments for human vs algor advisors without the social elements. 
Some legacy code remains as of now, cleaning up in progress!

HOW TO RUN:
To run this code on a local machine, navigate to the filepath of this folder on your machine via terminal/comman prompt. Then run a localhost php server with the following command:

```
php -S localhost:8000
```

And then go to your localhost page on your browser:

```
localhost:8000
```

Items included in current implementation that are ready for use:
- Demographic questions
- Ability to plug in your own questionnaire at the start of the experiment
- Open ended debrief form

Items items in current implementation that are not ready for use (ie you will find some code alluding to these):
- Memory maintenance task
- N-Back Task
- Forecasting (Dietvorst) task

Experimental settings can be changed in the index.html file. You will find a code block with a bunch of variables that can be altered. It also gives you an idea of which features are currently in progress vs ready for use. 

When the experiment is complete, the data should save under the directory data/public/JSONs. You will then need Python installed to run the scripts in the public folder: jsonConvert.py and aggregateData.py. The former takes the JSON data files and extracts the key data points as CSVs to save in the data/public/Trials folder. The latter then computes aggregate data points for one csv file summarising all participants in the file allSubjects.csv. 
