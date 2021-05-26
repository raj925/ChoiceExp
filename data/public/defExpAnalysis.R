library(ggplot2)
library(ggrepel)
library(ggthemes)
library(extrafont)
library(grid)
library(cowplot)
library(hrbrthemes)
library(tidyverse)
library(viridis)
library(ggcorrplot)
library(psych)
library(Hmisc)
library(lavaan)
install.packages("ggpubr")
require(ggpubr)
install.packages("jtools")
require(jtools)
install.packages('ggstance')
require(ggstance)
install.packages('broom.mixed')
require(broom.mixed)
source("http://www.sthda.com/upload/rquery_cormat.r")
install.packages("lmerTest")
require(lmerTest)
install.packages("rmarkdown")
require(rmarkdown)
library(car)
install.packages("semPlot")
require(semPlot)
install.packages("aod")
require(aod)
library(lme4)
install.packages("factoextra")
require(factoextra)
library(pwr)
library(ltm)
library(lmtest)
install.packages("splithalf")
library(splithalf)
###################################################################
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
path <- paste(wd, "/DefOutput.csv", sep="")
expdata <- read.csv(path, header=TRUE, sep=",")
# Convert tibble to dataframe
expdata <- as.data.frame(expdata)

ncolumns <- ncol(expdata)
nrows <- nrow(expdata)

# Survey merging

wd <- dirname(rstudioapi::getSourceEditorContext()$path)
path <- paste(wd, "/IntandSpecSysSurveys.csv", sep="")
survey <- read.csv(path, header=TRUE, sep=",")
survey <- as.data.frame(survey)

# Reverse scoring

reversesScores <- c(6,7,8,9,12,14,15,16,17)
for (val in reversesScores)
{
  survey[1:102,val] <- 8 - survey[1:102,val]
}

# Int Sys Subscales and total

survey <- cbind(survey, abilitySub = rowSums(survey[2:5]))
survey <- cbind(survey, integritySub = rowSums(survey[6:9]))
survey <- cbind(survey, understandingSub = rowSums(survey[10:13]))
survey <- cbind(survey, intTotal = rowSums(survey[2:13]))
ncolumns <- ncol(survey)
survey <- cbind(survey, abilityDiff = rowSums(survey[(ncolumns-3)]-16))
survey <- cbind(survey, integrityDiff = rowSums(survey[(ncolumns-2)]-16))
survey <- cbind(survey, understandingDiff = rowSums(survey[(ncolumns-1)]-16))
survey <- cbind(survey, intTotalDiff = rowSums(survey[(ncolumns)]-48))


# Spec Sys Subscales and total

survey <- cbind(survey, transportSub = rowSums(survey[14:16]))
survey <- cbind(survey, decisionSub = rowSums(survey[17:19]))
survey <- cbind(survey, socialSub = rowSums(survey[20:22]))
survey <- cbind(survey, specTotal = rowSums(survey[14:22]))
ncolumns <- ncol(survey)
survey <- cbind(survey, transportDiff = rowSums(survey[(ncolumns-3)]-12))
survey <- cbind(survey, decisionDiff = rowSums(survey[(ncolumns-2)]-12))
survey <- cbind(survey, socialDiff = rowSums(survey[(ncolumns-1)]-12))
survey <- cbind(survey, specTotalDiff = rowSums(survey[(ncolumns)]-36))

# Merge the survey and other data together

names(expdata)[names(expdata) == "Participant.ID"] <- "PROLIFIC_PID"
mergedDf <- merge(x = survey, y = expdata, by = "PROLIFIC_PID", all = FALSE)

###################################################################
# Post-experiment survey data binding
totalCol <- ncol(mergedDf)
masterColOrder <- c("The.human.advisor.is.reliable.",
                    "I.am.confident.in.the.human.advisor.",
                    "I.can.trust.the.human.advisor.",
                    "The.human.advisor.is.dependable." ,
                    "The.computer.advisor.is.reliable.",                                              
                    "I.am.confident.in.the.computer.advisor.",                                        
                    "I.can.trust.the.computer.advisor.",                                              
                    "The.computer.advisor.is.dependable.",
                    "This.task.would.be.much.harder.without.an.advisor.",                             
                    "I.believe.the.human.s.advice.was.from.a.real.person.who.was.a.past.participant.",
                    "I.believe.the.computer.s.advice.was.from.a.real.algorithm.",
                    "I.feel.that.the.task.was.too.easy.")
for (x in 1:101)
{
  path <- paste(wd, "/Surveys/", toString(mergedDf$PROLIFIC_PID[x]), "_SURVEY.csv", sep="")
  trustSurvey <- read.csv(path, header=TRUE, sep=",")
  trustSurvey <- as.data.frame(trustSurvey)
  trustSurvey <- trustSurvey[,masterColOrder]
  ncolumns <- ncol(trustSurvey)
  for (y in 1:ncolumns)
  {
    mergedDf[x,totalCol+y] <- trustSurvey[1,y]
  }
}
for (n in 1:ncolumns)
{
  names(mergedDf)[totalCol+n] <- masterColOrder[n]
}

mergedDf <- cbind(mergedDf, humanTrustReport = rowSums(mergedDf[97:100]))
mergedDf <- cbind(mergedDf, algorTrustReport = rowSums(mergedDf[101:104]))
mergedDf <- cbind(mergedDf, algorTrustDiff = rowSums(mergedDf[110] - mergedDf[109]))

###################################################################
# Demographic data

wd <- dirname(rstudioapi::getSourceEditorContext()$path)
path <- paste(wd, "/DemoFile.csv", sep="")
demo <- read.csv(path, header=TRUE, sep=",")
demo <- as.data.frame(demo)

mergedDf <- merge(x = demo, y = mergedDf, by = "PROLIFIC_PID", all = FALSE)

# Gender Survey Split

mergedDfMales <- subset(mergedDf, Sex == "Male")
mergedDfFemales <- subset(mergedDf, Sex == "Female")
mergedDfOver25s <- subset(mergedDf, age > 25)
mergedDfUnder25s <- subset(mergedDf, age < 26)

se <- c(sd(mergedDfMales$intTotalDiff)/sqrt(length(mergedDfMales$intTotalDiff)),sd(mergedDfFemales$intTotalDiff)/sqrt(length(mergedDfFemales$intTotalDiff)))
xb <- c("Male Survey Total Average","Female Survey Total Average")
yb <- c(mean(mergedDfMales$intTotalDiff),mean(mergedDfFemales$intTotalDiff))
df <- data.frame("SurveyTotal" = xb, "Mean"= yb)
survey <- ggplot(df) +
  geom_bar( aes(x=SurveyTotal, y=Mean), colour="black", stat="identity", fill="blue", alpha=0.5) +
  geom_errorbar( aes(x=SurveyTotal, y=Mean, ymin=Mean-se, ymax=Mean+se), colour="orange", alpha=0.9, size=1.1, width = 0.4) +
  ylim(0, 10)

print(survey +
        ggtitle("Mean Survey Score by Gender") +
        labs(x = "Gender", y = "Average Score")) 

t.test(mergedDfMales$intTotalDiff,mergedDfFemales$intTotalDiff)

# Age Survey Split

se <- c(sd(mergedDfUnder25s$intTotalDiff)/sqrt(length(mergedDfUnder25s$intTotalDiff)),sd(mergedDfOver25s$intTotalDiff)/sqrt(length(mergedDfOver25s$intTotalDiff)))
xb <- c("Under 25 Survey Total Average","25 and Over Survey Total Average")
yb <- c(mean(mergedDfUnder25s$intTotalDiff),mean(mergedDfOver25s$intTotalDiff))
df <- data.frame("SurveyTotal" = xb, "Mean"= yb)
survey <- ggplot(df) +
  geom_bar( aes(x=SurveyTotal, y=Mean), colour="black", stat="identity", fill="blue", alpha=0.5) +
  geom_errorbar( aes(x=SurveyTotal, y=Mean, ymin=Mean-se, ymax=Mean+se), colour="orange", alpha=0.9, size=1.1, width = 0.4) +
  ylim(0, 10)

print(survey +
        ggtitle("Mean Survey Score by Age") +
        labs(x = "Age Group", y = "Average Score")) 

t.test(mergedDfUnder25s$intTotalDiff,mergedDfOver25s$intTotalDiff)


###################################################################
# Checking influence measures

for (x in 1:nrows)
{
  path <- paste(wd, "/Trials/", toString(mergedDf$PROLIFIC_PID[x]), "_TRIALS.csv", sep="")
  trials <- read.csv(path, header=TRUE, sep=",")
  trials <- as.data.frame(trials)
  forcedTrials <- trials[trials$trialType=="force",]
  humanTrials <- forcedTrials[forcedTrials$whichAdvisor=="1",]
  algorTrials <- forcedTrials[forcedTrials$whichAdvisor=="2",]
  
  humanTrials$cj2 <- with(humanTrials, ifelse(int1 == int2, cj2, cj2*-1))
  algorTrials$cj2 <- with(algorTrials, ifelse(int1 == int2, cj2, cj2*-1))
  
  
  humanAgTrials <- humanTrials[humanTrials$int1==humanTrials$advAnswer,]
  algorAgTrials <- algorTrials[algorTrials$int1==algorTrials$advAnswer,]
  humanDisTrials <- humanTrials[humanTrials$int1!=humanTrials$advAnswer,]
  algorDisTrials <- algorTrials[algorTrials$int1!=algorTrials$advAnswer,]
  
  humanAgConfDiff <- mean(humanAgTrials$cj2 - humanAgTrials$cj1)
  humanDisConfDiff <- mean(humanDisTrials$cj2 - humanDisTrials$cj1)
  algorAgConfDiff <- mean(algorAgTrials$cj2 - algorAgTrials$cj1)
  algorDisConfDiff <- mean(algorDisTrials$cj2 - algorDisTrials$cj1)
  
  influenceR <- (algorAgConfDiff - algorDisConfDiff) - (humanAgConfDiff - humanDisConfDiff)
  mergedDf$influenceR[x] <- influenceR
  
  mergedDf$humanAgConfDiffR[x] <- humanAgConfDiff
  mergedDf$humanDisConfDiffR[x] <- humanDisConfDiff
  mergedDf$algorAgConfDiffR[x] <- algorAgConfDiff
  mergedDf$algorDisConfDiffR[x] <- algorDisConfDiff
  
  # Now do change influence measures
  
  changeTrials <- trials[trials$trialType=="change",]
  humanTrials <- changeTrials[changeTrials$whichAdvisor=="1",]
  algorTrials <- changeTrials[changeTrials$whichAdvisor=="2",]
  
  humanAgTrials <- humanTrials[humanTrials$int1==humanTrials$advAnswer,]
  algorAgTrials <- algorTrials[algorTrials$int1==algorTrials$advAnswer,]
  humanDisTrials <- humanTrials[humanTrials$int1!=humanTrials$advAnswer,]
  algorDisTrials <- algorTrials[algorTrials$int1!=algorTrials$advAnswer,]
  
  humanAgConfDiff <- mean(humanAgTrials$cj2 - humanAgTrials$cj1)
  humanDisConfDiff <- mean(humanDisTrials$cj2 - humanDisTrials$cj1)
  algorAgConfDiff <- mean(algorAgTrials$cj2 - algorAgTrials$cj1)
  algorDisConfDiff <- mean(algorDisTrials$cj2 - algorDisTrials$cj1)
  
  humanAgKeptTrials <- humanAgTrials[humanAgTrials$advisorChanged=="0",]
  humanDisKeptTrials <- humanDisTrials[humanDisTrials$advisorChanged=="0",]
  algorAgKeptTrials <- algorAgTrials[algorAgTrials$advisorChanged=="0",]
  algorDisKeptTrials <- algorDisTrials[algorDisTrials$advisorChanged=="0",]
  humanAgChangedTrials <- humanAgTrials[humanAgTrials$advisorChanged=="1",]
  humanDisChangedTrials <- humanDisTrials[humanDisTrials$advisorChanged=="1",]
  algorAgChangedTrials <- algorAgTrials[algorAgTrials$advisorChanged=="1",]
  algorDisChangedTrials <- algorDisTrials[algorDisTrials$advisorChanged=="1",]
  
  humanAgKeptConfDiff <- mean(humanAgKeptTrials$cj2 - humanAgKeptTrials$cj1)
  humanDisKeptConfDiff <- mean(humanDisKeptTrials$cj2 - humanDisKeptTrials$cj1)
  algorAgKeptConfDiff <- mean(algorAgKeptTrials$cj2 - algorAgKeptTrials$cj1)
  algorDisKeptConfDiff <- mean(algorDisKeptTrials$cj2 - algorDisKeptTrials$cj1)
  humanAgChangedConfDiff <- mean(humanAgChangedTrials$cj2 - humanAgChangedTrials$cj1)
  humanDisChangedConfDiff <- mean(humanDisChangedTrials$cj2 - humanDisChangedTrials$cj1)
  algorAgChangedConfDiff <- mean(algorAgChangedTrials$cj2 - algorAgChangedTrials$cj1)
  algorDisChangedConfDiff <- mean(algorDisChangedTrials$cj2 - algorDisChangedTrials$cj1)
  
  influenceKept <- (humanAgKeptConfDiff - humanDisKeptConfDiff) - (algorAgKeptConfDiff - algorDisKeptConfDiff)
  mergedDf$influenceKept[x] <- influenceKept
  influenceChanged <- (humanAgChangedConfDiff - humanDisChangedConfDiff) - (algorAgChangedConfDiff - algorDisChangedConfDiff)
  mergedDf$influenceChanged[x] <- influenceChanged
  
  mergedDf$humanAgKeptConfDiff[x] <- humanAgKeptConfDiff
  mergedDf$humanDisKeptConfDiff[x] <- humanDisKeptConfDiff
  mergedDf$algorAgKeptConfDiff[x] <- algorAgKeptConfDiff
  mergedDf$algorDisKeptConfDiff[x] <- algorDisKeptConfDiff
  mergedDf$humanAgChangedConfDiff[x] <- humanAgChangedConfDiff
  mergedDf$humanDisChangedConfDiff[x] <- humanDisChangedConfDiff
  mergedDf$algorAgChangedConfDiff[x] <- algorAgChangedConfDiff
  mergedDf$algorDisChangedConfDiff[x] <- algorDisChangedConfDiff
  
  ##########
  # Z Score Influence Values
  
  path <- paste(wd, "/Trials/", toString(mergedDf$PROLIFIC_PID[x]), "_TRIALS.csv", sep="")
  trials <- read.csv(path, header=TRUE, sep=",")
  trials <- as.data.frame(trials)
  forcedTrials <- trials[trials$trialType=="force",]
  humanTrials <- forcedTrials[forcedTrials$whichAdvisor=="1",]
  algorTrials <- forcedTrials[forcedTrials$whichAdvisor=="2",]
  
  meanCjH <- mean(c(humanTrials$cj1,humanTrials$cj2))
  sdCjH <- sd(c(humanTrials$cj1,humanTrials$cj2))
  meanCjA <- mean(c(algorTrials$cj1,algorTrials$cj2))
  sdCjA <- sd(c(algorTrials$cj1,algorTrials$cj2))
  
  # meanCj1H <- mean(humanTrials$cj1)
  # meanCj2H <- mean(humanTrials$cj2)
  # meanCj1A <- mean(algorTrials$cj1)
  # meanCj2A <- mean(algorTrials$cj2)
  # 
  # sdCj1H <- sd(humanTrials$cj1)
  # sdCj2H <- sd(humanTrials$cj2)
  # sdCj1A <- sd(algorTrials$cj1)
  # sdCj2A <- sd(algorTrials$cj2)
  
  humanTrials$cj1 <- (humanTrials$cj1 - meanCjH)/sdCjH
  humanTrials$cj2 <- (humanTrials$cj2 - meanCjH)/sdCjH
  algorTrials$cj1 <- (algorTrials$cj1 - meanCjA)/sdCjA
  algorTrials$cj2 <- (algorTrials$cj2 - meanCjA)/sdCjA
  
  humanAgTrials <- humanTrials[humanTrials$int1==humanTrials$advAnswer,]
  algorAgTrials <- algorTrials[algorTrials$int1==algorTrials$advAnswer,]
  humanDisTrials <- humanTrials[humanTrials$int1!=humanTrials$advAnswer,]
  algorDisTrials <- algorTrials[algorTrials$int1!=algorTrials$advAnswer,]
  
  humanAgConfDiff <- mean(humanAgTrials$cj2 - humanAgTrials$cj1)
  humanDisConfDiff <- mean(humanDisTrials$cj2 - humanDisTrials$cj1)
  algorAgConfDiff <- mean(algorAgTrials$cj2 - algorAgTrials$cj1)
  algorDisConfDiff <- mean(algorDisTrials$cj2 - algorDisTrials$cj1)
  
  influenceR <- (algorAgConfDiff - algorDisConfDiff) - (humanAgConfDiff - humanDisConfDiff)
  mergedDf$influenceZScore[x] <- influenceR
  
  mergedDf$humanAgConfDiffZScore[x] <- humanAgConfDiff
  mergedDf$humanDisConfDiffZScore[x] <- humanDisConfDiff
  mergedDf$algorAgConfDiffZScore[x] <- algorAgConfDiff
  mergedDf$algorDisConfDiffZScore[x] <- algorDisConfDiff
  
}

###################################################################

# Calculating confidence difference when changing or keep advisors

for (x in 1:nrows)
{
  path <- paste(wd, "/Trials/", toString(mergedDf$PROLIFIC_PID[x]), "_TRIALS.csv", sep="")
  trials <- read.csv(path, header=TRUE, sep=",")
  trials <- as.data.frame(trials)
  defaultTrials <- trials[trials$trialType=="change",]
  humanTrials <- defaultTrials[defaultTrials$whichAdvisor=="1",]
  algorTrials <- defaultTrials[defaultTrials$whichAdvisor=="2",]
  
  keptTrials <- defaultTrials[defaultTrials$advisorChanged=="0",]
  changedTrials <- defaultTrials[defaultTrials$advisorChanged=="1",]
  
  ovKeptConfDiff <- mean(keptTrials$cj2 - keptTrials$cj1)
  ovChangedConfDiff <- mean(changedTrials$cj2 - changedTrials$cj1)
  
}


###################################################################


path <- paste(wd, "/DefDataSurveyMerged.csv", sep="")
write.csv(mergedDf, path)

###################################################################
thresholdU <- mean(mergedDf$influenceR) + (sd(mergedDf$influenceR)*2)
thresholdL <- mean(mergedDf$influenceR) - (sd(mergedDf$influenceR)*2)
tempDfNoOLs <- mergedDf[mergedDf$influenceR>thresholdL,]
tempDfNoOLs <- tempDfNoOLs[tempDfNoOLs$influenceR<thresholdU,]
tempOLS <-  mergedDf[mergedDf$influenceR>thresholdU,]

aboveChance <- mergedDf[mergedDf$Mean.Cj1.Accuracy>0.58,]

infChoice <- ggplot(data = mergedDf, aes(x=influenceZScore, y=intTotalDiff), xlab="Influence" , ylab="Advisor Change Proportion") +
  geom_point() +
  ylim(-25, 25) + 
  xlim(-1,1) + 
  geom_smooth(method=lm , color="blue", fill="#69b3a2", se=TRUE) 

print(infChoice + 
        ggtitle("Relative Influence against Intelligent Systems Total Diff - Default Dataset")
      + labs(y="Intelligent Systems Total Diff", x = "Relative Influence"))

cor.test(mergedDf$influenceZScore,mergedDf$intTotalDiff)

###################################################################

df <- data.frame(
  choice = mergedDf$Choice.of.Algorithm,
  influence = mergedDf$influenceR,
  algorDefaultKept = mergedDf$Algor.Default.Trials.Not.Changed,
  humanDefaultChanged = mergedDf$Human.Trials.Changed.to.Algor,
  algorTrustDiff = mergedDf$algorTrustDiff,
  intTotalDiff = mergedDf$intTotalDiff,
  specTotalDiff = (mergedDf$specTotal-36),
  ability = mergedDf$abilityDiff,
  integrity = mergedDf$integrityDiff,
  understanding = mergedDf$understandingDiff,
  transport = mergedDf$transportDiff,
  social = mergedDf$socialDiff,
  decision = mergedDf$decisionDiff
)

df <- data.frame(
  intTotalDiff = mergedDf$intTotalDiff,
  specTotalDiff = (mergedDf$specTotal-36),
  ability = mergedDf$abilityDiff,
  integrity = mergedDf$integrityDiff,
  understanding = mergedDf$understandingDiff,
  transport = mergedDf$transportDiff,
  social = mergedDf$socialDiff,
  decision = mergedDf$decisionDiff
)

corrs <- round(cor(df), 4)
p.mat <- cor_pmat(corrs)

rquery.cormat(df)

# Plot
ggcorrplot(corrs, 
           hc.order = FALSE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle",
           outline.col = "white",
           colors = c("#6D9EC1", "white", "#E46726"), 
           title="Correlogram of Study Variables", 
           p.mat = p.mat,
           ggplot2::theme_gray)

plot(df , pch=20 , cex=1.5 , col="#69b3a2")

pairs.panels(df, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             lm = TRUE
)

##################################################################
####################################################################
infInt <- ggplot(data = mergedDf, aes(x=intTotalDiff, y=specTotal-36), xlab="Intelligent Systems Score Diff" , ylab="Relative Influence") +
  geom_point() +
  ylim(-20, 20) + 
  xlim(-20, 20) +
  geom_smooth(method=lm , color="orange", fill="#69b3a2", se=TRUE) 

print(infInt + 
        ggtitle("Intelligent Systems Total against Specific Systems Total")
      + labs(y="Specific Systems Diff", x = "Intelligent Systems Diff"))

cor.test(mergedDf$intTotalDiff,mergedDf$specTotal-36)

model <- lm(intTotalDiff ~ influenceR, data = mergedDf)
plot(model, 3)

####################################################################
mergedDf$ovKeptConfDiff[mergedDf$ovChangedConfDiff == "NaN"] <- NA
mergedDf$ovChangedConfDiff[mergedDf$ovChangedConfDiff == "NaN"] <- NA
tempKept <- na.omit(mergedDf$ovKeptConfDiff)
tempChanged <- na.omit(mergedDf$ovChangedConfDiff)

se <- c(sd(tempKept)/sqrt(length(tempKept)),sd(tempChanged)/sqrt(length(tempChanged)))
xb <- c("Mean Confidence Difference When Keeping an Advisor","Mean Confidence Difference When Changing Advisor")
yb <- c(mean(tempKept),mean(tempChanged))
df <- data.frame("Trial" = xb, "Mean"= yb)
defBeh <- ggplot(df) +
  geom_bar( aes(x=Trial, y=Mean), colour="black", stat="identity", fill="blue", alpha=0.5) +
  geom_errorbar( aes(x=Trial, y=Mean, ymin=Mean-se, ymax=Mean+se), colour="orange", alpha=0.9, size=1.1, width = 0.4) +
  ylim(-5, 5)

print(defBeh +
        ggtitle("Confidence Difference When Keeping or Changing Advisors") +
        labs(x = "Trial Type", y = "Confidence Difference"))

t.test(tempKept,tempChanged)


mergedDf$humanKeptConfDiff[mergedDf$humanKeptConfDiff == "NaN"] <- NA
mergedDf$humanChangedConfDiff[mergedDf$humanChangedConfDiff == "NaN"] <- NA
mergedDf$algorKeptConfDiff[mergedDf$algorKeptConfDiff == "NaN"] <- NA
mergedDf$algorChangedConfDiff[mergedDf$algorChangedConfDiff == "NaN"] <- NA

tempHumanKept <- na.omit(mergedDf$humanKeptConfDiff)
tempHumanChanged <- na.omit(mergedDf$humanChangedConfDiff)
tempAlgorKept <- na.omit(mergedDf$algorKeptConfDiff)
tempAlgorChanged <- na.omit(mergedDf$algorChangedConfDiff)

se <- c(sd(tempHumanKept)/sqrt(length(tempHumanKept)),sd(tempHumanChanged)/sqrt(length(tempHumanChanged)),sd(tempAlgorKept)/sqrt(length(tempAlgorKept)),sd(tempAlgorChanged)/sqrt(length(tempAlgorChanged)))
xb <- c("1. Keeping Human Advisor","2. Changing to Human Advisor","3. Keeping Algor Advisor","4. Changing to Algor Advisor")
yb <- c(mean(tempHumanKept),mean(tempHumanChanged),mean(tempAlgorKept),mean(tempAlgorChanged))
df <- data.frame("Trial" = xb, "Mean"= yb)
defBeh <- ggplot(df) +
  geom_bar( aes(x=Trial, y=Mean), colour="black", stat="identity", fill="blue", alpha=0.5) +
  geom_errorbar( aes(x=Trial, y=Mean, ymin=Mean-se, ymax=Mean+se), colour="orange", alpha=0.9, size=1.1, width = 0.4) +
  ylim(-5, 5)

print(defBeh +
        ggtitle("Confidence Difference When Keeping or Changing Advisors") +
        labs(x = "Trial Type", y = "Confidence Difference"))



####################################################################
histo <- hist(mergedDf$Advisor.Changed, breaks=12, plot=F)
colours <- ifelse(histo$breaks < 0.2, rgb(0.2,0.2,0.2,0.2) , ifelse (histo$breaks >=0.8, "lightblue", rgb(0.2,0.8,0.5,0.5) ))
plot(histo, col=colours, border = F, main = "Distribution of Advisor Changes", xlab="Percentile", xlim=c(0,1), ylim=c(0,20))

####################################################################
humanKeptInf <- mergedDf$humanAgKeptConfDiff - mergedDf$humanDisKeptConfDiff
humanChangedInf <- mergedDf$humanAgChangedConfDiff - mergedDf$humanDisChangedConfDiff
algorKeptInf <- mergedDf$algorAgKeptConfDiff - mergedDf$algorDisKeptConfDiff
algorChangedInf <- mergedDf$algorAgChangedConfDiff - mergedDf$algorDisChangedConfDiff

confDiffData <- c(humanKeptInf, humanChangedInf, algorKeptInf,algorChangedInf)
confDiffData[confDiffData == "NaN"] <- NA
confDiffData <- as.data.frame(confDiffData)
confDiffData <- cbind(confDiffData,c(rep("kept",nrows),rep("changed",nrows),rep("kept",nrows),rep("changed",nrows)))
confDiffData <- cbind(confDiffData,c(rep("human",nrows*2),rep("algor",nrows*2)))
names(confDiffData)[1] <- "ConfDiff"
names(confDiffData)[2] <- "TrialType"
names(confDiffData)[3] <- "Advisor"

confDiffDataComp <- confDiffData[complete.cases(confDiffData), ]

p <- ggboxplot(confDiffDataComp, x = "TrialType", y = "ConfDiff", color = "Advisor",
               palette = c("#00AFBB", "#E7B800"))

plot(p)

res.aov2 <- aov(ConfDiff ~ TrialType + Advisor + TrialType:Advisor, data = confDiffDataComp)
summary(res.aov2)

####################################################################


HS.model1 <- ' latentTrust  =~ intTotalDiff + Choice.of.Algorithm + influenceR
              influenceR ~~ Choice.of.Algorithm 
              influenceR ~~ intTotalDiff
              Choice.of.Algorithm ~~ intTotalDiff
              algorTrustDiff ~ latentTrust'



# fit the model
fit1 <- sem(HS.model1, data=tempDfNoOLs, se="bootstrap")

# display summary output
summary(fit1, fit.measures=TRUE, standardized=TRUE)

semPaths(fit1)

####################################################################
defChangeDiff <- mergedDf$Human.Trials.Changed.to.Algor - mergedDf$Algor.Trials.Changed.to.Human
mergedDf$defChangeDiff <- defChangeDiff

infChoice <- ggplot(data = mergedDf, aes(x=influenceR, y=defChangeDiff), xlab="Influence" , ylab="Advisor Change Proportion") +
  geom_point() +
  ylim(-1, 1) + 
  xlim(-50,50) + 
  geom_smooth(method=lm , color="blue", fill="#69b3a2", se=TRUE) 

print(infChoice + 
        ggtitle("Relation Between Relative Influence and Difference in Default Changes")
      + labs(y="Default Change Diff", x = "Relative Influence"))

cor.test(mergedDf$influenceR,mergedDf$defChangeDiff,method="kendall")


####################################################################
pwr.r.test(r=0.2,power=0.95,sig.level=0.01,alternative="two.sided")

####################################
# Aggregate all trials into one file
allTrials <- data.frame(stringsAsFactors = FALSE)
for (x in 1:nrows)
{
  path <- paste(wd, "/Trials/", toString(expdata$Participant.ID[x]), "_TRIALS.csv", sep="")
  trials <- read.csv(path, header=TRUE, sep=",")
  trials <- as.data.frame(trials)
  allTrials <- rbind(allTrials,trials)
}
allTrialsRows <- nrow(allTrials)
allTrials <- cbind(participantID = "ID", allTrials)
allTrials$participantID <- as.character(allTrials$participantID)
expDataCount <- 1
count <- 1
for (x in 1:allTrialsRows)
{
  if (count == 451)
  {
    expDataCount = expDataCount + 1
    count <- 1
  }
  allTrials$participantID[x] <- as.character(expdata$Participant.ID[expDataCount])
  count <- count + 1
}
 
####################################

splithalf(data = Hedge_raw,
          outcome = "RT",
          score = "difference",
          permutations = 5000,
          var.trialnum = "Trial",
          var.condition = "time",
          conditionlist = c(1, 2),
          var.compare = "Condition",
          compare1 = "congruent",
          compare2 = "incongruent",
          var.participant = "ppid",
          var.RT = "Reactiontime" )

####################################

summary(lm(formula = influenceR ~ integrityDiff + Choice.of.Algorithm + algorTrustDiff, data=mergedDf))
