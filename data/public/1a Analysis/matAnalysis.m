% clc;
% sca;
% clear;
%1a Replication Analysis

paths = {'./Trials/30032020_5cabd60c4311a70001cfcc14_TRIALS.csv'...
    './Trials/30032020_56ce42d9465e580006846f57_TRIALS.csv'...
    './Trials/30032020_5e59bb355f26921d02b4ec5f_TRIALS.csv'...
    './Trials/30032020_5e5912f6ff7f7b121cf4e6b1_TRIALS.csv'...
    './Trials/30032020_5d40688398eff00001b7f3de_TRIALS.csv'...
    './Trials/30032020_5c60792cced0b900018cbe77_TRIALS.csv'...
    './Trials/26032020_5e1482e0f82df8000d66665f_TRIALS.csv'...
    './Trials/26032020_575341f0dcd903000606a800_TRIALS.csv'...
    './Trials/26032020_59f0501a2f63d30001c902bd_TRIALS.csv'...
    './Trials/26032020_59e0be41d838ae0001850712_TRIALS.csv'...
    './Trials/26032020_59bd9d713c45a10001ccca7a_TRIALS.csv'...
    './Trials/26032020_59b53d8ed98aab00019bb17f_TRIALS.csv'...
    './Trials/26032020_5e6011026e31c836f4ca5ac9_TRIALS.csv'...
    './Trials/26032020_5e3681557cbf7461513bc8f0_TRIALS.csv'...
    './Trials/26032020_5e5554bb2f1d9c56332af459_TRIALS.csv'...
    './Trials/26032020_5e668f3107e41f04e1ff4a95_TRIALS.csv'...
    './Trials/26032020_5e66a6f3ae413f08251afcb0_TRIALS.csv'...
    './Trials/26032020_5e25b9d13da41b02348ec39e_TRIALS.csv'...
    './Trials/26032020_5e23a2cb93a689642fffaa30_TRIALS.csv'...
    './Trials/26032020_5e4ea83560c37d2eafb2ae3b_TRIALS.csv'...
    './Trials/26032020_5e4e86b070e1ed2d6e4e3f62_TRIALS.csv'...
    './Trials/26032020_5e4bc0c29b9a2e01310c6506_TRIALS.csv'...
    './Trials/26032020_5e3df34cfb9ed506f1028bb5_TRIALS.csv'...
    './Trials/26032020_5e0e925fff28a85e40514175_TRIALS.csv'...
    './Trials/26032020_5dfd52e3b63853a4125bd77d_TRIALS.csv'...
    './Trials/26032020_5dfcc5cf7dd4779b542dc38d_TRIALS.csv'...
    './Trials/26032020_5de66395047da35de324deed_TRIALS.csv'...
    './Trials/26032020_5dcfaad2dd9a740c2493fc28_TRIALS.csv'...
    './Trials/26032020_5dc81319f375a959ed0ae171_TRIALS.csv'...
    './Trials/26032020_5db396d5b449ad000cbfc38c_TRIALS.csv'...
    './Trials/26032020_5db29ce254945b003c8ef699_TRIALS.csv'...
    './Trials/26032020_5db6ff8c7f9e6b000dd24a19_TRIALS.csv'...
    './Trials/26032020_5dafea4de40355001651fa2f_TRIALS.csv'...
    './Trials/26032020_5da98138d11d560013a3d22d_TRIALS.csv'...
    './Trials/26032020_5d504197f279ae000195bb85_TRIALS.csv'...
    './Trials/26032020_5d531cd93c38f100016b53f9_TRIALS.csv'...
    './Trials/26032020_5d36f9692fd5a4001ae5929f_TRIALS.csv'...
    './Trials/26032020_5d6e9abe5e156a00018de2bc_TRIALS.csv'...
    './Trials/26032020_5cc180984d7d8d001858d5e2_TRIALS.csv'...
    './Trials/26032020_5c916456ec114b0016418dca_TRIALS.csv'...
    './Trials/26032020_5c8566ba0d4ac0000196d065_TRIALS.csv'...
    './Trials/26032020_5c6400cbcb937d00012d8866_TRIALS.csv'...
    './Trials/26032020_5c741cdd2af891001707e1e8_TRIALS.csv'...
    './Trials/26032020_5c38d2e708510f0001445b61_TRIALS.csv'...
    './Trials/26032020_5c6d1972349b1f000104f83e_TRIALS.csv'...
    './Trials/26032020_5c0f111d52bf070001d27721_TRIALS.csv'...
    './Trials/26032020_5bfdca7233f05f00016234fb_TRIALS.csv'...
    './Trials/26032020_5be1c4f8397d120001a01d76_TRIALS.csv'...
    './Trials/26032020_5bc0ae2c9dd2d9000112c0b6_TRIALS.csv'...
    './Trials/26032020_5b85544331eb7b0001efd72e_TRIALS.csv'...
    './Trials/26032020_5b59a51fca6d01000157a8c3_TRIALS.csv'...
    './Trials/26032020_5aea8ea1a110a00001f7e6a9_TRIALS.csv'...
    './Trials/26032020_5a5459cce0cf3d0001260ebd_TRIALS.csv'...
    './Trials/16032020_5df2235d5a34251266ea645e_TRIALS.csv'...
    './Trials/16032020_5d175bfe7a5f6d001a64b545_TRIALS.csv'...
    './Trials/16032020_5c3ddbdc337ac90001a4e62e_TRIALS.csv'...
    './Trials/13032020_5e629c6ad154ce000d45fb63_TRIALS.csv'...
    './Trials/13032020_5c6709bb5aa5610001dd6cfc_TRIALS.csv'};

nparticipants = 58;
fullDataCount = 0;



for x = 1:nparticipants
    
   trials=readtable(paths{x});
   trials = table2struct(trials);
   filename = paths{x};
   filename = split(filename, '/');
   matfile = split(filename(3), '_');
   ID = char(matfile(2));
   matfile = char(strcat('./matFiles/', matfile(1), '_', ID, '.mat'));
   save(matfile, 'trials');
   
   if size(trials,1) == 450 && ~isnan(trials(450).cj1)
       fullDataCount = fullDataCount + 1;
       s = fullDataCount;
          
        %%  Trials Data Variables
        % Where we call all of the data columns that we want from the trials
        % dataset for each participant.
        trainingDD = [trials(1:90).dotdifference].';
        wherelarger = [trials(91:450).wherelarger].';
        cj1 = [trials(91:450).cj1].';
        cj2 = [trials(91:450).cj2].';
        int1 = [trials(91:450).int1].';
        int2 = [trials(91:450).int2].';
        advisorAnswer = [trials(91:450).advAnswer].';
        whichAdvisor = [trials(91:450).whichAdvisor].';
        trialType = {trials(91:450).trialType}.';
        cor1 = [trials(91:450).cor1].';
        cor2 = [trials(91:450).cor2].';
        advCorrect = [trials(91:450).advCorrect].';
        dotRT = [trials(91:450).rt1].'; 
        ctcTime = [trials(91:450).ctcTime].';
        block = [trials(91:450).block].';
        forcedTrial = trialType=="force";

        % Compute computer advisor selection on choice trials
        computerAdvisorChoice = (whichAdvisor==2*(trialType=="choice"));

        % Compute whether advisor was right or wrong on forced trials
        humanAdvisorCorrect = (whichAdvisor==1&trialType=="force"&advCorrect==1);
        humanAdvisorWrong = (whichAdvisor==1&trialType=="force"&advCorrect==0);
        computerAdvisorCorrect = (whichAdvisor==2&trialType=="force"&advCorrect==1);
        computerAdvisorWrong = (whichAdvisor==2&trialType=="force"&advCorrect==0);

        % Compute computer advisor selection on choice trials
        computerAdvisorChoice = (whichAdvisor==2*(trialType=="choice"));

        % Move HAC/HAW/CAC/CAW down one line and add a line on to the end of trialType and computerAdvisorChoice by adding zeros
        a=0;
        tempHAC = [a;humanAdvisorCorrect];
        tempHAW = [a;humanAdvisorWrong];
        tempCAC = [a;computerAdvisorCorrect];
        tempCAW = [a;computerAdvisorWrong];
        tempTrialType = [trialType;a];
        tempComputerAdvisorChoice = [computerAdvisorChoice;a];
        tempForcedTrial = [forcedTrial;a];

        nslots = 359;
        for n = 1:nslots
            if char(tempTrialType(n,1)) == "force"
                if char(tempTrialType(n+1)) == "force"
                    tempHAC(n+1,1) = 0;tempHAW(n+1,1) = 0;tempCAC(n+1,1) = 0;tempCAW(n+1,1) = 0;
                end
            end
            if tempHAC(n,1) == 0 && tempHAW(n,1) == 0 && tempCAC(n,1) == 0 && tempCAW(n,1) == 0
                tempComputerAdvisorChoice(n,1)= 0;
            end
        end

        % Computer as choice after HAC, HAW, CAC, and CAW
        computerChoiceAfterHAC = (tempHAC==1 & tempComputerAdvisorChoice==1);
        computerChoiceAfterHAW = (tempHAW==1 & tempComputerAdvisorChoice==1);
        computerChoiceAfterCAC = (tempCAC==1 & tempComputerAdvisorChoice==1);
        computerChoiceAfterCAW = (tempCAW==1 & tempComputerAdvisorChoice==1);

        % Percent Computer after HAC, HAW, CAC, and CAW
        percentCCAfterHAC = sum(computerChoiceAfterHAC)/sum(tempHAC);
        percentCCAfterHAW = sum(computerChoiceAfterHAW)/sum(tempHAW);
        percentCCAfterCAC = sum(computerChoiceAfterCAC)/sum(tempCAC);
        percentCCAfterCAW = sum(computerChoiceAfterCAW)/sum(tempCAW);

        % Check if actual advice seen is balanced between advisor type and correct/incorrect advice
        humanCorrectForcedTrialsPercent = (sum(humanAdvisorCorrect)/60);
        humanWrongForcedTrialsPercent = (sum(humanAdvisorWrong)/60);
        computerCorrectForcedTrialsPercent = (sum(computerAdvisorCorrect)/60);
        computerWrongForcedTrialsPercent = (sum(computerAdvisorWrong)/60);
        
        meanCj1WhenWrong = mean(cj1(cor1==0));
        meanCj1WhenCorrect = mean(cj1(cor1==1));
        meanCj2WhenWrong = mean(cj2(cor1==0));
        meanCj2WhenCorrect = mean(cj2(cor1==1));
    
        resolution = abs(meanCj1WhenWrong - meanCj1WhenCorrect);
        resolution2 = abs(meanCj2WhenWrong - meanCj2WhenCorrect);
        
        algorChoice = sum(whichAdvisor==2*(trialType=="choice"))/sum(trialType=="choice");
        
        cj1Raw = cj1;
        cj2Raw = cj2;
        cj1Raw(int1==0) = -cj1Raw(int1==0);
        cj2Raw(int2==0) = -cj2Raw(int2==0);
        
        % Flipped values for confidence difference
        cj2flip = cj2Raw;
        elementsToChange = (int1 == 0);
        cj2flip(elementsToChange) = -cj2flip(elementsToChange);
%         cj1flip = [trials(91:450).cj1].';
%         elementsToChange = (int1 == 0);
%         cj1flip(elementsToChange) = -cj1flip(elementsToChange);
        flippedConfDiff = (cj2flip - cj1);
    
        % Compute agree and disagree advice by advisor type
        algorAdvisorSameAnswer = (whichAdvisor==2&trialType=="force"&int1 == advisorAnswer);
        algorAdvisorDiffAnswer = (whichAdvisor==2&trialType=="force"&int1 ~= advisorAnswer);
        humanAdvisorSameAnswer = (whichAdvisor==1&trialType=="force"&int1 == advisorAnswer);
        humanAdvisorDiffAnswer = (whichAdvisor==1&trialType=="force"&int1 ~= advisorAnswer);
       
            % Compute means
        algorAgreeConfDiff = mean(flippedConfDiff(algorAdvisorSameAnswer==1));
        algorDisagreeConfDiff = mean(flippedConfDiff(algorAdvisorDiffAnswer==1));
        humanAgreeConfDiff = mean(flippedConfDiff(humanAdvisorSameAnswer==1));
        humanDisagreeConfDiff = mean(flippedConfDiff(humanAdvisorDiffAnswer==1));
        
        % calculate the 'Computer Influence Bigger Than Human' variable
        algorInfluence = ((algorAgreeConfDiff-algorDisagreeConfDiff)-(humanAgreeConfDiff-humanDisagreeConfDiff));
        
       % Create quintiles by first confidence judgement (more self confidence)
       % and percentage of advisor 1 being chosen
       % BLOCKS 5 AND 6
        quants1=quantile(cj1,[.25 .5 .75]);
        quantAdvisor1=sum(trialType=="choice"&whichAdvisor==2&cj1<=quants1(1))/sum(trialType=="choice"&cj1<=quants1(1));
        quantAdvisor2=sum(trialType=="choice"&whichAdvisor==2&cj1>quants1(1)&cj1<=quants1(2))/sum(trialType=="choice"&cj1>quants1(1)&cj1<=quants1(2));
        quantAdvisor3=sum(trialType=="choice"&whichAdvisor==2&cj1>quants1(2)&cj1<=quants1(3))/sum(trialType=="choice"&cj1>quants1(2)&cj1<=quants1(3));
        quantAdvisor4=sum(trialType=="choice"&whichAdvisor==2&cj1>quants1(3))/sum(trialType=="choice"&cj1>quants1(3));

        quantCorrect1=sum(cor1==1&cj1<=quants1(1))/sum(cj1<=quants1(1));
        quantCorrect2=sum(cor1==1&cj1>quants1(1)&cj1<=quants1(2))/sum(cj1>quants1(1)&cj1<=quants1(2));
        quantCorrect3=sum(cor1==1&cj1>quants1(2)&cj1<=quants1(3))/sum(cj1>quants1(2)&cj1<=quants1(3));
        quantCorrect4=sum(cor1==1&cj1>quants1(3))/sum(cj1>quants1(3));
        
        obsAdvAccDiffBlk4 = ((sum(block==4&advCorrect==1&whichAdvisor==2))/sum(block==4&whichAdvisor==2)) - ((sum(whichAdvisor==1&block==4&advCorrect==1))/sum(whichAdvisor==1&block==4));
        obsAdvAccDiffBlk4Force = (sum(block==4&advCorrect==1&whichAdvisor==2&trialType=="force")/sum(block==4&whichAdvisor==2&trialType=="force")) - (sum(whichAdvisor==1&block==4&advCorrect==1&trialType=="force")/sum(whichAdvisor==1&block==4&trialType=="force"));
        algorAccBlk4 = sum(block==4&advCorrect==1&whichAdvisor==2)/sum(block==4&whichAdvisor==2);
        blk4Choice = sum(whichAdvisor==2*(block==4*(trialType=="choice")))/sum(block==4*(trialType=="choice"));
        choiceAfterBlk4 = sum(whichAdvisor==2*(block~=4*(trialType=="choice")))/sum(block~=4*(trialType=="choice"));
        cj1Acc = sum(cor1==1)/360;
        cj2Acc = sum(cor2==1)/360;
        algorSway = abs(mean(cj2Raw(whichAdvisor==2))-mean(cj1Raw(whichAdvisor==2)));
        humanSway = abs(mean(cj2Raw(whichAdvisor==1))-mean(cj1Raw(whichAdvisor==1)));
        trainingDDMin = min(trainingDD);

        
        alldata(s).ID = ID;
        alldata(s).minDD = trainingDDMin;
        alldata(s).algorChoice = algorChoice;
        alldata(s).resolution = resolution;
        alldata(s).resolution2 = resolution2;
        alldata(s).algorInfluence = algorInfluence;
        alldata(s).AdvisorIgnoredTrials = sum(cj1Raw==cj2Raw);
        
        alldata(s).quantAdvisor1=quantAdvisor1;
       alldata(s).quantAdvisor2=quantAdvisor2;
       alldata(s).quantAdvisor3=quantAdvisor3;
       alldata(s).quantAdvisor4=quantAdvisor4;

       alldata(s).quantCorrect1=quantCorrect1;
       alldata(s).quantCorrect2=quantCorrect2;
       alldata(s).quantCorrect3=quantCorrect3;
       alldata(s).quantCorrect4=quantCorrect4;

       alldata(s).finalDD = trials(450).dotdifference;
       alldata(s).obsAdvAccDiffBlk4 = obsAdvAccDiffBlk4;
       alldata(s).obsAdvAccDiffBlk4Force = obsAdvAccDiffBlk4Force;
       alldata(s).algorAccBlk4 = algorAccBlk4;
       alldata(s).blk4Choice = blk4Choice;
       alldata(s).choiceAfterBlk4 = choiceAfterBlk4;
       alldata(s).meanCj1 = mean(cj1);
       alldata(s).cj1Acc = cj1Acc;
       alldata(s).cj2Acc = cj2Acc;
       alldata(s).meanRT = mean(dotRT);
       alldata(s).meanCTC = mean(ctcTime);
       alldata(s).algorSway = algorSway;
       alldata(s).humanSway = humanSway;
       alldata(s).algorRelativeSway = algorSway - humanSway;
       
       alldata(s).algorAgreeConfDiff = algorAgreeConfDiff;
       alldata(s).algorDisagreeConfDiff = algorDisagreeConfDiff;
       alldata(s).humanAgreeConfDiff = humanAgreeConfDiff;
       alldata(s).humanDisagreeConfDiff = humanDisagreeConfDiff;
       
       alldata(s).percentCCAfterHAC = percentCCAfterHAC;
       alldata(s).percentCCAfterHAW = percentCCAfterHAW;
       alldata(s).percentCCAfterCAC = percentCCAfterCAC;
       alldata(s).percentCCAfterCAW = percentCCAfterCAW;
       alldata(s).humanCorrectForcedTrialsPercent = humanCorrectForcedTrialsPercent;
       alldata(s).humanWrongForcedTrialsPercent = humanWrongForcedTrialsPercent;
       alldata(s).computerCorrectForcedTrialsPercent = computerCorrectForcedTrialsPercent;
       alldata(s).computerWrongForcedTrialsPercent = computerWrongForcedTrialsPercent;
%        
%        [sdtCounts1, sdtCounts2] = trials2counts(wherelarger,int1,cj1,50,1);
%        metaD = fit_meta_d_MLE(sdtCounts1, sdtCounts2, 1);
%        alldata(s).metaDPrime = [metaD.meta_da].';
%        
       mdl1 = fitglm(cj1,cor1,'Distribution','binomial','Link','logit');
       scores1 = mdl1.Fitted.Probability;
       [X1,Y1,T1,AUC1] = perfcurve(cor1,scores1,1);
       alldata(s).rocAUC1 = AUC1;
       
       mdl2 = fitglm(cj2,cor2,'Distribution','binomial','Link','logit');
       scores2 = mdl2.Fitted.Probability;
       [X2,Y2,T2,AUC2] = perfcurve(cor2,scores2,1);
       alldata(s).rocAUC2 = AUC2;
% 
       [Az,tp,fp,fc] = rocarea(cj1,cor1);
%        
%        [N,edges] = histcounts(cj1,5);
%        cjBins = N';
%       
%             figure(200)
%             % Plot the ROC Curves for the sample C.
%             % Code for ROC on both cj1 and cj2.
%             subplot(7,7,s)
%             plot(fp,tp); axis([0 1 0 1]); hold on
%             plot([0 1],[1 0],':'); hold off
%             xlabel('false positive rate') 
%             ylabel('true positive rate') 
%             title('ROC Curve'); axis([0 1 0 1]); 
%             text(0.4,0.2,sprintf('Az = %.2f',Az))
%             text(0.4,0.1,sprintf('fc = %.2f',fc))
%             hold on;
%             a = [0:0.1:1];
%             b = a;
%             plot(a,b);
%             txt2 = ['ROC p' num2str(s)];
%             title(txt2)
%             hold off;
% 
% %             figure(400)
% %             subplot(7,7,s);
% %             scatter(cj1(trialType=="choice"),whichAdvisor(trialType=="choice"));
% %             xlabel('Initial Confidence');
% %             ylabel('Advisor Choice');
% %             txt = ['p' num2str(s-80)];
% %             title(txt);
% 
%             figure(700)
%             subplot(7,7,s);
%             histfit(cj1);
%             xlim([0 50]);
%             txt = ['p' num2str(s-80) ': algor1Choice = ' num2str(alldata(s).algorChoice)];
%             title(txt);

   end
   
   
   subjectPath = '../../private/Subjects/';
   filename = paths{x};
   filename = split(filename, '/');
   filename = split(filename(3), '_');
   filename = strcat(subjectPath, filename(1), '_', filename(2), '_SUBJECT.csv');
    subject=readtable(char(filename));
    subject = table2struct(subject);
    alldata(s).gender = subject(1).gender;
    alldata(s).age = subject(1).age;
    alldata(s).deviceUse = subject(1).deviceUse;
   
end

survey=readtable('./1aSurveyResponsesRaw.csv');
alldata = struct2table(alldata);
alldata = join(alldata,survey);
alldata = table2struct(alldata);

for y = 1:s
    alldata(y).Q5 = 8 - alldata(y).Q5;
    alldata(y).Q6 = 8 - alldata(y).Q6;
    alldata(y).Q7 = 8 - alldata(y).Q7;
    alldata(y).Q8 = 8 - alldata(y).Q8;
    alldata(y).Q11 = 8 - alldata(y).Q11;
    
    alldata(y).abilityTot = alldata(y).Q1+alldata(y).Q2+alldata(y).Q3+alldata(y).Q4;
    alldata(y).integrityTot = alldata(y).Q5+alldata(y).Q6+alldata(y).Q7+alldata(y).Q8;
    alldata(y).understandingTot = alldata(y).Q9+alldata(y).Q10+alldata(y).Q11+alldata(y).Q12;
    
    alldata(y).abilityDiff = alldata(y).abilityTot-16;
    alldata(y).integrityDiff = alldata(y).integrityTot-16;
    alldata(y).understandingDiff = alldata(y).understandingTot-16;
    alldata(y).totalDiff = alldata(y).abilityDiff + alldata(y).integrityDiff + alldata(y).understandingDiff;
    
end

X = [[alldata.obsAdvAccDiffBlk4]'];

Y = [alldata.algorInfluence]';
mdl = fitlm(X,Y,'linear');


% [d,p,stats] = manova1([algor2TrustDiff algor2Influence swayDiff],algor2Choice,0.05);
% [d2,p2,stats2] = manova1([resolutionAll swayDiff deviceUseAll meanChoiceCj1 ctcTime],algor2Choice,0.05);

t = anova(mdl,'summary');
coefCI(mdl);
[p1,F1,d1] = coefTest(mdl);
[P1, DW] = dwtest(mdl);

mdlResiduals = mdl.Residuals.Raw;
h = archtest(mdlResiduals);
mdlFitted = mdl.Fitted;
rSq = mdl.Rsquared.Adjusted;

% figure(89)
% plot(mdl);
% figure(90)
% plotResiduals(mdl);
% figure(91)
% plotResiduals(mdl,'probability');
% figure(92)
% plotEffects(mdl);
% figure(93)
% plotDiagnostics(mdl);
% figure(94)
% plotDiagnostics(mdl,'cookd');
% figure(95)
% plotResiduals(mdl,'lagged');
% figure(96)
% plotDiagnostics(mdl,'contour');
% figure(97)
% plotResiduals(mdl,'fitted');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Figures

gender = [alldata.gender].';
ageAll = [alldata.age].';
algorChoice = [alldata.algorChoice].';
resolutionAll = [alldata.resolution].';
algorInfluence = [alldata.algorInfluence].';
% metaDPrime = [alldata.metaDPrime].';
cj1Acc = [alldata.cj1Acc].';
obsAdvAccDiffBlk4 = [alldata.obsAdvAccDiffBlk4].';
choiceAfterBlk4 = [alldata.choiceAfterBlk4].';
abilityDiff = [alldata.abilityDiff].';
integrityDiff = [alldata.integrityDiff].';
understandingDiff = [alldata.understandingDiff].';
totalDiff = [alldata.totalDiff].';

quantAdvisor1 = [alldata.quantAdvisor1].';
quantAdvisor2 = [alldata.quantAdvisor2].';
quantAdvisor3 = [alldata.quantAdvisor3].';
quantAdvisor4 = [alldata.quantAdvisor4].';

algorAgreeConfDiff = [alldata.algorAgreeConfDiff].';
algorDisagreeConfDiff = [alldata.algorDisagreeConfDiff].';
humanAgreeConfDiff = [alldata.humanAgreeConfDiff].';
humanDisagreeConfDiff = [alldata.humanDisagreeConfDiff].';
   
quantCorrect1 = [alldata.quantCorrect1].';
quantCorrect2 = [alldata.quantCorrect2].';
quantCorrect3 = [alldata.quantCorrect3].';
quantCorrect4 = [alldata.quantCorrect4].';

figure(1)
b = barwitherr([stdErr(algorChoice) stdErr(algorChoice)],[nanmean(algorChoice) nanmean(1-algorChoice)]);
title('Choice of Advisor');
set(gca,'xticklabel',{'Algorithm';'Human'})
ylabel('Proportion of Choice Trials')
set(b,'FaceColor',[0.8500, 0.3250, 0.0980])

figure(2)
b = barwitherr([stdErr(quantCorrect1) stdErr(quantCorrect2) stdErr(quantCorrect3) stdErr(quantCorrect4)], [nanmean([alldata.quantCorrect1]) nanmean([alldata.quantCorrect2]) nanmean([alldata.quantCorrect3]) nanmean([alldata.quantCorrect4])]);
title('Quartiles of Confidence Calibrated to Accuracy');
ylabel('Proportion of Correct Trials')

figure(3)
b = barwitherr([stdErr(quantAdvisor1) stdErr(quantAdvisor2) stdErr(quantAdvisor3) stdErr(quantAdvisor4)],[nanmean([alldata.quantAdvisor1]) nanmean([alldata.quantAdvisor2]) nanmean([alldata.quantAdvisor3]) nanmean([alldata.quantAdvisor4])]);
title('Quartiles of Confidence Against Advisor Choice');
ylabel('Proportion of Trials Where Algorithm is Chosen')

figure(4)
histogram(algorChoice,10)
title('Distribution of Preferences for the Computer Advisor')
xlabel('Proportion of Trials Where Algorithm is Chosen')
ylabel('Frequency')

figure(5)
scatter(algorInfluence, algorChoice,'red','+','LineWidth',1)
title('Relation between Choice and Relative Influence of Advisors')
xlabel('Influence of Algorithm Relative to Human')
ylabel('Choice of Algorithm')
lsline;

% figure(6)
% scatter(metaDPrime, cj1Acc,'blue','+','LineWidth',1)
% title('Metacognitive Senstivity against Accuracy')
% xlabel('Meta D Prime')
% ylabel('Accuracy')
% lsline;

figure(7)
scatter(obsAdvAccDiffBlk4, choiceAfterBlk4,'red','+','LineWidth',1)
title('Effect of Early Experience on Later Advisor Preference')
xlabel('Observed Difference in Advisor Accuracy on First Block')
ylabel('Choice of Algorithm After First Block')
lsline;

figure(8)
scatter(abilityDiff, algorInfluence,'blue','+','LineWidth',1)
title('Ability Against Influence')
xlabel('Ability Score')
ylabel('Relative Influence of Algorithm')
lsline;

figure(9)
scatter(understandingDiff, algorChoice,'blue','+','LineWidth',1)
title('Understanding Against Choice')
xlabel('Understanding Score')
ylabel('Choice of Algorithm')
lsline;

figure(10)
scatter(totalDiff, algorChoice,'blue','+','LineWidth',1)
title('Survey Total Against Choice')
xlabel('Survey Total')
ylabel('Choice of Algorithm')
lsline;

maleUnd = understandingDiff(gender=='m');
femaleUnd = understandingDiff(gender=='f');

figure(11)
b = barwitherr([stdErr(maleUnd) stdErr(femaleUnd)],[nanmean(maleUnd) nanmean(femaleUnd)]);
title('Understanding Across Gender');
set(gca,'xticklabel',{'Male';'Female'})
ylabel('Mean Subscore')
set(b,'FaceColor',[0.8500, 0.3250, 0.0980])