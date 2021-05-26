/**
 * Definitions for advisorChoice experiment.
 * Matt Jaquiery, Feb 2018 (Original)
 * Edited by Sriraj Aiyer, June 2019
 */
"use strict";

import {DoubleDotGrid, Trial, Governor, Advisor, Cue, Line, utils} from './exploringSocialMetacognition.js';
import {dotTask, advisorChoice} from "./analysis.js";
import debriefForm from "./debriefForm.js";
import "./forcedData.js";
import "./choiceData.js";
import "./blk4Data.js";
import "./whereDots.js";
import "./questions.js"

/**
 * Trial type identifiers
 * @type {{catch: number, force: number, choice: number}}
 */
const trialTypes = {
    catch: 0,   // no advisor
    force: 1,   // forced to use a specific advisor
    choice: 2,  // choice of advisors
    dual: 3,    // see advice from two advisors
    change: 4,   // see advice from a specific advisor with an option to see the other instead
    forceblk4: 5, // forced to use a specific advisor as part of a block of all forced trials

};

/**
 * Trial type names
 * @type {{}}
 */
const trialTypeNames = {
    [trialTypes.catch]: 'catch',
    [trialTypes.force]: 'force',
    [trialTypes.choice]: 'choice',
    [trialTypes.dual]: 'dual',
    [trialTypes.change]: 'change',
    [trialTypes.forceblk4]: 'forceblk4',

};

const permForced = utils.randPerm(120);
const permChoice = utils.randPerm(240);
const permBlk4 = utils.randPerm(60);
let points = 0;

/**
 * @classdesc A dot task governor controls the dot display experiment functionality
 * @class
 * @augments Governor
 */
class DotTask extends Governor {
    /**
     * @constructor
     *
     * @param {Object} [args={}] - properties to assign to the Governor
     * @param {Trial[]} [args.trials=[]] - trial list
     * @param {Object[]} [args.miscTrials] - miscellaneous trials (breaks, instructions, etc)
     * @param {int} [args.currentTrialIndex=0] - index of current trial in trial list
     * @param {string} [args.completionURL=''] - URL to which to refer participants for payment
     * @param {string} [args.experimentCode=''] - code identifying the experiment
     *
     * @param {int} [args.dotCount] - number of dots in a box
     * @param {int} [args.dotDifference] - half the difference between the dot counts in the two boxes; the difficulty
     * @param {int} [args.difficultyStep] - amount the difficulty increases/decreases after success/failure
     * @param {number} [args.minimumBlockScore] - lowest proportion of successful trials allowed on a block
     * @param {int} [args.blockCount] - number of blocks in the study
     * @param {Object|Object[]} [args.blockStructure] - the structure of each block, where each object is a series of [trialType: number of instances] mappings. Multiple objects represent different subblocks run consecutively.
     * @param {Object|Object[]} [args.practiceBlockStructure] - the structure of each practice block
     * @param {Object|Object[]} [args.blk4Structure] - structure for block 4 forced trials if indicated in index.html
     * @param {int} [args.preTrialInterval] - delay before each trial begins
     * @param {int} [args.preStimulusInterval] - fixation delay before the stimulus is displayed
     * @param {int} [args.stimulusDuration] - duration the dot stimulus is displayed
     * @param {int} [args.feedbackDuration] - duration of the feedback screen     *
     */
    constructor(args = {}) {
        super(args);
        this.dotCount = typeof args.dotCount === 'undefined'? null : args.dotCount;
        this.dotDifference = typeof args.dotDifference === 'undefined'? null : args.dotDifference;
        this.difficultyStep = typeof args.difficultyStep === 'undefined'? null : args.difficultyStep;
        this.minimumBlockScore = typeof args.minimumBlockScore === 'undefined'? null : args.minimumBlockScore;
        this.blockCount = typeof args.blockCount === 'undefined'? null : args.blockCount;
        this.blockStructure = typeof args.blockStructure === 'undefined'? [
            {
                0: 0
            }
        ] : utils.shuffle(args.blockStructure);
        this.practiceBlockStructure = typeof args.practiceBlockStructure === 'undefined'? {
            0: 0 
        } : args.practiceBlockStructure;
        this.blk4Structure = typeof args.blk4Structure === 'undefined' ? {
            0: 0
        } : args.blk4Structure;
        this.preTrialInterval = typeof args.preTrialInterval === 'undefined'? null : args.preTrialInterval;
        this.preStimulusInterval = typeof args.preStimulusInterval === 'undefined'? null : args.preStimulusInterval;
        this.stimulusDuration = typeof args.stimulusDuration === 'undefined'? null : args.stimulusDuration;
        this.feedbackDuration = typeof args.feedbackDuration === 'undefined'? null : args.feedbackDuration;
        this.numOfTrials = utils.sumList(this.blockStructure) + utils.sumList(this.practiceBlockStructure) + utils.sumList(this.blk4Structure);
        this.workingMemoryStack = [];
        this.maintainMemoryString = "";
        this.totalReward = 0;
        this.dotReward = 0;
        this.estimateReward = 0;
        this.estimateErr = [];
        this.meanEstimateErr = [];
        this.dotsComplete = 0;
        this.estimates = [];
        this.radio = [];
    }

    /**
     * Return a list of Trial objects.
     *
     * A large part of the work of defining the experiment takes place here, although the key properties are
     * actually defined in the Governor definition.
     *
     * The trials defined here are the master list used by the Governor to decide which stimuli to serve,
     * which advisor or choice to offer, etc. This list is **not necessarily the same as** the trial list
     * established at the beginning of the experiment and handed to jsPsych. It is therefore the responsibility
     * of the programmer to ensure that these lists are lawfully aligned such that the block structures, etc.
     * match.
     *
     * A possible alternative strategy - push new trials to the jsPsych timeline at
     * the end of each completed trial. Since we don't get nice progress bar this way we may as well use on-the-fly
     * timeline tweaking. This may just be more work to duplicate jsPsych's capabilities, though
     */
    getTrials() {
        let trials = [];
        let id = 0;
        let realId = 0;
        let blockCount = this.blockStructure.length * this.blockCount;
        let practiceBlockCount = this.practiceBlockStructure.length;
        let blk4Count = this.blk4Structure.length;
        this.totalBlocks = practiceBlockCount+blockCount+blk4Count;
        // Shuffle which side the correct answer appears on
        let whichSideDeck = utils.shuffleShoe([0, 1], utils.sumList(this.blockStructure));
        // Define trials
        for (let b=0; b<practiceBlockCount+blockCount+blk4Count; b++) {
            let blockIndex = b; // the block we're in
            if (b >= practiceBlockCount) {
                blockIndex = (b-practiceBlockCount)%this.blockStructure.length; // account for practice blocks
            }
            let blockLength = b<practiceBlockCount? utils.sumList(this.practiceBlockStructure[blockIndex]) :
                utils.sumList(this.blockStructure[blockIndex]);
            // Work out what type of trial to be
            let trialTypeDeck = [];
            if (blk4Count > 0 && blockIndex == practiceBlockCount)
            {
                let structure = this.blk4Structure[blockIndex];
            }
            else
            {
                let structure = b<practiceBlockCount?
                this.practiceBlockStructure[blockIndex] : this.blockStructure[blockIndex];
            }
            for (let tt=0; tt<Object.keys(trialTypes).length; tt++) {
                for (let i=0; i<structure[tt]; i++)
                    trialTypeDeck.push(tt);
            }
            trialTypeDeck = utils.shuffle(trialTypeDeck);
            for (let i=1; i<=blockLength; i++) {
                id++;
                let isPractice = b<practiceBlockCount;
                let trialType = trialTypeDeck.pop();
                let left;
                let right;
                let larger;
                let algAns;
                let algCor;
                let algCon;
                // Below we retrieve the dot configs, advice and confidences (if relevant) used for the trial
                // This is only for data recording purposes after the experiment.
                if (trialType == 1)
                {
                    left = (forcedWhereDots.dots[((this.forcedTrials[0])-1)+(120*(this.dotDifference-1))])[0];
                    right = (forcedWhereDots.dots[((this.forcedTrials[0])-1)+(120*(this.dotDifference-1))])[1];
                    if (utils.sumList(left,false,true) > utils.sumList(right,false,true))
                    {
                        larger = 0;
                    }
                    else
                    {
                        larger = 1;
                    }
                    algAns = ((forcedData.advisors[(this.dotDifference)-1].AlgorithmAnswer[this.forcedTrials[0]])-1);
                    if (algAns == larger)
                    {
                        algCor = 1;
                    }
                    else
                    {
                        algCor = 0;
                    }
                    algCon = ((forcedData.advisors[(this.dotDifference)-1].cj1[this.forcedTrials[0]])-1)
 
                }
                else if (trialType == 2)    
                {
                    left = (choiceWhereDots.dots[((this.choiceTrials[0])-1)+(120*(this.dotDifference-1))])[0];
                    right = (choiceWhereDots.dots[((this.choiceTrials[0])-1)+(120*(this.dotDifference-1))])[1];
                    if (utils.sumList(left,false,true) > utils.sumList(right,false,true))
                    {
                        larger = 0;
                    }
                    else
                    {
                        larger = 1;
                    }
                    algAns = ((choiceData.advisors[(this.dotDifference)-1].AlgorithmAnswer[this.choiceTrials[0]])-1);
                    if (algAns == larger)
                    {
                        algCor = 1;
                    }
                    else
                    {
                        algCor = 0;
                    }
                    algCon = ((choiceData.advisors[(this.dotDifference)-1].cj1[this.choiceTrials[0]])-1)
                }
                else if (trialType == 5)    
                {
                    left = (blk4WhereDots.dots[((this.blk4Trials[0])-1)+(120*(this.dotDifference-1))])[0];
                    right = (blk4WhereDots.dots[((this.blk4Trials[0])-1)+(120*(this.dotDifference-1))])[1];
                    if (utils.sumList(left,false,true) > utils.sumList(right,false,true))
                    {
                        larger = 0;
                    }
                    else
                    {
                        larger = 1;
                    }
                    algAns = ((blk4Data.advisors[(this.dotDifference)-1].AlgorithmAnswer[this.blk4Trials[0]])-1);
                    if (algAns == larger)
                    {
                        algCor = 1;
                    }
                    else
                    {
                        algCor = 0;
                    }
                    algCon = ((blk4Data.advisors[(this.dotDifference)-1].cj1[this.blk4Trials[0]])-1)
                }
                else
                {
                    let leftDots;
                    let rightDots;
                    larger = Math.random();
                    if (larger < 0.5)
                    {
                        larger = 0;
                        leftDots = this.dotCount + this.dotDifference;
                        rightDots = this.dotCount - this.dotDifference;
                    }
                    else
                    {
                        larger = 1;
                        leftDots = this.dotCount - this.dotDifference;
                        rightDots = this.dotCount + this.dotDifference;
                    }
                    let leftArray = [];
                    let rightArray = [];
                    for (let y = 0;y<400;y++)
                    {
                        if (y<leftDots)
                        {
                            leftArray.push(1);
                        }
                        else
                        {
                            leftArray.push(0);
                        }
                        if (y<rightDots)
                        {
                            rightArray.push(1);
                        }
                        else
                        {
                            rightArray.push(0);
                        }
                    }
                    // Randomly shuffle the array of 1s and 0s
                    utils.shuffle(leftArray);
                    utils.shuffle(rightArray);
                    left = leftArray;
                    right = rightArray;
                    algAns = null;
                    algCor = null;
                    algCon = null;
                }
                trials.push(new Trial(id, {
                    type: trialType,
                    typeName: 'dot',
                    block: b,
                    answer: [NaN, NaN],
                    confidence: [NaN, NaN],
                    getCorrect: function(finalAnswer = true) {
                        let answer = finalAnswer? this.answer[1] : this.answer[0];
                        return answer === this.whichSide;
                    },
                    whichSide: isPractice? Math.round(Math.random()) : whichSideDeck[realId],
                    practice: isPractice,
                    feedback: isPractice,
                    warnings: [],
                    stimulusDrawTime: [],
                    stimulusOffTime: [],
                    fixationDrawTime: [],
                    advisorAnswer: algAns,
                    advisorCorrect: algCor,
                    advisorConfidence: algCon,
                    leftGrid: left,
                    rightGrid: right,
                    whereLarger: larger
                }));
                if (!isPractice)
                    realId++;
            }
        }
        return trials;
    }

    /**
     * Do the actual drawing on the canvas.
     *
     * This function is called by the trial (supplied as stimulus). Query the Governor to get the details for
     * drawing.
     *
     * @param {string} canvasId - id of the canvas on which to draw the dots (supplied by the trial)
     * @param {boolean} recalculateGrid - whether to recalculate the stimulus grid even if it exists
     */
    drawDots(canvasId, recalculateGrid = false) {
        let slider = document.querySelector('#jspsych-canvas-sliders-response-slider0');
        let marker = document.createElement('div');
        marker.className = 'advisorChoice-marker advisorChoice-middleBar';
        slider.parentElement.appendChild(marker);
        let container = document.querySelector('#jspsych-sliders-response-table0');
        container.style = "visibility: hidden";
        let button = document.querySelector('#jspsych-canvas-sliders-response-next');
        button.style = "visibility: hidden";
        let xOffset = 0;
        let xDistance = 50;
        if (this.currentTrialIndex < this.numOfTrials)
        {
            let trialType = typeof this.currentTrial.type === 'undefined'? null : this.currentTrial.type;
            if(!(this.currentTrial.grid instanceof DoubleDotGrid) && !recalculateGrid) {
                this.currentTrial.dotDifference = this.dotDifference;
                let low = this.dotCount - this.dotDifference;
                let high = this.dotCount + this.dotDifference;
                let dots = this.currentTrial.whichSide === 0 ? [high, low] : [low, high];
                this.currentTrial.grid = new DoubleDotGrid(dots[0], dots[1], {
                    gridWidth: 20,
                    gridHeight: 20,
                    dotWidth: 3,
                    dotHeight: 3,
                    spacing: 100,
                    currentTrialType: trialType,
                    forcedTrials: permForced,
                    choiceTrials: permChoice,
                    blk4Trials: permBlk4
                });
            }

            let self = this;
            setTimeout(function () {
                self.currentTrial.fixationDrawTime.push(performance.now());
                DotTask.drawFixation(canvasId);
            }, this.preTrialInterval);
            setTimeout(function(){
                self.currentTrial.stimulusDrawTime.push(performance.now());
                self.currentTrial.grid.drawBoundingBoxes(canvasId);
                self.currentTrial.grid.draw(canvasId);
            }, this.preTrialInterval+this.preStimulusInterval);
        }

    }

    /**
     * Cover the dots in the boxes by redrawing the outer frame boxes
     * @param {HTMLElement} canvasContainer - div containing the canvas
     */
    maskDots(canvasContainer) {
            let canvas = canvasContainer.querySelector('canvas');
            this.currentTrial.grid.drawBoundingBoxes(canvas.id);
            let self = this;
            setTimeout(function () {
                let container = document.querySelector('#jspsych-sliders-response-table0');
                container.style = "";
            }, 100);
    }

    /**
     * Ensure the participant did the intro trial correctly
     * @param {Object} trial - jsPsych plugin response
     *
     * @return {Boolean|void} false if trial should be repeated or void if okay
     */
    checkIntroResponse(trial) {
        switch(this.currentTrialIndex) {
            case 0: // First practice - have to get it right (it's very easy)
                if(DotTask.getAnswerFromResponse(trial.response) !== this.currentTrial.whichSide) {
                    // redo the first trial
                    // returning false tells jsPsych to repeat the trial
                    return false;
                } else
                    return this.initialResponse(trial);
            default:
                return this.initialResponse(trial);
        }
    }

    /** Check the initial response to ensure that the participant hasn't selected neither answer.
     * @param {Object} trialresponse - potential response from the plugin
     * @return {boolean} true to allow the response through, false to prevent it
     */
    checkResponse(trialresponse) {
        let okay = trialresponse.response[0].answer!=="50";
        if(okay)
            return true;
        // Add a warning and reject response
        document.querySelector('#jspsych-canvas-sliders-response-warnings').innerHTML =
            "<span style='color: red'>Please choose one side or the other.</span>";
        return false;
    }

    /**
     * Process the judge's initial response
     * @param {Object} trial - jsPsych plugin response
     * @param {Object} [args={}] - assorted arguments to customize behaviour
     * @param {boolean} [args.advisorAlwaysCorrect - whether to override advisor's behaviour and force them to advice the correct response
     */
    initialResponse(trial, args={}) {
        this.storePluginData(trial);
        this.currentTrial.stimulusOffTime.push(trial.stimulusOffTime);
        // trial is the complete trial object with its trial.response object
        this.currentTrial.answer[0] = DotTask.getAnswerFromResponse(trial.response);
        this.currentTrial.confidence[0]  = DotTask.getConfidenceFromResponse(trial.response, this.currentTrial.answer[0]);
        this.getLastConfidenceCategory(); // set the confidence category
        this.closeTrial(trial);
    }

    /**
     * Draw a fixation cross on *canvasId*
     * @param {string} canvasId - id of the canvas on which to draw
     */
    static drawFixation(canvasId) {
        let ctx = document.querySelector('#'+canvasId).getContext('2d');
        let len = 5;
        ctx.strokeStyle = 'black';
        ctx.lineWidth = '2';
        ctx.beginPath();
        // horizontal
        ctx.moveTo((ctx.canvas.clientWidth/2)-len, ctx.canvas.clientHeight/2);
        ctx.lineTo((ctx.canvas.clientWidth/2)+len, ctx.canvas.clientHeight/2);
        ctx.stroke();
        // vertical
        ctx.strokeStyle = 'black';
        ctx.beginPath();
        ctx.moveTo(ctx.canvas.clientWidth/2, (ctx.canvas.clientHeight/2)-len);
        ctx.lineTo(ctx.canvas.clientWidth/2, (ctx.canvas.clientHeight/2)+len);
        ctx.stroke();
    }

    /**
     * Extract the answer from the plugin's response. The plugin provides a value from 1-100,
     * values 0-49 indicate 'left', 50-100 indicate 'right'.
     *
     * @param {Object} response - response field provided by the jspsych-canvas-sliders-response plugin
     *
     * @return {int} 1 for a rightward response, 0 for a leftward response
     */
    static getAnswerFromResponse(response) {
        let ans = parseInt(response[0].answer);
        if(ans === 50)
            return NaN;
        return ans > 50 ? 1 : 0;
    }

    /**
     * Return the confidence score associated with a given slider.
     *
     * @param {Object} response - response field provided by jspsych-canvas-sliders-response plugin
     * @param {int} side - which side the answer was on (1 = right, 0 = left)
     *
     * @return {int} Confidence of the response from 1-50
     */
    static getConfidenceFromResponse(response, side) {
        let ans = parseInt(response[0].answer);
        if (isNaN(ans))
            return NaN;
        switch(side) {
            case 0: // left response
                return 50 - ans;
            case 1: // right response
                return ans - 50;
            default:
                return NaN;
        }
    }

    /**
     * Sets a click function on the sliders which makes them behave as if they are moved to 50 when they are focussed.
     * This prevents users clicking on the slider, getting the visual feedback of the slider being activated and set,
     * and then being told they have not moved the slider.
     *
     * @param {boolean} [drawMiddleBar=true] - whether to draw the middle bar on the slider track
     * @param {boolean} [drawLabels=true] - whether to draw the labels on the slider track
     */
    setSliderClick(drawMiddleBar = true, drawLabels = true) {
        let sliders = document.querySelectorAll('.jspsych-sliders-response-slider');
        sliders.forEach(function (slider) {
            slider.addEventListener('click', function () {
                let button = document.querySelector('#jspsych-canvas-sliders-response-next');
                button.style = "";
                if (typeof this.clickFunctionRun !== 'undefined')
                    return;
                this.clickFunctionRun = true;
                if (this.value !== "50")
                    return;
                // send a change event
                let event = new Event('change');
                this.dispatchEvent(event);
            });
            let parent = slider.parentElement;
            if(drawMiddleBar) {
                // Add a visual indicator to the middle of the slider to show the excluded zone
                let marker = document.createElement('div');
                marker.className = 'advisorChoice-middleBar advisorChoice-marker';
                parent.appendChild(marker);
            }
            if(drawLabels) {
                let left = parent.appendChild(document.createElement('div'));
                let mid = parent.appendChild(document.createElement('div'));
                let right = parent.appendChild(document.createElement('div'));
                left.id = 'advisorChoice-slider-labels-left';
                mid.id = 'advisorChoice-slider-labels-mid';
                right.id = 'advisorChoice-slider-labels-right';
                left.className = "advisorChoice-slider-label advisorChoice-slider-label-left";
                mid.className = "advisorChoice-slider-label advisorChoice-slider-label-mid";
                right.className = "advisorChoice-slider-label advisorChoice-slider-label-right";
                left.innerHTML = '<span class="advisorChoice-slider-label-direction">LEFT <br/> 100% </span>';
                right.innerHTML = '<span class="advisorChoice-slider-label-direction">RIGHT <br/> 100%</span>';
                mid.innerHTML = '<br/> &nbsp; 50% &nbsp; &nbsp; &nbsp; 50%';
            }
        });
        this.setContentHeight();
        if (this.currentTrialIndex == 0)
        {
            this.drawProgressBar();
        }
    }

    /**
     * Apply a minimum height to the content so that things look consistent with teh advice and response sliders
     * @param {boolean} [unset=false] - whether to remove the class instead
     */
    setContentHeight(unset = false) {
        if(unset)
            document.querySelector('#jspsych-content').classList.remove('advisorChoice-minHeight');
        else
            document.querySelector('#jspsych-content').classList.add('advisorChoice-minHeight');
    }

    /**
     * Save plugin data and reset the content height fixing
     * @param {Object} plugin
     */
    storePluginData(plugin) {
        this.setContentHeight(false);
        super.storePluginData(plugin);
    }

    /**
     * inject the proportion correct into the block feedback
     */
    blockFeedback(rewardInput = -1){
        let block;
       // this.exportGovernor();
        if (this.currentTrial == null)
        {
            block = this.practiceBlockStructure.length-1;
        }
        else
        {
            block = this.currentTrial.block-1;
        }
        // if (typeof this.currentTrial !== 'undefined')
        // {
        //     block = (this.trials[this.trials.length-1]).block-1;
        // }
        let trialList = utils.getMatches(this.trials, (trial)=>{
            return trial.block === block;
        });
        let hitList = utils.getMatches(trialList, (trial)=>{
            let answer = trial.answer[1];
            if (answer === null || isNaN(answer))
                answer = trial.answer[0];
            return answer === trial.whichSide;
        });


        let score = hitList.length / trialList.length * 100;
        let trial = this.trials[this.currentTrialIndex-1];
        // if (score < this. BlockScore) {
        //     this.terminateExperiment(score);
        //     return;
        // }
        let div = document.querySelector('#jspsych-content');
        let p = div.insertBefore(document.createElement('p'), div.querySelector('p'));
        let blockScore = (Math.round(score*100)/100).toString();
        if (rewardInput < 0)
        {
            p.innerHTML = "Your score on the last block was " + Math.round(blockScore) + "%.";
            let practiceBlockCount = this.practiceBlockStructure.length;
            //console.log(block);
            //console.log(practiceBlockCount);
            if (block > practiceBlockCount)
            {
                let threshTrialList = utils.getMatches(this.trials, (trial)=>{
                    return trial.block === practiceBlockCount;
                });
                let threshHitList = utils.getMatches(threshTrialList, (trial)=>{
                    let answer = trial.answer[1];
                    if (answer === null || isNaN(answer))
                        answer = trial.answer[0];
                    return answer === trial.whichSide;
                });
                let threshold = threshHitList.length / threshTrialList.length * 100;
                threshold = (Math.round(threshold*100)/100);
                let scoreVal = (Math.round(score*100)/100);
                let rewardAmount = (Math.round(scoreVal - threshold))*0.5;
                if (rewardAmount < 0)
                {
                    rewardAmount = 0;
                }
                if (rewardAmount > 2.5)
                {
                    rewardAmount = 2.5;
                }
                //p.innerHTML = p.innerHTML + "<br /><p>" + "Based on your performance in the last block, you are awarded £" + rewardAmount + "</p>";;
                //this.totalReward = this.totalReward + rewardAmount;
                //this.dotReward = this.dotReward + rewardAmount;
            }
        }
        else
        {
            p.innerHTML = "Based on your performance so far in this task, you are awarded £" +  this.estimateReward;
        }
        if (block == this.totalBlocks-1)
        {
            this.dotsComplete = 1;
            if (this.estimates !== 'undefined')
            {
                this.showTotalReward();
            }
        }
    }

    /**
     * Take the stimuli from the cleanest previous trials and use them for the forthcoming trials
     * @param {boolean} [includePractice=false] whether to allow practice trial stimuli to be repeated
     */
    createRepetitionList(includePractice = false) {

        // Work out how many trials will be required
        let index = this.currentTrialIndex + 1;
        let trialsRemaining = this.trials.length - (index);
        let trialsAvailable = index;
        if(!includePractice) trialsAvailable -= utils.sumList(this.practiceBlockStructure);

        if(trialsAvailable < trialsRemaining)
            this.currentTrial.warnings.push("Repetition list has too few trials for stimuli repetition.");

        // Take the best trials from those available
        let allowableExclusions = trialsAvailable - trialsRemaining;

        let self = this;
        let trialPool = utils.getMatches(this.trials, function(trial) {
            if(!includePractice && trial.practice)
                return false;
            return trial.id < index;
        });

        let reactionTime = false;
        let difficulty = false;
        // remove the worst trial until we're at the limit
        let reps = 0;
        while(allowableExclusions > 0) {
            let tempPool = [];
            if(!reactionTime) {
                // Worst trial is defined as that with longest RT
                let RTs = [];
                trialPool.forEach((trial)=>RTs.push(parseInt(trial.pluginResponse[0].rt)));

                let mean = utils.mean(RTs);
                let sd = utils.stDev(RTs);

                let Zs = [];
                RTs.forEach((rt)=>Zs.push((rt - mean) / sd));

                let max = utils.max(Zs, true);
                if(max < 3) // Less than 3SD is fine, all remaining trials can be included
                    reactionTime = true;
                else {
                    let rt = max > 0? utils.max(RTs) : utils.min(RTs);
                    tempPool = utils.getMatches(trialPool, function(trial) {
                        if(parseInt(trial.pluginResponse[0].rt) === rt) {
                            trial.repeatRejection = "reaction time";
                            return false;
                        } else {
                            trial.repeatRejection = false;
                            return true;
                        }
                    });
                }
            } else if(!difficulty) {
                // Worst trials are those with difficulty furthest from the median difficulty
                let dotDiffs = [];
                trialPool.forEach((trial)=>dotDiffs.push(trial.dotDifference));
                let median = dotDiffs.sort((a, b) => a - b); // sort numerically rather than alphabetically
                if(median.length % 2 === 1)
                    median = (median[Math.floor(median.length / 2)] + median[Math.ceil(median.length / 2)]) / 2;
                else
                    median = median[median.length / 2];
                let err = [];
                trialPool.forEach((trial)=>err.push(Math.abs(trial.dotDifference - median)));

                if(utils.max(err) <= gov.difficultyStep.end)
                    difficulty = true;
                else {
                    let worst = dotDiffs[err.indexOf(utils.max(err))];
                    tempPool = utils.getMatches(trialPool, function(trial) {
                        if(trial.dotDifference === worst) {
                            trial.repeatRejection = "difficulty";
                            return false;
                        }
                        return true;
                    });
                }
            }

            if(tempPool.length < trialsRemaining)
                break; // don't over-prune

            trialPool = tempPool;
            allowableExclusions = trialPool.length - trialsRemaining;

            if(++reps > 1000)
                return false;
        }

        // Shuffle stimuli from selected trials and assign them to forthcoming trials
        trialPool = utils.shuffle(trialPool);
        for(let i = index; i < this.trials.length; i++) {
            if(i - index >= trialPool.length)
                break; // ran out of trials in the pool
            let trial = trialPool[i - index];
            this.trials[i].grid = new DoubleDotGrid(trial.grid);
            this.trials[i].dotDifference = trial.dotDifference;
            // swap the boxes around if necessary
            if(this.trials[i].whichSide !== trial.whichSide)
                this.trials[i].grid.swapSides();
            this.trials[i].stimulusParent = trial.id;
        }

        return true;
    }

    /**
     * Stop the experiment prematurely
     *
     * @param {number} score - score obtained on previous block
     */
    // terminateExperiment(score = 0.0) {
    //     let div = document.querySelector('#jspsych-content');
    //     div.innerHTML = "<p>Your score on the last block was " + (Math.round(score*100)/100).toString() + "%.</p>" +
    //     "<p>This is below the score required to continue with the study, so your participation has been ended prematurely.</p>";
    //     div.classList.add('terminated');
    //     this.drawProgressBar();
    // }

        /**
     * Draw the form which asks participants for age and gender
     */
    drawDemographicForm() {
        let owner = this;
        // Create form
        let div = document.querySelector('.jspsych-content').appendChild(document.createElement('div'));
        div.id = 'demoContainer';
        div.className = 'demo';
        let form = div.appendChild(document.createElement('form'));
        form.id = 'demoForm';
        form.className = 'demo';
        let questions = [];
        questions.push({
            prompt: 'Please provide gender as either "m" or "f"',
            mandatory: true,
            type: 'text'
        });
        questions.push({
            prompt: 'How many hours on any given day would you say you spend on an electronic device such as a smartphone or computer? (enter a single digit, such as: 1)',
            mandatory: true,
            type: 'text'
        });
        questions.push({
            prompt: 'Please provide your age',
            mandatory: true,
            type: 'text'
        });

        // Define error messages for demo form
        let errorStyle = "style='color: red;position: absolute;left: 50%;transform: translate(-50%, -50%);top: 60%;'";
        let genError = document.querySelector('div.jspsych-content-wrapper').appendChild(document.createElement('div'));
        genError.innerHTML = "<div " + errorStyle + ">Please only provide a single letter: 'm' or 'f'</div>"
        genError.classList.add('hidden');
        let devError = document.querySelector('div.jspsych-content-wrapper').appendChild(document.createElement('div'));
        devError.innerHTML = "<div " + errorStyle + ">Please provide a single digit as an answer</div>"
        devError.classList.add('hidden');
        let ageError = document.querySelector('div.jspsych-content-wrapper').appendChild(document.createElement('div'));
        ageError.innerHTML = "<div " + errorStyle + ">Please provide an age from 18 to 100</div>"
        ageError.classList.add('hidden');

        for(let i = 0; i < questions.length; i++) {
            let comment = form.appendChild(document.createElement('div'));
            comment.id = 'demoCommentContainer'+i;
            comment.className = 'demo demo-container';
            if(i > 0)
            {  
                comment.classList.add('hidden');
            }
            let commentQ = comment.appendChild(document.createElement('div'));
            commentQ.id = 'demoCommentQuestion'+i;
            commentQ.className = 'demo question';
            commentQ.innerHTML = "<strong>Q"+(i+1)+"/"+(questions.length)+":</strong> " + questions[i].prompt;
            let commentA = comment.appendChild(document.createElement('textarea'));

            commentA.id = 'demoCommentAnswer'+i;
            commentA.className = 'demo answer';

            if (i ==0)
            {
                commentA.placeholder = 'Enter your gender here'
            }

            else if (i == 1)
            {
                commentA.placeholder = 'Enter the number of hours here'
            }

            else
            {
                commentA.placeholder = 'Enter your age here'
            }

            let ok = comment.appendChild(document.createElement('button'));
            ok.innerText = i === questions.length - 1? 'submit' : 'next';
            ok.className = 'demo jspsych-btn';

            let checkResponse;
            let saveResponse;
            checkResponse = function(form) {
                let div = form.querySelector('.demo-container:not(.hidden)');
                let ok = div.querySelector('textarea').value !== "";
                if(!ok)
                    div.classList.add('bad');
                else
                    div.classList.remove('bad');
                return ok;
            };
            saveResponse = function(form) {
                let q = questions[i];
                q.answer = form.querySelector('.demo-container:not(.hidden) textarea').value;
                gov.demo.push(q);
            };
            if(!questions[i].mandatory)
                checkResponse = ()=>true;

            if(i === questions.length - 1)
                ok.onclick = function (e) {
                    e.preventDefault();
                    if(!checkResponse(this.form))
                        return false;
                    if (isNaN(parseInt(form.querySelector('#demoCommentAnswer2').value)))
                    {
                        ageError.classList.remove('hidden');
                        return false;
                    }
                    else
                    {
                        if (parseInt(form.querySelector('#demoCommentAnswer2').value) < 18 || parseInt(form.querySelector('#demoCommentAnswer1').value) > 100)
                        {
                            ageError.classList.remove('hidden');
                            return false;
                        }
                    }
                    ageError.classList.add('hidden');
                    saveResponse(this.form);
                    owner.demoFormSubmit(form);
                };
            else
                ok.onclick = function(e) {
                    e.preventDefault();
                    if(!checkResponse(this.form))
                        return false;
                    if (form.querySelector('#demoCommentAnswer0').value != 'm' && form.querySelector('#demoCommentAnswer0').value != 'f')
                    {
                        genError.classList.remove('hidden');
                        return false;
                    }
                    if (i > 0)
                    {
                        if (isNaN(parseInt(form.querySelector('#demoCommentAnswer1').value)))
                        {
                            devError.classList.remove('hidden');
                            return false;
                        }
                        if (form.querySelector('#demoCommentAnswer1').value.length > 1)
                        {
                            devError.classList.remove('hidden');
                            return false;
                        }
                    }
                    saveResponse(this.form);
                    devError.classList.add('hidden');
                    genError.classList.add('hidden');
                    let div = this.form.querySelector('.demo-container:not(.hidden)');
                    div.classList.add('hidden');
                    div.nextSibling.classList.remove('hidden');
                }
        }

        gov.demo = [];
    }    

    demoFormSubmit(form) {
        this.demo = [
            {
                question: 'gender',
                answer: form.querySelector('#demoCommentAnswer0').value
            },
            {
                question: 'deviceUse',
                answer: form.querySelector('#demoCommentAnswer1').value
            },
            {
                question: 'age',
                answer: form.querySelector('#demoCommentAnswer2').value
            }
        ];
        jsPsych.finishTrial(this.demo);
    }

    /**
     * Draw the form which asks participants for final comments
     */
    drawDebriefForm() {
        let owner = this;
	this.exportGovernor();
        // Create form
        let div = document.querySelector('.jspsych-content').appendChild(document.createElement('div'));
        div.id = 'debriefContainer';
        div.className = 'debrief';
        let header = div.appendChild(document.createElement('h1'));
        header.id = 'debriefTitle';
        div.className = 'debrief';
        header.innerText = 'finally...';
        let form = div.appendChild(document.createElement('form'));
        form.id = 'debriefForm';
        form.className = 'debrief';
        let questions = [];
        if(gov.repeatTrials || gov.showRepeatDebrief) {
            questions.push({
                prompt: 'Did you notice anything about the dots?',
                mandatory: true,
                type: 'text'
            });
            questions.push({
                prompt: 'Some of the dot grids were repeated, did you recognise any from previous rounds?',
                mandatory: true,
                type: 'scale',
                labels: [
                    'No, none',
                    'Yes, lots'
                ],
                nOptions: 7
            });
            questions.push({
                prompt: 'Please select any grids you recognise from the experiment.',
                mandatory: true,
                type: 'grids',
                nOptions: 5,
                nRepeats: 1,
                nSwapped: 1
            });
        }
        questions.push({
            prompt: 'Finally, do you have any comments or concerns about the experiment? <em>(optional)</em>',
            mandatory: false,
            type: 'text'
        });
        for(let i = 0; i < questions.length; i++) {
            let comment = form.appendChild(document.createElement('div'));
            comment.id = 'debriefCommentContainer'+i;
            comment.className = 'debrief debrief-container';
            if(i > 0)
                comment.classList.add('hidden');
            let commentQ = comment.appendChild(document.createElement('div'));
            commentQ.id = 'debriefCommentQuestion'+i;
            commentQ.className = 'debrief question';
            commentQ.innerHTML = "<strong>Q"+(i+1)+"/"+(questions.length)+":</strong> " + questions[i].prompt;
            let commentA = null;
            switch(questions[i].type) {
                case 'text':
                    commentA = comment.appendChild(document.createElement('textarea'));
                    break;
                case 'scale':
                    commentA = comment.appendChild(document.createElement('div'));
                    let labels = commentA.appendChild(document.createElement('div'));
                    labels.className = "debrief labels";
                    for(let L = 0; L < questions[i].labels.length; L++) {
                        let label = labels.appendChild(document.createElement('div'));
                        label.innerHTML = questions[i].labels[L];
                    }

                    let radios = commentA.appendChild(document.createElement('div'));
                    radios.className = 'radios';
                    for(let o = 0; o < questions[i].nOptions; o++) {
                        let radio = radios.appendChild(document.createElement('input'));
                        radio.type = 'radio';
                        radio.value = (o + 1).toString();
                        radio.name = commentQ.id;
                    }
                    break;
                case 'grids':
                    commentA = comment.appendChild(document.createElement('div'));
                    let x = this.blockStructure.length;
                    let targets = utils.shuffle(utils.getMatches(this.trials, (t)=>t.block === x));
                    let grids = [];
                    let meta = []; // stores meta properties for the grid. Don't write to grid or it screws up hashing
                    for(let g = 0; g < questions[i].nOptions; g++) {
                        let target = targets.pop();
                        let grid = new DoubleDotGrid(target.grid);
                        let t = null;
                        if(g >= questions[i].nRepeats && g < questions[i].nRepeats + questions[i].nSwapped) {
                            // swap grid
                            grid.swapSides();
                            t = "swapped";
                        } else if(g >= questions[i].nRepeats + questions[i].nSwapped) {
                            grid.gridL = grid.renewGrid(grid.dotCountL);
                            grid.gridR = grid.renewGrid(grid.dotCountR);
                            t = "new";
                        } else
                            t = "repeat";
                        grids.push(grid);
                        meta.push({id: g, type: t, parentTrialId: target.id});
                    }
                    let order = utils.shuffle(utils.getSequence(0, grids.length -1));
                    grids = utils.orderArray(grids, order);
                    meta = utils.orderArray(meta, order);
                    let div = commentA.appendChild(document.createElement('div'));
                    div.classList.add('grid-MCQ');
                    grids.forEach((g)=>{
                        let canvas = div.appendChild(document.createElement('canvas'));
                        canvas.id = 'Grid' + meta[grids.indexOf(g)].id;
                        canvas.width = g.displayWidth * 2 + g.spacing;
                        canvas.height = g.displayHeight;
                        g.draw(canvas.id);
                        canvas.addEventListener('click', (e)=>{
                            e.target.classList.toggle('selected');
                            document.querySelector('#GridMCQNone').checked = false;
                        });
                        canvas.grid = g;
                        canvas.meta = meta[grids.indexOf(g)];
                    });
                    let zeroDiv = div.appendChild(document.createElement('div'));
                    let zero = zeroDiv.appendChild(document.createElement('input'));
                    zero.type = 'checkbox';
                    zero.id = 'GridMCQNone';
                    zero.addEventListener('click', (e)=>{
                       document.querySelectorAll('.grid-MCQ canvas').forEach((c)=>c.classList.remove('selected'));
                    });
                    zeroDiv.appendChild(document.createElement('p')).innerText = "No grids are repeated";

            }
            commentA.id = 'debriefCommentAnswer'+i;
            commentA.className = 'debrief answer';

            let ok = comment.appendChild(document.createElement('button'));
            ok.innerText = i === questions.length - 1? 'submit' : 'next';
            ok.className = 'debrief jspsych-btn';

            let checkResponse;
            let saveResponse;
            switch(questions[i].type) {
                case 'text':
                    checkResponse = function(form) {
                        let div = form.querySelector('.debrief-container:not(.hidden)');
                        let ok = div.querySelector('textarea').value !== "";
                        if(!ok)
                            div.classList.add('bad');
                        else
                            div.classList.remove('bad');
                        return ok;
                    };
                    saveResponse = function(form) {
                        let q = questions[i];
                        q.answer = form.querySelector('.debrief-container:not(.hidden) textarea').value;
                        gov.debrief.push(q);
                    };
                    break;
                case 'scale':
                    checkResponse = function(form) {
                        let div = form.querySelector('.debrief-container:not(.hidden) .radios');
                        let ok = false;
                        let radios = div.querySelectorAll('input[type="radio"]');
                        radios.forEach((r)=>{if(r.checked) ok = true});
                        if(!ok)
                            form.querySelector('.debrief-container:not(.hidden)').classList.add('bad');
                        else
                            form.querySelector('.debrief-container:not(.hidden)').classList.remove('bad');
                        return ok;
                    };
                    saveResponse = function(form) {
                        let q = questions[i];
                        form.querySelectorAll('.debrief-container:not(.hidden) input[type="radio"]').forEach(
                            (r)=>{ if(r.checked) q.answer = r.value}
                        );
                        gov.debrief.push(q);
                    };
                    break;
                case 'grids':
                    checkResponse = function(form) {
                        let div = form.querySelector('.debrief-container:not(.hidden)');
                        let ok = false;
                        if(document.querySelector('#GridMCQNone').checked)
                            ok = true;
                        document.querySelectorAll('.grid-MCQ canvas').forEach((c)=>{
                            if(c.classList.contains('selected'))
                                ok = true;
                        });
                        if(ok)
                            form.querySelector('.debrief-container:not(.hidden)').classList.remove('bad');
                        else
                            form.querySelector('.debrief-container:not(.hidden)').classList.add('bad');
                        return ok;
                    };
                    saveResponse = function(form) {
                        let quiz = [];
                        form.querySelectorAll('.grid-MCQ canvas').forEach((c)=>{
                            let q = {};
                            q.grid = c.grid; // write grid
                            Object.keys(c.meta).forEach((k) => q[k] = c.meta[k]); // write metadata
                            q.selected = c.classList.contains('selected');
                            quiz.push(q);
                        });
                        gov.dotRepQuiz = quiz;
                    };
                    break;
            }
            if(!questions[i].mandatory)
                checkResponse = ()=>true;

            if(i === questions.length - 1)
                ok.onclick = function (e) {
                    e.preventDefault();
                    if(!checkResponse(this.form))
                        return false;
                    saveResponse(this.form);
                    owner.debriefFormSubmit(form);
                };
            else
                ok.onclick = function(e) {
                    e.preventDefault();
                    if(!checkResponse(this.form))
                        return false;
                    saveResponse(this.form);
                    let div = this.form.querySelector('.debrief-container:not(.hidden)');
                    div.classList.add('hidden');
                    div.nextSibling.classList.remove('hidden');
                }
        }

        gov.debrief = [];
    }

    /**
     * submit the debrief form and finish the experiment
     *
     */
    debriefFormSubmit(form) {
        document.querySelector('body').innerHTML = "";
        this.endExperiment();
    }

    /**
    * Create an estimation task with help from an advisor.
    * @param {object} trials - trial data for running the advisor estimation task.
    * trials should be of the format as follows:
    * practice flag, array of estimation criteria, advisor estimate}
    */
    drawAdvisorEstimateTask(qs,type)
    {
        let estimateNumber = 1;
        let rewardOutput = 0;
        let owner = this;
        // Create form
        let div = document.querySelector('.jspsych-content').appendChild(document.createElement('div'));
        div.id = 'estimateContainer';
        div.className = name;
        let header = div.appendChild(document.createElement('h1'));
        header.id = name + 'Title';
        div.className = name;
        let form = div.appendChild(document.createElement('form'));
        form.id = name + 'Form';
        form.className = 'name';
        let comment;

        // Show factors, allow estimate
        // Show advice, allow for resestimate
        // Show true answer
        // Block feedback?

        // Can be mixture of feedback trials and incentivised predictions
        // after training with no feedback

        // Original paper: 15 unincentivised, 10 incentivised

        let questions = [];
        let numOfQuestions = qs.length;

        for (let i = 0;i<numOfQuestions;i++)
        {

            questions.push({
                prompt: "Provide an estimate of this student's percentile.",
                mandatory: true,
                type: 'text'
            });
        }

        let errorStyle = "style='color: red;position: absolute;left: 50%;transform: translate(-50%, -50%);top: 65%;'";
        let advError = document.querySelector('div.jspsych-content-wrapper').appendChild(document.createElement('div'));
        advError.innerHTML = "<div " + errorStyle + ">Please only provide values from 1 to 100</div>";
        advError.classList.add('hidden');


        for(let i = 0; i < questions.length; i++) 
        {

            comment = form.appendChild(document.createElement('div'));
            comment.id = 'estimateCommentContainer'+i;
            comment.className = 'estimateContainer';
            if(i > 0)
            {
                comment.classList.add('hidden');
            }
            let commentQ = comment.appendChild(document.createElement('div'));
            commentQ.id = 'estimateCommentQuestion'+i;
            commentQ.className = 'estimate question';
            let innerhtml = '<table style="position: absolute; left: 50%; transform: translate(-50%, -110%);" >';

            for (let r = 0;r<qs[i].factorNames.length;r++)
            {
                innerhtml = innerhtml + '<tr class="estimate"><th class="estimate">' + qs[i].factorNames[r] + '</th><th class="estimate">' + qs[i].factorValues[r] + '</th></tr>';
            }


            innerhtml = innerhtml + '</table>';

            commentQ.innerHTML = innerhtml;
            let origHtml = commentQ.innerHTML;
            let commentA = comment.appendChild(document.createElement('textarea'));

            commentA.id = 'estimateCommentAnswer'+estimateNumber+i;
            commentA.className = 'estimate answer';

            commentA.placeholder = 'Enter your estimate here (1-100)';

            let ok = comment.appendChild(document.createElement('button'));
            ok.innerText = i === questions.length - 1? 'submit' : 'next';
            ok.className = 'question jspsych-btn';

            let checkResponse;
            let saveResponse;

            checkResponse = function(form) {

            let div = form.querySelector('.estimateContainer:not(.hidden)');
            let ok = div.querySelector('textarea').value !== "";

            if(!ok)
                form.querySelector('.estimateContainer:not(.hidden)').classList.add('bad');
            else
                form.querySelector('.estimateContainer:not(.hidden)').classList.remove('bad');
            return ok;
            };

            saveResponse = function(form) {
                let q = questions[i];
                form.querySelectorAll('.estimateContainer:not(.hidden)').forEach(
                    (r)=>{ if(r.checked) q.answer = r.value}
                );
                gov.estimateResponses.push(q);
            };       

           if(!questions[i].mandatory)
           {
                checkResponse = ()=>true;
           }

            if (type == "reward")
            {
                if(i === questions.length - 1)
                ok.onclick = function (e) {
                    if (estimateNumber == 4)
                    {
                        e.preventDefault();
                        if(!checkResponse(this.form))
                            return false;
                        let check = '#estimateCommentOwnGuess'
                        if (isNaN(parseInt(form.querySelector(check).value)))
                        {
                            advError.classList.remove('hidden');
                            return false;
                        }
                        if (parseInt(form.querySelector(check).value) > 100 || parseInt(form.querySelector(check).value) < 1)
                        {
                            advError.classList.remove('hidden');
                            return false;
                        }
                        else
                        {
                            advError.classList.add('hidden');
                            saveResponse(this.form);
                            commentQ.classList.remove('hidden');
                            owner.estimateFormSubmit(form,qs,numOfQuestions,type,comment);
                        }  
                    }
                    else if (estimateNumber == 3)
                    {
                        e.preventDefault();
                        if(!checkResponse(this.form))
                            return false;
                        let check = '#estimateCommentModelGuess'
                        if (isNaN(parseInt(form.querySelector(check).value)))
                        {
                            advError.classList.remove('hidden');
                            return false;
                        }
                        if (parseInt(form.querySelector(check).value) > 100 || parseInt(form.querySelector(check).value) < 1)
                        {
                            advError.classList.remove('hidden');
                            return false;
                        }                   
                        advError.classList.add('hidden');
                        commentQ.innerHTML = origHtml + "<h2>On average, how many percentiles do you think your estimates are from the students' actual percentiles?</h2>"
                        commentQ.innerHTML = commentQ.innerHTML + "<h2>An answer of 0 would mean you think you perfectly estimate all students' percentiles. An answer of 1 would mean that you think your estimates are off by 1 percentile, on average.</h2>";
                        commentQ.innerHTML = commentQ.innerHTML + "<h3>Your answer can range from 0-100</h3>"; 
                        let tab = document.getElementsByTagName("table")[numOfQuestions-1];
                        tab.classList.add('hidden');
                        let box = form.querySelector('#estimateCommentModelGuess');
                        box.classList.add('hidden');
                        let commentAv1 = comment.appendChild(document.createElement('textarea'));
                        commentAv1.id = 'estimateCommentOwnGuess';
                        commentAv1.className = 'estimate answer';
                        estimateNumber = 4;
                    }
                    if (estimateNumber == 2)
                    {
                        e.preventDefault();
                        if(!checkResponse(this.form))
                            return false;
                        let check = '#estimateCommentAnswerReward'
                        if (isNaN(parseInt(form.querySelector(check).value)))
                        {
                            return false;
                        }
                        if (parseInt(form.querySelector(check).value) > 10 || parseInt(form.querySelector(check).value) < 0)
                        {
                            return false;
                        }                   
                        commentQ.innerHTML = origHtml + "<h2>Based on your observations throughout the task, on average, how many percentiles do you think the model's estimates are from the students' actual percentiles?</h2>"
                        commentQ.innerHTML = commentQ.innerHTML + "<h2>An answer of 0 would mean the model perfectly estimates all students. An answer of 1 would mean that you think the model's estimates are off by 1 percentile, on average.</h2>";
                        commentQ.innerHTML = commentQ.innerHTML + "<h3>Your answer can range from 0-100</h3>"; 
                        let tab = document.getElementsByTagName("table")[numOfQuestions-1];
                        tab.classList.add('hidden');
                        let box = form.querySelector('#estimateCommentAnswerReward');
                        box.classList.add('hidden');
                        let commentAv1 = comment.appendChild(document.createElement('textarea'));
                        commentAv1.id = 'estimateCommentModelGuess';
                        commentAv1.className = 'estimate answer';
                        estimateNumber = 3;
                    }
                    else if (estimateNumber == 1)
                    {
                        e.preventDefault();
                        if(!checkResponse(this.form))
                            return false;
                        let check = '#estimateCommentAnswer'+estimateNumber+i;
                        if (isNaN(parseInt(form.querySelector(check).value)))
                        {
                            advError.classList.remove('hidden');
                            return false;
                        }
                        if (parseInt(form.querySelector(check).value) > 100 || parseInt(form.querySelector(check).value) < 1)
                        {
                            advError.classList.remove('hidden');
                            return false;
                        }
                        advError.classList.add('hidden');
                        commentQ.innerHTML = origHtml + "<h2>You may now choose to randomly replace some of your estimates with the model's estimates before calculating your bonus for this section."
                        commentQ.innerHTML = commentQ.innerHTML + "<h2>How many of the 10 estimates do you want the advisor to make for you when deciding your bonus?</h2>";
                        commentQ.innerHTML = commentQ.innerHTML + "<h3> Type a value from 0 to 10 </h3>"; 
                        let tab = document.getElementsByTagName("table")[numOfQuestions-1];
                        tab.classList.add('hidden');
                        let box = form.querySelector('#estimateCommentAnswer'+estimateNumber+i);
                        box.classList.add('hidden');
                        let commentAF = comment.appendChild(document.createElement('textarea'));
                        commentAF.id = 'estimateCommentAnswerReward';
                        commentAF.className = 'estimate answer';
                        estimateNumber = 2;
                    }
                };
                else
                {
                    ok.onclick = function(e) {
                        e.preventDefault();
                        if(!checkResponse(this.form))
                        {
                            return false;
                        } 
                        let check = '#estimateCommentAnswer'+estimateNumber+i;
                        if (isNaN(parseInt(form.querySelector(check).value)))
                        {
                            advError.classList.remove('hidden');
                            return false;
                        }
                        if (parseInt(form.querySelector(check).value) > 100 || parseInt(form.querySelector(check).value) < 1)
                        {
                            advError.classList.remove('hidden');
                            return false;
                        }
                        advError.classList.add('hidden');
                        saveResponse(this.form);
                        let div = this.form.querySelector('.estimateContainer:not(.hidden)');
                        div.classList.add('hidden');
                        div.nextSibling.classList.remove('hidden');
                        estimateNumber = 1;
                        return true;
                    }
                }

            }
            else if (type == "influence")
            {
                ok.onclick = function(e) {
                    e.preventDefault();
                    if (estimateNumber == 3)
                    {
                        if(i === questions.length - 1)
                        {
                            owner.estimateFormSubmit(form,qs,numOfQuestions,type,comment);
                        }
                        else
                        {
                            let div = this.form.querySelector('.estimateContainer:not(.hidden)');
                            div.classList.add('hidden');
                            div.nextSibling.classList.remove('hidden');
                            estimateNumber = 1;
                            return true;
                        }
                    }
                    if(!checkResponse(this.form))
                    {
                        return false;
                    } 
                    let check = '#estimateCommentAnswer'+estimateNumber+i;
                    if (isNaN(parseInt(form.querySelector(check).value)))
                    {
                        advError.classList.remove('hidden');
                        return false;
                    }
                    if (parseInt(form.querySelector(check).value) > 100 || parseInt(form.querySelector(check).value) < 1)
                    {
                        advError.classList.remove('hidden');
                        return false;
                    }
                    advError.classList.add('hidden');
                    saveResponse(this.form);
                    if (estimateNumber == 1)
                    {
                        innerhtml = innerhtml + "<h2>Advisor Estimate: " + qs[i].advEstimate + "</h2>";
                        innerhtml = innerhtml + "<h2 style = 'padding-bottom: 1em;'>Your Previous Estimate: " + form.querySelector('#estimateCommentAnswer1' + i).value + "</h2>";

                        commentQ.innerHTML = innerhtml;
                        commentA.classList.add('hidden');
                        let commentB = comment.appendChild(document.createElement('textarea'));
                        estimateNumber = 2;
                        commentB.id = 'estimateCommentAnswer'+estimateNumber+i;
                        commentB.placeholder = 'Make your final estimate (1-100)';
                    }
                    else if (estimateNumber == 2)
                    {
                        commentQ.innerHTML = origHtml + "<h1>True Answer: " + qs[i].trueAnswer + "</h1>";
                        let box = form.querySelector('#estimateCommentAnswer'+estimateNumber+i);
                        box.classList.add('hidden');
                        estimateNumber = 3;
                    }
                }
            }
            else if (type == "training")
            {
                 ok.onclick = function(e) {
                        e.preventDefault();
                        if (estimateNumber == 3)
                        {
                            if(i === questions.length - 1)
                            {
                                owner.estimateFormSubmit(form,qs,numOfQuestions,type,comment);
                            }
                            else
                            {
                                let div = this.form.querySelector('.estimateContainer:not(.hidden)');
                                div.classList.add('hidden');
                                div.nextSibling.classList.remove('hidden');
                                estimateNumber = 1;
                                return true;
                            }
                        }
                        if (estimateNumber == 1)
                        {
                            if(!checkResponse(this.form))
                            {
                                return false;
                            } 
                            let check = '#estimateCommentAnswer'+estimateNumber+i;
                            if (isNaN(parseInt(form.querySelector(check).value)))
                            {
                                advError.classList.remove('hidden');
                                return false;
                            }
                            if (parseInt(form.querySelector(check).value) > 100 || parseInt(form.querySelector(check).value) < 1)
                            {
                                advError.classList.remove('hidden');
                                return false;
                            }
                            advError.classList.add('hidden');
                            saveResponse(this.form);
                            innerhtml = innerhtml + "<h2>Your Answer: " + form.querySelector(check).value + "</br> Advisor Estimate: " + qs[i].advEstimate + "</h2>";
                            commentQ.innerHTML = innerhtml;
                            commentA.classList.add('hidden');
                            let box = form.querySelector('#estimateCommentAnswer'+estimateNumber+i);
                            box.classList.add('hidden');
                            estimateNumber = 2;

                        }
                        else if (estimateNumber == 2)
                        {
                            let check = '#estimateCommentAnswer1'+i;
                            commentQ.innerHTML = origHtml + "<h2>Your Answer: " + form.querySelector(check).value + "</br> Advisor Estimate: " + qs[i].advEstimate + "</br> True Answer: " + qs[i].trueAnswer + "</h2>";
                            estimateNumber = 3;
                        }
                    }

            }
            else if (type == "adjust")
            {
                innerhtml = innerhtml + "<h2>Advisor Estimate: " + qs[i].advEstimate + "</h2>";
                commentQ.innerHTML = innerhtml;
                commentA.placeholder = 'Adjust the Estimate of the Advisor Here';
                ok.onclick = function(e) {
                    e.preventDefault();
                    // if (estimateNumber == 2)
                    // {
                    //     if(i === questions.length - 1)
                    //     {
                    //         rewardOutput = owner.estimateFormSubmit(form,qs,numOfQuestions,type);
                    //     }
                    //     else
                    //     {
                    //         let div = this.form.querySelector('.estimateContainer:not(.hidden)');
                    //         div.classList.add('hidden');
                    //         div.nextSibling.classList.remove('hidden');
                    //         estimateNumber = 1;
                    //         return true;
                    //     }
                    // }
                    // else if (estimateNumber == 1)
                    // {
                    //     if(!checkResponse(this.form))
                    //     {
                    //         return false;
                    //     } 
                    //     let check = '#estimateCommentAnswer'+estimateNumber+i;
                    //     if (isNaN(parseInt(form.querySelector(check).value)))
                    //     {
                    //         advError.classList.remove('hidden');
                    //         return false;
                    //     }
                    //     if (parseInt(form.querySelector(check).value) > 100 || parseInt(form.querySelector(check).value) < 1)
                    //     {
                    //         advError.classList.remove('hidden');
                    //         return false;
                    //     }
                    //     advError.classList.add('hidden');
                    //     saveResponse(this.form);
                    //     commentQ.innerHTML = origHtml + "<h1>True Answer: " + qs[i].trueAnswer + "</h1>";
                    //     commentA.classList.add('hidden');
                    //     estimateNumber = 2;
                    // }

                    if(!checkResponse(this.form))
                    {
                        return false;
                    } 
                    let check = '#estimateCommentAnswer'+estimateNumber+i;
                    if (isNaN(parseInt(form.querySelector(check).value)))
                    {
                        advError.classList.remove('hidden');
                        return false;
                    }
                    if (parseInt(form.querySelector(check).value) > 100 || parseInt(form.querySelector(check).value) < 1)
                    {
                        advError.classList.remove('hidden');
                        return false;
                    }
                    advError.classList.add('hidden');
                    saveResponse(this.form);
                    if(i === questions.length - 1)
                    {
                        owner.estimateFormSubmit(form,qs,numOfQuestions,type,comment);
                    }
                    else
                    {
                        let div = this.form.querySelector('.estimateContainer:not(.hidden)');
                        div.classList.add('hidden');
                        div.nextSibling.classList.remove('hidden');
                        estimateNumber = 1;
                        return true;
                    }
                }

            }

        gov.estimateResponses = [];

    }
    }


    /**
     * Submit the form of answers to the estimate task trials.
     * @param form - HTML container of the form from the previous function to pull responses from
     * @param qs - object containing question and answer data.
     * @param numOfQuestions - how many questions are being asked in this function call
     * @param type - type of task can be training, influence, adjust or reward.
     * @param elementHTML - HTML element where we put text on the screen on rewards
     */
    estimateFormSubmit(form,qs,numOfQuestions,type,elementHTML) {
        var estimateData = [];
        var surveyData = [];
        this.estimateErr.push(0);
        var lastIndex = this.estimateErr.length-1;
        let numOfAdvisorEsts = 0;
        let advEstArray = [];
        let modelGuess = 0;
        let ownGuess = 0;
        if (type == "reward")
        {
            let questionQuery = '#estimateCommentAnswerReward';
            numOfAdvisorEsts = form.querySelector(questionQuery).value;
            questionQuery = '#estimateCommentModelGuess';
            modelGuess = form.querySelector(questionQuery).value;
            questionQuery = '#estimateCommentOwnGuess';
            ownGuess = form.querySelector(questionQuery).value;
        }
        let advCount = numOfAdvisorEsts;
        for (let q = 0;q<numOfQuestions;q++)
        {
            if (advCount > 0)
            {
                advEstArray.push(1);
                advCount -= 1;
            }
            else
            {
                advEstArray.push(0);
            }
        }
        utils.shuffle(advEstArray);
        for (let q = 0;q<numOfQuestions;q++)
        {
            let questionQuery = '#estimateCommentAnswer1' + q;
            estimateData.push([{question: qs[q]},{answer: form.querySelector(questionQuery).value},{taskType: type}]);
            if (advEstArray[q] == 1)
            {
                this.estimateErr[lastIndex] = this.estimateErr[lastIndex] + qs[q].advErr;
            }
            else
            {
                this.estimateErr[lastIndex] = this.estimateErr[lastIndex] + (Math.abs((qs[q].trueAnswer) - (form.querySelector(questionQuery).value)));
            }
            if (type == "influence")
            {
                questionQuery = '#estimateCommentAnswer2' + q;
                estimateData.push([{question: qs[q]},{answer: form.querySelector(questionQuery).value},{taskType: type}]);
            }
        }
        if (type == "reward")
        {
            surveyData.push([{question: "reward choice"},{answer: numOfAdvisorEsts},{taskType: type}]);
            surveyData.push([{question: "model guess"},{answer: modelGuess},{taskType: type}]);
            surveyData.push([{question: "own guess"},{answer: ownGuess},{taskType: type}]);
            for (let x = 0; x < surveyData.length; x++)
            {
                this.radio.push(surveyData[x]);
            }
        }
        for (let x = 0; x < estimateData.length; x++)
        {
            this.estimates.push(estimateData[x]);
        }
        var meanErr = this.estimateErr[lastIndex]/numOfQuestions;
        var estRew = Math.round(meanErr);
        if (estRew > 25 || type == 'training')
        {
            estRew = 0
        }
        else
        {
            estRew = (6-Math.ceil(estRew/5))*0.5;
            if (estRew > 2.5)
            {
                estRew = 2.5;
            }
            this.meanEstimateErr.push(meanErr);
        }
        this.estimateReward = this.estimateReward + estRew;
        this.totalReward = this.totalReward + estRew;
        this.advEstArray = advEstArray;
        this.exportGovernor();
        if (type !== "training")
        {
            let elementToHide = "estimateCommentQuestion" + (elementHTML.id)[elementHTML.id.length -1];
            elementHTML.classList.add('hidden');
            let p = form.insertBefore(document.createElement('p'), form.querySelector('div'));
            p.innerHTML = "In the previous block, based on your performance, you were awarded: £" + estRew;
            let ok = p.appendChild(document.createElement('button'));
            ok.innerText = 'next';
            ok.className = 'question jspsych-btn';
            ok.onclick = function(e) {
                    p.remove();
                    elementHTML.classList.remove('hidden');
                    jsPsych.finishTrial(this.estimates);
                    if (type == 'reward' && this.dotsComplete == 1)
                    {
                        this.showTotalReward();
                    }
            }
        }
    }

    /**
    * Show on the screen the total reward from the entire experiment
    */
    showTotalReward(redirectURL)
    {
        let debriefCon = document.querySelector('#debriefContainer');
        debriefCon.classList.add('hidden');
       var totRew = this.totalReward;
        let div = document.querySelector('#jspsych-content');
        let p = div.insertBefore(document.createElement('p'), div.querySelector('p'));
        p.innerHTML = "<p>Over the entire experiment, based on your performance, your total reward is:</p><p>£" + totRew;
        p.innerHTML = p.innerHTML + "<p>The researchers will arrange for your bonus reward on Prolific in the coming days.</p>"
        let ok = p.appendChild(document.createElement('button'));
        ok.innerText = 'finish';
        ok.className = 'question jspsych-btn';
        ok.onclick = function(e) {
                p.remove();
                //this.exportGovernor();
                //this.endExperiment();
                document.querySelector('body').innerHTML = "";
                window.location.replace(redirectURL);
        }
    }


    /**
    * Draw a questionnaire with multiple choice radio buttons.
    * @param {array} qs - array of strings for question texts
    * @param {array} options - array of strings for choices for the questions.
    * @param {string} name - name of the questionnaire.
    */
    drawRadioQuestionnaire(qs,options,name)
    {
        let owner = this;
        // Create form
        let div = document.querySelector('.jspsych-content').appendChild(document.createElement('div'));
        div.id = 'radioContainer';
        div.className = name;
        let header = div.appendChild(document.createElement('h1'));
        header.id = name + 'Title';
        div.className = name;
        let form = div.appendChild(document.createElement('form'));
        form.id = name + 'Form';
        form.className = 'name';

        let numOfQuestions = qs.length;
        let numOfOptions = options.length;

        let questions = [];

        // Go through questions list and append to form each 
        // With options laid out in the function argument

        for (let i = 0;i<numOfQuestions;i++)
        {
            questions.push({
                prompt: qs[i],
                mandatory: true,
                type: 'scale',
                labels: options,
                nOptions: numOfOptions
            });

        }

        for(let i = 0; i < questions.length; i++) 
        {
            let comment = form.appendChild(document.createElement('div'));
            comment.id = 'radioCommentContainer'+i;
            comment.className = 'radioContainer';
            if(i > 0)
            {
                comment.classList.add('hidden');
            }
            let commentQ = comment.appendChild(document.createElement('div'));
            commentQ.id = 'radioCommentQuestion'+i;
            commentQ.className = 'question question';
            commentQ.innerHTML = "<strong>Q"+(i+1)+"/"+(questions.length) + ":  " + questions[i].prompt + "</strong> ";
            commentQ.style = "font-size: 1.5em";
            let commentA = comment.appendChild(document.createElement('div'));

            let radios = commentA.appendChild(document.createElement('div'));
            let table = radios.appendChild(document.createElement('table'));
            table.style = 'margin-right: auto; margin-left: auto';
            for(let o = 0; o < questions[i].nOptions; o++) {
                let tr = table.appendChild(document.createElement('tr'));
                radios.className = 'radios';
                radios.id = 'options';
                radios.style = 'margin-top: 10px';

                let td = tr.appendChild(document.createElement('td'));

                let label = td.appendChild(document.createElement('label'));
                let labelid = "label-"+i+"-"+o;
                label.id = labelid;
                label.style = "display:block; margin:5px";

                let choice = document.getElementById(labelid);
                choice.innerHTML = options[o];


                let td2 = tr.appendChild(document.createElement('td'));

                let radio = td2.appendChild(document.createElement('input'));
                radio.type = 'radio';
                radio.value = (o + 1).toString();
                radio.name = commentQ.id;
                radio.style = "justify-self: center; height: 3em; width: 3em";
            } 

            let ok = comment.appendChild(document.createElement('button'));
            ok.innerText = i === questions.length - 1? 'submit' : 'next';
            ok.className = 'question jspsych-btn';

            let checkResponse;
            let saveResponse;

            checkResponse = function(form) {
            let div = form.querySelector('.radioContainer:not(.hidden) .radios');
            let ok = false;
            let radios = div.querySelectorAll('input[type="radio"]');
            radios.forEach((r)=>{if(r.checked) ok = true});
            if(!ok)
                form.querySelector('.radioContainer:not(.hidden)').classList.add('bad');
            else
                form.querySelector('.radioContainer:not(.hidden)').classList.remove('bad');
            return ok;
            };

            saveResponse = function(form) {
                let q = questions[i];
                form.querySelectorAll('.radioContainer:not(.hidden) input[type="radio"]').forEach(
                    (r)=>{ if(r.checked) q.answer = r.value}
                );
                gov.radioResponses.push(q);
            };       

           if(!questions[i].mandatory)
           {
                checkResponse = ()=>true;
           }

            if(i === questions.length - 1)
                ok.onclick = function (e) {
                    e.preventDefault();
                    if(!checkResponse(this.form))
                        return false;
                    saveResponse(this.form);
                    owner.radioFormSubmit(form,questions,numOfQuestions,name);
                };
            else
                ok.onclick = function(e) {
                    e.preventDefault();
                    if(!checkResponse(this.form))
                        return false;
                    saveResponse(this.form);
                    let div = this.form.querySelector('.radioContainer:not(.hidden)');
                    div.classList.add('hidden');
                    div.nextSibling.classList.remove('hidden');
                    }
        }

        gov.radioResponses = [];
    }

    /**
     * Submit the questionnaire form
     * Needs to be done for each questionnaire to allow for multiple sets of survey data
     * to be collected. 
     */
    radioFormSubmit(form,qs,numOfQuestions,name) {
        var radioData = [];
        for (let q = 0;q<numOfQuestions;q++)
        {
            let questionQuery = '#radioCommentContainer' + q;
            //radioData.push({question: qs[q]},{answer: form.querySelector(questionQuery).answer});
            radioData.push({question: qs[q].prompt, answer: qs[q].answer});
        }
        for (let x = 0; x < radioData.length; x++)
        {
            this.radio.push(radioData[x]);
        }
        this.exportGovernor();
        document.querySelector('#radioContainer').innerHTML = "";
        jsPsych.finishTrial(this.radio);
    }

    /**
    * Draw a questionnaire with multiple choice radio buttons for attention check
    * @param {array} qs - array of strings for question texts
    * @param {array} options - array of strings for choices for the questions.
    * @param {string} name - name of the questionnaire.
    */
    attentionCheckQuestionnaire(qs,options,name,corrects)
    {
        let owner = this;
        // Create form
        let div = document.querySelector('.jspsych-content').appendChild(document.createElement('div'));
        div.id = 'questionContainer';
        div.className = name;
        let header = div.appendChild(document.createElement('h1'));
        header.id = name + 'Title';
        div.className = name;
        let form = div.appendChild(document.createElement('form'));
        form.id = name + 'Form';
        form.className = 'name';

        let numOfQuestions = qs.length;
        let numOfOptions = options[0].length;

        let questions = [];

        // Go through questions list and append to form each 
        // With options laid out in the function argument

        for (let i = 0;i<numOfQuestions;i++)
        {
            questions.push({
                prompt: qs[i],
                mandatory: true,
                type: 'scale',
                labels: options[i],
                nOptions: numOfOptions,
                correctAns: corrects[i]
            });

        }

        for(let i = 0; i < questions.length; i++) 
        {
            let comment = form.appendChild(document.createElement('div'));
            comment.id = 'questionCommentContainer'+i;
            comment.className = 'questionContainer';
            if(i > 0)
            {
                comment.classList.add('hidden');
            }
            let commentQ = comment.appendChild(document.createElement('div'));
            commentQ.id = 'questionCommentQuestion'+i;
            commentQ.className = 'question question';
            commentQ.innerHTML = "<strong>Q"+(i+1)+"/"+(questions.length) + ":  " + questions[i].prompt + "</strong> ";
            commentQ.style = "font-size: 1.5em";
            let commentA = comment.appendChild(document.createElement('div'));

            let radios = commentA.appendChild(document.createElement('div'));
            let table = radios.appendChild(document.createElement('table'));
            table.style = 'margin-right: auto; margin-left: auto';
            for(let o = 0; o < questions[i].nOptions; o++) {
                let tr = table.appendChild(document.createElement('tr'));
                radios.className = 'radios';
                radios.id = 'options';
                radios.style = 'margin-top: 10px';

                let td = tr.appendChild(document.createElement('td'));

                let label = td.appendChild(document.createElement('label'));
                let labelid = "label-"+i+"-"+o;
                label.id = labelid;
                label.style = "display:block; margin:5px";

                let choice = document.getElementById(labelid);
                choice.innerHTML = (options[i])[o];


                let td2 = tr.appendChild(document.createElement('td'));

                let radio = td2.appendChild(document.createElement('input'));
                radio.type = 'radio';
                radio.value = (o + 1).toString();
                radio.name = commentQ.id;
                radio.style = "justify-self: center; height: 3em; width: 3em";
            } 

            let ok = comment.appendChild(document.createElement('button'));
            ok.innerText = i === questions.length - 1? 'submit' : 'next';
            ok.className = 'question jspsych-btn';

            let checkResponse;
            let saveResponse;
            let fail = false;

            checkResponse = function(form) {
            let div = form.querySelector('.questionContainer:not(.hidden) .radios');
            let ok = false;
            let radios = div.querySelectorAll('input[type="radio"]');
            radios.forEach((r)=>{if(r.checked) ok = true});
            if(!ok)
                form.querySelector('.questionContainer:not(.hidden)').classList.add('bad');
            else
                form.querySelector('.questionContainer:not(.hidden)').classList.remove('bad');
            return ok;
            };

            saveResponse = function(form) {
                let q = questions[i];
                form.querySelectorAll('.questionContainer:not(.hidden) input[type="radio"]').forEach(
                    (r)=>{ if(r.checked) q.answer = r.value}
                );
                gov.radioResponses.push(q);
            };       

           if(!questions[i].mandatory)
           {
                checkResponse = ()=>true;
           }

            if(i === questions.length - 1)
                ok.onclick = function (e) {
                    e.preventDefault();
                    if(!checkResponse(this.form))
                        return false;
                    saveResponse(this.form);
                    for (let q = 0;q<numOfQuestions;q++)
                    {
                        let questionQuery = '#questionCommentContainer' + q;
                        let ans = questions[q].answer;
                        let choices = options[q];
                        if (parseInt(ans) !== corrects[q])
                        {
                            fail = true;
                        }
                    }
                    attCheck = fail;
                    document.querySelector('#questionContainer').innerHTML = "";
                    jsPsych.finishTrial(this.fail);
                };
            else
                ok.onclick = function(e) {
                    e.preventDefault();
                    if(!checkResponse(this.form))
                        return false;
                    saveResponse(this.form);
                    let div = this.form.querySelector('.questionContainer:not(.hidden)');
                    div.classList.add('hidden');
                    div.nextSibling.classList.remove('hidden');
                    }
        }

        gov.radioResponses = [];
    }

    attentionCheckFail()
    {
        return this.fail;
    }

    /**
    * Memory maintanence task stimulus 
    * Show a string of letters for participant to memorise.
    */
    maintainMemoryTaskStim(len)
    {
        let alphabet = "bcdfghjklmnpqrstvwxz";
        let nextString = "";
        let number;
        while (nextString.length < len)
        {
            number = utils.genRandInt(0,19);
            if (nextString.includes(alphabet[number]) == false)
            {
                nextString = nextString + alphabet[number];
            }
        }
        this.maintainMemoryString = nextString;
        let div = document.querySelector('.jspsych-content').appendChild(document.createElement('div'));
        div.id = 'maintainMemoryStimContainer';
        div.innerHTML = "<h2>Please take a moment to memorise the letters below:</h2><br/><br/>"+"<h1>"+nextString+"</h1>";

        let form = div.appendChild(document.createElement('form'));
        form.id = name + 'Form';
        form.className = 'name';

        let ok = div.appendChild(document.createElement('button'));
        ok.innerText = 'next';
        ok.className = 'question jspsych-btn';

        ok.onclick = function (e) {
            e.preventDefault();
            jsPsych.finishTrial();
        };
    }

    maintainMemoryTaskRecall(audio="")
    {
        let owner = this;
        let alphabet = "bcdfghjklmnpqrstvwxz";
        let startString = this.maintainMemoryString;
        let shownString = startString;
        let change;
        if (Math.random() < .5)
        {
            change = 0;
        }
        else
        {
            change = 1;
            let flag = 0;
            let charToChange = utils.genRandInt(0,4);
            let number = utils.genRandInt(0,19);
            if (shownString.includes(alphabet[number]) == true)
            {
                flag = 1;
            }
            while (flag == 1)
            {
                number = utils.genRandInt(0,19);
                if (shownString.includes(alphabet[number]) == false)
                {
                    flag = 0;
                }
            }
            shownString = shownString.split(shownString[charToChange])[0] + alphabet[number] + shownString.split(shownString[charToChange])[1];
        }

        let div = document.querySelector('.jspsych-content').appendChild(document.createElement('div'));
        div.id = 'maintainMemoryRecallContainer';
        div.innerHTML = "<h2>Are the below letters the same as you were shown before?</h2><br/><br/>"+"<h1>"+shownString+"</h1>";

        let form = div.appendChild(document.createElement('form'));
        form.id = name + 'Form';
        form.className = 'name';

        let radios = div.appendChild(document.createElement('div'));
        radios.className = 'radios';
        radios.id = 'options';
        radios.style = 'margin-top: 10px';

        let labelYes = radios.appendChild(document.createElement('label'));
        labelYes.id = "label-yes";
        labelYes.style = "display:block; margin:5px";
        labelYes.innerHTML = "Yes";

        let radioYes = labelYes.appendChild(document.createElement('input'));
        radioYes.type = 'radio';
        radioYes.name = "maintainMemYN";
        radioYes.value = 0;
        radioYes.style = "justify-self: center";

        let labelNo = radios.appendChild(document.createElement('label'));
        labelNo.id = "label-no";
        labelNo.style = "display:block; margin:5px";
        labelNo.innerHTML = "No";

        let radioNo = labelNo.appendChild(document.createElement('input'));
        radioNo.type = 'radio';
        radioNo.name = "maintainMemYN";
        radioNo.value = 1;
        radioNo.style = "justify-self: center";

        let ok = div.appendChild(document.createElement('button'));
        ok.innerText = 'submit';
        ok.className = 'question jspsych-btn';

        let checkResponse;
        let saveResponse;
        let answer;

        checkResponse = function(form) {
           // let div = form.querySelector('.radios');
            let ok = false;
            let rads = div.querySelectorAll('input[type="radio"]');
            rads.forEach((r)=>{if(r.checked) ok = true});
            return ok;
        };

        saveResponse = function(form) {
            div.querySelectorAll('input[type="radio"]').forEach(
                (r)=>{ if(r.checked) answer = r.value}
            );
            gov.maintainMems.push(startString,shownString,answer);
        };       

        ok.onclick = function (e) {
                e.preventDefault();
                if(!checkResponse(this.form))
                    return false;
                saveResponse(this.form);
                if (audio.length > 0 && parseInt(answer) !== change)
                {
                    var tone = new Audio(audio);
                    tone.play();
                }
                owner.maintainMemFormSubmit(startString,shownString,answer);
                if (document.querySelector('#maintainMemoryRecallContainer') !== null)
                {
                    document.querySelector('#maintainMemoryRecallContainer').innerHTML = "";
                }
                
        };

        gov.maintainMems = [];

    }

    maintainMemFormSubmit(starting,shown,answer) {
        this.maintainMem = [
            {
                starting: starting,
                shown: shown,
                answer: answer
            }
        ];
        jsPsych.finishTrial(this.maintainMem);
    }


    workingMemoryTaskStim()
    {
        let alphabet = "bcdfghjklmnpqrstvwxz";
        let number = utils.genRandInt(0,19);
        let currentLetterStack = this.workingMemoryStack;
        let flag = 0;

        if (currentLetterStack.includes(alphabet[number]) == true)
        {
            flag = 1;
        }

        while (flag == 1)
        {
            number = utils.genRandInt(0,19);
            if (currentLetterStack.includes(alphabet[number]) == false)
            {
                flag = 0;
            }
        }

        currentLetterStack.unshift(alphabet[number]);
        this.workingMemoryStack = currentLetterStack;

        let div = document.querySelector('.jspsych-content').appendChild(document.createElement('div'));
        div.id = 'workingMemoryStimContainer';
        div.innerHTML = "<h2>Please take a moment to memorise the letter below:</h2><br/><br/>"+"<h1>"+alphabet[number]+"</h1>";

        let form = div.appendChild(document.createElement('form'));
        form.id = name + 'Form';
        form.className = 'name';

        let ok = div.appendChild(document.createElement('button'));
        ok.innerText = 'next';
        ok.className = 'question jspsych-btn';


        ok.onclick = function (e) {
            e.preventDefault();
            jsPsych.finishTrial();
        };

    }

    workingMemoryTaskRecall(N,audio="")
    {
        let owner = this;
        let alphabet = "bcdfghjklmnpqrstvwxz";
        let currentLetterStack = this.workingMemoryStack;
        let letter = currentLetterStack[N-1];
        let div = document.querySelector('.jspsych-content').appendChild(document.createElement('div'));
        div.id = 'workingMemoryStimContainer';

        let form = div.appendChild(document.createElement('form'));
        form.id = name + 'Form';
        form.className = 'name';

        let change;
        let shownLetter;

        if (Math.random() < .5)
        {
            change = 0;
            shownLetter = letter;
        }

        else
        {
            change = 1;
            let flag = 0;
            let number = utils.genRandInt(0,19);
            if (letter == alphabet[number] == true)
            {
                flag = 1;
            }
            while (flag == 1)
            {
                number = utils.genRandInt(0,19);
                if (letter == alphabet[number] == false)
                {
                    flag = 0;
                }
            }
            shownLetter = alphabet[number];
        }

        div.innerHTML = "<h2>Is this the same letter that was shown "+N+" times ago?<br/><br/>"+"<h1>"+shownLetter+"</h1>";

        let radios = div.appendChild(document.createElement('div'));
        radios.className = 'radios';
        radios.id = 'options';
        radios.style = 'margin-top: 10px';

        let labelYes = radios.appendChild(document.createElement('label'));
        labelYes.id = "label-yes";
        labelYes.style = "display:block; margin:5px";
        labelYes.innerHTML = "Yes";

        let radioYes = labelYes.appendChild(document.createElement('input'));
        radioYes.type = 'radio';
        radioYes.name = "maintainMemYN";
        radioYes.value = 0;
        radioYes.style = "justify-self: center";

        let labelNo = radios.appendChild(document.createElement('label'));
        labelNo.id = "label-no";
        labelNo.style = "display:block; margin:5px";
        labelNo.innerHTML = "No";

        let radioNo = labelNo.appendChild(document.createElement('input'));
        radioNo.type = 'radio';
        radioNo.name = "maintainMemYN";
        radioNo.value = 1;
        radioNo.style = "justify-self: center";

        let ok = div.appendChild(document.createElement('button'));
        ok.innerText = 'next';
        ok.className = 'question jspsych-btn';

        let checkResponse;
        let saveResponse;
        let answer;

        checkResponse = function(form) {
            div.querySelectorAll('input[type="radio"]').forEach(
                (r)=>{ if(r.checked) answer = r.value}
            );
            return ok;
        };
            
        saveResponse = function(form) {
            gov.workingMems.push(letter,N,answer);
        };

        ok.onclick = function (e) {
            e.preventDefault();
            if(!checkResponse(this.form))
                return false;
            saveResponse(this.form);
            if (audio.length > 0 && parseInt(answer) !== change)
            {
                var tone = new Audio(audio);
                tone.play();
            }
            owner.workingMemFormSubmit(letter,shownLetter,N,answer);
            if (document.querySelector('workingMemoryStimContainer') !== null)
            {
                document.querySelector('workingMemoryStimContainer').innerHTML = "";
            }
        };

        gov.workingMems = [];

    }


    workingMemFormSubmit(letter,shownLetter,N,answer) {
        this.workingMem = [
            {
                letter: letter,
                shownLetter: shownLetter,
                N: N,
                answer: answer
            }
        ];
        jsPsych.finishTrial(this.workingMem);
    }

    feedback(data, includePayment = false) {
        if(typeof data === 'undefined')
            data = this;
        google.charts.load('current', {'packages':['corechart']});
        google.charts.setOnLoadCallback(function(){dotTask.showFeedback(data, includePayment)});
    }

    endExperiment(saveData = true, clearScreen = true) {
        this.timeEnd = (new Date()).getTime();
        // Exit fullscreen
        //this.fullscreenMode(false);
        if(saveData === true)
            this.exportGovernor();
        // reset background colour
        if(clearScreen === true) {
            document.querySelector('body').style.backgroundColor = '';
            //document.body.innerHTML = "<div id='content'></div>";
            document.body.innerHTML = '<img width="1000" height="700" src="./instructions/Slide18.jpeg" style="transform: translate(50%, 20%)"></img>';
        }
        //this.feedback(this, (saveData && this.completionURL !== ''));
    }

    /**
     * Return the category of the confidence offered in the trial with id *trialId*. Confidence category is 0, 1, or 2
     * depending upon whether the confidence is in the lower 30%, middle 40%, or upper 30% of trials.
     * @param {int} trialId - identifier of the trial
     * @param {Object} [args] - additional options object
     * @param {int} [args.nTrialsBack=null] - maximum number of trials to search
     * @param {boolean} [args.correctOnly=true] - whether to only extract confidence on correct trials
     * @param {boolean} [args.initialAnswerCorrect=true] - whether to test the inital (as opposed to final answer) correctness
     * @param {boolean} [args.initialConfidence=true] - whether to count the initial (as opposed to final) confidence
     */
    getConfidenceCategory(trialId, args) {
        args = args || {};
        args.nTrialsBack = typeof args.nTrialsBack === 'undefined'? this.trials.length : args.nTrialsBack;
        if(args.nTrialsBack === null)
            args.nTrialsBack = this.trials.length;
        args.correctOnly = typeof args.correctOnly === 'undefined'? true : args.correctOnly;
        args.initialAnswerCorrect = typeof args.initialAnswerCorrect? true : args.initialAnswerCorrect;
        args.initialConfidence = typeof args.initialConfidence? true : args.initialConfidence;
        let trialIndex = this.trials.indexOf(utils.getMatches(this.trials, function (trial) {
            return trial.id === trialId;
        })[0]);
        if (trialIndex === -1) {
            this.currentTrial.warnings.push('getConfidenceCategory: trial not found in this.trials');
            return 1;
        }
        if (trialIndex === 0)
            return 1; // first trial has no history
        let confidenceScore = this.trials[trialIndex].confidence[(args.initialConfidence? 0 : 1)];

        // collate valid trials and get confidence
        let confidenceList = [];
        for (let i=0; i<args.nTrialsBack; i++) {
            // stop if we run out of trials
            if (i+1 === trialIndex) {
                break;
            }
            let trial = this.trials[trialIndex-(i+1)];
            // have to have provided a confidence
            if (isNaN(trial.confidence[(args.initialConfidence? 0 : 1)]))
                continue;
            // have to be correct if we want only correct trials
            if (args.correctOnly && trial.answer[(args.initialAnswerCorrect? 0 : 1)] !== trial.whichSide)
                continue;
            confidenceList.push(trial.confidence[(args.initialConfidence? 0 : 1)]);
        }

        // Put confidence list in order
        confidenceList.sort();
        // Find the markers at 30% and 70%
        let bounds = {
            low: confidenceList[Math.ceil(confidenceList.length*.3)],
            high: confidenceList[Math.floor(confidenceList.length*.7)]
        };

        // Protect against too few trials
        if (typeof bounds.low === 'undefined' || typeof bounds.high === 'undefined') {
            this.currentTrial.warnings.push('getConfidenceCategory: too few trials available to estimate confidence');
            return 1;
        }

        if (confidenceScore > bounds.low && confidenceScore < bounds.high)
            return 1;
        if (confidenceScore <= bounds.low)
            return 0;
        if (confidenceScore >= bounds.high)
            return 2;

        // Fallback
        this.currentTrial.warnings.push('getConfidenceCategory: confidence score ('+confidenceScore+') fell through ' +
            'bounds ['+bounds.low+', '+bounds.high+']');
    }

    /**
     * Get the confidence category of the last response
     * @param {Object} [args] - additional options object
     * @param {int} [args.nTrialsBack=null] - maximum number of trials to search
     * @param {boolean} [args.correctOnly=true] - whether to only extract confidence on correct trials
     * @param {boolean} [args.initialAnswerCorrect=true] - whether to test the inital (as opposed to final answer) correctness
     * @param {boolean} [args.initialConfidence=true] - whether to count the initial (as opposed to final) confidence
     */
    getLastConfidenceCategory(args) {
        let last = utils.getMatches(this.trials, function (trial) {
            return !isNaN(trial.answer[0]);
        }).length-1;

        let cc = this.getConfidenceCategory(this.trials[last].id, args);
        this.trials[last].confidenceCategory = cc;
        return cc;
    }

    pointsTotal(points) {
        let ele = document.querySelector(".confidence-points-text");
        if (ele == null)
        {
            let ctx = document.querySelector('body');
            //let tr = ctx.appendChild(document.createElement('tr'));
            let tr = ctx.insertBefore(document.createElement('tr'),document.querySelector(".jspsych-content-wrapper"))
            tr.className = "confidence-points-table";
            tr.style = "align-self: center";
            let th = tr.appendChild(document.createElement('th'));
            th.className = "confidence-points-text";
            let p = th.appendChild(document.createElement('p'));
            let text = "Current Points: " + points.toString();
            p.style = "margin-block-start: 0em; margin-block-end: 0em";
            p.innerText = text;
        }
        else
        {
            let p = document.querySelector(".confidence-points-text p")
            p.style = "margin-block-start: 0em; margin-block-end: 0em; display: block";
            p.innerText = "Current Points: " + points.toString();
        }
    }

    /**
     * Show feedback for the current trial.
     * Called by jspsych-canvas-sliders-response
     * @param {string} canvasId - id of the canvas to draw on
     */
    showTrialFeedback(canvasId) {
        let canvas = document.getElementById(canvasId);
        // give feedback on previous trial
        let trial = this.trials[this.currentTrialIndex-1];
        DotTask.drawFixation(canvasId);
        trial.grid.drawBoundingBoxes(canvasId);
        trial.grid.draw(canvasId, trial.whichSide);
    }

    /**
     * Wrap up a trial. Store data, staircase difficulty, and prepare next trial.
     * @param {Object} trial - jsPsych plugin response
     */
    closeTrial(trial) {
        // Staircasing stuff
        let warning = "";
        if (this.currentTrialIndex > 1) {
            // two-down one-up staircase
            let lastTrial = this.trials[this.currentTrialIndex - 1];
            if (!this.currentTrial.getCorrect(false)) {
                // Wrong! Make it easier
                this.dotDifference += this.difficultyStep.current;
                if (this.dotDifference > this.dotCount - 1) {
                    this.dotDifference = this.dotCount - 1;
                    warning = "Difficulty at minimum!";
                }

                if (this.dotDifference > this.maxDD)
                {
                    this.dotDifference = this.maxDD;
                }

                // Update the step size
                //if (this.difficultyStep.current > this.difficultyStep.end
                //    && --this.difficultyStep.currentReversals <= 0) {
                //    this.difficultyStep.current--;
                //    this.difficultyStep.currentReversals = this.difficultyStep.nReversals;
                //}
            } else if (lastTrial.getCorrect(false) &&
                this.currentTrial.getCorrect(false) &&
                this.currentTrial.dotDifference === lastTrial.dotDifference) {
                // Two hits, impressive! Make it harder
                this.dotDifference -= this.difficultyStep.current;
                if (this.dotDifference < 1) {
                    this.dotDifference = 1;
                    warning = "Difficulty at maximum!";
                }
            }
        } else {
            // First trial: initialize the difficulty step tracker variables
            this.difficultyStep.current = this.difficultyStep.start;
            this.difficultyStep.currentReversals = this.difficultyStep.nReversals;
        }
        if (warning.length > 0 && this.currentTrialIndex < this.trials.length) {
            this.currentTrial.warnings.push(warning);
            console.warn(warning);
        }
        // Move to next trial
        this.currentTrialIndex++;
    }
}

/**
 * @classdesc A dot task governor controls the dot display experiment functionality
 * @class
 * @augments DotTask
 */
class AdvisorChoice extends DotTask {
    /**
     * @constructor
     *
     * @param {Object} [args={}] - properties to assign to the Governor
     * @param {Trial[]} [args.trials=[]] - trial list
     * @param {Object[]} [args.miscTrials] - miscellaneous trials (breaks, instructions, etc)
     * @param {int} [args.currentTrialIndex=0] - index of current trial in trial list
     * @param {string} [args.completionURL=''] - URL to which to refer participants for payment
     * @param {string} [args.experimentCode=''] - code identifying the experiment
     *
     * @param {int} [args.dotCount] - number of dots in a box
     * @param {int} [args.dotDifference] - half the difference between the dot counts in the two boxes; the difficulty
     * @param {int} [args.difficultyStep] - amount the difficulty increases/decreases after success/failure
     * @param {number} [args.minimumBlockScore] - lowest proportion of successful trials allowed on a block
     * @param {int} [args.blockCount] - number of blocks in the study
     * @param {Object|Object[]} [args.blockStructure] - the structure of each block, where each object is a series of [trialType: number of instances] mappings. Multiple objects represent different subblocks run consecutively.
     * @param {Object|Object[]} [args.practiceBlockStructure] - the structure of each practice block
     * @param {int} [args.preTrialInterval] - delay before each trial begins
     * @param {int} [args.preStimulusInterval] - fixation delay before the stimulus is displayed
     * @param {int} [args.stimulusDuration] - duration the dot stimulus is displayed
     * @param {int} [args.feedbackDuration] - duration of the feedback screen
     *
     * @param {Advisor[]} [args.advisors=[]] - list of advisors
     * @param {Advisor[]} [args.practiceAdvisors=[]] - practice advisor
     * @param {[Advisor[]]} [args.advisorLists] - list of lists of advisors, each one being a set of advisors competing with one another in a block
     * @param {[Advisor[]]} [args.contingentAdvisors] - list of advisors to be used contingent on the confidence category of a response matching the list index
     * @param {[Advisor[]]} [args.questionnaireStack] - stack of advisors about whom questionnaires are to be asked
     * @param {Object} [args.generalisedTrustQuestionnaire=null] - Generalised Trust Questionnaire response
     * @param {int} [args.changeTime = 1500] - time to offer advisor change on change trials in ms
     *
     * @property {Advisor} currentAdvisor - advisor currently in focus
     * @property {Trial} currentTrial - trial currently underway
     */
    constructor(args = {}) {
        super(args);
        this.advisors = typeof args.advisors === 'undefined'? [] : AdvisorChoice.addAdvisors(args.advisors);
        this.practiceAdvisors = typeof args.practiceAdvisors === 'undefined'? [] : args.practiceAdvisors;
        this.advisorLists = typeof args.advisorLists === 'undefined'? null : args.advisorLists;
        this.contingentAdvisors = typeof args.contingentAdvisors === 'undefined'? null : args.contingentAdvisors;
        this.questionnaireStack = typeof args.questionnaireStack === 'undefined'? null : args.questionnaireStack;
        this.generalisedTrustQuestionnaire = typeof args.generalisedTrustQuestionnaire === 'undefined'?
            null : args.generalisedTrustQuestionnaire;
        this.changeTime = typeof args.changeTime === 'undefined'? 1500 : args.changeTime;
        this.drawDebriefForm = debriefForm; // why is this in a separate file?
        this.maxDD = args.dotDifference;
        this.forcedTrials = permForced;
        this.choiceTrials = permChoice;
        this.blk4Trials = permBlk4;
        this.confidenceOn = args.confidenceOn;
        this.beepOn = args.beepOn;
        this.audioSrc = args.audioSrc; 
        this.staircase = args.staircase;
        this.redirect = args.redirect;
        this.CBcondition = args.CBcondition;
        this.firstForced = 0;
        this.firstChoice = 0;
    }

    /**
     * Upgrade stored advisor details to genuine advisors
     * @param {Object[]} advisors - advisors stored as JSON-compressed objects
     * @return {Advisor[]} - advisors expanded to be Advisor objects
     */
    static addAdvisors(advisors) {
        let out = [];
        for(let i=0; i<advisors.length; i++) {
            if(advisors[i] instanceof Advisor)
                out[i] = advisors[i];
            else
                if(advisors[i].isCue === true)
                    out[i] = new Cue(advisors[i]);
                else
                    out[i] = new Advisor(advisors[i]);
        }
        return out;
    }

    /**
     * @return {Advisor} - The advisor registered for the current trial
     */
    get currentAdvisor() {
        return this.getAdvisorById(this.currentTrial.advisorId);
    }

    get practiceAdvisor() {
        return this.practiceAdvisors[0];
    }

    /**
     * @return {string} - Advice string for the current trial
     */
    get adviceString() {
        return this.currentTrial.advice.string;
    }

    /**
     * @param {int} advisorId
     * @return {Advisor} with the specified id
     */
    getAdvisorById(advisorId) {
        let advisor = this.advisors[this.getAdvisorIndex(advisorId)];
        if(typeof advisor === 'undefined')
            return this.practiceAdvisors[this.getAdvisorIndex(advisorId, true)];
        else
            return advisor;
    }

    /**
     * Return the index of an advisor in the advisors list
     * @param {int} id - id of the advisor whose index is required
     * @param {boolean} [practiceAdvisor=false] whether the sought advisor is a practice advisor
     * @return {int|null} - index of the advisor in the advisors list
     */
    getAdvisorIndex(id, practiceAdvisor = false) {
        let arr = practiceAdvisor? this.practiceAdvisors : this.advisors;
        for (let i=0; i<arr.length; i++) {
            if (arr[i].id === id)
                return i;
        }
        return null;
    }

     /**
    * Create an array of advisor choices based on desired reliability
    * Call this in getTrials("experimental")
    */
    reliabilityAdvisor(rel, trials, grouping = 0) {
        let arr = [];
        let temp;
        if (grouping == 0)
        {
            grouping = trials;
        }
        let correct = grouping*rel;
        let iterations = trials/grouping;
        for (let n = 0; n < iterations; n++)
        {
            temp = [];
            for (let x = 0; x < correct; x++){
                temp.push(1);
            }
            for (let y = 0; y < (grouping-correct); y++){
                temp.push(0);
            }
            utils.shuffle(temp);
            arr.push(temp);
        }
        arr = arr.flat();
        return(arr);
    }
    
    /**
    * Create an array of advisor choices based on desired reliability
    * Call this in getTrials("experimental")
    */
    createWhereLarger(trials) 
    {
        let arr = [];
       for (let x = 0; x < trials; x++)
        {
            let ra = Math.random();
            if (ra < 0.5)
            {
              arr.push(1);
            }
            else
            {
                arr.push(0);
            }

        }
        utils.shuffle(arr);
        return(arr);
    }


    /**
     * Return a list of Trial objects.
     *
     * A large part of the work of defining the experiment takes place here, although the key properties are
     * actually defined in the Governor definition.
     *
     * The trials defined here are the master list used by the Governor to decide which stimuli to serve,
     * which advisor or choice to offer, etc. This list is **not necessarily the same as** the trial list
     * established at the beginning of the experiment and handed to jsPsych. It is therefore the responsibility
     * of the programmer to ensure that these lists are lawfully aligned such that the block structures, etc.
     * match.
     *
     * A possible alternative strategy - push new trials to the jsPsych timeline at
     * the end of each completed trial. Since we don't get nice progress bar this way we may as well use on-the-fly
     * timeline tweaking. This may just be more work to duplicate jsPsych's capabilities, though
     */
    getTrials(section="experimental") {
        let trials = [];
        let realId = 0;
        let advisorSets = this.advisorLists.length;
        let blockCount = this.blockStructure.length * advisorSets;
        let practiceBlockCount = this.practiceBlockStructure.length;
        // Same for which side the correct answer appears on
        let whichSideDeck = utils.shuffleShoe([0, 1], advisorSets*utils.sumList(this.blockStructure));
        let forcedCount = 0;
        let choiceCount = 0;
        let blk4Count = 0;
        // Define trials
        let advisorSet = 0;
        let advisor0id = null;
        let advisor1id = null;
        let blockStructure = null;
        let advisorChoices = [];
        let advisorForceDeck = null;
        let advisorChangeDeck = null;
        let advisorForceBlk4Deck = null;
        let advisorBlk4Deck = null;
        if (section=="practice")
        {
            let id = 0;
            for (let b=0; b<practiceBlockCount; b++) {
                advisorSet = 0;
                blockStructure = this.practiceBlockStructure[b];
                advisorChoices = this.practiceAdvisors;
                let blockLength = utils.sumList(blockStructure);
                // Work out what type of trial to be
                let trialTypeDeck = [];
                for (let tt=0; tt<Object.keys(trialTypes).length; tt++) {
                    for (let i=0; i<blockStructure[tt]; i++)
                        trialTypeDeck.push(tt);
                }
                trialTypeDeck = utils.shuffle(trialTypeDeck);
                for (let i=1; i<=blockLength; i++) 
                {
                    id++;
                    let isPractice = true;
                    let trialType = trialTypeDeck.pop();
                    let advisorId = 0;
                    let left;
                    let right;
                    let larger;
                    let algAns;
                    let algCor;
                    let algCon;
                    let trialSelect;
                    // Below we pull the advisor, dots and confidence used for this trial again.
                    // We merely do this for data recoridng purposes at the end of the experiment.
                    left = null;
                    right = null;
                    larger = null;
                    algAns = null;
                    algCor = null;
                    algCon = null;
                    trialSelect = null;
                    advisorId = trialType===trialTypes.catch? 0 : this.practiceAdvisor.id;
                    let defaultAdvisor = trialType === trialTypes.change? advisorChangeDeck.pop().id : null;
                    let changes = trialType === trialTypes.change? 0 : null;
                    let r = Math.random() < .5? 1 : 0;
                    let choice = trialType === trialTypes.choice? [advisorChoices[r].id, advisorChoices[1-r].id] : [];
                    // let choice = isPractice? [] : [advisorChoices[r].id, advisorChoices[1-r].id];
                    trials.push(new Trial(id, {
                        type: trialType,
                        typeName: trialTypeNames[trialType],
                        block: b,
                        advisorSet,
                        defaultAdvisor,
                        advisorId,
                        advisor0id,
                        advisor1id,
                        choice,
                        changes,
                        answer: [NaN, NaN],
                        confidence: [NaN, NaN],
                        getCorrect: function(finalAnswer = true) {
                            let answer = finalAnswer? this.answer[1] : this.answer[0];
                            return answer === this.whichSide;
                        },
                        whichSide: isPractice? Math.round(Math.random()) : whichSideDeck[realId],
                        practice: isPractice,
                        feedback: isPractice,
                        warnings: [],
                        stimulusDrawTime: [],
                        stimulusOffTime: [],
                        fixationDrawTime: [],
                        advisorAnswer: algAns,
                        advisorCorrect: algCor,
                        //advisorConfidence: algCon,
                        advisorConfidence: 0,
                        leftGrid: left,
                        rightGrid: right,
                        whereLarger: larger,
                        trialSelect: trialSelect
                    }));
                    if (!isPractice)
                        realId++;
                }
            }
        }
        else
        {
            let id = (practiceBlockCount*this.practiceBlockStructure[0]["0"]);
            let advisorAccs = [0.5, 0.7, 0.8, 0.9];
            advisorAccs = utils.shuffle(advisorAccs);
            for (let b=0; b<blockCount; b++) {
                advisorSet = 0;
                if (this.blk4Structure.length > 0 && b == 0)
                {
                    blockStructure = this.blk4Structure[0%this.blockStructure.length];
                }
                else
                {
                    blockStructure = this.blockStructure[0%this.blockStructure.length];
                }
                advisorChoices = this.advisorLists[advisorSet];
                advisor0id = advisorChoices[0].adviceType % 2? advisorChoices[1].id : advisorChoices[0].id;
                if(advisorChoices.length > 1)
                    advisor1id = advisorChoices[0].adviceType % 2? advisorChoices[0].id : advisorChoices[1].id;
                else
                    advisor1id = advisor0id;
                // Shuffle advisors so they appear an equal number of times
                advisorForceDeck = utils.shuffleShoe(advisorChoices,
                    blockStructure[trialTypes.force]/advisorChoices.length);
                advisorChangeDeck = utils.shuffleShoe(advisorChoices,
                    blockStructure[trialTypes.change]/advisorChoices.length);
                advisorForceBlk4Deck = utils.shuffleShoe(advisorChoices,
                    blockStructure[trialTypes.forceblk4]);
                let blockLength = utils.sumList(blockStructure);
                let changeTrials = this.reliabilityAdvisor(0.7, blockLength);
                // Work out what type of trial to be
                let trialTypeDeck = [];
                for (let tt=0; tt<Object.keys(trialTypes).length; tt++) {
                    for (let i=0; i<blockStructure[tt]; i++)
                        trialTypeDeck.push(tt);
                }
                trialTypeDeck = utils.shuffle(trialTypeDeck);
                let currentAcc = advisorAccs.pop();
                let advAnsChoice = this.reliabilityAdvisor(currentAcc, 20,10);
                let advAnsForcedHuman = this.reliabilityAdvisor(currentAcc, 20,10);
                let advAnsForcedAlgor = this.reliabilityAdvisor(currentAcc, 20,10);
                let advAnsForcedHumanBlk4 = this.reliabilityAdvisor(0.7, 30,10);
                let advAnsForcedAlgorBlk4 = this.reliabilityAdvisor(0.7, 30,10);
                let wherelargers = this.createWhereLarger(blockLength);
                for (let i=1; i<=blockLength; i++) {
                    id++;
                    let isPractice = false;
                    let trialType = trialTypeDeck.pop();
                    let advisorId = 0;
                    let left;
                    let right;
                    let larger;
                    let algAns;
                    let algCor;
                    let algCon;
                    let trialSelect;
                    // // Below we pull the advisor, dots and confidence used for this trial again.
                    // // We merely do this for data recoridng purposes at the end of the experiment.
                    // if (trialType == 1)
                    // {
                    //     trialSelect = this.forcedTrials[forcedCount];
                    //     forcedCount++;
                    //     if (trialSelect !== undefined)
                    //     {
                    //         left = (forcedWhereDots.dots[trialSelect+(120*(this.dotDifference-1))])[0];
                    //         right = (forcedWhereDots.dots[trialSelect+(120*(this.dotDifference-1))])[1];
                    //         // Is the left or right larger?
                    //         if (utils.sumList(left,false,true) > utils.sumList(right,false,true))
                    //         {
                    //             larger = 0;
                    //         }
                    //         else
                    //         {
                    //             larger = 1;
                    //         }
                    //     }
                    //     else
                    //     {
                    //         larger = 0;
                    //     }
                    //     algAns = ((forcedData.advisors[(this.dotDifference)-1].AlgorithmAnswer[trialSelect])-1);
                    //     if (algAns == larger)
                    //     {
                    //         algCor = 1;
                    //     }
                    //     else
                    //     {
                    //         algCor = 0;
                    //     }
                    //     // algCon = ((forcedData.advisors[(this.dotDifference)-1].cj1[trialSelect-1])-1)
                    // }
                    // else if (trialType == 2 || trialType == 4)    
                    // {
                    //     trialSelect = this.choiceTrials[choiceCount];
                    //     choiceCount++;
                    //     if (trialSelect !== undefined)
                    //     {
                    //         left = (choiceWhereDots.dots[trialSelect+(240*(this.dotDifference-1))])[0];
                    //         right = (choiceWhereDots.dots[trialSelect+(240*(this.dotDifference-1))])[1];
                    //         if (utils.sumList(left,false,true) > utils.sumList(right,false,true))
                    //         {
                    //             larger = 0;
                    //         }
                    //         else
                    //         {
                    //             larger = 1;
                    //         }
                    //     }
                    //     else
                    //     {
                    //         larger = 0;
                    //     }
                    //     algAns = ((choiceData.advisors[(this.dotDifference)-1].AlgorithmAnswer[trialSelect])-1);
                    //     if (algAns == larger)
                    //     {
                    //         algCor = 1;
                    //     }
                    //     else
                    //     {
                    //         algCor = 0;
                    //     }
                    //     // algCon = ((choiceData.advisors[(this.dotDifference)-1].cj1[trialSelect-1])-1)
                    // }
                    // else if (trialType == 5)    
                    // {
                    //     trialSelect = this.blk4Trials[blk4Count];
                    //     blk4Count++;
                    //     left = (blk4WhereDots.dots[trialSelect+(60*(this.dotDifference-1))])[0];
                    //     right = (blk4WhereDots.dots[trialSelect+(60*(this.dotDifference-1))])[1];
                    //     if (utils.sumList(left,false,true) > utils.sumList(right,false,true))
                    //     {
                    //         larger = 0;
                    //     }
                    //     else
                    //     {
                    //         larger = 1;
                    //     }
                    //     algAns = ((blk4Data.advisors[(this.dotDifference)-1].AlgorithmAnswer[trialSelect])-1);
                    //     if (algAns == larger)
                    //     {
                    //         algCor = 1;
                    //     }
                    //     else
                    //     {
                    //         algCor = 0;
                    //     }
                    //     // algCon = ((blk4Data.advisors[(this.dotDifference)-1].cj1[trialSelect-1])-1)
                    // }
                    // else
                    // {
                    //     let leftDots;
                    //     let rightDots;
                    //     larger = Math.random();
                    //     if (larger < 0.5)
                    //     {
                    //         larger = 0;
                    //         leftDots = this.dotCount + this.dotDifference;
                    //         rightDots = this.dotCount - this.dotDifference;
                    //     }
                    //     else
                    //     {
                    //         larger = 1;
                    //         leftDots = this.dotCount - this.dotDifference;
                    //         rightDots = this.dotCount + this.dotDifference;
                    //     }
                    //     let leftArray = [];
                    //     let rightArray = [];
                    //     for (let y = 0;y<400;y++)
                    //     {
                    //         if (y<leftDots)
                    //         {
                    //             leftArray.push(1);
                    //         }
                    //         else
                    //         {
                    //             leftArray.push(0);
                    //         }
                    //         if (y<rightDots)
                    //         {
                    //             rightArray.push(1);
                    //         }
                    //         else
                    //         {
                    //             rightArray.push(0);
                    //         }
                    //     }
                    //     // Randomly shuffle the array of 1s and 0s
                    //     utils.shuffle(leftArray);
                    //     utils.shuffle(rightArray);
                    //     left = leftArray;
                    //     right = rightArray;
                    //     algAns = changeTrials[i];
                    //     if (algAns == larger)
                    //     {
                    //         algCor = 1;
                    //     }
                    //     else
                    //     {
                    //         algCor = 0;
                    //     }
                    //     algCon = null;
                    //     trialSelect = null;
                    // }

                    if (isPractice)
                    {
                        advisorId = trialType===trialTypes.catch? 0 : this.practiceAdvisor.id;
                    }
                    else if (trialType===trialTypes.forceblk4)
                    {
                        advisorId = advisorForceBlk4Deck.pop().id;
                    }
                    else
                    {
                        advisorId = trialType === trialTypes.force? advisorForceDeck.pop().id : 0;
                    }

                    if (trialType == 5) 
                    {
                        if (advisorId == 1) //human
                        {
                            algCor = advAnsForcedHumanBlk4.pop();
                        }
                        else if(advisorId == 2) //algor
                        {
                            algCor = advAnsForcedAlgorBlk4.pop();
                        }
                    }

                    else if (trialType == 1)
                    {
                        if (advisorId == 1) //human
                        {
                            algCor = advAnsForcedHuman.pop();
                        }
                        else if(advisorId == 2) //algor
                        {
                            algCor = advAnsForcedAlgor.pop();
                        }
                    } 

                    else if (trialType == 2)
                    {
                        algCor = advAnsChoice.pop();
                    }

                    larger = wherelargers[i-1];
                    if (algCor == 1)
                    {
                        algAns = larger;
                    }
                    else
                    {
                        if (larger == 0)
                        {
                            algAns = 1;
                        }
                        else if (larger == 1)
                        {
                            algAns = 0;
                        }
                    }
                    let defaultAdvisor = trialType === trialTypes.change? advisorChangeDeck.pop().id : null;
                    let changes = trialType === trialTypes.change? 0 : null;
                    let r = Math.random() < .5? 1 : 0;
                    let choice = trialType === trialTypes.choice? [advisorChoices[r].id, advisorChoices[1-r].id] : [];
                    // let choice = isPractice? [] : [advisorChoices[r].id, advisorChoices[1-r].id];
                    trials.push(new Trial(id, {
                        type: trialType,
                        typeName: trialTypeNames[trialType],
                        block: b+practiceBlockCount,
                        advisorSet,
                        defaultAdvisor,
                        advisorId,
                        advisor0id,
                        advisor1id,
                        choice,
                        changes,
                        answer: [NaN, NaN],
                        confidence: [NaN, NaN],
                        getCorrect: function(finalAnswer = true) {
                            let answer = finalAnswer? this.answer[1] : this.answer[0];
                            return answer === this.whichSide;
                        },
                        whichSide: larger,
                        practice: isPractice,
                        feedback: isPractice,
                        warnings: [],
                        stimulusDrawTime: [],
                        stimulusOffTime: [],
                        fixationDrawTime: [],
                        advisorAnswer: algAns,
                        advisorCorrect: algCor,
                        //advisorConfidence: algCon,
                        advisorConfidence: 0,
                        leftGrid: left,
                        rightGrid: right,
                        whereLarger: larger,
                        advisorAccuracy: currentAcc
                    }));
                    if (!isPractice)
                        realId++;
                }
            }
        }
        return trials;
    }

    /**
     * Apply the appropriate group class to the response container
     */
    setGroupVisual() {
        if(typeof this.groupId === 'undefined')
            return;
        document.querySelector('.jspsych-sliders-response-container').classList.add('group' + this.groupId);
    }

    /**
     * Show a ghost of the previous thumb placement to remind judges of their previous answer.
     * @param {string} [sliderID='#jspsych-canvas-sliders-response-slider0'] - slider to add the marker to
     */
    showMarker(sliderID = '#jspsych-canvas-sliders-response-slider0') {
        let slider = document.querySelector(sliderID);
        let marker = document.createElement('div');
        marker.className = 'advisorChoice-marker advisorChoice-prevAnswer';
        slider.parentElement.appendChild(marker);

        // let yOffset = -marker.clientHeight;
        // yOffset += 1; // compensate for box shadow on the slider making things look off
        // Massive HACK to compensate for Edge drawing sliders differently
        // if(window.navigator.userAgent.indexOf("Edge") > -1)
        //     yOffset -= 21;
        // marker.style.top = yOffset.toString() + 'px';

        let adjustedWidth = slider.clientWidth*0.75;
        let xOffset = this.currentTrial.answer[0] === 1? slider.clientWidth/2 : 0;
        let xDistance = this.currentTrial.answer[0] === 1?
            this.currentTrial.confidence[0] : 51 - this.currentTrial.confidence[0];
        xOffset -= marker.clientWidth/2;
        marker.style.left = Math.floor((xOffset + xDistance * (slider.clientWidth-marker.clientWidth)
            / 100)+(slider.clientWidth*(1/6))).toString() + 'px';

        // and call the slider-click function because we only get one on_load call
        this.setSliderClick();
    }

    /**
     * Show what the advisor thinks the answer is.
     * If confidence is switched on in index.html, the advisor will be a. red marker on the scale
     * to indicate left/right as well as confidence.
     * If confidence is switched off, the advice is simply a left or right word shown on screen.
     * @param {string} [sliderID='#jspsych-canvas-sliders-response-slider0'] - slider to add the marker to
     * @param {string} trial - the type of trial (usually forced or choice)
     * @param {integer} permArray - refers to which dot config to pull advice from.
     */
    showAdviceMarker(sliderID = '#jspsych-canvas-sliders-response-slider0', trial = "forced", permArray = 0) {
        let slider = document.querySelector('#jspsych-canvas-sliders-response-slider0');
        let marker = document.createElement('div');
        marker.className = 'advisorChoice-marker advisorChoice-middleBar';
        slider.parentElement.appendChild(marker);
        let xOffset = 0;
        let xDistance = 50;
        if (this.confidenceOn == true)
        {
            // ----- CJ CODE ------
            let slider = document.querySelector(sliderID);
            let marker = document.createElement('div');
            marker.className = 'advisorChoice-marker advisorChoice-advAnswer';
            slider.parentElement.appendChild(marker);

            let yOffset = -marker.clientHeight;
            yOffset += 1; // compensate for box shadow on the slider making things look off
                            //Massive HACK to compensate for Edge drawing sliders differently
            if(window.navigator.userAgent.indexOf("Edge") > -1)
                yOffset -= 21;
            marker.style.top = yOffset.toString() + 'px';

            if (trial == "forced")
            {
                let xOffset = ((forcedData.advisors[(this.dotDifference)-1].AlgorithmAnswer[permArray])-1) === 1? slider.clientWidth/2 : 0;
                let xDistance = ((forcedData.advisors[(this.dotDifference)-1].AlgorithmAnswer[permArray])-1) === 1?
                Math.abs(forcedData.advisors[(this.dotDifference)-1].cj1[permArray]) : 51 - Math.abs(forcedData.advisors[(this.dotDifference)-1].cj1[permArray-1]);
                xOffset -= marker.clientWidth/2;
                marker.style.left = (xOffset + xDistance * (slider.clientWidth-marker.clientWidth)
                / 100).toString() + 'px';
            }
            else if (trial == "choice")
            {
                let xOffset = ((choiceData.advisors[(this.dotDifference)-1].AlgorithmAnswer[permArray])-1) === 1? slider.clientWidth/2 : 0;
                let xDistance = ((choiceData.advisors[(this.dotDifference)-1].AlgorithmAnswer[permArray])-1) === 1?
                Math.abs(choiceData.advisors[(this.dotDifference)-1].cj1[permArray]) : 51 - Math.abs(choiceData.advisors[(this.dotDifference)-1].cj1[permArray]);
                xOffset -= marker.clientWidth/2;
                marker.style.left = (xOffset + xDistance * (slider.clientWidth-marker.clientWidth)
                / 100).toString() + 'px';
            }
            else if (trial == "forceblk4")
            {
                let xOffset = ((blk4Data.advisors[(this.dotDifference)-1].AlgorithmAnswer[permArray])-1) === 1? slider.clientWidth/2 : 0;
                let xDistance = ((blk4Data.advisors[(this.dotDifference)-1].AlgorithmAnswer[permArray])-1) === 1?
                Math.abs(blk4Data.advisors[(this.dotDifference)-1].cj1[permArray]) : 51 - Math.abs(blk4Data.advisors[(this.dotDifference)-1].cj1[permArray]);
                xOffset -= marker.clientWidth/2;
                marker.style.left = (xOffset + xDistance * (slider.clientWidth-marker.clientWidth)
                / 100).toString() + 'px';
            }
            else
            {
                let xOffset = this.currentTrial.answer[0] === 1? slider.clientWidth/2 : 0;
                let xDistance = this.currentTrial.answer[0] === 1?
                this.currentTrial.confidence[0] : 51 - this.currentTrial.confidence[0];
                xOffset -= marker.clientWidth/2;
                marker.style.left = (xOffset + xDistance * (slider.clientWidth-marker.clientWidth)
                / 100).toString() + 'px';
            }
        }

        else
        {
            let idSuffix =  document.querySelector('#jspsych-jas-present-advice-wrapper0') !== null? '1' : '0';
            let element = document.getElementById("jspsych-content");
            let arrowDiv = element.appendChild(document.createElement('div'));
            arrowDiv.id = 'jspsych-jas-present-advice-arrow' + idSuffix;
            arrowDiv.classList.add('jspsych-jas-present-advice-arrow');
            //console.log(this.currentTrial);
            if (this.currentTrial.advisorAnswer == 0)
            {
                arrowDiv.classList.add('jspsych-jas-present-advice-arrow-' + 'left');
                arrowDiv.innerText = "LEFT";
            }
            else
            {
                arrowDiv.classList.add('jspsych-jas-present-advice-arrow-' + 'right');
                arrowDiv.innerText = "RIGHT";
            }
            let picDiv = document.getElementById('jspsych-jas-present-advice-image0')
            picDiv.style = "transform: translate(0%, 200%)";

            // else
            // {
            //     // Falback: just give some random advice.
            //     let xOffset = ((forcedData.advisors[(this.dotDifference)-1].AlgorithmAnswer[permArray])-1) === 1? slider.clientWidth/2 : 0;
            //     let xDistance = ((forcedData.advisors[(this.dotDifference)-1].AlgorithmAnswer[permArray])-1) === 1?
            //     Math.abs(forcedData.advisors[(this.dotDifference)-1].cj1[permArray]) : 51 - Math.abs(forcedData.advisors[(this.dotDifference)-1].cj1[permArray]);
            //     xOffset -= marker.clientWidth/2;
            //     marker.style.left = (xOffset + xDistance * (slider.clientWidth-marker.clientWidth)
            //     / 100).toString() + 'px';
            // }
        }

        // and call the slider-click function because we only get one on_load call
        this.setSliderClick();
    }

    /**
     * Show advice over the stimulus presentation area
     */
    showAdvice(){
        // Hack an advisor display in here with a directional indicator
        let div = document.querySelector('canvas').parentElement;
        div.innerHTML = "";
        if(this.currentTrial.type === trialTypes.dual) {
            for(let i = 0; i < 2; i++)
                this.drawAdvice(div, this.currentTrial['advisor' + i.toString() + 'id']);
        } else if(typeof this.currentAdvisor !== 'undefined') {
            this.drawAdvice(div, this.currentAdvisor.id);
        }

        this.showMarker();
        if(this.currentTrial.type == 1)
        {
            this.showAdviceMarker('#jspsych-canvas-sliders-response-slider0', "forced", this.currentTrial.trialSelect);
        }
        else if (this.currentTrial.type == 2)
        {
            this.showAdviceMarker('#jspsych-canvas-sliders-response-slider0', "choice", this.currentTrial.trialSelect);
        }
        else if (this.currentTrial.type == 4)
        {
            this.showAdviceMarker('#jspsych-canvas-sliders-response-slider0', "change", this.currentTrial.trialSelect);
        }
        else if (this.currentTrial.type == 5)
        {
            this.showAdviceMarker('#jspsych-canvas-sliders-response-slider0', "forceblk4", this.currentTrial.trialSelect);
        }
    }

    /**
     * Draw the advisor portrait, advice, and advice text
     * @param div {HTMLElement} div in which to draw
     * @param advisorId {int} ID of the advisor to draw
     * @param textAboveImage {boolean} whether to draw the advice text above the image
     */
    drawAdvice(div, advisorId, textAboveImage = false, trialType = "forced", permArray = 0) {

        this.setAdvisorAgreement(advisorId);

        let a = this.getAdvisorById(advisorId);
        if(!(a.lastAdvice instanceof Line)) {
            let warning = 'No advice where advice expected (advisorID=' + advisorId +')';
            this.currentTrial.warnings.push(warning);
            console.warn(warning);
        }

        a.drawAdvice(div, textAboveImage);

    }

    /**
     * Advisor choice function called by the jspsych-jas-present-advice-choice plugin.
     * Offer a choice of advisors by drawing clickable portraits.
     *
     * @param {HTMLElement} display_element - element within which to display the choices
     * @param {function} callback - function to call when a portrait is clicked. Called with the choice as an argument.
     */
    getAdvisorChoice(display_element, callback) {
        let choices = this.currentTrial.choice;
        if(this.currentTrial.type === trialTypes.change) {
            this.offerAdvisorChange(display_element, callback);
            return;
        }
        if (this.currentTrial.type !== trialTypes.choice) {
            if(this.currentTrial.type === trialTypes.dual) {
                callback(null);
                return;
            }
            if (typeof this.currentAdvisor === 'undefined') {
                callback(-1); // catch trials
                return;
            }
            this.findAdvisorFromContingency();
                callback(this.currentAdvisor.id); // force trials
                return;
        }
        // present choices
        let choiceImgs = [];
        let self = this;
        for (let a=0; a<choices.length; a++) {
            let advisor = this.getAdvisorById(choices[a]);
            let advisorDiv = advisor.draw(display_element);
            advisorDiv.classList.add('advisorChoice-choice');
            let img = advisorDiv.querySelector('p');
            img.classList.add('advisorChoice-choice', 'advisor-portrait');
            img.id = 'advisorChoice-choice-' + a.toString();
            advisorDiv.addEventListener('click', function () {
                self.currentTrial.advisorId = choices[a];
                callback(choices[a]);
            });
            choiceImgs.push(img);
            if(a === 0) {
                let p = display_element.appendChild(document.createElement('p'));
                p.innerText = 'Click on an advisor to receive their advice.';
                p.className = 'advisorChoice-choice';
            }
        }
    }

    /**
     * Advisor change function.
     * Display a default portrait whose advice can be switched for an alternative by pressing spacebar.
     *
     * @param {HTMLElement} display_element - element within which to display the default option
     * @param {function} callback - function to call when a decision is made. Called with the choice and the change times arguments.
     */
    offerAdvisorChange(display_element, callback) {
        // draw portraits in the positions they'll be when advice is presented
        let advisorIDs = [];
        let div = display_element.appendChild(document.createElement('div'));
        div.id = 'jspsych-jas-change-advisors-wrapper';
        for(let i = 0; i < 2; i++) 
        {
            let advisor = this.getAdvisorById(this.currentTrial['advisor' + i.toString() + 'id']);
            advisorIDs.push(advisor.id);
            let idSuffix =  document.querySelector('#jspsych-jas-present-advice-wrapper0') !== null? '1' : '0';
            let advisorDiv = advisor.draw(div, {nth: idSuffix});
            if(this.currentTrial.defaultAdvisor === advisor.id) {
                advisorDiv.classList.add('advisorChoice-change-selected');
                // Fill in trial advisor values for the default advisor
                let a = 'advisor' + i.toString();
                this.currentTrial.advisorId = this.currentTrial[a + 'id'];
                this.currentTrial.advice = this.currentTrial[a + 'advice'];
                this.currentTrial.advisorAgrees = this.currentTrial[a + 'agrees'];
            }
            else
                advisorDiv.classList.add('advisorChoice-change-unselected');
            advisorDiv.changeTimes = [];
            advisorDiv.nth = i;
        }
        // Add the timer and instructions
        let instructionsDiv = display_element.appendChild(document.createElement('div'));
        instructionsDiv.id = 'advisorChoice-change-instructions';
        instructionsDiv.classList.add('advisorChoice-change-instructions');
        instructionsDiv.innerHTML = 'Press Spacebar to change advisor';
        instructionsDiv.updateDelay = 20;
        instructionsDiv.duration = this.changeTime;
        instructionsDiv.updateTick = 0;
        let progressAnimation = function(div) {
            let percent = div.updateDelay / div.duration * 100 * ++div.updateTick;
            div.style.backgroundImage = "linear-gradient(to right, var(--advisorChoice-change-instructions-fill) " +
                (percent - 5).toString() + "%, " +
                "var(--advisorChoice-change-instructions-background) " +
                (percent + 5).toString() + "%)";
            setTimeout(function(){progressAnimation(div)}, div.updateDelay);
        };
        //progressAnimation(instructionsDiv);

        // Add the keyboard response to the divs
        div.tabIndex = 1;
        div.classList.add('advisorChoice-change-noOutline');
        div.focus();
        div.frozenUntil = -Infinity;
        div.governor = this;
        div.addEventListener('keydown', (event)=>{
            if(performance.now() < this.frozenUntil)
                return;
            if(event.keyCode === 32) { // spacebar
                this.frozenUntil = performance.now() + 50;
                let advisorDivs = document.querySelectorAll('.jspsych-jas-present-advice-wrapper');
                for(let i = 0; i < advisorDivs.length; i++) {
                    let d = advisorDivs[i];
                    if(d.classList.contains('advisorChoice-change-selected')) {
                        d.classList.remove('advisorChoice-change-selected');
                        d.classList.add('advisorChoice-change-unselected');
                    } else {
                        d.classList.remove('advisorChoice-change-unselected');
                        d.classList.add('advisorChoice-change-selected');
                        let a = 'advisor' + d.nth.toString();
                        div.governor.currentTrial.advisorId = div.governor.currentTrial[a + 'id'];
                        div.governor.currentTrial.advice = div.governor.currentTrial[a + 'advice'];
                        div.governor.currentTrial.advisorAgrees = div.governor.currentTrial[a + 'agrees'];
                        div.governor.currentTrial.changes++;
                    }
                    d.changeTimes.push(performance.now());
                }
            }
        });

        setTimeout(function() {
            let selectedDiv = document.querySelectorAll('.advisorChoice-change-selected')[0];
            callback(selectedDiv.advisorId, selectedDiv.changeTimes);
        }, this.changeTime)
    }

    /**
     * Process the judge's initial response
     * @param {Object} trial - jsPsych plugin response
     * @param {Object} [args={}] - assorted arguments to customize behaviour
     * @param {boolean} [args.advisorAlwaysCorrect - whether to override advisor's behaviour and force them to advice the correct response
     */
    initialResponse(trial, args={}) {
        this.storePluginData(trial);
        this.currentTrial.stimulusOffTime.push(trial.stimulusOffTime);
        // trial is the complete trial object with its trial.response object
        this.currentTrial.answer[0] = AdvisorChoice.getAnswerFromResponse(trial.response);
        this.currentTrial.confidence[0]  = AdvisorChoice.getConfidenceFromResponse(trial.response, this.currentTrial.answer[0]);
        let errorStyle = "style='color: red;position: absolute;left: 50%;transform: translate(-50%, -50%);top: 52%;font-size:xxx-large'";
        let dotError = document.querySelector('div.jspsych-content-wrapper').appendChild(document.createElement('div'));
        dotError.innerHTML = "<div " + errorStyle + ">Incorrect!</div>";
        dotError.id = "dotError";
        dotError.classList.add('hidden');
        if(this.currentTrial.type === trialTypes.catch) {
            let answer = this.currentTrial.answer[0];
            if (answer !== this.currentTrial.whichSide && this.beepOn == true)
            {   
                var audio = new Audio(this.audioSrc);
                audio.preload = 'auto';
                audio.play();
            }
            let correct;
            let conf = (this.currentTrial.confidence[0]+50)/100;
            if (answer !== this.currentTrial.whichSide)
            {
                correct = 0;
                dotError.classList.remove('hidden');
                setTimeout(function () {
                    dotError.classList.add('hidden');
                }, 700);
            }
            else
            {
                correct = 1;
            }
            points = Math.round(points + (100*(1 - Math.pow((correct - conf),2))));
            //this.pointsTotal(points);
            this.currentTrial.points = points;
            //console.log(this.currentTrial.points);
            this.closeTrial(trial);
            this.drawProgressBar();
            return;
        }

        if(args.advisorAlwaysCorrect === true)
            this.advisorAlwaysCorrect = true;
        else
            this.advisorAlwaysCorrect = false;
    }

    setAdvisorAgreement(advisorId) {
        let advisor = this.getAdvisorById(advisorId);

        // Check if advisor already made up their mind on this trial
        if(advisor.lastAdvisedTrial === this.currentTrialIndex)
            return;
        else
            advisor.lastAdvisedTrial = this.currentTrialIndex;

        let self = this;
        let agree = false;
        if(gov.alwaysCorrect === true)
            agree = this.currentTrial.getCorrect(false);
        else
            agree = advisor.agrees(this.currentTrial.getCorrect(false), this.getLastConfidenceCategory());
        // Check the answer and dis/agree as appropriate
        if (agree) {
            advisor.lastAdvice = advisor.voice.getLineByFunction(function (line) {
                return line.side === self.currentTrial.answer[0];
            });
        } else {
            advisor.lastAdvice = advisor.voice.getLineByFunction(function (line) {
                let side = [1, 0][self.currentTrial.answer[0]];
                return line.side === side;
            });
        }
        if(this.currentTrial.type !== trialTypes.dual) {
            this.currentTrial.advisorAgrees = agree;
            this.currentTrial.advice = advisor.lastAdvice;
        }

        // Update the advisor's individual decision in the trial.
        // Advisors are identified by whether their adviceType is even.
        // This is done because R doesn't really want to be sorting through object representations,
        // so instead we provide a predictable column name.
        let index = '';
        if(this.currentTrial.advisor0id === advisor.id)
            index = 'advisor0';
        else if(this.currentTrial.advisor1id === advisor.id)
            index = 'advisor1';
        else
            return;

        this.currentTrial[index + 'agrees'] = agree;
        this.currentTrial[index + 'advice'] = advisor.lastAdvice;
    }

    /**
     * determine the advisor this trial should have on the basis of the confidence of the trial
     */
    findAdvisorFromContingency() {
        // Only apply where advisor ID is specified already (i.e. force trials)
        if (this.currentTrial.advisorId === 0 || this.currentTrial.practice)
            return;
        // Determine confidence
        let cc = this.getConfidenceCategory(this.currentTrial.id);
        // Redraw advisor list if the answer was not valid
        let a = this.contingentAdvisors[cc].pop();
        if (typeof a === 'undefined' || this.advisorLists[this.currentTrial.advisorSet].indexOf(a) === -1) {
            this.redrawContingency(cc);
            this.findAdvisorFromContingency();
            return;
        }
        // Store advisor
        this.currentTrial.advisorId = a.id;
    }

    /**
     * produce a shuffled list of advisors to be used for the specified confidence category
     */
    redrawContingency(confidenceCategory) {
        let advisors = utils.shuffle(this.advisorLists[this.currentTrial.advisorSet]);
        let blockLength = utils.getMatches(this.trials, (trial)=>{
            return trial.block === this.currentTrial.block;
        }).length;
        let tmp = [];
        for(let i = 0; i < blockLength; i++)
            tmp.push(advisors[i%advisors.length]); // advisors are listed in order, repeated
        this.contingentAdvisors[confidenceCategory] = tmp;
    }

    /**
     * Process the judge's final response
     * @param {Object} trial - jsPsych plugin response
     */
    finalResponse(trial) {
        this.currentTrial.stimulusOffTime.push(trial.stimulusOffTime); // always undefined - no stimulus!
        this.storePluginData(trial);
        this.currentTrial.answer[1] = AdvisorChoice.getAnswerFromResponse(trial.response);
        let errorStyle = "style='color: red;position: absolute;left: 50%;transform: translate(-50%, -50%);top: 52%;font-size:xxx-large;'";
        let dotError = document.querySelector('div.jspsych-content-wrapper').appendChild(document.createElement('div'));
        dotError.innerHTML = "<div " + errorStyle + ">Incorrect!</div>";
        dotError.classList.add('hidden');
        // empty responses are allowed 2nd time through (copy intial response)
        if (isNaN(this.currentTrial.answer[1])) {
            this.currentTrial.answer[1] = this.currentTrial.answer[0];
            this.currentTrial.confidence[1] = this.currentTrial.confidence[0];
        } else {
            this.currentTrial.confidence[1] = AdvisorChoice.getConfidenceFromResponse(trial.response, this.currentTrial.answer[1]);
        }
        let answer;
        answer = this.currentTrial.answer[1];
        if (this.beepOn == true)
        {
            if (answer === null || isNaN(answer))
            {
                answer = this.currentTrial.answer[0];
            }
            if (answer !== this.currentTrial.whichSide && this.beepOn == true)
            {   
                var audio = new Audio(this.audioSrc);
                audio.preload = 'auto';
                audio.play();
            }

        }
        let correct;
        let conf = (this.currentTrial.confidence[1]+50)/100;
        if (answer !== this.currentTrial.whichSide)
        {
            dotError.classList.remove('hidden');
            correct = 0;
            setTimeout(function () {
                    dotError.classList.add('hidden');
            }, 700);
        }
        else
        {
            correct = 1;
        }
        points = Math.round(points + (100*(1 - Math.pow((correct - conf),2))));
        this.currentTrial.points = points;
        //console.log(this.currentTrial.points);
        if ((this.staircase === true) || (this.currentTrial.type === 0))
        {
            this.closeTrial(trial);
        }
        else
        {
            this.currentTrialIndex++;
            this.drawProgressBar();
        }
    }

    /**
     * Draws the questionnaire portrait. Called by the jsPsych plugin
     *
     * @param {HTMLElement} display_element - element within which to draw the portrait
     * @param {Function} callback - function to execute when drawing is complete. Called with the portrait src
     */
    drawQuestionnaire(display_element, callback) {
        display_element.classList.add('advisorChoice-questionnaire');
        let advisor = this.questionnaireStack.pop();
        this.lastQuestionnaireAdvisorId = advisor.id;
        advisor.draw(display_element);
        callback(advisor.portrait.src);
    }

    /**
     * Move questionnaire prompts to within the slider box
     */
    static hackQuestionnaire() {
        // Remove the prompts and place them within the bars
        let bars = document.querySelectorAll('input.jspsych-function-sliders-response');
        for(let i = 0; i < bars.length; i++) {
            // Remove middle in-slider prompt
            bars[i].nextSibling.nextSibling.remove();
            let prompts = bars[i].parentElement.parentElement.nextSibling.childNodes;
            for(let isRight = 0; isRight < 2; isRight++) {
                let text = prompts[isRight].childNodes[0].innerText;
                // Insert text into the slider
                bars[i].parentElement.querySelectorAll('.advisorChoice-slider-label')[isRight].innerHTML =
                    text;
            }
            // Remove the prompt row
            bars[i].parentElement.parentElement.nextSibling.remove();
        }
    }

    /**
     * Save the response to the questionnaire.
     */
    questionnaireResponse(trial) {
        if (Object.keys(this).indexOf('questionnaires') === -1)
            this.questionnaires = [];
        trial.afterTrial = this.currentTrialIndex-1;
        trial.advisorId = this.lastQuestionnaireAdvisorId;
        this.questionnaires.push(trial);
    }

    /**
     * Save the response to the generalised trust questionnaire
     */
    genTrustQuestionnaireResponse(trial) {
        this.generalisedTrustQuestionnaire = trial;
    }

    /**
     * submit the debrief form and finish the experiment
     *
     */
    debriefFormSubmit(form) {
        let txt = form.querySelector('#dotsAnswer');
        // if (txt.value.length===0) {
        //     txt.style.border = '1px solid red';
        //     return;
        // }
        this.debrief = [
            {
                question: 'dots',
                answer: txt.value
            },
            {
                question: 'catchall',
                answer: form.querySelector('#catchallAnswer').value
            }
        ];
        if (this.totalReward > 0)
        {
            this.endExperiment(true,false);
            this.showTotalReward(this.redirect);
        }
        else
        {
            this.endExperiment();
            document.querySelector('body').innerHTML = "";
            window.location.replace(this.redirect);
        }
    }

    feedback(data, includePayment = false) {
        if(typeof data === 'undefined')
            data = this;
        google.charts.load('current', {'packages':['corechart']});
        google.charts.setOnLoadCallback(function(){advisorChoice.showFeedback(data, includePayment)});
    }
}


class HaloEffect extends AdvisorChoice {
    /**
     * @constructor
     *
     * @param {Object} [args={}] - properties to assign to the Governor
     * @param {Trial[]} [args.trials=[]] - trial list
     * @param {Object[]} [args.miscTrials] - miscellaneous trials (breaks, instructions, etc)
     * @param {int} [args.currentTrialIndex=0] - index of current trial in trial list
     * @param {string} [args.completionURL=''] - URL to which to refer participants for payment
     * @param {string} [args.experimentCode=''] - code identifying the experiment
     *
     * @param {int} [args.dotCount] - number of dots in a box
     * @param {int} [args.dotDifference] - half the difference between the dot counts in the two boxes; the difficulty
     * @param {int} [args.difficultyStep] - amount the difficulty increases/decreases after success/failure
     * @param {number} [args.minimumBlockScore] - lowest proportion of successful trials allowed on a block
     * @param {int} [args.blockCount] - number of blocks in the study
     * @param {Object|Object[]} [args.blockStructure] - the structure of each block, where each object is a series of [trialType: number of instances] mappings. Multiple objects represent different subblocks run consecutively.
     * @param {Object|Object[]} [args.practiceBlockStructure] - the structure of each practice block
     * @param {int} [args.preTrialInterval] - delay before each trial begins
     * @param {int} [args.preStimulusInterval] - fixation delay before the stimulus is displayed
     * @param {int} [args.stimulusDuration] - duration the dot stimulus is displayed
     * @param {int} [args.feedbackDuration] - duration of the feedback screen
     *
     * @param {Advisor[]} [args.advisors=[]] - list of advisors
     * @param {Advisor} [args.practiceAdvisor] - practice advisor
     * @param {[Advisor[]]} [args.advisorLists] - list of lists of advisors, each one being a set of advisors competing with one another in a block
     * @param {[Advisor[]]} [args.contingentAdvisors] - list of advisors to be used contingent on the confidence category of a response matching the list index
     * @param {[Advisor[]]} [args.questionnaireStack] - stack of advisors about whom questionnaires are to be asked
     *
     * @property {Advisor} currentAdvisor - advisor currently in focus
     * @property {Trial} currentTrial - trial currently underway
     */
    constructor(args = {}) {
        super(args);

    }

    /**
     * @return {string} - Advice string for the current trial
     */
    get adviceString() {
        let str = this.currentTrial.advice.string;
        if(!(this.currentTrial instanceof gkTask))
            return str;
        return str.replace('on the LEFT', this.currentTrial.answerOptions[0])
            .replace('on the RIGHT', this.currentTrial.answerOptions[1]);
    }

    /**
     * Display the question and the answer options for the current question
     */
    generalKnowledge(display_element, callback) {
        console.log('gkBegin')
        let qDiv = display_element.appendChild(document.createElement('div'));
        qDiv.id = 'gkQuestion';
        qDiv.classList.add('gk-question');
        let p = qDiv.appendChild(document.createElement('p'));
        p.id = 'gkQuestionText';
        p.classList.add('gk-question');
        p.innerText = this.currentTrial.question;

        callback(this.currentTrial.question);
    }

    /**
     * Process the initial response to a general knowledge question
     */
    generalKnowledgeResponse(trial) {

        // Generic behaviour
        this.initialResponse(trial);
    }

    /**
     * Process the final response to a general knowledge question
     */
    generalKnowledgeFinalResponse(trial) {

        // Generic behaviour
        this.finalResponse(trial);
    }

    /**
     * Replace the slider values with answer alternatives
     */
    generalKnowledgeOnLoad(finalDecision = false) {
        for(let i=0; i<2; i++)
            document.querySelector('#jspsych-function-sliders-response-labelS0L'+i.toString()).innerHTML =
                this.currentTrial.answerOptions[i];

        // Normal load functions
        if(!finalDecision)
            this.setSliderClick(true);
        else
            this.showAdvice();
    }

    /**
     * Override the showAdvice function to avoid relying on the canvas plugin
     */
    showAdvice(){
        if(!(this.currentTrial instanceof gkTask)) {
            super.showAdvice();
            return;
        }
        // Hack an advisor display in here with a directional indicator
        let parent = document.querySelector('#jspsych-content');
        let child = document.querySelector('#jspsych-function-sliders-response-stimulus');
        let div = parent.insertBefore(document.createElement('div'), child);
        div.innerHTML = "";
        let picDiv = div.appendChild(document.createElement('div'));
        picDiv.id = 'jspsych-jas-present-advice-choice-image0';
        picDiv.classList.add('jspsych-jas-present-advice-choice-image');
        let textDiv = div.appendChild(document.createElement('div'));
        textDiv.id = 'jspsych-jas-present-advice-choice-prompt0';
        textDiv.classList.add('jspsych-jas-present-advice-choice-prompt');
        let a = this.currentAdvisor;
        picDiv.innerHTML = a.portrait.outerHTML;
        textDiv.innerHTML = this.currentAdvisor.nameHTML + ': ' + this.adviceString;
        // Set the class of the slider the advisor endorsed
        let labelID = this.currentTrial.advice.side === 0? 0 : 1;
        let sliderLabel = document.querySelector('#jspsych-function-sliders-response-labelS0L' +
            labelID);
        sliderLabel.classList.add('advisor-endorsed');
        this.showMarker('#jspsych-function-sliders-response-slider0');
    }
}

/**
 * @class for running general knowledge trials
 * These trials contain a statement to which the answer is true/false rather than a dot discrimination
 */
class gkTask extends Trial {
    /**
     * @constructor
     */
    constructor(id, args = {}) {
        super(id, args);
        this.question = typeof args.question == "undefined"? null : args.question;
        this.answerOptions = typeof args.answerOptions == "undefined"? null : args.answerOptions;
    }
}

export {trialTypes, trialTypeNames, DotTask, AdvisorChoice, HaloEffect, gkTask};
