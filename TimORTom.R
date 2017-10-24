#In the example project we want to find out whether tims are hotter than toms. To put it more scientifically: Does vowelquality in the accented syllable of a first name influence how attractive we find a person. 

#Commands are in black and blue. to run them, mark them and press 'run' (top right of this window )

#read in data. Set the directory to where you saved the data. 
setwd("~/Documents/Projects/Tims versus Toms") # This is the working directory. Make sure the .csv file with your data is saved here. 
timdata <- read.csv("~/Documents/Projects/Tims versus Toms/Data_Tim_Tom_new 2.csv", header=TRUE) # Now we are reading in the data file. R works best with .csv files

# Now we are calling the libraries we need to analyse the data. For LMEMs we need lme4 and nlme
# When you get an error message when running these commands, this typically means that you forgot to install obe of these packages. With the command install.packages("name of missing package") this can be solved. Don't forget to run the call library command again after successful installation. 
library(lme4)
library(car)
library(ggplot2)
library(nlme)
library(reshape)
library(doBy)
library(lattice)
library(pbkrtest)
library(ggm)

#define factors
#Make sure the variable name in brackets matches exactly with the name of the corresponding column in the datafile. 
#Variables, which have a natural point zero and are continuos (e.g. age, reaction times) are coded as 'as.numeric'. All binary or likert scale type variables are coded as factors ('as.factor'). 
timdata$SubjectID=as.factor(timdata$SubjectID)
timdata$Gender=as.factor(timdata$Gender)
timdata$Age=as.numeric(timdata$Age)
timdata$Trial=as.numeric(timdata$Trial)
timdata$Name=as.factor(timdata$Name)
timdata$Bild=as.factor(timdata$Bild)
timdata$PicSex=as.factor(timdata$PicSex)
timdata$Vowel=as.factor(timdata$Vowel)

#define measures. Same as above but I keep them separate for tidyness. 
timdata$ResponseKey=as.numeric(timdata$ResponseKey, center=T, scale=F)

#models
#Here we are starting with our regressions. A typical basic LMM regression has the following form: 
# Name_of_the_Model = lmer (Dependent Variable ~ Predictor1 + Predictor2 + etc)
#lmer (=linear mixed effects regression)
#Predictors are independent or dependent variables which we expect to influence the dependent variable in question. 
# With a '+' symbol, we add them as independent factors to the model. With a, ' *' symbol instead of the '+' between factors we allow them to interact with other.
# Example Name_of_the_Model= lmer (Dependent Variable ~ Predictor1*Predictor2 + etc)
# It is not advisable to have too many factors interact, especially if not hypothesis driven.
#The variables we now called predictors are the 'Fixed effects'(the factors whose variance we are interested in)
#Now we add the random effects (we want to clean our effects from random variance). These are typically things like Subject, Item, Time of the day, Wheather on the test day...
# Random effects can be coded as (1|SubjectID). This means we let them vary on a random intercept. This coding works best for variables from which we can be sure that the variance related to them is completely random and uncorrelated with factors of interest. 
# An alternative way to code them is with random slopes for certain variables as '(VariableName|SubjectID)'. In case of the timtom data I did this for sensitity to vowelquality bc I predicted that subjects differ in their sensitivity to soundsymbolism. 
# incase you are not sure which way works best, just try both models and compare the fit (see below)
#At the end of the regression function, we refer to the dataset (data=timdata) and tell the model whether to use restricted estimates for maximum likelyhood. 
#Normally, this should be 'TRUE' unless you want to directly compare the performance of different models. Then you do not want any restriction and enter REML=FALSE. The analysis will take substantially longer with this setting. 
#Here is my example model for the timtom data (mark and press 'run'):
Attractiveness = lmer (ResponseKey ~ Vowel*Gender + PicSex + Age  + (Vowel|SubjectID) + (Vowel|Name) + (Vowel|Bild) + (1|Trial) , data=timdata, REML=TRUE)

#To call the statistic output:
summary (Attractiveness)

#Alternative models could be 
Attractiveness2 = lmer (ResponseKey ~ Vowel*PicSex + Gender + Age  + (1|SubjectID) + (1|Name) + (1|Bild) + (1|Trial) , data=timdata, REML=TRUE)
summary (Attractiveness2)

Attractiveness3 = lmer (ResponseKey ~ Vowel*Gender + PicSex + Age  + (1|SubjectID) + (Vowel|Name) + (1|Bild) + (1|Trial) , data=timdata, REML=TRUE)
summary (Attractiveness3)

#In order to check which model fits the data better, we can run a model comparison:
anova (Attractiveness3, Attractiveness2)

#p-values can be estimated by model comparison. This is very inaccurate and unreliable, but is common practice to make the results more relatable to typical inferential statistic outputs.
# If the dependent variable is not a natural scale (like ratings) we do a Chi Square test: 
PValues_Attractiveness2 <- Anova(Attractiveness2, type = 3, test = 'Chi')
PValues_Attractiveness2

#If the dependent variable is a natural scale (like RTs) we do an F Square test:
PValues_Attractiveness <- Anova(Attractiveness, type = 3, test = 'F')
PValues_Attractiveness


#Descriptives
install.packages("Hmisc")
library(Hmisc)
describe(timdata) 
# n, nmiss, unique, mean, 5,10,25,50,75,90,95th percentiles 
# 5 lowest and 5 highest scores

# Plots

Attractiveness<-ggplot(timdata, aes(x=PicSex, y=ResponseKey, colour=Vowel)) 
Attractiveness + geom_boxplot() + facet_grid(. ~ Vowel)

ggsave(file= "Attractiveness.png", width=9,height=7, dpi=1000)

**************************************************************************************************************
###Output (This is the output. Please don't run as code)
# Summary of the model
Linear mixed model fit by REML ['lmerMod']
Formula: ResponseKey ~ Vowel * Gender + PicSex + Age + (Vowel | SubjectID) +  
  (Vowel | Name) + (Vowel | Bild) + (1 | Trial)
Data: timdata
#Criterion of convergence
REML criterion at convergence: 13824.2

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-3.1301 -0.6619 -0.0092  0.6340  4.5725 

Random effects:
  Groups    Name        Variance Std.Dev. Corr 
Name      (Intercept) 0.015607 0.12493       
Vowelfront  0.015607 0.12493  -1.00
Trial     (Intercept) 0.005727 0.07568       
Bild      (Intercept) 1.163493 1.07865       
Vowelfront  0.063238 0.25147  0.16 
SubjectID (Intercept) 0.581859 0.76280       
Vowelfront  0.002604 0.05102  -1.00
Residual              2.159476 1.46952       
Number of obs: 3710, groups:  Name, 94; Trial, 70; Bild, 70; SubjectID, 53

#Results of the Fixed Effects
# There is a list in the order of which the factors were taken into the model (LMMs don't make an automatic ranking for you). Each factor is one row. 
#Interactions are always listed last
#Intercept is the sum of variance in the whole model without adding factors. All effects are to be interpreted in relation to the Intercept.
#First column are the variables
#Second column are the Beta values (estimated coefficient). This is the size of the associated effect. In the tim tom model, front vowels have a negative Effekt of -0.14 on the sum variance (Intercept) of 6.81. 
# Female face have a much larger effect in comparison (0.99). 
#Third column are the Standard Errors of the mean of the variable. 
#Last column is the t-value. Rule of thumb: t-values of 1.8 or larger are interesting. 
Fixed effects:
  Estimate Std. Error t value
(Intercept)         6.81676    1.06463   6.403
Vowelfront         -0.14045    0.07754  -1.811
Genderm            -0.40013    0.22598  -1.771
PicSexw             0.98873    0.26777   3.693
Age                -0.09367    0.04720  -1.985
Vowelfront:Genderm  0.29141    0.09860   2.955

#This opart of thje output is only interesting if there is unexpected high correlation
Correlation of Fixed Effects:
  (Intr) Vwlfrn Gendrm PicSxw Age   
Vowelfront  -0.042                            
Genderm      0.112  0.217                     
PicSexw     -0.126 -0.001  0.000              
Age         -0.974  0.000 -0.217  0.000       
Vwlfrnt:Gnd  0.036 -0.624 -0.346  0.000  0.000

#This is the summaryy of the model with which we estimated p-values
> PValues_Attractiveness <- Anova(Attractiveness, type = 3, test = 'Chi')
> PValues_Attractiveness
Analysis of Deviance Table (Type III Wald chisquare tests)

#and its output
#First column is DV, interactions are listed last 
#Second column is the Chi Square value
#Third colums is the degrees of freedom
#last column is the estimated p-value. The star indicates in addition how significant the effect is according to standard stats practice. 
Response: ResponseKey
Chisq Df Pr(>Chisq)    
(Intercept)  40.9973  1  1.524e-10 ***
  Vowel         3.2814  1   0.070071 .  
Gender        3.1352  1   0.076618 .  
PicSex       13.6348  1   0.000222 ***
  Age           3.9387  1   0.047188 *  
  Vowel:Gender  8.7343  1   0.003123 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1