
################################
## Mediation Package Vignette ##
################################

library(mediation)

vignette("mediation")

#the plan is to cover this to the point so that anything that wasn't
#covered before, is well addressed

#The core structure of mediation is meant to distinguish
#between model-based and design-based inference

#model based inference is the standard practice...
#with treatment randomized, and mediating and outcome variables
#are observed without any intervention by researchers...

#Imai 1010 shows that a range of parametric and semi-parametric
#models can be used to estimate the average causal mediation effect and other
#quantities of interest. This requires the sequential ignorability assumption
#for point identification.

#In contrast, design-based inference mainly uses the features of the
#experimental design itself, and does NOT require the sequential ignorability
#assumption. These formal properties can be found in Imai 2013

#the potential outcomes framework can define these quantities.
# Mi(t) is potential value of a mediator of interest for unit i under the
#treatment status Ti = t. Let Yi(t,m) denote the outcome that results if
#the treatment and mediating variables equal t and m respectively.

#Consider a standard experimental design where only treatment var is randomized
#We observe one of the potential outcomes, and the observed outcome Yi equals
# Yi(Ti,Mi[Ti]), or Outcome given treatment, and given mediator value due to treatment
# Thus Mi[Ti] represents the observed value of mediator Mi.

#Under this notation, the total unit treatment effect can be written as
# Total effect = Outcome given treatment, and mediator score given that treatment
# - Outcome given no treatment, and mediator score given no treatment
#This simply defines how much the effect of the treatment SHOULD be on a unit

#We can decompose this total effect further into two sub-elements.

#The first is the causal mediation effects. This is represented by
#causal effect = Outcome given t and mediator score given treatment
# - outcome given t and mediator score given no treatment
#We replace our outcome given treatment/not treatment with a generic standin
#'t' here, for each treatment status t = 0,1.

#'#The second is the 'direct' effects of the treatment. This is represented by
# Direct Effect = Outcome given treatment and mediator score given t
# - Outcome given no treatment and mediator score given t. With t = 0,1

#Adding the causal mediation and direct treatment effects sum up to our
#total effect, for t = 0,1. When considering multiple mediating variables, 
#we require additional notation, but conceptualy this is what matters.

#The average causal mediation effects (ACME = delta(t)), and the
#average direct effects (ADE = zeta(t)) represent the POPULATION averages
#estimated for these causal mediation and direct effects

#Identification of the ACME requires additional assumptions beyond strong ignorabilty
#of our treatment, which is sufficient to identify the average total effect of
#the treatment. Let Xi be a vector of the observed pre-treatment confounders
#for unit i, the key identifying assumption is called sequential ignorability
#written as Equation 5 in Imai 2010

#Equation 5 is the 'standard' strong ignorability of the treatment assignment and is
#satisfied (for example) if our treatment is randomized (it could even be conditional
#on Xi!). However, equation 6 requires that the mediator is ALSO ignorable
#given the observed treatment and pretreatment confounders.

#This is a STRONG additional assumption, because it excludes the existence of
#(measured or unmeasured) post-treatment confounders as well as that of unmeasured
#pre-treatment confounders (WHY??).

#Therefore, this assumption rules out the possibilty of multiple mediators that
#are causally related to each other.

###########################################
## Model-based Causal Mediation Analysis ##
###########################################

#How does mediate work for model-based causal analysis?

#This current version of mediate can handle a larger class of statistical models
#than is covered in Imai 2010

#The output of mediate can be passed to plot () and summary () to get details
#the function itself automatically detects the type of models used for
#mediator and outcome models and calculates the estimates of ACME and other
#quantities of interest, this is better than using product/difference of 
#coefficients, which are ONLY appropriate for the analysis of causal
#mediation effects when both the mediator and outcome models are linear regressions
#where Ti and Mi enter the models additively (without interaction).

###########################
# Example using 'Framing' #
###########################

library("mediation")
set.seed(2014)
data("framing", package = "mediation")

#Framing example has a randomized experiment where subjects are shown different
#stories about immigration, the effect of how framing influences attitude
# and political behavior regarding immigration policy

#Anxiety is seen as the mediating variable for the causal effect of framing 
#on public opinion.

#We first fit the mediator, where our measure of anxiety (emo) is modeled 
#as a function of the framing treatment (treat) and pretreatment covariates
# (age, educ, gender, and income). We also model the outcome variable, a binary
#about whether or not hte participant wanted to message congress on immigration
#The explanatory variables for our outcome model include the mediator, treatment
#status, and the same set of pre-treatment variables as those used in the mediator
#model. The treatment is expected to increase the level of respondents' emotional
#response, making it more likely to send a letter to congress.
#We fit with a linear regression using least squares and probit for our
#mediator and outcome models, respectively.

med.fit <- lm(emo ~ treat + age + educ + gender + income, data = framing)
out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))

#then we use mediate function to estimate the ACME and ADE, our inputs for
#the function are the model fits (med.fit and out.fit) as well as the
#names of the treatment and mediating variables, represented as the arguments
#treat and mediator. Note that our # of sims is pretty low (100), the default
#is 1000, and sometimes even higher # is better if estimates are varying
#too much from 1 sim to another. The default simulation type is Quasi-Bayesian
#Monte Carlo method based on normal approximation.

#White's heteroskedasticity-consistent estimator for the covariance matrix
#from the 'sandwich' package can be done by setting robustSE to TRUE.
#this can be omitted if standard uncertainty estimates are desired.

med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                   robustSE = TRUE, sims = 100)

summary(med.out)

#Looking at our output, the estiamtes for various ACME and ADE for both
#control and treated condition exists, as well as p-values for various estimates
#Here, our estimated ACME's are different from 0, but our estimated
#average direct and total effects are not! This suggests that the treatment in
#the framing experiment may have increased emotional response, which leads to
#subjects being more likely to send messages to congress.

#Since our outcome is binary, all estiamted effects are expressed as an increase in
#probability that the subject sent a message to his or her Congress person. Note that
#we can also use the nonparametric bootstral rather than quasi-Bayesian monte carlo
#for variance estimation by setting boot = TRUE.

med.out <- mediate(med.fit, out.fit, boot = TRUE, treat = "treat",
                   mediator = "emo", sims = 100)
summary(med.out)

#looking at our results, we see that for our output our bootstrap is what is used
#for inference. The results are largely the same. Generally, if you have computing power
#analysts should try to have more than 1000 resamples (the default)

#Two types of methods for calculating bootstrap based CI's are available via
#boot.ci.type argument. Percentiles are done by setting argument to 'perc',
#bias corrected and accelerated intervals are computed when setting to 'bca'.
#The latter has better asymptotic properties and is recommended for estimation
#of mediation effects

#An alternative, we can use mediate as the input into plot command

plot(med.out)

#Treatment and Mediator Interaction

#It's possible that our ACME values are different depending on baseline treatment
#status. Here, then we can add an interaction term betwen our treatment and our
#mediator to the outcome model. The mediate function automatically detects this change
#and explicitly estimates the ACME based on treatment status

med.fit <- lm(emo ~ treat + age + educ + gender + income, data=framing)

out.fit <- glm(cong_mesg ~ emo * treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))

med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                      robustSE = TRUE, sims = 100)

summary(med.out)

#The statistical significance of the treatment-mediator interaction can be tested
#via test.TMint function

test.TMint(med.out, conf.level = .95)
#Here, we do NOT reject the null hypothesis.

#The mediate function's output contains a range of additional quantitites that
#could be helpful, look at ?mediate for details.

#################
# Missing Data? #
#################

#this simulation based approach to estimation of mediation effects allows for
#users to deal with missing data via standard 'multiple imputation' procedures
#Our two utility functions, mediations and amelidiate. We simulate our data
#using imputation software, then we run each data set through mediations

#Next, the output of mediations is run into the amelidiate function, combining
#components of the output from mediations into a format that can be analyzed
#with plot and summary commands.

#Note, we can also manually run mediate on the imputed data sets, and 'stack' 
#the vectors of quantities that they are interested in, and use basic functions
#like 'quantile' to calculate the CI's


#########################
## Moderated Mediation ##
#########################

#We can examine moderated mediation! Some analysists hypothesize that the magnitude 
#of the ACME depends on (is moderated by) a pre-treatment covariate, a moderator.
#in our 'framing' example, the ACME may be STRONGER amongst older people than
#younger ones, essentially, the ACME may be moderated by age

#There are two alternative routes to the analysis of moderated mediation
#First, we can alter the models and syntax in the mediate function.

#We begin by ensuring that the mediator and outcome models contain the 
#moderator and it's interaction terms wrt the treatment and the mediating variables
#that are justified in theory. FOR EXAMPLE!

med.fit <- lm(emo ~ treat * age + educ + gender + income, data=framing)

out.fit <- glm(cong_mesg ~ emo + treat * age + emo * age + educ + gender
               + income, data = framing, family = binomial("probit"))

#Once our two models are fitted, we specify the levels of the moderator
#at which effects will be calculated by the mediate function.

#e.g. We can set our age covariate to a specific value, FOR EXAMPLE - we can set
#the value of age to be 20 in one model, and 60 in another!

med.age20 <- mediate(med.fit, out.fit, treat = "treat",
                     mediator = "emo", covariates = list(age = 20), sims = 100)

med.age60 <- mediate(med.fit, out.fit, treat = "treat",
                     mediator = "emo", covariates = list(age = 60), sims = 100)

summary(med.age20)
summary(med.age60)

#We thus now have two different outputs, the first output, the ACME is estimated
#for those who are 20 years old. The second output applies to those who are 60


#The second approach consists of DIRECTLY testing the statistical significance of
#the difference in the ACME and ADE between two chosen levels of pre-treatment covariates
#This analysis is conducted via the test.modmed function. 

#For example, we can test whether or not the ACME and ADE significantly differ
#between the subjects who are 20, and those who are 60

med.init <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo", sims=2)

test.modmed(med.init, covariates.1 = list(age = 20),
            covariates.2 = list(age = 60), sims = 100)

#Note, here we see that our initial mediate fit doesn't need our 'covariates'
#argument, and the choice of levels is made in the call to test.modmed itself.
#ALSO! Our initial mediate call doesn't need many sims, as the calculation
#for uncertainty happens within the test.modmed function.


#################################
# Nonbinary Treatment Variables #
#################################

#experimental manipulations can be complicated, our framing example is a 
#2x2 factorial design (each subj exposed to 2 different binary treatments
#thus resulting in 4 different manipulations). We focused on a comparison of
#one condition relative to the other 3, HOWEVER, we can handle something more complex!

#A Non-binary treatment variable.
#Instead of using the binary 'treat' variable, we use a variable named 'cond'
#which records which of the 4 conditions subj was exposed to. Using control.value
#and treat.value options, we can calculate whatever contrast of interest we want.

#FOR EXAMPLE, the comparison between the 2nd and 3rd conditions can be done as below

med.fit <- lm(emo ~ cond + age + educ + gender + income, data = framing)

out.fit <- glm(cong_mesg ~ emo + cond + age + educ + gender + income,
               data = framing, family = binomial("probit"))

med23.out <- mediate(med.fit, out.fit, treat = "cond", mediator = "emo",
                     control.value = 2, treat.value = 3, sims = 100)

summary(med23.out)

#We can also compare the 1st and 4th conditions!

med14.out <- mediate(med.fit, out.fit, treat = "cond", mediator = "emo",
                     control.value = 1, treat.value = 4, sims = 100)

summary(med14.out)

#Nothing changes in our output format, but our contrasts will change depending
#on categories chosen for comparison by the researcher. For a continous treatment
#the researcher would specify two values of treatment to make a contrast

#We can define the causal mediation effect for any two levels of our treatment
#Since the ACME is defined as delta = various values of treatment, we can set
#control.value and treat.value to t0 and t1 respectively!

#We can also vary the value of t1, while fixing the base value of t0, to see how
#ACME changes as a function of t1.


######################################################
## Sensitivity Analysis for Sequential Ignorability ##
######################################################

#Sequential ignorability is a strong assumption, and thus, sensitivity analysis can be
#a good value! Lets try one using mediation(), to see if we can gauge how likely we are to
#find unobserved pre-treatment covariates. We can pass the output of mediate to medsens()

#This Computes the values of causal quantitites as a fxn of sensitivity parameters.
#Both summary and plot functions are available for analysis, and they display the results in 
#a tablular and graphical form, respectively.

#For EXAMPLE! After runing ACME, we conduct a sensitivity analysis by using medsens
#Our parameter, rho (the correlation b/w residuals of the mediator and outcome regressions)
#If there exist unobserved pre-treatment confounders which affect BOTH the mediator and the
#outcome, we would expect that Rho is NOT zero! The sensitivity analysis is conducted by varying
#the value of rho and examining how the estimated ACME changes

med.fit <- lm(emo ~ treat + age + educ + gender + income, data = framing)

out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                   robustSE = TRUE, sims = 100)
sens.out <- medsens(med.out, rho.by = 0.1, effect.type = "indirect", sims = 100)

summary(sens.out)

#looking at our result, here's what we can say. Rho by .1 sets our rho varying from -.9 to .9
#in increments of .1, indirect effect type means sensitivity analysis is conducted for ACME
#We could have effect type = 'direct', performing sensitivity analysis for ADE, and we 
#can put in 'both' if we want both!

#Our summary function shows the values of rho at which the CI contains zero for ACME
#The CI's contain zero when rho equals .3 and .4, looking at the other columns
#our first row captures whrere acme is 0 as a function of proportion of residuals variance
#in the mediator and outcome explained by an hypothesized unobserved confounder.

#The second line uses total variance instead of residual variance, with R*2 for resid, and
#R_2 for total var. For example, when the product of the original variacne explained
#by the omitted confounding is .049, the point estimate for ACME is 0

#We can also use graphics instead, which we can do by passing to plot

plot(sens.out)
plot(sens.out, sens.par = "R2")

#our graphics can be very easy for sens analysis!

plot(sens.out, sens.par = "rho", main = "Anxiety", ylim = c(-0.2, 0.2))

#We must specify whether the hypothesized confounder affects the mediator and outcome in the
#same or different directions. This matters because sensitivity anlaysis in terms of
#the product of rsquared stats. For our currente xample, we can assume same direction
#by setting sign.prod = "positive" (rather than "negative")

#Plotting our total variacne of our sens analyasis, we see which combinations of Rsquare
#where our ACME would be 0 (the product equals 0.049)

plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive")

#Causal mediation analysis of Multilevel Data

#we can look at multilevel data via lmer and glmer in lme4 package. Analyzing data w/ individual
#observations that are clustered within groups. Individual data may be correlated, but different
#groups have different data generating processes, MLM allows us to take this heterogeneity
#within and between groups simultaneously.

#We can categorize this analysis into multiple types based on whether treatment, mediator,
#and outcome variables are measured at the individual and gorup level

#Regardless, researchers can use mediate to analyze by choosing appropriate models for
#the mediator and outcome variables. We have two examples of data structure here.

# 1: The treatment is assigned at group level, mediator and outcome are measured at
#the individual level

# 2: Both the treatment and mediator are group-level variables, while the outcome is recorded
#at the individual level. 

#We look at the education longitiduinal study, w/ students clustered within schools.
#The student data set contains student and school level vars, organized at the student level.
#The school dataset has school level vars, which is only needed when we analyze the data
#where both treatment and mediator are group-level variables.

 
 #######################################################
 # Group-level treatment and individual-level mediator #
 #######################################################

data("student", package = "mediation")

#we analyze whether a school is Catholic or not, affects likelihood of fighting, and hypo
#thesize that the mediation is emotional attachment to the school.

#Thus, we postulate that students in Catholic school may have greater sense of attachement
#to their school, decreasing the likelihood of getting into a fight.

#This is modelled using i and j as student and school indicators, error being normally distributed
# group level error with mean zero, and Xij is the vector of student-level pre-treatment
#covariates (gender, income, and pared)

#our binary outcome is modeled with a logistic link, with group level errors jointly
#allowed to distribute with mean zero. We can estimate this using the glmer function

library(lme4)

med.fit <- glmer(attachment ~ catholic + gender + income + pared + (1|SCH_ID),
                 family = binomial(link = "logit"), data = student)
out.fit <- glmer(fight ~ catholic*attachment +
                   gender + income + pared + (1 + attachment|SCH_ID),
                 family = binomial(link = "logit"), data = student)

#we can feed these functions into 'mediate' as usually

med.out <- mediate(med.fit, out.fit, treat = "catholic", mediator = "attachment",
                   sims = 100)

summary(med.out)

#Our estimated mediation, direct, and total effects are all different from zero. The results
#suggest that school-level treatment (catholic) increase the value of the mediator (attachment)
#which in turn decreases the value of our outcome (fight), and also our treatment decreases
#the value of the outcome directly, on a different causal path!