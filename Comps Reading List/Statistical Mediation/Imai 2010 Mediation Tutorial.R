
 #########################################################################
 #########################################################################
 ##                                                                     ##
 ## BIG NOTE, UPDATED TUTORIAL CAN BE FOUND IN vignette("mediation")!!! ##
 ##                                                                     ##
 #########################################################################
 #########################################################################

# From "Advances in Social Science Research Using R #
### Chapter 8: Causal Mediation Analysis Using R ####


#following up from Imai 2008 and Imai 2009 algorithims to estimate causal mediation
#we can estimate causal mediation effects for linear and nonlinear relations

#How to do this in R?

install.packages("mediation")
library("mediation")
update.packages("mediation")
#mediation is up to date
?mediation

#this is the design and architecture overview of the Mediation package
# details of the methods can be found within Imai 2010a and 2010b

#mediation relies on sequential ignorability assumption in previous paper
#Sequential ignorability states:
#The observed mediator status is as if randomly assigned condition
#on the randomized treatment variable and the pretreatment covariates

#requires 2 statistical models, one for mediator: f (Mi | Ti,Xi)
#Other for outcome: f (Yi | Ti,Mi,Xi)

#once these models are chosen and fitted, then mediation will compute the 
#estimated causal mediation effect, and other estimates using the Imai 2010 algos.

#The algos produce CI's based on either nonparametric bootstrap (for para and nonpara models)
#or quasi-Bayesian Monte Carlo approximation (for parametric only)

#Three steps for mediation analysis
# 1: Fit mediator and outcome models using for example, regression models
# e.g. the usual lm() and glm() functions

# 2: Analyst takes the output objects from these models (e.g. model.m and model.y)
# We then use these output objects as inputs for our main function, mediate()

# 3: Mediate function then estimates the causal mediation effects, direct effects
# and total effect, along with uncertainty estimates.

# 4: FINALLY, sensitivity analysis can be done using medsens() function,
# this function takes the output of mediate as an input

# Generally, for the output of the 'mediate' function, a summary() method reports
# the key findings in table form, medsens() can also output as summary() or plot()


################################################
#### Estimation of Causal Mediation Effects ####
################################################

#Estimation of these effects is based off of algorithms 1 and 2 of Imai 2010
# How are these algorithms implemented in mediation()?

#######################################
## Algorithm 1 for Parametric Models ##
#######################################

#Start by fitting parametric models for mediator and out come variables
#e.g. we model observed mediatior Mi given treatment Ti and pretreatment 
#covariates Xi. 

#Similarly, model observed outcome Yi given the treatment, mediator, and
# pretreatment covariates.

#For example, to implement the original Baron-Kenny procedure, linear models
# are fitted for both the mediator and outcome models using the lm command

#The model outputs from these two parametric models form the inputs for the
#mediate() function, the user supplies the names for which variables are the
#mediator and the ou tcome, as well as what # of simulations should be used
#for inference, and whether the mediator variable is allowed to interact with the
#treatment variable in the outcome model (no-interaction assumption is the norm,
# but it's not necessarily realistic)

#given these model objects, the estimation proceeds by simulating the model
#parameters based on their approximate asymptotic distribution (the multivariate
# normal distribution w/ mean = to parameter estimates, and variance = to 
# asymptotic variance estimate)

#we then compute causal mediation effects of interest for each parameter draw
#this method of inference is similar to an approximation of the Bayesian
#posterior distribution due to Bernstein-von Mises Theorem (??)
#this is VERY computationally efficient compared to algo #2

#R's object oriented works well in mediate(), functions like coef() and vcov()
#are used to extract the point and uncertainty estimates from the model objects
#to form the multivariate normal distribution from which parameter draws
#are sampled. Additionally, computation of estimated causal mediation effects
#requires prediction of mediator values under different treatment regimes as well
#as the prediction of outcome values under different treatment and mediator values

#We can do this using 'model.frame()' to set treatment and mediator values to
#specific levels, while keeping values of OTHER variables unchanged! 
#Model.matrix and matrix multiplication is then used with the distribution of 
#simulated parameters to compute the mediation and direct effects

#the biggest strength: APPLICABLE TO A WIDE RANGE OF PARAMETRIC MODELS
# allows us to avoid coding a completely seperate function for different models!

##################################################
## Algorithm 2 for Non/Semiparametric Inference ##
##################################################

#main disadvantage of algo 1 is that it isn't easily applied to non/semiparametric
#models. For these models, algo 2, based on nonparametric bootstrp, can be used
# HOWEVER, it is MUCH more computationally intensive.

#Algo 2 can also be used for our parametric models; Algorithm 2 resamples the
#observed data with replacement. Then for each of the bootstrapped samples, we fit
#both the outcome and the mediator models, and compute the quantities of interest
#As in algo 1, this requires the prediction of mediator values under different
#treatment regimes, as well as the prediction of outcome values under different
#treatment and mediator values. 

# R's leverages object orientation here for value, Algo 2 relies on predict()
#function to compute these predictions, while the algo manipulates the treatment
#and mediator status using the model.frame function. This process is repeated
#a large # of times and returns a bootstrap distribution of the mediation
#direct, and total effects. Algorithm 2 allows analysts to estimate mediation
#effects with more flexible model specifications, or to estimate mediation
#effects for quantiles of the distribution

##########################
## Sensitivity Analysis ##
##########################

#Causal mediation analysis relies on sequential ignorability assumption
#that BY DEFINITION cannot be verified with the observed data, this assumption
#implies that the treatment is ignorable given the observed pretreatment
#confounders, and that the mediator is ignorable given the observed treatment
#and observed pretreatment covariates.

#in order to probe the plausibility of this key identification assumption, we need
#to perform sensitivity analysis. Which historically has been hard af to do for
#a general form that applies to any parametric or nonparametric model.
#Imai 2010 balled out and developed this analysis to work for commonly used models


##Baron-Kenny Procedure##

#Imai develops a sensitivity analysis based on the Baron-Kenny procedure
#which is generalised to the LSEM with an interaction term.
#the sensitivity parameter, Rho, is equal to the correlation between
#the two error terms, for the prediction of mediator and outcome.

#Under sequential ignorability, Rho is equal to zero, and thus the magnitude
#of this correlation coefficient represents the departure from the ignorability
#assumption (about the mediator)

#Note that our treatment is assumed to be ignorable, as that's the case in 
#randomized experiments with randomized treatments, but the mediator is not.
#Our theorem shows how the ACME changes as a function of Rho

#To obtain the CI's for sensitivity anlaysis, an iterative algo is applied
# for a fixed value of rho, at the t'th iteration, given the current values and 
#a given error correlation value (Rho), we compute the var/cov matrix of 
#our two errors, and then we update the parameters via generalized least squares

#this is essentially an application of the iterative feasible generalized
#least square algorithm.

##Binary Outcome Case

#Sensitivity analysis for binary outcome paralells the case when both
#mediator and outcome are continuous. We assume the model is a probit regression
#Using this probit regression for our outcome allows us to assume the error
#terms are jointly normal with a possible nonzero correlation, Rho.

#We derive the average causal mediation effects as a function of Rho, and a set
#of parameters that are identifiable due to the randomization of the treatment
# Rho is used here as a sensitivity parameter, similar to how it's used in the
#Baron-Kenny Procedure, CI's are calculated using quasi-Bayesian algorithm 1
#by approximating the posterior distribution with the sampling distribution of
#the maximum likelihood estimates

## Binary Mediator Case

#We can also pull sensitivity analysis when the mediator is dichotomous
#and the outcome is continous. In this case, we assume the mediator can
#be modeled as a probit regression w/ the error term iid as standard normal
# a linear normal regression w/ error variance equal to error_3 is used to
#model the continous outcome variable.

#Also assume that the 2 error terms jointly follow a bivariate normal 
#distribution, w/ mean 0 and covariacne rho * error_3. Like the other two
#cases, the correlation b/w error terms, rho, as our sensitivity parameter
#we show that the causal mediation effects can be expressed as a function of 
#model parameters, consistently estimable, given a fixed value of Rho.

#we can summarize these using plot() similar to the first two cases.

#################################################
# Alternative Interpretation Based on R_squared #
#################################################

# Main advantage of using Rho as our sensitivity parameter is increased simplicity
# However... interpreting the correlation coefficient magnitude can be hard.

#Alternative interpretation of Rho based on coefficients of determination (Rsquared)
#that can be extended to binary mediator and outcome cases. In these formulations,
#we assume that a common unobserved pretreatment confounder exists for both the 
#mediator and outcome models.

#applied researchers then specify whether the coeffcients of this unobserved confounder
#in the 2 models have the same sign or not, sgn(l2 * l3) = 1 or −1 where l2 and l3
#are the lambda coefficients in the mediator and outcome models, respectively.

#once we have this... the ACME can be expressed as a function of the 
#"proportions of original variances explained by the unobserved confounder"
#where the original variances refer to the variances of the mediator and the outcome
#or the var of the latent variable in the case of binary dependent variable.

#alternatively... ACME can be expressed in terms of 
#"the proportion of the previously unexplained variances explained by the unobserved
# confounder". This is an excellent explanation!

#we must quantify how large the unobserved confounder must be (relative to our 
#observed pretreatment covariates in the model) in order for our original conclusions
#to be REVERSED!!


#######################
# Current Limitations #
#######################

#The package, mediation, is VERY flexible and can easily handle many of the
#model tyupes that researchers are likely to use in practice
# This includes mediator types (continuous, ordered and binary) using 
#both Continuous outcomes for all three, and binary for all except ordered.
# for all these examples, we can produce both point and uncertainty estimates
# of the causal mediation effects

## This software can also conveniently probe sensitivity of results to
## potential violations of the ignorability assumption for certain model types
## This requires specific derivations for each combo of models
## For continous mediator, we use can use sensitivity analysis to look at
# continous and binary outcomes, for binary mediator, we can 
#use sensitivity analysis for continous outcomes. For those that combine
#binary and continous mediators/outcomes, we can use probit regression model
#with a linear regression model this allows for jointly normal errors in the analysis!

############
# EXAMPLES #
############

library("mediation")
data("jobs")

#Example using dataset from JOBS II study
# outcome variables are continous depression variable, binary 'has a job' variable
# Continous measure of job-search self-effiacy is mediating variable

#also measures baseline covariates: pretreatment depression, education
# income, race, martial status, age, sex, previous job, and economic hardship.

##########################################
# Estimation of Causal Mediation Effects #
##########################################

#Baron-Kenny Procedure

#First example has both mediator and outcome as continous
#results from EITHER algo will give point estimates identical to the usual
#Baron and Kenny procedure, using either the quasi-Bayesian, or nonparametric
#bootstrap approximation

#we first estimate 2 linear regressions for both mediator and outcome
#using the lm() function

model.m<-lm(job_seek~treat+depress1+econ_hard+sex+age+occp+marital+nonwhite+educ+income,
            data= jobs)
model.y<-lm(depress2~treat+ job_seek+ depress1+econ_hard+sex+age+occp+marital+nonwhite+educ+income,
            data= jobs)

#our first two model objects, model.m and model.y are the arguments for 
#the mediate function itself.

#note that we have to give a fuck about missing values!!
#While model fxns in R handle missing values in the data using listwise
#deletion procedures, the functions in mediation  assume that missing values
#have been ALREADY removed before the estimation of the two models

# THUS, the two models MUST have identitical observations
#sorted in the EXACT SAME ORDER with all missing values removed
#the R fxn na.omit() can address this nicely

#in our first usage of mediate(), we specify boot = TRUE to specify a nonparametric
#bootstrap wth 1000 resamples (sims = 1000). When we set it to FALSE in the
#second usage, inference instead proceeds via the qasi-Bayesian Monte Carlo
#approximation using algo 1 rather than aglo 2

#Note that we MUST specify the variable names for the treatment indicator
#and the mediator variable using treat and mediator respectively!

out.1 <- mediate(model.m, model.y, sims = 1000,
                 boot = TRUE, treat = "treat", mediator = "job_seek")

out.2 <- mediate(model.m, model.y, sims = 1000,
                 treat = "treat", mediator = "job_seek")

#the objects that we make using mediate(), i.e. out.1 and out.2, are LISTS
#that contain several different quantities from the analysis...

#for example, out.1$d0 returns the point estimate for the ACME
#based on algo 1

out.1$d0

#The help file contains a full list of values contained in mediate() objects
#the summary() function prints out the results in table form

summary(out.1)

summary(out.2)

#The output from summary() fxn dispalys the estimates for the 
#average causal mediation effect, the direct effect, the total effect
#and the proportion of total effect mediated

#the 1st column is the quantity of interest, the 2nd column displays the point
#estimate, and the other columns have the 95% CI.
#Researchers can easily report these point estimates and corresponding uncertainty
#in their work!, we find that job serach self efficacy mediated the effect of the
#trreatment on depression in the NEGATIVE direction,

#this effect was small with a point estimate of -.134, but the 95% CI contains
#exactly 0

###############################################
# Baron-Kenny Procedure with INTERACTION term #
###############################################

#we can also allow for our causal mediation effect to vary with treatment status
#here, our model for our outcome MUST be altered by including an interaction term
#between the treatment indicator 'treat' and the mediator variable 'job_seek'