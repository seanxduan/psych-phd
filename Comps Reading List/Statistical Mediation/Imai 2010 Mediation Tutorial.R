
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

model.y <- lm(depress2 ~ treat + job_seek
              + treat:job_seek + depress1 + econ_hard + sex
              + age + occp + marital + nonwhite + educ
              + income, data = jobs)

# NOTE! under this current implementation the interaction term MUST be specified in the form
#of treat.name:med.name, w/ the names of the treatment and mediator variable in the model
#respectively. Next, we have a call made again using the mediate function, but with
#the option INT = TRUE specified!

out.3 <- mediate(model.m, model.y, sims = 1000,
                 boot = TRUE, treat = "treat", mediator =
                   "job_seek")

#oh, int has been depreciated! existence of interaction terms is now automatically detected from
#the model formulas

out.4 <- mediate(model.m, model.y, sims=1000,
                 treat = "treat", mediator =
                   "job_seek")

summary(out.3)

#looking at our summary... we see that the estimates for the mediation and direct effects
#correspond to the levels of the treatment and are referred to as such in the summary.

#in this case, our mediation effect under the treatment c ondition, listed as 'treat'
#is estimated to be -.0117, while the control condition has -0.0185

summary(out.4)

########################################
# Use of non/semiparametric regression #
########################################

#The flexibility of mediation becomes very apparent when we move beyond standard linear regression
#models, e.g. the mediator can be suspected to have a nonlinear effect on the outcome
# Generalized Additive Models (GAMs) allow us to use splines to for flexible nonlinear fits
#this is VERY easy for the mediate() function!

#we do the same analysis with the mediator as before, but we alter the outcome model
#using the gam() function from the mgcv library

library(mgcv)

model.m <- lm(job_seek ~ treat + depress1
              + econ_hard + sex + age + occp + marital
              + nonwhite + educ + income, data = jobs)

model.y <- gam(depress2 ~ treat + s(job_seek,
                                    bs = "cr") + depress1 + econ_hard + sex + age
               + occp + marital + nonwhite + educ + income,
               data = jobs)

#Here, we fit a GAM for the outcome variable, and allow the effect of the job_seek variable
#to be nonlinear and determined by the data. This is done by using the s() notation, which
#allows for the fit between the mediator and outcome to be modeled with a spline.

#using the spline for the fit allows the estimate for hte mediator on the outcome to be a
#series of 'piecewise' polynomial regression fits. This semiparametric regression model
#is a more general version of nonparametric regression models such as lowess.

#the model above allows the estiamte to vary across the range of the predictor variable
#Here, we specify the model with a cubic basis function (bs = "cr") for the smoothing spline
#and leave the smoothing selection to be done at program defaults (generalized cross-validation)

#The full scope of how to fit these models is in your old data III textbook


#The call to mediate() with a gam() fit remains unchanged, except that when the outcome model
#is a semiparametric regression... only the nonparametric bootstrap is valid for calculating 
#uncertainty estimates, e.g. boot = TRUE

out.5 <- mediate(model.m, model.y, sims = 1000,
                 boot = TRUE, treat = "treat", mediator = "job_seek")

summary(out.5)

#The model for the mediator can also be modeled wit the gam() function as well, the 
#gam() function allows analysts to include interactions; thus, we can still allow mediation
#effects to vary with treatment status. This requires us to alter our model by using the 'by'
#option in the gam() function, and using TWO separate indicator variables for treatment status

#to fit this model, we need one variable that indicates whether the observation was in the
#treatment group, and a second variable that indicates whether the obs was in the control group
#to allow the mediation effect to vary with treatment status, the call to gam() is the following

model.y <- gam(depress2 ~ treat + s(job_seek, by = treat)
               + s(job_seek, by = control) + depress1 + econ_hard + sex
               + age + occp + marital + nonwhite + educ + income,
               data = jobs)

#next, we must also alter the options in mediate() by providing
#the variable name for the control group indicator using the control option

out.6 <- mediate(model.m, model.y, sims = 1000,
                 boot = TRUE, treat = "treat",
                 mediator = "job_seek", control = "control")

#weird error here... double check the vignette?
#my guess here is that treat is set to 'treat' instead of '1' which is the value
#in the treatment column?

#if not...

summary(out.6)

#Based on the output that we're SUPPOSED to get...(from the tutorial itself)
#we see that even tho the mediator was specified as a nonparametric function
#we still see point estimates and CI's for the mediation effect across each
#treatment level.

#In the table, mediation effect_0 and direct effect_0 are the medation and direct
#effects respectively under control and effect_1 is for under treatment.

#######################################
## Quantile Causal Mediation Effects ##
#######################################

#what if we want to model mediation effects for quantiles of the outcome?
#Quantile regression allows for us to address this, while adjusting for
#a variety of covariates.

#For example... we might be wanted to know the .5 quantile (i.e Median)
#of our distribution. This is SUPER easy to do in mediate()!

#For these models, uncertainty estimates are calculated using nonparametric bootstrp
#thus, we need to load the quantreg library and model the median of our outcome
#although we can look at any quantile that we want. Note that we can also relax
#our no-interaction assumption for the quantile regression model as well

#we estimate our mediator using a standard linear regression, but our outcome
#is estimated with a rq() to model the median

install.packages("quantreg")
library(quantreg)

model.m <- lm(job_seek ~ treat + depress1 + econ_hard
              + sex + age + occp + marital + nonwhite + educ + income,
              data = jobs)

model.y <- rq(depress2 ~ treat + job_seek + depress1
              + econ_hard + sex + age + occp + marital + nonwhite
              + educ + income, tau= 0.5, data = jobs)

out.7 <- mediate(model.m, model.y, sims = 1000,
                 boot = TRUE, treat = "treat", mediator = "job_seek")

summary(out.7)

#looking at our summary, we see that the estimated median causal mediation
#effect and estimates for other quantities.

#we can have other quantiles by specifying different values in the tau argument
#for rq() out of the quantreg package

########################################
## Discrete Mediator and Outcome Data ##
########################################

#we can have measures for mediator and outcome that are DISCRETE, for standard methods
#there are a shit-ton of complications for doing so. Mediate() can deal with
#it using the algos developed in Imai 2010

#For example, lets use the JOBS II binary indicator (work1) to see whether
#or not the subject was employed after training. We use a probit instead of
#a linear regression for our outcome, and call mediate the same way as before

model.m <- lm(job_seek ~ treat + depress1 + econ_hard
              + sex + age + occp + marital + nonwhite + educ + income,
              data = jobs)

model.y <- glm(work1 ~ treat + job_seek + depress1
               + econ_hard + sex + age + occp + marital + nonwhite + educ
               + income, family = binomial(link = "probit"), data = jobs)

out.8 <- mediate(model.m, model.y, sims = 1000,
                 boot = TRUE, treat = "treat", mediator = "job_seek")

out.9 <- mediate(model.m, model.y, sims = 1000,
                 treat = "treat", mediator = "job_seek")

summary(out.8)

summary(out.9)

#We see in our second summary (non bootstrapped one) that the estimated ACME
#and the quasi-bayesian CI is printed on the 1st line, followed by the direct
#and total effects, as well as the proportion of the total effect due to mediation.

#We can use a logit model here as well, but we recommend a probit because implemention
#of sensitivty analysis requires probit model for analytical tractability

#The mediator itself can also be binary or ordered measure, but this requires
#modeling the mediator with either a probit or ordered probit model

#we have job_dich and job_disc, which are recoded versions of job_seek that 
#we can use to test this. The first, is our continous scale divided at the median
#into a binary variable. Our second measure, recodes the continous scale into
#a DISCRETE four point scale. THIS IS FOR DEMONSTRATION ONLY

#lets look at a probit model for our mediator and linear regression for our
#outcome!

model.m <- glm(job_dich ~ treat + depress1 + econ_hard
               + sex + age + occp + marital + nonwhite + educ + income,
               data = jobs, family = binomial(link = "probit"))

model.y <- lm(depress2 ~ treat + job_dich + treat:job_dich
              + depress1 + econ_hard + sex + age + occp + marital
              + nonwhite + educ + income, data = jobs)

#we look to allow the effect of the mediator to vary with treatment status
#we then call mediate and use either quasi-Bayesian approximation or
#the nonparametric bootstrap

out.10 <- mediate(model.m, model.y, sims = 1000,
                  boot=TRUE, treat="treat", mediator="job_dich")

out.11 <- mediate(model.m, model.y, sims = 1000,
                  treat = "treat", mediator = "job_dich")

summary(out.10)

summary(out.11)

#looking at our results for the non-bootstrap, we see the mediation effect 
#under our control condition, and the mediation effect under our treatment

#the direct effect of both is also visible.

#When our mediator is ordered... we can switch to an ordered probit model
#for our mediator. In R, the polr() function in MASS library does this.
library(MASS)

jobs$job_disc<-as.factor(jobs$job_disc)

model.m <- polr(job_disc ~ treat + depress1 + econ_hard
                + sex + age + occp + marital + nonwhite + educ + income,
                data = jobs, method = "probit", Hess = TRUE)
model.y <- lm(depress2 ~ treat + job_disc + depress1
              + econ_hard + sex + age + occp + marital + nonwhite
              + educ + income, data = jobs)

#note that Hess = TRUE needs to be specified to use quasi-bayesian approximation

out.12 <- mediate(model.m, model.y, sims = 1000,
                  boot = TRUE, treat = "treat", mediator = "job_disc")
out.13 <- mediate(model.m, model.y, sims = 1000,
                  treat = "treat", mediator = "job_disc")

summary(out.12)

summary(out.13)

#Final note, we can relax the no-interaction assumption as before by including
#the interaction between treatment and mediator in the outcome model
#and using the INT = TRUE option.


##########################
## Sensitivity Analysis ##
##########################

#We can explore how robust our findings are to the ignorability assumption.
#the medsens() function in mediate allows for us to conduct sensitivty analysis
#this can be done for continuous/continuous, binary/continuous, and continuous/binary

#Through the Baron-Kenny procedure, we first fit these models for our mediator and outcome

model.m <- lm(job_seek ~ treat + depress1 + econ_hard
              + sex + age + occp + marital + nonwhite + educ + income,
              data = jobs)

model.y <- lm(depress2 ~ treat + job_seek + depress1
              + econ_hard + sex + age + occp
              + marital + nonwhite + educ + income, data = jobs)

med.cont <- mediate(model.m, model.y, sims=1000,
                    treat = "treat", mediator = "job_seek")

#then the output from the mediate function becomes the argument for medsens()
#medsens() recognizes the options specified in mediate, thus, we don't need to respecify

sens.cont <- medsens(med.cont, rho.by = 0.05)

#The rho.by option specifies how finely incremented the parameter rho is for sensitivity
#analysis. Using a coarser grid for rho speeds up estimation, but comes at the cost of
#estimating the robustness of the original conclusion only imprecisely.

#after running medsens, we can use the summary function on our output to produce
#a table with values of rho for which the CI contains zero. This allows for the analyst
#to immediately see the approximate range of rho where the sign of causal mediation effect
#is indeterminate

#the second section of the table contains value of rho for which the mediation effect is 
#exactly zero, here, it's around -.19. This table also presentes coefficients of determination
#that correspond to the critical value of  rho when the mediation effect is zero. First,
#R*2MR*2Y is the product of coefficients of determination which represents the proportion of 
#previously unexplained variance in the mediator and outcome variables that is explained
#by an unobserveable pretreatment unconfounder.

#an alternative formluation is in terms of the proportion of ORIGINAL variance explained
#by an unobserved confounder, here denoted in the last column

summary(sens.cont)

#this table above presents the estimated mediation effect along with it's CI for each
#value of rho. The reader can verify that when rho is equal to zero, the mediation effect
#matches the estimate produced by the mediate() function.

#for other values of rho, the mediation effect is calculated under different levels 
#of unobserved confounding

#the information from the sensitivity analysis can also be summarized graphically using
#the plot() function. We pass the medsens object, and specify sens.par option to rho

plot(sens.cont, sens.par = "rho")

#the dashed horizontal line represents the estimated mediation effect 
#under sequential ignorability. Solid line represents mediation effect
#under various values of rho. The grey region represents 95% confidence bands

#similarity you can plot sensitivity analysis in terms of the 
#coefficients of determination as discussed above. Here, we specify
#sens.par to 'R2', also, r.type option tells the plot function whether to
#plot the first or second time of proportional variance (type 1 or 2)
#finally, the sign.prod option specifies the sign of the product of 
#the coefficients of the unobserved confounder in the mediator and outcome models
#this product indicates whether the unobserved confounder affects both
#mediator and outcome variables in the same direction (1) or different
#directions (-1), reflecting the analyst expectations about the nature
#of the confounding.

#for example, the command below produces a plot representing sensitivityy of 
#estimates with respect to the proportion of the original variances explained 
#by the unobserved confounder when the confounder is hypothesized
#to affect the mediator and outcome variables in opposite directions.

plot(sens.cont, sens.par = "R2", r.type = 2,
     sign.prod = -1)

#Each contour line represents the mediation effect for various values of
#R2M and R2Y. For example, the zero  contour line corresponds to values of the product
#such that the ACME is 0. Even a small proportion of original variance
#unexplained by the confounder produces mediation effects of 0!

#Accordingly, the plot shows how increases in the product (moving from lower
#left to upper right) produces POSITIVE mediation effects!

#note that we can specify additional options (main, xlab, ylab, xlim, etc.)

##################
# Binary Outcome #
##################

#continuous mediator, outcome binary
#If either variable is binary, medsens() takes an additional argument

model.y <- glm(work1 ~ treat + job_seek + depress1
               + econ_hard + sex + age + occp + marital + nonwhite
               + educ + income, family = binomial(link = "probit"),
               data = jobs)

med.bout <- mediate(model.m, model.y, sims = 1000,
                    treat = "treat", mediator = "job_seek")

sens.bout <- medsens(med.bout, rho.by = 0.05,
                     sims = 1000)

#the 'sims' option provides control over the # of draws in the parametric
#bootstrap procedure which is used to compute confidence bands.
#when either the mediator our outcome is binary, the exact values of sens
#itivity parameters where the mediation effects are 0 cannot be
#analytically obtained, THUS this information is reported based on
#the signs of the estimated mediation effects under various values of
#rho and corresponding coefficients of determination.

#the use of summary() function is identical to fully continuous case
#in that the output table contains estimated mediation effects and values
#of rho for which the CI contains zero.

#we can plot this as well!

plot(sens.bout, sens.par = "rho")

plot(sens.bout, sens.par = "R2", r.type = 2,
     sign.prod = 1)

#on the first graph, we plot the ACME in terms of rho, while we use
#proportion mediated on the second plot.

#in the rho plot, the dashed line represents the estimated mediation effect
#under sequential ignorability, the solid line represents mediation effect
#under various values of rho, grey represents 95% confidence bands

#interpretation is the same!

###################
# Binary Mediator #
###################

#Finally, we can have outcome continuous, and mediator binary, we examine
#this by using our dichotomized variable, job_dich. We begin by fitting a
#probit model for our mediator and linear regression for our outcome variable.

model.m <- glm(job_dich ~ treat + depress1
               + econ_hard + sex + age + occp + marital + nonwhite
               + educ + income, data = jobs,
               family = binomial(link = "probit"))

model.y <- lm(depress2 ~ treat + job_dich+ depress1
              + econ_hard + sex + age + occp
              + marital + nonwhite + educ + income, data = jobs)

med.bmed <- mediate(model.m, model.y, sims = 1000,
                    treat = "treat", mediator = "job_dich")

sens.bmed <- medsens(med.bmed, rho.by = 0.05,
                     sims = 1000)

#the output of which can be passed to the plot function ()

plot(sens.bmed, sens.par = "rho")

#This plot is interpreted same as the above cases, we can plot 
#sensitivity results in terms of the coefficients of determination just
#as in the case with continous outcome and mediator variables.

#note thatwhen mediator variable is binary, the plotted values of the
#mediation effects and their confidence bands may not be perfectly smooth curves
#due to simulation errors, ESPECIALLY when # of sims is set to a small value

#we can use smooth.effect and smooth.ci set to TRUE in the plot() function
#so that the corresponding values are smoothed via lowess and look better
#but this adjustment can affect conclusions in a substantive way.

#You should just increase the # of simulations!