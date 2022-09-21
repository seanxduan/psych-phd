
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
