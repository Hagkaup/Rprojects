


############################### Script for survival analyses



rm(list=ls()) ## Clearing the console memory

## Set your working directory by going to "file" and choosing "Change dir..."

install.packages("survival")# installing the survival package "survival". Follow installation instructions.
install.packages("bbmle") # installing a package with maximum likelihood estimator
install.packages("rms") # for plotting Kaplan-Meier estimator

library(survival) # loading the survival package
library(bbmle) # loading package with maximum likelihood estimator
library(rms) # loading Kaplan-Meier plotting function




## Example data
flies <- read.csv(file="Ina.csv") # loading the csv file containing Ina's survival data and naming it "flies"

attach(flies) # attach the data allowing easy access to all the vectors
names(flies) # inspecting variables of the data sheet

plot(survfit(Surv(DeathTime, censor) ~ 1), conf.int=TRUE) # plotting Kaplan-Meier survival curve for all data

plot(survfit(Surv(DeathTime, censor) ~ Genotype), conf.int=TRUE) # plotting Kaplan-Meier curve one for each genotype, looks like age-specific hazard





######## The predictor being modelled is 'time of death', so the process is time-dependent (but may or may not be independent of age)


### GLM with Gamma distribution - constant hazard, no censor, survival type II

## Compare the generalized linear model with Gamma distribution (exponential shape) with the other analyses below, are they similar??? - we can use this approach here because there are no censored data
model4 <- glm(DeathTime ~ Genotype + Sex + Vial, family=Gamma)
summary(model4) # Significant effect of genotype on survival, significant effect of sex, but no effect of vial. Vial looks at whether the variation between replicates is greater than within replicates.




### Parametric models


## Exponential distribution (very similar to Gamma) - constant hazard, censor allowed, survival type II

model5 <- survreg(Surv(DeathTime, censor) ~ Genotype + Sex + Vial, dist="exponential") # The function used here is "survreg"
summary(model5) # This one is similar as the GLM. Effect of Genotype, Sex, but not vial
                # NOTE scale is set to 1 meaning hazard is constant

## Weibull distribution, parametric with age-specific constant hazard (NOTE: Weibull is "survreg" default)

model6 <- survreg(Surv(DeathTime, censor) ~ Genotype + Sex + Vial)
summary(model6) # This one finds effects of Genotype and Sex, and scale is 0.197, which means that death decreases with age (because less than 1). BUT, somewhat close to constant hazard. If scale is greater than 1 then death increases with age

AIC(model5, model6) # overall the parametric with Weibull distribution significantly outperforms the parametric with exponential distribution

AICtab(model5, model6) # ranking the models in terms of delta AIC values - "Weibull" model is superior (and therefore assumption of age-specific hazard is supported)

### Let's look at mean age at death for the two genotypes
model7 <- survreg(Surv(DeathTime, censor) ~ Genotype + Sex) # NOTE Vial is removed because no significant effects and remember "Weibull" is default in R.
summary(model7) # scale is now 0.197 when we ommit 'Vial'
tapply(predict(model7, type="response"), Genotype, mean)

### mean age at death for females
model8 <- survreg(Surv(DeathTime[Sex==1], censor[Sex==1]) ~ Genotype[Sex==1])
summary(model8)
tapply(predict(model8, type="response"), Genotype[Sex==1], mean)
# mean age at death rover females = 61.96 hours
# mean age at death sitter females = 96.54 hours


### mean age at death for males
model9 <- survreg(Surv(DeathTime[Sex==0], censor[Sex==0]) ~ Genotype[Sex==0])
summary(model9)
tapply(predict(model9, type="response"), Genotype[Sex==0], mean)
# mean age at death rover males = 36.36 hours
# mean age at death sitter males = 59.69 hours



### Cox Proportional Hazard regression - age specific hazard, censor allowed, survival type I & III or mix

model1 <- coxph(Surv(DeathTime, censor)~Genotype + Sex + Vial) # THis is the full model inspecting the effects of Genotype, Sex, and Vial number on survival. Vial looks at whether the variation between replicates is greater than within replicates.
summary(model1) # This calls a table over significant effects and their coefficients

## There are differences between rovers and sitters in general: (rover is the reference genotype) P < 2e-16
## There are differences between the sexes within genotypes: P < 2e-16
## The variation between replicates is not greater than within replicates, as expected P < 0.51

# coef= -2.305 is the estimated logarithm of the hazard ratio for rover versus sitters
# exp(coef) = exp(-2.305) = estimated hazard ratio indicating that rovers are at greater risk of dying than sitters

# coef= -2.535 is the estimated logarithm of the hazard ratio for males versus females
# exp(coef) = exp(-2.535) indicating that males are at greater risk of dying than females


## We therefore simplify the model and remove "Vial" from the model
model1 <- coxph(Surv(DeathTime, censor)~Genotype + Sex)
summary(model1) # Result is identical to the above, but has one less parameter compared to above

## Female rover vs female sitter including confidence intervals
model2 <- coxph(Surv(DeathTime[Sex==1], censor[Sex==1])~ Genotype[Sex==1])
plot(survfit(Surv(DeathTime[Sex==1], censor[Sex==1])~ Genotype[Sex==1]), lty=c(1:2), ylab="Survivorship", xlab="Time (h)", conf.int = TRUE)
summary(model2) # Inspecting model effects

## printing and inspecting the confidence intervals for female model
CImodel <- survfit(Surv(DeathTime[Sex==1], censor[Sex==1])~ Genotype[Sex==1]) # run the female model
summary(CImodel) # calling the upper and lower C limits for individual time points
CImodel # to see the median times at death (54 hours for rover females, 96 hours for sitter females)

## Male rover vs male sitter

model3 <- coxph(Surv(DeathTime[Sex==0], censor[Sex==0])~ Genotype[Sex==0])
plot(survfit(Surv(DeathTime[Sex==0], censor[Sex==0])~ Genotype[Sex==0]), lty=c(1:2), ylab="Survivorship", xlab="Time (h)", conf.int = TRUE)
summary(model3)


