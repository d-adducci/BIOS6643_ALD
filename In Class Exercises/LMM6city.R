############################################################
##Program Name: LMM6city.R
##
##Purpose: To read in the lead dataset from FLW
##        to fit random effects models
###########################################################

###Load the libraries (not all are necessary for this example but I use them often enough)
library(tidyverse)
library(wrapr)
library(purrr)
library(arsenal)
library(table1)
library(ggplot2)
library(kableExtra)
library(olsrr)
library(broom)
library(reshape)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(nlme)
library(emmeans)
library(multcomp)


###Read in the fev data from the csv file (available on CANVAS for students).
fev.data <- read.csv(file="/Users/nichole/Dropbox/Teaching/Bios6643/Fall2023/Datasets/fev1.csv",header=T)

####Check graphs ###
p <- ggplot(data = fev.data, aes(x = age, y = logfev1, group = id))

###Spaghetti Plot
p + geom_line() 

###Histograms of baseline values

hist.p <- ggplot(data = fev.data, aes(x=baseht)) 
hist.p + geom_histogram()

hist.p <- ggplot(data = fev.data, aes(x=baseage)) 
hist.p + geom_histogram()

##summary table of amount of data per person
numbobs<-as.numeric(table(fev.data$id))
summary(numbobs)
hist(numbobs)

################################################################
##Fit random intercept models with different trends for age

##linear age and random intercept. This code uses ML

fev1.age<- lmer(logfev1 ~ 1 + age + (1 | id), REML=FALSE, data=fev.data)

##print general output
fev1.age

sum.fev1.age <- summary(fev1.age)

##print summary info including a test stat for age effect
sum.fev1.age

###This is the estimate, random eff, 
beta.ri <- fixef(fev1.age)
randomeff.ri<-ranef(fev1.age)
fitted.ri<-fitted(fev1.age)



###Make a plot of fitted lines###
fev.data <- cbind(fev.data,fitted.ri) ##this is a weird formatting because I have to trust the ordering is correct.
p.fitted <- ggplot(data = fev.data, aes(x = age, y = fitted.ri, group = id))
p.fitted + geom_line(color='red')

###Spaghetti Plot
p.fitted + geom_line(color='red') + geom_line(aes(x=age,y=logfev1,group=id), color='black')

###compute ICC
vc.fev1.age <- VarCorr(fev1.age)
print(vc.fev1.age,comp="Variance")



###squared age model. Could consider centering age if age wasn't pos. only
fev.data <- fev.data %>% mutate(agec = scale(age,scale=FALSE)) %>%
                     mutate(agesqc = agec*agec)

fev1.age2<- lmer(logfev1 ~ 1 + agec + agesqc + (1 | id), REML=FALSE, data=fev.data)

sum.fev1.age2<-summary(fev1.age2)

fitted.risq<-fitted(fev1.age2)

###Make a plot of fitted lines for squared model###
fev.data <- cbind(fev.data,fitted.risq) ##this is a weird formatting because I have to trust the ordering is correct.
p.fittedsq <- ggplot(data = fev.data, aes(x = age, y = fitted.risq, group = id))
p.fittedsq + geom_line(color='red')

###Spaghetti Plot
p.fittedsq + geom_line(color='red') + geom_line(aes(x=age,y=logfev1,group=id), color='black')


###cubic age model. Centered age
fev.data <- fev.data %>% mutate(agecubedc = agec*agec*agec)

fev1.age3<- lmer(logfev1 ~ 1 + agec + agesqc + agecubedc + (1 | id), REML=FALSE, data=fev.data)

sum.fev1.age3<-summary(fev1.age3)

fitted.ricube<-fitted(fev1.age3)

###Make a plot of fitted lines for squared model###
fev.data <- cbind(fev.data,fitted.ricube) ##this is a weird formatting because I have to trust the ordering is correct.
p.fittedcube <- ggplot(data = fev.data, aes(x = age, y = fitted.ricube, group = id))
p.fittedcube + geom_line(color='red')

###Spaghetti Plot
p.fittedcube + geom_line(color='red') + geom_line(aes(x=age,y=logfev1,group=id), color='black')


################################################################
##Fit random intercept and age models with different trends for age

##linear age and random intercept. This code uses ML

fev1.re.age<- lmer(logfev1 ~ 1 + age + (1 + age | id), REML=FALSE, data=fev.data)

##one approach to fixing convergence is to try to center because some times the intercept and 
##slope are very correlated
fev1.re.agec<- lmer(logfev1 ~ 1 + agec + (1 + agec | id), REML=FALSE, data=fev.data)

###This is the estimate, random eff, 
beta.rs <- fixef(fev1.re.agec)
randomeff.rs<-ranef(fev1.re.agec)
fitted.rs<-fitted(fev1.re.agec)

###Make a plot of fitted lines###
fev.data <- cbind(fev.data,fitted.rs) ##this is a weird formatting because I have to trust the ordering is correct.
p.fitted <- ggplot(data = fev.data, aes(x = agec, y = fitted.rs, group = id))
p.fitted + geom_line(color='red')

###Spaghetti Plot
p.fitted + geom_line(color='red') + geom_line(aes(x=agec,y=logfev1,group=id), color='black')




fev1.re.age2<- lmer(logfev1 ~ -1 + agec + agesqc + (1 + agec | id), REML=FALSE, data=fev.data)
