############################################################
##Program Name: MixedEffectsAnalysis.R
##
##Purpose: To conduct various marginal model analysis and random effects analyses
###########################################################

###Load the libraries (not all are necessary for this example but I use them often enough)
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


###Read in the lead data from the website.
leaddatalong <- read.csv("/Users/nichole/Dropbox/Teaching/Bios6643/Fall2023/Week3/leaddatalong.csv")


####Fit several different var-cov structures for a marginal analysis and evaluate

##1. Fit a multiple linear regression with no correlation assumed

lr.fit<-lm(leadlevel ~ as.factor(week) + as.factor(Group) + as.factor(week):as.factor(Group), data=leaddatalong)
lr.summary <- summary(lr.fit)

##2. Fit a mixed effects model with a no random effects and an unstructured var-cov

lmm.un.fit<-gls(leadlevel~as.factor(week) + as.factor(Group) + as.factor(week):as.factor(Group), 
                    data=leaddatalong, correlation = corSymm(form = ~ 1 | ID))
lmm.un.summary <- summary(lmm.un.fit)

##3. Fit a mixed effects model with a compound symmetry structure

lmm.cs.fit <- gls(leadlevel~as.factor(week) + as.factor(Group) + as.factor(week):as.factor(Group), 
                  data=leaddatalong, correlation = corCompSymm(form = ~ 1 | ID))
lmm.cs.summary <- summary(lmm.cs.fit)

##4. Fit a mixed effects model with a no random effects and an unstructured var-cov

lmm.un.fit.ml<-gls(leadlevel~as.factor(week) + as.factor(Group) + as.factor(week):as.factor(Group), 
                data=leaddatalong, correlation = corSymm(form = ~ 1 | ID), method = "ML")
lmm.un.ml.summary <- summary(lmm.un.fit.ml)

##5. Fit a mixed effects model with a compound symmetry structure

lmm.cs.fit.ml <- gls(leadlevel~as.factor(week) + as.factor(Group) + as.factor(week):as.factor(Group), 
                  data=leaddatalong, correlation = corCompSymm(form = ~ 1 | ID), method = "ML")
lmm.cs.summary <- summary(lmm.cs.fit.ml)