
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
# Basic GLM to compare fish ingestion outside vs inside
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
library(scales) # For plotting
# Setwd
setwd(" ")
# read data
dat.raw = read.csv("fish.ingestion.csv")
samples = unique(dat.raw$sample)
samples
results = NULL

# Ensure response is binary
dat.raw$density = ifelse(dat.raw$density>0,1,0)

# fit binomial GLM
fit = glm(density~type,dat.raw,family=binomial)
summary(fit)

# make prediction dataset
pdat = data.frame(type=c(levels(dat.raw$type)))

inv.logit = function(x){exp(x)/(1+exp(x))}
# predict in logit for
pr = predict(fit,pdat,type="link",se.fit = TRUE)
# predict with confidence intervals 
glm.mu= inv.logit(pr$fit)
glm.lcl = inv.logit(pr$fit-1.96*pr$se.fit) 
glm.ucl = inv.logit(pr$fit+1.96*pr$se.fit)  

# Check stats
anova(fit, test="Chisq") # Analysis of Deviance
# results suggest "type" explains a signifant amount of variation in the data
summary(fit)$coef
# The Outside is signifantly (lower prob) different from inside   
glm.mu
cbind(glm.lcl,glm.ucl)  

