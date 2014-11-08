#===============================================================================
# purpose: examples from Robert & Casella (2010; Chapter 1)
# author: tirthankar chakravarty
# created: 8th november 2014
# revised:
# comments:
#   1. Need to find an explanation and a justification of the double bootstrap
# TODO:
#===============================================================================

# splitting a factor in ANOVA
lmChk = lm(weight ~ feed, data = chickwts)
summary(lmChk)

# get a factor label/level mapping
with(chickwts,data.frame(code = seq_along(levels(feed)),
                         levels = levels(feed)))
summary(aov(weight ~ feed, data = chickwts), 
        split = list(feed = list(F1 = 1,
                                 F2 = 2,
                                 F3 = 3,
                                 F4 = 4,
                                 F5 = 5)))

#==========================================================
# exercise 1.7: 
#==========================================================
vX = rgamma(8, shape = 4, scale = 1)
vXBSMeans = replicate(1000, mean(sample(vX, replace = TRUE)))

# sampling distribution of the mean
hist(vXBSMeans, breaks = 100, probability = TRUE)
curve(dnorm(x, mean = mean(vXBSMeans), sd = sd(vXBSMeans)), add = TRUE)
lines(density(vXBSMeans), col = "blue")

# 95th percentile of the sampling distribution of the means
quantile(vXBSMeans, probs = 0.95)

# compute the sampling distribution of the 95th percentile of the 
#   sampling distribution of the means
vBSQuantileOfMeans = replicate(1000, 
                               quantile(
                                 replicate(1000, 
                                           mean(sample(vX, replace = TRUE))
                                 ), 
                                 probs = 0.95)
)

# confidence interval of the sampling distribution of the 95th 
#   percentile of the sampling distribution of the means
quantile(vBSQuantileOfMeans, probs = c(0.05, 0.95))

#==========================================================
# exercise 1.8: 
#==========================================================
