## simulation of the effect for publication

#library(shiny)
library(fabricatr)
library(fmsb)
library(ggplot2)
library(gridExtra)
library(xkcd)
library(xkcdcolors)
library(extrafont)
library(dplyr)



## for debugging
#N <- 84
#rxx <- .1
#bias <- 15


## Create the data
makeMyDat <- function(N, rxx, bias){ # N = sample size, rxx = reliability of Perc measure, bias = percent bias in effect
  Noise <- rnorm(N) # my noise or error variable
  Ab <- rnorm(N) # the true score for ability
  PerAb.Werr <- scale(Ab)*sqrt(rxx) + scale(residuals(lm(Noise~Ab))) * sqrt(1-rxx) # Perception with measurement error ONLY
  PerAb.bi <- Ab + (-1*bias*Ab) # Perception with only bias
  PerAb.biWerr <- PerAb.Werr + (-1*bias*Ab) # Perception but biased with error
  #Ab.quant <- split_quantile(Ab,4) # used to create the quantiles for comparison
  #Ab.perc <- percentile(Ab) # used to compare by percentiles in analysis
  #Ab.thirds <-  split_quantile(Ab,3)
  #PerAb.perc.Werr <- percentile(PerAb.Werr) # Perception percentile with only measurement error
  #PerAb.perc.bi <- percentile(PerAb.bi) # Perception percentile with only bias
  #PerAb.perc.biWerr <- percentile(PerAb.biWerr) # Perception percentile with bias and error
  #Delta.perc.Werr <- PerAb.perc.Werr - Ab.perc # Difference scores between Perceived and True with only meas error
  #Delta.perc.bi <- PerAb.perc.bi - Ab.perc # Difference in percentiles between Per and True with only bias
  #Delta.perc.biWerr <- PerAb.perc.biWerr - Ab.perc # Different in percentiles between Per and True with error and bias
  df.w <- data.frame(id=1:N, # 84
                     Ab=Ab, #raw
                     PerAb.Werr = PerAb.Werr,
                     PerAb.bi = PerAb.bi,
                     PerAb.biWerr = PerAb.biWerr)
  #                   Ability=Ab.perc, # 84
  #                   Perc.Werr=PerAb.perc.Werr, # 84
  #                   Perc.biased=PerAb.perc.bi, #84
  #                   Perc.biasedWerr=PerAb.perc.biWerr, 
  #                   Quants=Ab.quant,
  #                   Thirds=Ab.thirds,
  #                   Delta1=Delta.perc.Werr,
  #                   Delta2=Delta.perc.bi,
  #                   Delta3=Delta.perc.biWerr)
  #df.l <- data.frame(id=rep(1:N,4),
  #                   Quants=c(Ab.quant,Ab.quant,Ab.quant,Ab.quant),
  #                   Measure=gl(4,N,labels=c("Ability","Perc.Werr","Perc.bias","Perc.biasWerr")),
  #                   Percentiles=c(Ab.perc,PerAb.perc.Werr,PerAb.perc.bi,PerAb.perc.biWerr))
  out <- list(N=N,rxx=rxx,bias=bias,df.w=df.w) #,df.l=df.l)
  return(out)
}

# test here
x <- makeMyDat(84,.1,15)
str(x)

#testAb <- mutate(df.w,ntile(as.numeric(Ab),100))
#testAb


## from KD 1999:

# study 1 (humor) selected only those in bottom quartile and did a paired t to
# test whether the "low" performers were overestimating their true or measured
# ability.  Also reported the correlation between measures for the full sample.
# Finally, they reported the top quartile or "experts" and found the negative
# t-score (paired).

## CODE:  use wide format df.w and compute the following:

# mean(Perception); mean(Ability) # tells us the first KD estimate
# t.test(Pair(Ability,Perception)~1, data=df.w)
# t.test(Pair(Ability,Perception)~1, data=subset(df.w,Quants==1)) # first quartile t-test (paired)
# t.test(Pair(Ability,Perception)~1, data=subset(df.w,Quants==4)) # top quartile t-test (paired)


analyzeMyDat <- function(x){
  # first get the parameters and data
  N <- x$N
  rxx <- x$rxx
  bias <- x$bias
  df.w <- x$df.w
  # df.l <- x$df.l
  
  df.w <- df.w %>% mutate(Quants=ntile(Ab,4)) # used to create the quantiles for comparison
  df.w <- df.w %>% mutate(Ability=ntile(Ab,100))
  df.w <- df.w %>% mutate(Perc.Werr=ntile(PerAb.Werr,100)) 
  df.w <- df.w %>% mutate(Perc.biased=ntile(PerAb.bi,100)) 
  df.w <- df.w %>% mutate(Perc.biasedWerr=ntile(PerAb.biWerr,100)) 
  
  ## first the true score stuff
  abX <- mean(as.numeric(df.w$Ability),na.rm=T)
  abSD <- sd(as.numeric(df.w$Ability),na.rm=T) # these four X's and SD's tell us the first KD estimate
  
  ## now the perceptions
  # only error
  percX.err <- mean(as.numeric(df.w$Perc.Werr),na.rm=T)
  percSD.err <- sd(as.numeric(df.w$Perc.Werr),na.rm=T)
  # only bias
  percX.bi <- mean(as.numeric(df.w$Perc.biased),na.rm=T)
  percSD.bi <- sd(as.numeric(df.w$Perc.biased),na.rm=T)
  # bias + error
  percX.biWerr <- mean(as.numeric(df.w$Perc.biasedWerr),na.rm=T)
  percSD.biWerr <- sd(as.numeric(df.w$Perc.biasedWerr),na.rm=T)
  
  ## T-tests
  # Omnibus tests
  t.omni.err.t <- t.test(Pair(Ability,Perc.Werr)~1, data=df.w)$statistic
  t.omni.err.p <- t.test(Pair(Ability,Perc.Werr)~1, data=df.w)$p.value
  t.omni.biased.t <- t.test(Pair(Ability,Perc.biased)~1, data=df.w)$statistic
  t.omni.biased.p <- t.test(Pair(Ability,Perc.biased)~1, data=df.w)$p.value
  t.omni.biasedWerr.t <- t.test(Pair(Ability,Perc.biasedWerr)~1, data=df.w)$statistic
  t.omni.biasedWerr.p <- t.test(Pair(Ability,Perc.biasedWerr)~1, data=df.w)$p.value
  
  # Lowest performers
  t.low.err.t <- t.test(Pair(Ability,Perc.Werr)~1, data=subset(df.w,Quants==1))$statistic # first quartile t-test (paired)
  t.low.err.p <- t.test(Pair(Ability,Perc.Werr)~1, data=subset(df.w,Quants==1))$p.value # first quartile t-test (paired)
  t.low.biased.t <- t.test(Pair(Ability,Perc.biased)~1, data=subset(df.w,Quants==1))$statistic # first quartile t-test (paired)
  t.low.biased.p <- t.test(Pair(Ability,Perc.biased)~1, data=subset(df.w,Quants==1))$p.value # first quartile t-test (paired)
  t.low.biasedWerr.t <- t.test(Pair(Ability,Perc.biasedWerr)~1, data=subset(df.w,Quants==1))$statistic # first quartile t-test (paired)
  t.low.biasedWerr.p <- t.test(Pair(Ability,Perc.biasedWerr)~1, data=subset(df.w,Quants==1))$p.value # first quartile t-test (paired)
  
  # Highest performers
  t.hi.err.t <- t.test(Pair(Ability,Perc.Werr)~1, data=subset(df.w,Quants==1))$statistic # first quartile t-test (paired)
  t.hi.err.p <- t.test(Pair(Ability,Perc.Werr)~1, data=subset(df.w,Quants==1))$p.value # first quartile t-test (paired)
  t.hi.biased.t <- t.test(Pair(Ability,Perc.biased)~1, data=subset(df.w,Quants==1))$statistic # first quartile t-test (paired)
  t.hi.biased.p <- t.test(Pair(Ability,Perc.biased)~1, data=subset(df.w,Quants==1))$p.value # first quartile t-test (paired)
  t.hi.biasedWerr.t <- t.test(Pair(Ability,Perc.biasedWerr)~1, data=subset(df.w,Quants==1))$statistic # first quartile t-test (paired)
  t.hi.biasedWerr.p <- t.test(Pair(Ability,Perc.biasedWerr)~1, data=subset(df.w,Quants==1))$p.value # first quartile t-test (paired)
  
  out <- c(N,rxx,bias,
           abX,
           abSD,
           percX.err,
           percSD.err,
           percX.bi,
           percSD.bi,
           percX.biWerr,
           percSD.biWerr,
           t.omni.err.t,
           t.omni.err.p,
           t.omni.biased.t,
           t.omni.biased.p,
           t.omni.biasedWerr.t,
           t.omni.biasedWerr.p,
           t.low.err.t,
           t.low.err.p,
           t.low.biased.t,
           t.low.biased.p,
           t.low.biasedWerr.t,
           t.low.biasedWerr.p,
           t.hi.err.t,
           t.hi.err.p,
           t.hi.biased.t,
           t.hi.biased.p,
           t.hi.biasedWerr.t,
           t.hi.biasedWerr.p
           )
  out <- as.numeric(out)
  return(out)
}

## test
test <- analyzeMyDat(makeMyDat(N=84,rxx=.1,bias=15))
#as.numeric(test)
str(test)
test

outMe <- rep(NA,29)
outMe <- rbind(outMe,analyzeMyDat(makeMyDat(N,rxx,bias)))

## run the full spectrum of models
for (N in seq(80,250,by=10)){
  for (rxx in seq(.1,.95,by=.05)){
    for (bias in seq(1,30,by=1)){
      outMe <- rbind(outMe,analyzeMyDat(makeMyDat(N,rxx,bias)))
    }
  }
}

str(outMe)
finalData <- as.data.frame(outMe[-1,])
str(finalData)
names(finalData) <- c("N","rxx","bias",
                      "abX",
                      "abSD",
                      "percX.err",
                      "percSD.err",
                      "percX.bi",
                      "percSD.bi",
                      "percX.biWerr",
                      "percSD.biWerr",
                      "t.omni.err.t",
                      "t.omni.err.p",
                      "t.omni.biased.t",
                      "t.omni.biased.p",
                      "t.omni.biasedWerr.t",
                      "t.omni.biasedWerr.p",
                      "t.low.err.t",
                      "t.low.err.p",
                      "t.low.biased.t",
                      "t.low.biased.p",
                      "t.low.biasedWerr.t",
                      "t.low.biasedWerr.p",
                      "t.hi.err.t",
                      "t.hi.err.p",
                      "t.hi.biased.t",
                      "t.hi.biased.p",
                      "t.hi.biasedWerr.t",
                      "t.hi.biasedWerr.p")
str(finalData)
write.csv(finalData,file = "SimulationData.csv",row.names = F)

#### Now the secondary analysis

## first, focus on our point...the low with just error (look at the p values only)
lm1 <- lm(t.low.err.p~N*rxx*bias,data=finalData)
summary(lm1) ## 30% of the variance in p-values associated with error only (no bias in the results)

## how about for the biased results
lm2 <- lm(t.low.biased.p~N*rxx*bias,data=finalData)
summary(lm2) ## less than 1% of the variance in p-values associated with bias alone

## now for both
lm3 <- lm(t.low.biasedWerr.p~N*rxx*bias,data=finalData)
summary(lm3) ## 10% of the variance associated with both for p-values

######## WINNER:  measurement error
######## LOWER:  bias

## note:  simulation above is not fully crossed

##################### DO OVER ---------------------------

makeMyDat2 <- function(N, rxx, bias){ # N = sample size, rxx = reliability of Perc measure, bias = percent bias in effect
  Noise <- rnorm(N) # my noise or error variable
  Ab <- rnorm(N) # the true score for ability
  Perc <- scale(Ab)*sqrt(rxx) + (scale(residuals(lm(Noise~Ab))) * sqrt(1-rxx)) - (bias/100*scale(Ab)) # Perception with measurement error ONLY
  df.w <- data.frame(id=1:N, # 84
                     Ab=Ab, #raw
                     Perc=Perc)
  out <- list(N=N,rxx=rxx,bias=bias,df.w=df.w) #,df.l=df.l)
  return(out)
}

analyzeMyDat2 <- function(x){
  N <- x$N
  rxx <- x$rxx
  bias <- x$bias
  df.w <- x$df.w
  
  df.w <- df.w %>% mutate(Quants=ntile(Ab,4)) # used to create the quantiles for comparison
  df.w <- df.w %>% mutate(Ability=ntile(Ab,100))
  df.w <- df.w %>% mutate(Perception=ntile(Perc,100)) 

  ## first the true score stuff
  AbilityX <- mean(as.numeric(df.w$Ability),na.rm=T)
  AbilitySD <- sd(as.numeric(df.w$Ability),na.rm=T) # these four X's and SD's tell us the first KD estimate
  ## now the perceptions
  PerceptionX <- mean(as.numeric(df.w$Perception),na.rm=T)
  PerceptionSD <- sd(as.numeric(df.w$Perception),na.rm=T)
  ## T-tests
  # Omnibus tests
  t.omni.t <- t.test(Pair(Ability,Perception)~1, data=df.w)$statistic
  t.omni.p <- t.test(Pair(Ability,Perception)~1, data=df.w)$p.value
  # Lowest performers
  t.low.t <- t.test(Pair(Ability,Perception)~1, data=subset(df.w,Quants==1))$statistic # first quartile t-test (paired)
  t.low.p <- t.test(Pair(Ability,Perception)~1, data=subset(df.w,Quants==1))$p.value # first quartile t-test (paired)
  # Highest performers
  t.hi.t <- t.test(Pair(Ability,Perception)~1, data=subset(df.w,Quants==1))$statistic # first quartile t-test (paired)
  t.hi.p <- t.test(Pair(Ability,Perception)~1, data=subset(df.w,Quants==1))$p.value # first quartile t-test (paired)
  
  out <- c(N,rxx,bias,
           AbilityX,
           AbilitySD,
           PerceptionX,
           PerceptionSD,
           t.omni.t,
           t.omni.p,
           t.low.t,
           t.low.p,
           t.hi.t,
           t.hi.p)
  out <- as.numeric(out)
  return(out)
}

outMe2 <- rep(NA,13)

## run the full spectrum of models fully crossed now
for (N in seq(40,280,by=10)){
  for (rxx in seq(0,1,by=.05)){
    for (bias in seq(0,100,by=10)){
      outMe2 <- rbind(outMe2,analyzeMyDat2(makeMyDat2(N,rxx,bias)))
    }
  }
}

str(outMe2)
finalData2 <- as.data.frame(outMe2[-1,])
str(finalData2)
names(finalData2) <- c("N","rxx","bias",
                      "AbilityX",
                      "AbilitySD",
                      "PerceptionX",
                      "PerceptionSD",
                      "t.omni.t",
                      "t.omni.p",
                      "t.low.t",
                      "t.low.p",
                      "t.hi.t",
                      "t.hi.p")
str(finalData2)
write.csv(finalData2,file = "SimulationData2-FC.csv",row.names = F)

source("https://raw.githubusercontent.com/pem725/MRES/master/R/gtheory.R")

str(finalData2)
finalData2$N.f <- as.factor(finalData2$N)
finalData2$rxx.f <- as.factor(finalData2$rxx)
finalData2$bias.f <- as.factor(finalData2$bias)
#aov1 <- aov(t.low.p~N.f*rxx.f*bias.f,data=finalData2)
#summary(aov1)

dk.gt1 <- gtheory(t.omni.p~N.f*rxx.f*bias.f,finalData2)
summary(dk.gt1)
# Generalizability Theory Results: 
#   
#   
#   ANOVA Table Summary: 
#   
#   Df  Sum Sq Mean Sq
# N.f                24 1.0e-26 3.7e-28
# rxx.f              20 1.0e-26 3.6e-28
# bias.f             10 0.0e+00 3.7e-28
# N.f:rxx.f         480 1.7e-25 3.6e-28
# N.f:bias.f        240 9.0e-26 3.7e-28
# rxx.f:bias.f      190 7.0e-26 3.7e-28
# N.f:rxx.f:bias.f 4560 1.7e-24 3.7e-28
# 250 observations deleted due to missingness
# 
# G-Study Results: 
#   
#                    df MS Var Comp Percent Var G Coef
# N.f                24  0        0           0   0.00
# rxx.f              20  0        0           0   0.00
# bias.f             10  0        0           0   0.00
# N.f:rxx.f         480  0        0           0   0.00
# N.f:bias.f        240  0        0           0   0.00
# rxx.f:bias.f      190  0        0           0   0.00
# N.f:rxx.f:bias.f 4560  0        0         100   0.96

dk.gt2 <- gtheory(t.low.p~N.f*rxx.f*bias.f,finalData2)
summary(dk.gt2)

## for H2 from KD-1999 - does the p-value differ by these facets and by how much?

# Generalizability Theory Results: 
#   
#   
#   ANOVA Table Summary: 
#   
#   Df Sum Sq Mean Sq
# N.f                24    1.4   0.059
# rxx.f              20    3.1   0.153
# bias.f             10    1.0   0.098
# N.f:rxx.f         480    2.8   0.006
# N.f:bias.f        240    1.4   0.006
# rxx.f:bias.f      190    3.0   0.016
# N.f:rxx.f:bias.f 4560    6.7   0.001
# 250 observations deleted due to missingness
# 
# G-Study Results: 
#   
#   df   MS Var Comp Percent Var G Coef
# N.f                24 0.06        0           6   0.78
# rxx.f              20 0.15        0          14   0.89
# bias.f             10 0.10        0           4   0.72
# N.f:rxx.f         480 0.01        0          12   0.87
# N.f:bias.f        240 0.01        0           6   0.78
# rxx.f:bias.f      190 0.02        0          16   0.91
# N.f:rxx.f:bias.f 4560 0.00        0          42   0.96


### for shits and grins, let's find out what it takes to get a significant H2 effect

## change the p-value to be zero at .05

finalData2$h2.p0 <- finalData2$t.low.p - .05
finalData2$h2.sig <- 0
finalData2$h2.sig[finalData2$t.low.p < .05] <- 1
mean(finalData2$h2.sig)

lm.h2.p0 <- lm(h2.p0~N*rxx*bias,data=finalData2)
summary(lm.h2.p0)
#install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")
require(ggiraph)
require(ggiraphExtra)
require(plyr)
library(ggfortify)

ggPredict(lm.h2.p0,se=T, interactive=T, digits=2)
autoplot(lm.h2.p0)
## not helpful given the complexity of interpreting small p-values

### logistic regression on reject H2 - given that the decision is to reject or fail to reject
lm.h2.sig <- glm(h2.sig~N+rxx+bias,family=binomial(link="logit"),data=finalData2)
summary(lm.h2.sig)
ggPredict(lm.h2.sig, se=T, interactive=T, digits=2)
autoplot(lm.h2.sig)
