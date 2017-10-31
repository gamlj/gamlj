library(lmerTest)
library(lme4)
dat<-read.csv("data/data3by2.csv")
dat$twogroups<-factor(dat$twogroups)
dat$threegroups<-factor(dat$threegroups)
contrasts(dat$twogroups)<-contr.sum(2)
contrasts(dat$threegroups)<-contr.sum(3)

model<-lm(y~twogroups+x+twogroups:x,data=dat)
lf.simpleEffects(model,"twogroups","x")

dat<-read.csv2("data/dat3x2x2_mixed.csv")
dat$cluster<-factor(dat$cluster)
dat$wfac<-factor(dat$wfac)
dat$bfac<-factor(dat$bfac)
library(lmerTest)
model<-lmer(y~(1|cluster)+wfac*bfac*x,data=dat)
#lf.simpleEffects(model,"x","wfac")
mf.confint(model,level=0.95)

## model logistic #####

dat<-read.csv("data/generalized.csv")
dat$bfac<-factor(dat$bfac)
dat$dic<-factor(dat$dic)

model<-glm(counts~x*bfac,data=dat,family = poisson())
mf.confint(model,level=0.95)
lf.meansTables(model,"bfac")

qq<-lf.simpleEffects(model,"bfac","x")
a<-qq[[1]]
class(a)
a$estimate

####### multinomial ########à
library(nnet)
names(dat)
dat$groups3<-factor(group3)
model<-multinom(groups3 ~bfac*x, data = dat, model = TRUE)
lf.simpleEffects(model,"bfac","x")
#mf.confint(model,level=0.95)
terms(model)
