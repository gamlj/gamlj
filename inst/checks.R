library(lmerTest)
library(lme4)
dat<-read.csv("data/data3by2.csv")
dat$twogroups<-factor(dat$twogroups)
dat$threegroups<-factor(dat$threegroups)
contrasts(dat$twogroups)<-contr.sum(2)
contrasts(dat$threegroups)<-contr.sum(3)

model<-lm(y~threegroups*twogroups,data=dat)
confint(model,level = 0.95)

dat<-read.csv("data/dat3x2x2_mixed.csv")
dat$wfac<-factor(dat$wfac)
dat$wfac3<-factor(dat$wfac3)
contrasts(dat$wfac)<-contr.sum(2)
contrasts(dat$wfac3)<-contr.sum(3)
r.squared(model1)
model1<-lmer(y~(1|cluster)+wfac*wfac3,data=dat,REML = F)
model2<-lmer(y~(1|cluster),data=dat,REML = F)
model<-model2
ss<-summary(model)
eresults<-ss[['coefficients']]
ci<-confint(model,method="Wald")
ci<-ci[!is.na(ci[,1]),]
if (is.null(dim(ci)))
  ci<-matrix(ci,ncol=2)
ci
cbind(eresults,ci)
dim(ci)
dim(eresults)
