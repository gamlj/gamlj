library(lmerTest)
library(lme4)
dat<-read.csv("data/data3by2.csv")
dat$twogroups<-factor(dat$twogroups)
dat$threegroups<-factor(dat$threegroups)
contrasts(dat$twogroups)<-contr.sum(2)
contrasts(dat$threegroups)<-contr.sum(3)
dat$x<-dat$x+100
model<-lm(y~twogroups,data=dat)
summary(model)
tapply(dat$y,dat$twogroups,mean)
tapply(predict(model),dat$twogroups,mean)
model<-lm(y~twogroups*x,data=dat)
summary(model)
tapply(dat$y,dat$twogroups,mean)
tapply(predict(model),dat$twogroups,mean)
lsmeans::lsm(model)

dat<-read.csv2("data/dat3x2x2_mixed.csv")
dat$cluster<-factor(dat$cluster)
dat$wfac<-factor(dat$wfac)
dat$bfac<-factor(dat$bfac)

model<-lmer(y~(1|cluster)+wfac*bfac+x,data=dat)
q<-summary(model)



lsmeans::lsmeans(model, "twogroups")

## model logistic #####

dat<-read.csv("data/generalized.csv")
dat$bfac<-factor(dat$bfac)
dat$dic<-factor(dat$dic)

dat$x<-dat$x-mean(dat$x)+2000
model<-lm(y~bfac*x,data = dat)
model<-lm(y~bfac*dic,data = dat)

lsmeans::lsmeans(model, "bfac")

q<-lsmeans::lsmeans(model,c("bfac"))
x<-print(q)

q<-lsmeans::lsmeans(model,c("bfac"))
ss<-as.data.frame(summary(q))
ss
ss[,-(1:3)]
