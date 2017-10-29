library(lmerTest)
library(lme4)
dat<-read.csv("data/data3by2.csv")
dat$twogroups<-factor(dat$twogroups)
dat$threegroups<-factor(dat$threegroups)
contrasts(dat$twogroups)<-contr.sum(2)
contrasts(dat$threegroups)<-contr.sum(3)
<<<<<<< HEAD

factors<-c("twogroups","threegroups")
print(factors)
factors<-c("twogroups")
data<-dat[0,]
(levs<-sapply(factors,function(f) levels(data[[f]]),simplify=F))
class(levs)
gg<-expand.grid(levs,stringsAsFactors = F)
ff<-function(a) base::expand.grid(a,stringsAsFactors = F)
ff(levs)
grid<-do.call(expand.grid, as.list(levs))
str(grid)
q<-grid[1, drop=T]
as.list(q)
sapply(colnames(q),function(x) q[x])
model<-lm(y~threegroups*twogroups,data=dat)
confint(model,level = 0.95)
=======
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
>>>>>>> 285d3ad00e924044136b8ca35801c277d1607f31

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
