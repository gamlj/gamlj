library(lmerTest)
library(lme4)
dat<-read.csv("data/data3by2.csv")
dat$twogroups<-factor(dat$twogroups)
dat$threegroups<-factor(dat$threegroups)
contrasts(dat$twogroups)<-contr.sum(2)
contrasts(dat$threegroups)<-contr.sum(3)

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
m2<-glm(dic~x,data=dat,family = binomial())
ss<-summary(m2)
ss$null.deviance
model$convergence

alias(model.matrix(model))
#write.csv(dat,"data/generalized.csv")
#dat$groups3<-cut(dat$y,breaks = 3)
levels(dat$groups3)<-c(1,2,3)
####### multinomial ########à
library(nnet)
names(dat)
dat$q<-dat$bfac
model<-multinom(groups3 ~ x +bfac+ w, data = dat)
a<-function() {
q<-summary(model)
q
}
loadNamespace("nnet")
confint(model)
library(gnm)

gnm(groups3~x,data=dat,family = multinom)

Anova(model,type=3)
model$AIC
sigma(model)
terms(model)
library(car)
model$deviance
vcov(model)
ss<-summary(model)
ss$deviance
model2<-multinom(groups3 ~ 1, data = dat)
model2$deviance

sumr<-summary(model)
rcof<-sumr$coefficients
cof<-as.data.frame(matrix(rcof,ncol=1))
names(cof)<-"estimate"
cof$dep<-rownames(rcof)
cof$variable<-rep(colnames(rcof),each=nrow(rcof))
colnames(cof)<-rep(rownames(rcof),2)
ci<-confint(model)
se<-matrix(sumr$standard.errors,ncol=1)
cof$se<-se
cof$z<-cof$estimate/cof$se
cof$p<-(1 - pnorm(abs(cof$z), 0, 1)) * 2
sumtab<-cof[order(cof$dep),]
cim<-NULL
for (i in seq_len(dim(ci)[3]))
  cim<-rbind(cim,(ci[,,i]))
sumtab$lower<-cim[,1]
sumtab$upper<-cim[,2]
sumtab

