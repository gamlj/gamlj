library(lmerTest)
library(lme4)
dat<-read.csv("data/data3by2.csv")
dat$twogroups<-factor(dat$twogroups)
dat$threegroups<-factor(dat$threegroups)
contrasts(dat$twogroups)<-contr.sum(2)
contrasts(dat$threegroups)<-contr.sum(3)

model<-lm(y~twogroups+x+twogroups:x,data=dat)

dat<-read.csv2("data/dat3x2x2_mixed.csv")
dat$cluster<-factor(dat$cluster)
dat$wfac<-factor(dat$wfac)
dat$bfac<-factor(dat$bfac)
model<-lmer(y~(1|cluster)+wfac*bfac*x,data=dat)
coef(model)
p <- ggplot(dat, aes(x = bfac, y = y, group=cluster, colour = cluster)) +
  geom_point(size=3,alpha=.2) +
  geom_line(aes(y = predict(model),x=bfac,group=cluster),size=1.01) 
p<-p+ geom_smooth(aes(group = cluster), size = 2, method = "lm")
print(p)

dat<-read.csv("data/generalized.csv")
dat$bfac<-factor(dat$bfac)
dat$dic<-factor(dat$dic)
contrasts(dat$dic)<-contr.sum(2)
contrasts(dat$bfac)<-contr.sum(2)
dat$groups3<-factor(dat$groups3)
model<-glm(counts~groups3*bfac*dic,data=dat,family = poisson())

####### multinomial ########à
library(nnet)
library(lsmeans)
names(dat)
dat$groups3<-factor(dat$group3)
contrasts(dat$groups3)
model<-multinom(groups3 ~bfac*dic, data = dat, model = TRUE)

library(lmerTest)
dat<-read.csv("data/beers_bars.csv")
plot(dat$smile~dat$beer)
lmodel<-lm(smile~beer,data=dat)
summary(lmodel)
model<-lmer(smile~(1|bar)+beer,data=dat)
summary(model)
coef(model)
ranef(model)
plot(predict(model)~dat$beer)
ggplot(dat)
library(ggplot2)

p <- ggplot(dat, aes(x = beer, y = smile)) +
  geom_point(size=3) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black"))
#  geom_line(aes(y = predict(model)),size=1) 
print(p)

p <- ggplot(dat, aes(x = beer, y = smile, colour = bar)) +
  geom_point(size=3) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  geom_line(aes(y = predict(model)),size=1) 
print(p)


dat<-read.csv("data/exercise.csv")
summary(dat)
dat
