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
dat<-read.csv2("problems/Exp2.csv")
dat$Group..1tennis..2calcio.
dat$`group (male=1?)`<-1
names(dat)
jmvcore::toB64(names(dat))
jmvcore::constructFormula("(dd /",list("ciao () ciao","dla"))
model<-lm(Delta~`group (male=?)`,data=dat)
mt<-terms(model)
an<-names(attr(mt,"dataClass"))[2]
dat[,an]
attributes(model)
make.names(names(dat))
names(dat)
dat<-read.csv("data/exercise.csv")
dat$
summary(dat)
summary(lm(yendu~xage*zexer,data = dat))

library(foreign)
dat<-read.spss("data/bambini_aggressivi.sav",to.data.frame = T)
dat2<-data.frame(1:length(dat$id))
dat2$counts<-dat$numero_atti
dat2$agg_test<-dat$test_aggress
dat2$age<-dat$eta
dat2$X1.length.dat.id.<-NULL
write.csv(dat2,"data/aggression_test.csv",row.names = F)
dat<-read.csv("data/aggression_test.csv")
dat$age<-factor(dat$age)
contrasts(dat$age)<-contr.sum(3)
model<-glm(counts~agg_test*age,data = dat,family = poisson())
summary(model)

test<-round(rnorm(100,15,2),digits = 2)

y<-(test+rnorm(100,0,1))/5
summary(y)
ey<-exp(y)
hist(y)

hist(ey)
p<-(test-min(test))/(max(test)-min(test))
counts<-sapply(p, function(a) rpois(1,a))
cor(counts,p)
hist(counts)
model<-glm(counts~test,family = poisson())
summary(model)

