library(lmerTest)
library(lme4)
dat<-read.csv("data/data3by2.csv")
dat$twogroups<-factor(dat$twogroups)
dat$threegroups<-factor(dat$threegroups)
contrasts(dat$twogroups)<-contr.sum(2)
contrasts(dat$threegroups)<-contr.sum(3)

model<-lm(y~threegroups*twogroups,data=dat)
confint(model,level = 0.95)

dat<-read.csv2("data/dat3x2x2_mixed.csv")


## model logistic #####

dat<-read.csv("data/generalized.csv")
dat$counts<-factor(dat$counts)
jmvcore::toNumeric(dat$counts)
mm<-glm(counts~x,data = dat,family = poisson())
summary(mm)
mm<-glm(dic~x,data = dat,family = binomial(link = "identity"))
str(mm)
glm(dic~x,data = dat,family = "binomial")
mm$family$family
a<-binomial()
a$link
# x<-rnorm(100,0,1)
# w<-rnorm(100,0,1)
# bfac<-replicate(100,sample(2,1))
# bfac<-bfac-1.5
# y<-bfac*x+w+x*w+rnorm(100,0,.7)
# #y<-(y-min(y))/(max(y)-min(y))
# hist(y)
# summary(lm(y~x*w*bfac))
# p<-exp(y)/(1+exp(y))
# dic<-as.numeric(p>.5)
# bfac<-factor(bfac)
# contrasts(bfac)<-contr.sum(2)
# model<-glm(dic~x*w*bfac,family = binomial())
# summary(model)
#x<-x+10
#w<-(w)*13+100
#dat<-cbind(dic,y,w,x,bfac)

#write.csv(dat,"data/logistic.csv",row.names = F)
