library(lmerTest)
library(lme4)
dat<-read.csv("data/data3by2.csv")
dat$twogroups<-factor(dat$twogroups)
dat$threegroups<-factor(dat$threegroups)
contrasts(dat$twogroups)<-contr.sum(2)
contrasts(dat$threegroups)<-contr.sum(3)
model<-lm(y~`two groups`+threegroups+`two groups`:threegroups+x*`two groups`,data=dat)
#### mixed ###########
library(lmerTest)
dat<-read.csv("data/dat3x2x2_mixed.csv")
dat$cluster<-factor(dat$cluster)
dat$wfac<-factor(dat$wfac)
dat$bfac<-factor(dat$bfac)
contrasts(dat$wfac)<-contr.sum(2)
model<-lmer(y~(1+x|cluster)+x,data=dat)
summary(model)


################ count data poisson ############à

dat<-read.csv("data/generalized.csv")
names(dat)
dat$bfac<-factor(dat$bfac)
dat$dic<-factor(dat$dic)
contrasts(dat$dic)<-contr.sum(2)
contrasts(dat$bfac)<-contr.sum(2)
dat$groups3<-factor(dat$groups3)
contrasts(dat$groups3)<-contr.sum(3)
table(dat$counts)
library(jmvcore)
library(emmeans )

model<-glm(counts~x,data=dat,family = poisson(link = "log"))
summary(model)
car::Anova(model,type=3)

##### perfect poisson #####
 n <- 10
   #regression coefficients
 beta0 <- 1
 beta1 <- 0.2
 #generate covariate values
 x <- runif(n=n, min=0, max=1.5)
 #compute mu's
 mu <- exp(beta0 + beta1 * x)
   #generate Y-values
 y <- rpois(n=n, lambda=mu)
   #data set
 data <- data.frame(y=y, x=x)

model<-glm(y~x,data=data,family = poisson())

library(tweedie)
library(statmod)

model3<-glm(counts~x,data=dat,family =tweedie(var.power = 1))
summary(model3)$dispersion

AICtweedie(model,dispersion = 1)

nobs(model)
summary(model,dispersion = 1)
car::Anova(model,type=3)
emmeans(model,~bfac)
library(MASS)
model0<-glm(counts~x,data=dat,family =poisson())
summary(model0)
AIC(model0)
AIC(model1)

model1<-glm.nb(counts~x,data=dat,control=glm.control(maxit = 500))
summary(model1)
vuong(model0,model1)

sd(model$residuals)
class(model)
summary(model)$dispersion
summary.glm()
model<-glm.nb(counts~w,data=dat)
mean(dat$counts)
sd(dat$counts)
model$th.warn


class(model)
model$theta
summary(model)
car::Anova(model,type=3)
mi.dispersion(model)

model0<-glm(dic~bfac+x+w,data=dat,family = binomial())
summary(model)

model1<-glm(dic~bfac+x+w,data=dat,family = binomial("probit"))
summary(model)

q<-cbind(coef(model0),coef(model1))
cbind(exp(q[,1]),coef(model1))

term=c("bfac")
plotsName<-NULL
mm<-emmeans::emmeans(model,term,by = plotsName,cov.reduce=function(a) pretty(a))

model0<-glm(counts~1,data=dat,family = poisson())
r<-logLik(model0)
1-(f/r)
summary(model)


dat<-read.csv("data/generalized.csv")
dat$bfac<-factor(dat$bfac)
dat$dic<-factor(dat$dic)
contrasts(dat$dic)<-contr.sum(2)
contrasts(dat$bfac)<-contr.sum(2)
dat$groups3<-factor(dat$groups3)
model<-glm(counts~groups3,data=dat,family = gaussian())
model$df.residual


####### multinomial ########
dat<-read.csv("data/generalized.csv")
dat$bfac<-factor(dat$bfac)
dat$dic<-factor(dat$dic)
contrasts(dat$dic)<-contr.sum(2)
contrasts(dat$bfac)<-contr.sum(2)
dat$groups3<-factor(dat$groups3)


library(nnet)
library(emmeans)
names(dat)
contrasts(dat$groups3)

model<-multinom(groups3 ~dic+x+w, data = dat, model = TRUE)
model$residuals
model$deviance
model$edf
summary(model)
term<-c("groups3", "dic")
plotsName<-NULL
FUN<-function(x) c(mean(x)-sd(x),mean(x)+sd(x))
mm<-emmeans(model,term,by = plotsName,cov.reduce=FUN,type="response")
mm

qq<-emmeans::ref_grid(model,term)
qq
qq@grid
summary(mm)
dd<-as.data.frame(ss)
simage<-list(state=list(data=dd))

term<-"bfac"
dep<-"groups3"
terms<-paste(term,collapse = ":")
tterm<-as.formula(paste("~",paste(dep,terms,sep = "|")))  
table<-emmeans::emmeans(model,tterm,transform = "response",data=dat)
table
model$
model0$value
summary(model0)
library(lmerTest)
dat<-read.csv("data/beers_bars.csv")
plot(dat$smile~dat$beer)
lmodel<-lm(smile~beer,data=dat)
summary(lmodel)
model<-lmer(smile~(1|bar)+beer,data=dat)



dat<-read.csv("data/facXcont.csv")
dat$x<-dat$x-mean(dat$x)
#dat$y<-dat$x*(dat$fac3)+.5*dat$x*dat$x+rnorm(30,0,.5)
dat$fac3<-factor(dat$fac3)
dat$x2<-dat$x*dat$x
write.csv(dat,"data/facXcont.csv",row.names = F)
contrasts(dat$fac3)<-contr.sum(3)

model<-lm(y~x*x2,data=dat)
plot(predict(model)~dat$x)
summary(model)
model<-lm(y~x,data=dat)
summary(model)
contrasts(dat$fac3)

########## gmlm #####################
library(lmerTest)
library(emmeans)
hdp <- read.csv("https://stats.idre.ucla.edu/stat/data/hdp.csv")
hdp <- within(hdp, {
  Married <- factor(Married, levels = 0:1, labels = c("no", "yes"))
  DID <- factor(DID)
  HID <- factor(HID)
})
head(hdp)
m <- glmer(remission ~ IL6 + CRP + CancerStage + LengthofStay + Experience +
             (1 | DID), data = hdp, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
m <- glmer(remission ~ CancerStage + (1 | DID), data = hdp, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)

summary(m)
emmeans(m,~CancerStage)
car::Anova(m,type=3)
emm_s.t <- emmeans(m, ~ CancerStage, contr = "poly")
test(emm_s.t,join=T)
confint(m)
class(m)


a<-1:100
b<--10:34
pretty(c(a,b))



