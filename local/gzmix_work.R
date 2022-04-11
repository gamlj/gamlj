library(lme4)
library(gamlj)

data<-schoolexam

data$math<-as.numeric(as.character(data$math))
formula = pass ~ 1 + math+( 1+math | school )
model<-glmer(formula,data = data, family=binomial(),)
r.squared(model)

library(gamlj)
data<-schoolexam
mod<-gamlj::gamljGlmMixed(
  formula = pass ~ 1 + math+( 1 | school ),
  data = data)

ranef(mod$model)
coef(mod$model)
fixef(mod$model)

gamlj::gamljGlmMixed(
  formula = pass ~ math+I(math^2)+( 1+math | school ),
  data = data,
  fixedIntercept = F,
  cimethod = "wald")
formula = pass ~ 1  +( 1 | school )
library(lmerTest)
formula = pass ~ math+I(math^2)+( 1 | school )
model<-glmer(formula,data = data, family=binomial(),)
summary(model)
as.character(model@call)[[2]]
class(model)
model@optinfo$conv$lme4$messages


data<-read.csv("extdata/mixlogistic.csv")
head(data)
class(data$emot_probs_binomial)
library(lme4)
names(data)
contrasts(data$Time)<-contr.sum(2)
mod<-glmer(emot_probs_binomial~Time*Group+(1|Participant),family = binomial(),data=data)
summary(mod)
library(emmeans)
emmeans(mod,"Time",type="response")
emmeans(mod,"Group",type="response")
emmeans(mod,c("Group","Time"),type="response")
t<-predict(mod,type="response")
aggregate(t,list(data$Time),mean)
aggregate(t,list(data$Group),mean)
aggregate(t,list(data$Group,data$Time),mean)


form<-y~1+cond+(1|stimulus)
mod<-glmer(form,data=subjects_by_stimuli,family = poisson())

paste(paste(LETTERS[c(7,1,13,12)],collapse =""),paste(letters[10]),sep="")

a<-((length(letters)+2)/4)
c(a,a/a,(a*2)-(a/a),(a*2)-2*(a/a))

a<-factor(1:3)
contrasts(a)<-contr.treatment(3)-1/3

