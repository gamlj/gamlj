data<-schoolexam
gmod<-gamlj::gamljGlmMixed(
  formula = pass ~ math+( 1 | school ),
  data = data,
  fixedIntercept = T,
  cimethod = "wald")

mod<-gamlj_model(gmod)
class(mod)<-"lmerMod"
rand(mod)

pp<-profile(mod,which="theta_",optimizer=mod@optinfo$optimizer,prof.scale="varcov")
confint(pp,parm = "theta_",level = .95)

formula = pass ~ 1  +( 1 | school )
library(lmerTest)
formula = pass ~ math+I(math^2)+( 1 | school )
model<-glmer(formula,data = data, family=binomial(),)
summary(model)
as.character(model@call)[[2]]
class(model)
model@optinfo$conv$lme4$messages
lmerTest::ranova

N<-1000
data<-data.frame(id=1:N)
data$y<-rnorm(N)
data$x<-rnorm(N)
data$id<-rep(1:100,N/100)

gamlj::gamljMixed(
  formula = y ~ x+(1|id),
  data = data,
  normTest =  T)

