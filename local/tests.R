library(foreign)
library(lme4)

data<-read.csv("extdata/generalized.csv")

names(data)
data$fac3<-factor(rbinom(nrow(data),3,.5))
table(data$fac3)
library(nnet)
.model<-multinom(groups3~fac3*x,data=data)
sumy<-summary(.model)
.model$AIC
extractAIC(.model)
cf<-sumy$coefficients[,variable]
se<-sumy$standard.errors[,variable]
ci<-confint(.model)
cilabs<-colnames(ci)
ci<-sapply(seq_along(levs), function(i) ci[,,i][variable,])
result<-cbind(cf,se,ci)
colnames(result)[c(3,4)]<-cilabs
result
data$bfac<-factor(data$bfac)
head(data)
gamlj::gamljGzlm(modelSelection = "multinomial",formula=groups3~fac3+x+w+fac3:x,data=data,
                simpleVariable = "bfac",
                simpleModerator = "x",
                simple3way = "w")

library(foreign)
data<-read.dta("https://stats.idre.ucla.edu/stat/stata/faq/eyestudy.dta")
data

table(data$carrot,data$lenses)
data$lenses<-factor(data$lenses)
data$carrot<-factor(data$carrot)
contrasts(data$carrot)<-contr.treatment(2,base=2)
contrasts(data$lenses)<--1*contr.sum(2)/2

mo<-glm(lenses~carrot,data=data,family = binomial())
summary(mo)

mo<-glm(lenses~carrot,data=data,family = binomial(link="log"))
summary(mo)
library(sandwich)
data$lenses<-as.numeric(data$lenses)
data$lenses<-data$lenses-1
pois.reg <- glm(lenses~carrot, family = poisson(link = "log"), data = data)
summary(pois.reg)
library(lmtest)
confint(coeftest(pois.reg, vcov = sandwich))
library(geepack)
data$id<-seq_len(dim(data)[1])
gmod<-geeglm(lenses~carrot, family = poisson(link = "log"), id = id, corstr = "exchangeable", data = data)
summ<-summary(gmod)
pp<-qnorm(0.05 / 2)

summ$coefficients$upper<-summ$coefficients$Estimate+ pp * summ$coefficients$Std.err
summ$coefficients$lower<-summ$coefficients$Estimate- pp * summ$coefficients$Std.err

data<-read.csv("../gamlj_docsource/data/neuralgia.csv")
mo<-glm(Pain~Treatment*Sex,data=data,family = binomial(link="log"))
summary(mo)
pain<-as.numeric(data$Pain)-1
pois.reg <- glm(pain~Treatment*Sex, family = poisson(link = "log"), data = data)
summary(pois.reg)
confint(coeftest(pois.reg, vcov = sandwich))
data$id_id_id<-seq_len(dim(data)[1])
gmod<-geeglm(pain~1+Treatment*Sex, family = poisson(link = "log"), id = id_id_id, corstr = "exchangeable", data = data)
summ<-summary(gmod)

pp<-qnorm(0.05 / 2)

summ$coefficients$upper<-summ$coefficients$Estimate+ pp * summ$coefficients$Std.err
summ$coefficients$lower<-summ$coefficients$Estimate- pp * summ$coefficients$Std.err

library(lme4)
data<-read.csv("../gamlj_docsource/data/temptime.csv")
data$cond<-factor(data$cond)
data$subj<-factor(data$subj)
contrasts(data$cond)<-contr.sum(2)
library(lmerTest)
mod<-lmer(y~(1|subj)+cond+timebin,data=data,REML = T)
mod@devcomp
extractAIC(mod)
summary(mod)
sd(data$y)

gamlj::gamljMixed(
  formula = y ~ 1 + cond+( 1 | subj ),
  data = data,
  plotSepLines = cond,
  plotRandomEffects = TRUE)

library(lme4)
library(foreign)
library(jtools)
data<-read.spss("../exdata/zaslan.sav")


data$task_order<-factor(data$task_order)
contrasts(data$task_order)<-contr.sum(2)/2
data$target_intensity<-factor(data$target_intensity)
contrasts(data$target_intensity)<--1*contr.sum(2)/2
data$language<-factor(data$language)
contrasts(data$language)<--1*contr.sum(2)/2

data$z_PPVT<-data$PPVT.Score-mean(data$PPVT.Score)
data$z_AoA<-data$AoA-mean(data$AoA)

data$subject<-factor(data$subject)
data$face<-factor(data$face)
mod<-lmer(response_time ~ 1 + language + target_intensity + task_order + z_AoA +z_PPVT 
          + language:target_intensity + language:z_AoA + target_intensity:z_AoA + language:z_PPVT + target_intensity:z_PPVT + language:task_order + language:target_intensity:z_AoA + language:target_intensity:z_PPVT+( 1 | subject )+( 1 | face ),data=data)


summary(mod)
interact_plot(mod, pred = z_AoA, modx = language, interval = TRUE,
              colors = "Set2", x.label = "Age of Acquisition", y.label = "Reaction Time (msecs)",
              legend.main = "Language")

contrasts(data$language)<-contr.treatment(2,1)
mod<-lmer(response_time ~ target_intensity+language+task_order
          +target_intensity*language+task_order*language
          +z_AoA+z_PPVT
          +target_intensity*z_AoA+language*z_AoA
          +target_intensity*z_PPVT+language*z_PPVT
          +language*target_intensity*z_AoA+language*target_intensity*z_PPVT
          +(1|subject)+(1|face),data=data)
summary(mod)
contrasts(data$language)<-contr.treatment(2,2)
mod<-lmer(response_time ~ target_intensity+language+task_order
          +target_intensity*language+task_order*language
          +z_AoA+z_PPVT
          +target_intensity*z_AoA+language*z_AoA
          +target_intensity*z_PPVT+language*z_PPVT
          +language*target_intensity*z_AoA+language*target_intensity*z_PPVT
          +(1|subject)+(1|face),data=data)
summary(mod)


data$language
devtools::install_github("jacob-long/interactions")
interact_plot(mod, pred = z_AoA, modx = language, interval = TRUE,
              modx.values=c("ENG","TR "),
              colors = "Set2", x.label = "Age of Acquisition", y.label = "Reaction Time (msecs)",
              legend.main = "Language")



mod1<-lmer(response_time ~ target_intensity+language*target_intensity+ target_intensity*language
           + z_AoA+z_AoA*language+z_AoA*target_intensity
           +(1|subject)+(1|face),data=data)

interact_plot(mod1, pred = z_AoA, modx = language, interval = TRUE,
              modx.values=c("ENG","TR "),
              colors = "Set2", x.label = "Age of Acquisition", y.label = "Reaction Time (msecs)",
              legend.main = "Language")

mod2<-lmer(response_time ~ task_order*target_intensity+language*target_intensity+ target_intensity*language
           + z_AoA+z_AoA*language+z_AoA*target_intensity
           +  language*target_intensity*z_AoA
           +(1|subject)+(1|face),data=data)

interact_plot(mod2, pred = z_AoA, modx = language, interval = TRUE,
              modx.values=c("ENG","TR "),
              colors = "Set2", x.label = "Age of Acquisition", y.label = "Reaction Time (msecs)",
              legend.main = "Language")

pred<-predict(mod2,re.form=~0)
plot(pred~data$z_AoA,col=data$language)

mod<-glm(language~1,data=data,family = binomial())
mod<-stats::glm(language~z_AoA,data=data,family = binomial("log"))

family(mod)

summary(mod)
a<-mf.confint(mod,level = .95)
if (is.null(dim(a)))
  a<-t(as.matrix(a))
colnames(a)<-c("a","b")
class(a)
a

.04*20

data$Age
fam<-do.call("gaussian",list("log"))
mod<-stats::glm(Age~z_AoA,data=data,family = fam)
family(mod)
fam[1]


example(lmer)
plot(1~1)
Sys.setlocale("LC_NUMERIC","it_IT.UTF-8")
a<-c(1.2,1.4)
as.numeric(as.character(a))

Sys.setlocale("LC_NUMERIC","C")
a<-c(1.2,1.4)
as.numeric(as.character(a))

data("hsbdemo")

mod<-gamlj::gamljGLM(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  ciWidth=90,
  paramCIWidth = 90,
  simpleVariable = "math",
  simpleModerator = "schtyp",
  plotHAxis = "math",
)
mod
mod<-gamlj::gamljGLM(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  ciWidth=90,
  simpleVariable = "schtyp",
  simpleModerator = "math",
  plotHAxis = "math",
)
mod
hsbdemo$read
mod<-gamlj::gamljGLM(
  data = hsbdemo,
  formula=science~math+read+math:read+schtyp,
  simpleVariable = read,
  simpleModerator = math,
  simple3way = schtyp,
  plotHAxis = math,
)
mod

data("temptime")
dat<-temptime
dat$subj<-factor(dat$subj)
dat$cond<-factor(dat$cond)
dat$timebin<-dat$timebin-mean(dat$timebin)
modf<-as.formula(y~cond*timebin+I(timebin^2)+(1+timebin+I(timebin^2)|subj))
library(lme4)
mod<-lmer(modf,data=dat)
mod@optinfo
if (!is.null(mod@optinfo$conv$lme4$code))
  print("l")

ss<-summary(mod)
ss$optinfo
isSingular(mod)
ll<-lmerControl()$checkConv
ll
summary(mod)
ff<-allFit(mod)
ff$bobyqa
ff$Nelder_Mead
data("subjects_by_stimuli")
dat<-subjects_by_stimuli
dat$subj<-factor(dat$subj)
dat$cond<-factor(dat$cond)
dat$stimulus<-factor(dat$stimulus)
modf<-as.formula(y~cond+(1+cond|subj)+(1|stimulus))
ctr=lme4::lmerControl(optimizer = "nloptwrap")
mod<-lmer(modf,data=dat,control = ctr)
mod@optinfo
mod@devcomp
mod@optinfo$conv$lme4$code
mod@optinfo$conv$lme4$code==-1
mod@optinfo$optimizer
summary(mod)
ff<-allFit(mod)
summary(ff)
ss<-summary(ff)
names(ss$which.OK[1])
ff$bobyqa
