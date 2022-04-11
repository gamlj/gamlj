library(gamlj)

##### poisson 
data<-poissonacts
names(data)

form<-acts ~ agg_test*age
gmod<-gamlj::gamljGzlm(
  formula = form,
  data = data,
  modelSelection = "poisson")
gmod

mod<-glm(form,data=data,family = poisson(log))


mod<-glm(form,data=data,family = quasipoisson())

mod0<-glm(acts~1,data,family = quasipoisson())
anova(mod,mod0,test="LR")
summary(mod)
mod<-MASS::glm.nb(form,data)
mod0<-MASS::glm.nb(acts~1,data)
results<-anova(mod,mod0)
as.data.frame(q)
class(mod)
summary(mod)
y<-c(1.4,1.2)
y<-c(1,2)
all.equal(as.numeric(y), as.integer(y))==T

##### custom
data("poissonacts")
data<-poissonacts

data$q<-data$acts+1
gmod<-gamlj::gamljGzlm(
  formula=q~agg_test,
  data=data,
  modelSelection = "custom",
  custom_family = "Gamma",
  custom_link = "inverse",
  emmeans = ~agg_test)
gmod
mod<-glm(form,data=data,family = quasipoisson())



form<-acts ~ agg_test*age*aggressive*id
gamlj::gamljGzlm(
  formula = form,
  data = data,
  modelSelection = "poisson")

gamlj::gamljGzlm(
  formula = acts ~ agg_test + id + aggressive + agg_test:id + agg_test:aggressive + id:aggressive + agg_test:id:aggressive + agg_test:id:I(aggressive^2),
  data = data,
  modelSelection = "poisson")

formula = acts ~ agg_test + id + aggressive + agg_test:id + agg_test:aggressive + id:aggressive + agg_test:id:aggressive + agg_test:id:I(aggressive^2)

mod<-glm(formula,data=data,family = poisson())
car::Anova(mod)
mod$converged
summary(mod)
mod$control
str(mod)
last.warning
mod<-glm(acts ~ agg_test,data=data,family = poisson())
mod$converged
summary(mod)
mod$control


gamlj::gamljGzlm(
  formula = acts ~ agg_test + id + aggressive + age + agg_test:id + agg_test:aggressive + id:aggressive + agg_test:age + id:age + aggressive:age + agg_test:id:aggressive + agg_test:id:age + agg_test:aggressive:age + id:aggressive:age + I(aggressive^2):id:agg_test + agg_test:id:aggressive:age,
  data = data,
  modelSelection = "poisson")

data("schoolexam")
data<- schoolexam
names(data)
table(data$math)
data$pass<-factor(data$pass)
opts<-list(formula=pass ~ math,data=schoolexam,family=binomial())

mod1<-do.call(glm,opts)
mod2<-stats::glm(pass~math,data=schoolexam,family="binomial")
stats::glm(pass~math,data=schoolexam,family=binomial())

mod2$call[[1]]
mod1$call[[1]]
as.data.frame(parameters::parameters(mod2))
gamlj::gamljGzlm(
  formula = pass ~ math,
  data =data,
  modelSelection = "logistic",
  effectSize = "RR")

q<-"binomial(link='probit')"
a<-binomial(link="probit")

str2lang(q)

str2lang("abc"   )
quote(abc -> qa)

mod<-glm(pass~math,data=data,family=binomial(link="probit"),model = T)
ll<-list(formula=pass~math,data=quote(data),family=q,model=quote(T))
mod2<-do.call("glm",ll)
mod2$call
data$id<-1:length(data$pass)
data$npass<-as.numeric(data$pass)-1
rr<-geepack::relRisk(npass~math,id=id, data= data)
summary(rr)
formula=choice~colourGroup*interest
dat<-read.csv2("/home/marcello/Skinner/Teaching/vu/master/2020/Tests/Exam/take/data/abn515.study1.csv")
dat$colourGroup<-factor(dat$colourGroup)
gamlj::gamljGzlm(
  data=dat,
  formula=choice~colourGroup*interest,
  modelSelection = "logistic",
  simpleVariable = colourGroup,
  simpleModerator = interest,
  eDesc = T,
  eCovs = T
)
datx<-dat
datx$interest<-dat$interest-mean(dat$interest)
mod<-glm(formula,data=datx,family = binomial())
summary(mod)
car::Anova(mod,type=3)

data("subjects_by_stimuli")

model<-glm(y~cond,data=subjects_by_stimuli,family = poisson())


##### multinom 
data("hsbdemo")
data<-hsbdemo
data$female<-factor(data$female)

head(data)

form<-prog~female
model<-nnet::multinom(form,data=data)
term<-"female"
dep<-"prog"
tterm <- stats::as.formula(paste("~", paste(dep, term, sep = "|")))
data <- insight::get_data(model)
str(bmodel)

omodel<-attr(bmodel,"original_model")

#bmodel<-parameters::bootstrap_model(model)
  referenceGrid <- emmeans::emmeans(model, tterm, transform = "response", data = data)
  referenceGrid <- emmeans::emmeans(bmodel, tterm, transform = "response", data = data)
  referenceGrid <- emmeans::emmeans(omodel, tterm, transform = "response", data = data)
  

  terms <- jmvcore::decomposeTerm(term)
  labs <- referenceGrid@grid[terms]
  newlabs <- sapply(labs, function(a) sapply(a, function(b) tob64(as.character(b))))
  referenceGrid@grid[terms] <- newlabs
  results <- as.data.frame(summary(graphics::pairs(referenceGrid, by=dep),infer = c(TRUE,FALSE)))
results
insight::get_data(model)
results<-as.data.frame(parameters::parameters(referenceGrid,ci=.95,ci_method="bci"))
emmeans::recover_data
object<-model

recover_data.multinom<-NULL
recover_data.multinom<-function(object,...) {
  insight::get_data(object)
}
  
opts_list<-list(object=model,specs=termf, type = "response")

referenceGrid <- do.call(emmeans::emmeans,opts_list)
referenceGrid$contrasts
results<-as.data.frame(parameters::parameters(referenceGrid$contrasts,ci=.95))

results <- summary(graphics::pairs(referenceGrid, by=dep),infer = c(ci,TRUE))

 contrasts(data$female)<-contr.sum(length(levels(data$female)))
data$ses<-factor(data$ses)
contrasts(data$ses)<-contr.sum(length(levels(data$ses)))
data$math<-data$math<-mean(data$math)
form<-prog~female+math
mmodel<-nnet::multinom(form,data = hsbdemo,model = T)
summary(mmodel)
class(mmodel)
pp<-parameters::parameters(mmodel,ci=T,ci_method="bcai",bootstrap=T,iterations=10)
as.data.frame(pp)
pp

prop.table(table(predict(model),hsbdemo$female))
eg<-emmeans::ref_grid(model,specs=c("prog","female"),by="math",at=list(math=c(-1.5,0,1.5)),type="link")
eg

em<-emmeans::emmeans(model,specs=c("prog","female"),by="math",at=list(math=c(-1.1,1.1)),type="link")
em<-emmeans::emmeans(model,specs=c("prog","female"),df=Inf)

q<-emmeans::contrast(em,method=list(prog="trt.vs.ctrl1",female=my.emmc))
q
emmeans::test(q,joint=TRUE,df=Inf)
q
car::Anova(model,test="LR",type=3)
ss<-summary(model)
ss
ss$coefficients/ss$standard.errors
my.emmc<-function(levs) {
  cc<-contr.sum(length(levs))
  as.data.frame(MASS::ginv(t(cc)))
  as.data.frame(cc)
}

form<-honors~female
model<-glm(form,data = hsbdemo,family=binomial())

em<-emmeans::emmeans(model,specs=c("female"))
em
summary(model)
q<-emmeans::contrast(em,method=my.emmc)
q
emmeans::test(q,joint=T,test="chisq")
car::Anova(model,type=3)

library(MASS)



model <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing,Hess = T)
ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
summary(model)
car::Anova(model)
table(housing$Sat)

data<-wicksell
data$time<-factor(data$time)
model <- MASS::polr(time ~ 1+dv, data = data,Hess = T)
performance::r2(model)


car::Anova(model,type=3,test="LR")
ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable<-as.data.frame(ctable)
class(model)
mm1<-emmeans::emmeans(model, ~dv*time,mode="prob",type="link" , data = data,at=list(dv=c(pretty(data$dv,n = 20))))
mm2<-emmeans::emmeans(model, ~dv,mode="mean.class",  data = data,at=list(dv=c(pretty(data$dv,n = 20))))
mm1
mm1<-as.data.frame(mm1)
mm2<-as.data.frame(mm2)



names(mm)
library(ggplot2)
ggplot(mm1,aes(x=dv,y=prob,group=time))+geom_line()
ggplot(mm2,aes(x=dv,y=mean.class))+geom_line()

cmod<-ordinal::clm(time ~ 1+dv,nominal = ~dv, data = data)
summary(cmod)


mm$lp<-log(mm$prob/(1-mm$prob))
ggplot(mm,aes(x=dv,y=lp,group=time))+geom_line()

q<-performance::r2(lm(dv~time,data=data))
ctable<-as.data.frame(ctable)
y1<-ctable$Value[2]+ctable$Value[1]*1:100
y2<-ctable$Value[3]+ctable$Value[1]*1:100
y3<-ctable$Value[4]+ctable$Value[1]*1:100
y<-c(y1,y2,y3)
x<-c(1:100,1:100,1:100)
g<-c(rep(1,100),rep(2,100),rep(3,100))
data<-data.frame(x=x,y=y,g=factor(g))
ggplot(data,aes(x=x,y=y,group=g))+geom_line()
data$p<-exp(data$y)/(1+(exp(data$y)))
ggplot(data,aes(x=x,y=p,group=g))+geom_line()

jmvcore::fromB64("XcHJvZw")
table(data$id)
library(geepack)
names(respiratory)
form<-outcome ~ treat*sex
fit <- relRisk(form ,
               id = id, corstr = "ex", data = respiratory, ncopy=10000)

summary(fit)

geepack::geeglm(form, family = poisson(link = "log"), id = id, corstr = "exchangeable", data = respiratory)

library(logisticRR)
logisticRR(outcome ~ treat ,data=respiratory)
rr<-nominalRR(outcome ~ treat ,data=respiratory)
exp(rr$fit$coefficients)
respiratory$foutcome<-factor(respiratory$outcome)

mod<-gamlj::gamljGzlm(
  formula=foutcome ~ treat + center + sex + age + baseline + visit,
  data=respiratory,
  modelSelection = "logistic",
  effectSize = "RR")



### zero intercept

mod<-stats::glm(pass~0,data=schoolexam,family="binomial")
mod
