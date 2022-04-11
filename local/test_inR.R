library(gamlj)
library(foreign)

######### mixed ##############
data<-read.csv("../../jamm/jamm/data/coopmedmod.csv")
data$prime<-factor(data$prime)

gobj<-gamlj::gamljGlm(
  data = data,
  dep = BEH,
  factors= "prime",
  covs = c("EXP", "SVO"),
  modelTerms = list("prime", "EXP","SVO",list("EXP","SVO")),
  plotHAxis = SVO,
  plotSepLines = prime,
  simpleVariable = SVO,
  simpleModerators = c(EXP,prime),
  scaling = list(
    list(
      var="EXP",
      type="centered"),
    list(
      var="SVO",
      type="centered")))
gobj
gobj<-gamlj::gamljGlm(
  data = data,
  formula = BEH~SVO*prime,
  plotHAxis = SVO,
  plotSepLines = prime,
  scaling = list(EXP="centered",SVO="centered"),
  posthoc = ~prime
)


gobj
gobj$descPlot
myplot<-gamlj_ggplot(gobj)
gamlj_update(gobj,list(simpleVariable=NULL))

data<-read.csv("extdata/dat3x2x2_mixed.csv")
data$wfac<-factor(data$wfac)
data$bfac<-factor(data$bfac)

names(data)
q<-gamlj::gamljGlm(
  data = data,
  formula = y ~ 1 + x + wfac + bfac + I(x^2) + x:bfac + x:wfac + bfac:wfac + I(x^2):bfac + I(x^2):wfac + I(x^2):bfac:wfac,
  plotHAxis = x,
  plotSepLines = wfac,
  plotSepPlots = bfac,
  posthoc = ~wfac:bfac
)
object.size(q)

posthoc(q)
q$posthoc
model<-gamlj_model(q)
model
summary(model)
update(model)
do.call(.call,list(formula=newformula,data=.data))
class(a)
fromb64(a)
q<-gamlj::gamljGlm(
  data = data,
  formula = y ~ 1 + x + wfac + bfac + I(x^2) + x:bfac + x:wfac + bfac:wfac + I(x^2):bfac + I(x^2):wfac + I(x^2):bfac:wfac,
  plotHAxis = x,
  plotSepLines = wfac,
  plotSepPlots = bfac,
  simpleVariable = wfac,
  simpleModerators = c("bfac","x")

)

object.size(q)
gamlj_data(q)

q$simpleEffects$anova
q$simpleEffects$coefficients$asDF
q$simpleEffects$coefficients
q$main$fixed$asDF
jmvcore::stringifyTerm(c("x","x"),raise=T)
a<-c("a","b",list("x","x"))
a64<-jmvcore::toB64(a)
r<-jmvcore::stringifyTerm(a64,raise = T)
jmvcore::fromB64(r)


myformula<-BEH~EXP+I(EXP^2)

gobj<-gamlj::gamljGLM(
formula = myformula,
data=data)

gobj

data("subjects_by_stimuli")
data<-subjects_by_stimuli
data$x<-rnorm(length(data$nrow))

model<-gamlj::gamljMixed(
  formula =y ~ 1 + x + I(x^2)+( 1|subj ),
  data = data
)
model
decomposeTerm("I(x^2)")
formula =y ~ 1 + x + I(x^2)+( 1|subj )


###############à

formula<- y ~ 1 + cond+( 1|subj )
q<-lme4::findbars(formula)
inherits(q,"formula")
data("subjects_by_stimuli")
data<-subjects_by_stimuli
data$cond<-factor(data$cond)
formula<-y~1+cond+(1|stimulus)+(1|subj)
library(lmerTest)
model<-lmer(formula, data)
f<-lme4::findbars(formula)


a<-c("q", "stimulus","subj","ciao")
a<-"q"
which(a %in% names(model@cnms))
data$cond
subset(data,"cond"=1)
model<-gamlj::gamljMixed(
  formula =y ~ 1 + cond+( 1|subj ),
  data = data
)
model
round(as.numeric(as.character(model$info$asDF[3,2])),digits = 2)

mm<-gamlj_update(model,plotHAxis = "cond")
d<-mm$descPlot$plot

gamlj_plot(model,haxis = "cond", plotRandomEffects=T)

gamlj::gamljMixed(
  formula =formula,
  data = data, plotHAxis = cond, plotRandomEffects = TRUE
    )

gamlj::gamljGLM(
  formula =y~cond,
  data = data)


.listdeep<-function(aList,n=0) {
  if (!inherits(aList,"list"))
    return(n)
    max(sapply(aList,.listdeep,n+1))
}
aList<-list(list(list("c","dd"),list("w","s")),list("1","w"))

.listdeep(aList)
q<-list("1+c+d","1+a+b")
lapply(q, function(a) 
   lapply(strsplit(a,"+",fixed=T), function(b)
     paste(b,"r")))


bars<-lme4::findbars(formula)
as.character(bars[[1]])[[2]]
ret<-rep(list(NULL),length(bars))
for (b in seq_along(bars)) {
  bar<-strsplit(as.character(bars[[b]])[[2]],"+",fixed=T)
  barlist<-list()
  for (term in bar) {
    print(term)
    barlist<-append(barlist,term)
  }
  ret[[b]]<-barlist
}
str(ret)


data("beers_bars")
data<-beers_bars

model<-gamlj::gamljMixed(
  formula = smile ~ 1 + beer + I(beer^2)+( 0 + beer  | bar )+( 1 +I(beer^2) | bar ),
  data = data)

data(qsport)
mod<-gamlj::gamljGLM(
  formula = performance ~ hours,
  data = qsport)
gamlj_plot(mod,haxis = "hours")

gamlj::gamljMixed(
  data = subjects_by_stimuli,
  dep = "y",
  factors = "cond",
  modelTerms = "cond",
  cluster = "subj",
  randomTerms=list(list(c("cond","subj"))))


data<-read.csv("../gamlj_docs/data/neuralgia.csv")
res<-nnet::multinom(Treatment ~ Sex * Duration,data=data)
length(data$X)
data$cc<-factor(rep(c(1,2,3),20))


mod<-gamlj::gamljGzlm(
  formula = Treatment ~ cc * Duration+X,
  data = data,
  contrasts = list(
    list(
      var="cc",
      type="deviation")),
  simpleVariable = X,
  simpleModerator = Duration,
  modelSelection = "multinomial")

mod

mod<-gamlj::gamljGzlm(
  formula = Treatment ~ cc * Duration+X,
  data = data,
  contrasts = list(
    list(
      var="cc",
      type="deviation")),
  simpleVariable = X,
  simpleModerator = Duration,
  simple3way = cc,
  modelSelection = "multinomial")
mod
mod<-gamlj::gamljGzlm(
  formula = Treatment ~ cc * Duration+X,
  data = data,
  contrasts = list(
    list(
      var="cc",
      type="deviation")),
  simpleVariable = X,
  simpleModerator = Duration,
  modelSelection = "multinomial")
mod
mod<-gamlj::gamljGzlm(
  formula = Pain ~ cc * Duration,
  data = data,
  contrasts = list(
    list(
      var="cc",
      type="deviation")),
  simpleVariable = cc,
  simpleModerator = Duration,
  modelSelection = "logistic")

mod

data<-read.csv("../gamlj_docs/data/howell_rep.csv")
data$time<-factor(data$time)
data$group<-factor(data$group)
gamlj::gamljMixed(
  formula = dv ~ 1 + time + group + time:group+( 1 | subj ),
  data = data,
  contrasts = list(
    list(
      var="time",
      type="polynomial"),
    list(
      var="group",
      type="simple")),
  simpleVariable = time,
  simpleModerator = group)


library(lmerTest)
formula = dv ~ 1 + time + group + time:group+( 1 | subj )


nc<-6000
nr<-4
a<-runif(nc)
ai<-rep(a,each=nr)
cl<-factor(rep(1:nc,each=nr))
fac<-rep(1:4,nc)
x<-rnorm(nc*nr)
y<-ai+fac*x+rnorm(nc*nr)
fac<-factor(fac)
library(lmerTest)
mod<-lmer(y~fac*x+(1|cl))

data<-data.frame(y,x,fac,cl)
#write.csv(data,"extdata/longmixed.csv")
mod<-emmeans::emtrends(mod,specs="fac",var="x",at=list(-1,0,1))

emmeans::test(mod)

gamlj::gamljMixed(
  formula = y ~ 1 + x + fac +( 1 | cl ),
  data = data,
  simpleVariable = x,
  simpleModerator = fac)

gamlj::gamlj_drop()

gamlj::gamljGLM(
  formula = y ~ 1 + x + fac ,
  data = data,
  simpleVariable = x,
  simpleModerator = fac)

data("qsport")
model<-gamlj::gamljGLM(
  formula = performance ~ hours * type,
  data = qsport)

gamlj::gamlj_simpleEffects(model,variable = "hours",moderator = "type")
  

data(qsport)
mod<-gamlj::gamljGLM(
  formula = performance ~ hours+ I(hours^2),
  data = qsport)

gamlj_plot(mod,haxis = "hours")

# passing a gamljGLM option in "..."

gamlj_plot(mod,haxis = "hours",plotRaw = T)

gamlj::gamljGLM(formula = len ~ supp,  data = ToothGrowth)

mod<-gamlj::gamljGLM(dep= len,factors=supp, modelTerms = c("supp"),  data = ToothGrowth,
                plotHAxis = "supp")
names(ToothGrowth)
class(ToothGrowth$dose)
q<-mod$descPlot$plot
class(q)

q<-gamlj_ggplot(mod)
x<-rnorm(100)
z = 1 + 2*x        # linear combination with a bias
pr = 1/(1+exp(-z))  # pass through an inv-logit function
r<-10
y = rbinom(100,r,pr)/r      # bernoulli response variable
 
mod<-glm(y~1+x,family = binomial(),weights = rep(r,100))
summary(mod)
library(lme4)

############

data("qsport")
obj<-gamlj::gamljGLM(
  formula = performance ~ hours,
  data = qsport,
  scaling = list(
    list(
      var="hours",
      type="standardized")))

obj$model
gaml_predict(obj)

terms(performance ~ hours+(1|id))
q<-attr(obj$model$terms,"term.labels")
attr(mm$terms,"term.labels")<-"x"
names(mm$coefficients)<-c("I","x")
summary(mm)
attributes(mm)
data("qsport")
obj<-gamlj::gamljGLM(
    formula = performance ~ hours,
    data = qsport)
gmodel<-gamlj_model(obj)
str(obj)
obj$options
class(obj)
gmodel<-gamlj_model(obj)

hsdemo<-read.csv("../gamlj_docs/data/hsbdemo.csv")
gamlj::gamljGLM(
  formula = read ~ write + math + science,
  data = hsdemo,
  plotHAxis = write,
  plotSepLines = math,
  plotSepPlots = science)

data<-hsdemo
q<-gamlj::gamljGzlm(
  formula = prog ~ ses + write,
  data = data,
  contrasts = list(
    list(
      var="ses",
      type="deviation")),
  plotHAxis = write,
  plotSepLines = ses,
  modelSelection = "multinomial")
q

gamlj::gamljGLM(
  formula = read ~ 1,
  data = data)

gamlj::gamljGLM(
  formula = read ~ 0,
  data = data,
  fixedIntercept = FALSE)

gamlj::gamljGLM(
  formula = read ~ write,
  data = data)

formula = read ~ 0
mod<-lm(formula ,data)
ss<-summary(mod)
ss


data<-read.csv("extdata/dat3x2x2_names.csv")
names(data)[10]<-"wfac3 more"
names(data)
data$`wfac3 more`<-factor(data$`wfac3 more`)
data$bfac<-factor(data$bfac)
data$wfac3<-factor(data$wfac3)

gamlj::gamljGlm(
  formula = y ~ 1 + x + bfac + `wfac3 more` + bfac:`wfac3 more`,
  data = data,
  contrasts=c(bfac = "simple", `wfac3 more` = "difference"),
  emmeans = ~ bfac:`wfac3 more`,
  posthoc = ~ bfac:`wfac3 more`,
  simpleVariable = x,
  simpleModerators=c(bfac,"wfac3 more"),
  simpleScaleLabels = "values_labels")

gamlj::gamljGlm(
  formula = y ~ 1 + x + bfac + `wfac3 more` + bfac:`wfac3 more`,
  data = data,
  contrasts=c(bfac = "simple", "wfac3 more" = "difference"),
  emmeans = ~ bfac:`wfac3 more`,
  posthoc = ~ bfac:`wfac3 more`,
  simpleVariable = x,
  simpleModerators=c(bfac,"wfac3 more"),
  simpleScaleLabels = "values_labels")
