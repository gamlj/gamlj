hdp <- read.csv("https://stats.idre.ucla.edu/stat/data/hdp.csv")
hdp <- within(hdp, {
  Married <- factor(Married, levels = 0:1, labels = c("no", "yes"))
  DID <- factor(DID)
  HID <- factor(HID)
})

contrasts(hdp$CancerStage)

library(lme4)
mod<-glmer(remission ~ IL6 + I(IL6^2) + CancerStage + LengthofStay + Experience +
        (1 | DID), data = hdp, family = binomial,nAGQ = 10)
summary(mod)
pp<-ggeffects::ggpredict(mod,terms = c("IL6 [all]"), type="fe")
pp<-ggeffects::ggpredict(mod,terms = c("IL6"), type="fe")
pp<-ggeffects::ggpredict(mod,terms = c("CancerStage"), type="fe")


plot(pp)
pp<-ggeffects::ggpredict(mod,terms = c("IL6","DID"), type="re")
pp

library(ggplot2)
ggplot(pp, aes(x, predicted, group=group)) +  geom_line()


plot(pp$predicted~pp$x,pp$group)
vc<-VarCorr(mod)
sd(resid(mod))
sigma(mod)

mod@devcomp
af2<-allFit(mod)
m1 <- glmer(cbind(incidence, size - incidence) ~ period + (1+period | herd),data = cbpp, family = binomial)
m1@optinfo
summary(m1)
af<-allFit(m1)

library(gamlj)
af2$nloptwrap.NLOPT_LN_NELDERMEAD
class(af2$nloptwrap.NLOPT_LN_BOBYQA)
class(af2$nmkbw)
ss<-summary(af2)
ss$which.OK
library(car)
Anova(mod,type=3)

mod2<-glmer(remission ~ CancerStage+
             (1 | DID), data = hdp, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 1)

aa<-car::Anova(mod,type=3,test="Chisq")

aa

summary(mod)
contrasts(hdp$CancerStage)
str(mod)
summary(mod)
mod@devcomp$dims['nmp']
confint(mod,method="boot")

cluster<-1:70
data<-data.frame()
for (i in cluster) {
  n<-round(runif(1,50,100))
  a<-runif(1,-3,3)
  b<-runif(1,-3,3)
  x1 = round(rnorm(n),digits = 3)           # some continuous variables 
  x2 = round(rnorm(n),digits = 3)
  z = a + .5*x1 + b*x2 + .4*x1*x2        # linear combination with a bias
  pr = 1/(1+exp(-z))         # pass through an inv-logit function
  y = rbinom(n,1,pr)      # bernoulli response variable
  dat<-cbind(y,x1,x2,i)
  data<-rbind(data,dat)
}
data$cluster<-factor(data$i)

#write.csv2(data,"../gamlj_docsource/data/mlogistic.csv",row.names = F)
data2<-data
data2$i<-NULL
names(data2)<-c("pass","math","activity","school")
#write.csv2(data2,"../gamlj_docsource/data/school_exam.csv")
data<-read.csv2("../gamlj_docsource/data/mlogistic.csv")
head(data)
library(lme4)
library(lmerTest)
ctr<-glmerControl(optCtrl=list(maxfun=120000))
model1<-glmer( y~(1|cluster)+x1+x2,data=data,family="binomial")
model2<-glmer( y~(1+x2|cluster)+x1+x2,data=data,family="binomial")
model2<-glmer( y~(1+x1|cluster)+x1+x2,data=data,family="binomial")

##### find random effects for LRT ###########
form<-y~(1+x1+x2+x2:x2+x3+x3:x2|cluster)+(0+x1|cluster)+x1+x2
bb<-findbars(form)
tt<-bb[[1]]
mform<-structure(list(formula=form,
               dep=NULL,
               random=NULL,
               fixed=NULL),
               class="merFormula")

.merformula.getRandom<-function(lformula) {
  fb<-findbars(form)
  lapply(fb,function(f) 
    trimws(strsplit(as.character(unlist(tt))[[2]],"+",fixed = T)[[1]])
    )  
}
lf<-.merformula.getRandom(form)
lapply(lf, function(l) {
  l[l!="x1"]
  
} )

sapply(lf,paste,collapse="+")

formula(paste(lf,collapse = "+"))
utt<-strsplit(as.character(unlist(tt))[[2]],"+",fixed = T)[[1]]
ltt<-sapply(jmvcore::decomposeTerms(utt),trimws)  
ltt

lorder<-sapply(ltt,length)
m<-max(lorder)
remove<-which(sapply(ltt, function(t) length(t)==m))
for (i in remove) {
  wtt<-ltt
  wtt[i]<-NULL

}
tt
q<-deparse(tt)
q
terms(form)
drop.terms(form,1)
ff<-as.formula(paste("~",q))
terms(ff)
rm_complete_terms("x1",tt)
update.formula(tt,"x1")
drop.terms(tt,drop=1)
rm_complete_terms <- function(terms, full_formula, random=TRUE) {
  # Remove random-effect formula terms from original model formula (full_formula)
  forms <- lapply(terms, function(reform) {
    form <- update.formula(full_formula, paste0("~.- (", reform, ")"))
    environment(form) <- environment(full_formula)
    form
  })
  names(forms) <- if(!random) terms else
    sapply(terms, function(form) paste0("(", form, ")"))
  forms
}



lmerTest::ranova
#####################################

findq
anova(model1,model2)

nAGQ<-1
form<-y~(1+x2+x1|cluster)+x1+x2+x1:x2
afamily<-binomial()
mod<-do.call(lme4::glmer, list(formula=form, data=data,
                          family = afamily, nAGQ = nAGQ))

warnings()

drop1(mod,~x1:x2,test="Chisq")

stats::anova(model)
car::Anova(model,type=3)
af<-allFit(model)
update(model,control=ctr)
ss<-summary(af)
ss
exp(ss$coefficients[,1])/(1+exp(ss$coefficients[,1]))
class(model)<-"lmerMod"
ranova(model)
model<-glm( y~1,data=df,family="binomial")
ss<-summary(model)
ss
exp(ss$coefficients[,1])/(1+exp(ss$coefficients[,1]))
mean(y)

pp<-predict(model,type="response")
mean(pp)
data$pp<-pp
da<-data[1,]
p <- ggplot2::ggplot(data=da) +
  ggplot2::labs(x=x1, y=pp) +
  ggplot2::scale_y_continuous(limits=c(min(0), max(1))) 
for (clu in unique(da$cluster)) {
  one<-da[da$cluster==clu,]
  print(one)
  p <- p + ggplot2::geom_line(data=one,ggplot2::aes_string(x="x1",y="y"),size=.2,show.legend = F) 
}
p

data<-read.csv("../gamlj_docsource/data/pass_school.csv")
head(data)
data$A<-NULL
data$math<-NULL
data$activity<-NULL
head(data)
names(data)<-c("pass","math","activity","school")
#write.csv2(data,"../gamlj_docsource/data/school_exam.csv",row.names = F)
length(unique(data$school))



data("schoolexam")
mod<-gamlj::gamljGlmMixed(
  formula = pass ~ 1 + math*activity+( 1 +math| school ),
  data = schoolexam,
  modelSelection = "logistic",
  plotHAxis = math,
  correlatedEffects = "nocorr",
  cimethod = "wald")

problems<-FALSE
if (length(mod$info$notes)>0)
  problems<-mod$info$notes[[1]]$note

problems
mod<-gamlj::gamljGlmMixed(
    formula = pass ~ 1 + math*activity+( 1 | school ),
    data = schoolexam,
    modelSelection = "logistic",
    plotHAxis = math,
    correlatedEffects = "nocorr",
    cimethod = "wald")

length(mod$info$notes)

#mod$info$notes$
  