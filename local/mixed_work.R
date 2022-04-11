source("R/constants.R")
source("R/jScafFunctions.R")
source("R/functions.R")

fromb64("ciao ..b_b_b..XU1ZP")
x<-"a string ..b_b_b..XU1ZP:ciao ..b_b_b..XU1ZP"
library(stringr)
obj<-paste0("a string var1 e var1:var2")

fromb64(obj)

bregex<-paste0("(?<=",B64_REGEX,")\\w+")
matches64<-str_extract_all(obj,bregex)[[1]]
matches<-jmvcore::fromB64(matches64)
astring<-obj
for (i in seq_along(matches64))
        astring<-str_replace_all(astring,matches64[i],matches[i])
astring<-str_replace_all(astring,B64_SYMBOL,"")
astring<-str_replace_all(astring,FACTOR_SYMBOL,"")
astring<-str_replace_all(astring,LEVEL_SYMBOL,"")
astring<-str_replace_all(astring,INTERACTION_SYMBOL,":")
astring

patient <- "Name: Jane
            Age: 25
            Condition: OK"
str_extract_all(patient, "(?<=Name: )")
str_extract_all(patient, "(?<=Name: )\\w+")

stringr::regex("\\d+(?= ..b_b_b..?)")


library(lmerTest)
data("wicksell")
data<-wicksell
class(data$group)
data$cgroup<-as.numeric(data$group)
data$group<-factor(data$group)
data$time<-factor(data$time)

form<-"..b_b_b..XRUM ~ 1  +( 1 | ..b_b_b..XQ09VTlRSWUlE ) "
lme4::findbars(form)
formula<-dv ~ 1 +  time+( 1| subj )
lme4::findbars(formula)
mod<-lmer(formula,data=data)
model<-mod

gobj<-gamlj::gamljMixed(
    formula =formula ,
    data = data,
    postHoc = ~time,
    eDesc=TRUE)
gobj


a<-2161+387+1136
b<- 22674
cc<-5887
b+cc
a+b
mod<-gamlj::gamlj_model(gobj)
mod<-lme4::lmer(formula,data)
mod
parameters::parameters(mod)
pp<-profile(mod,which="theta_",optimizer=mod@optinfo$optimizer)
confint(mod,level = .90, method="profile")


library(gamlj)
emm_options(pbkrtest.limit = 5041)
data<-rbind(schoolexam,schoolexam)
mod<-gamlj::gamljMixed(
  formula = math ~ 1 + activity + pass+( 1 | school ),
  data = data,
  simpleVariable = activity,
  simpleModerator = pass)

mm<-mod$model
mm
gamlj::gamljMixed(
  formula = math ~ 1 + activity*pass+( 1 | school ),
  data = data,
  plotHAxis = activity,
  plotSepLines = pass,
  simpleVariable = activity,
  simpleModerator = pass)

data
formula = math ~ 1 + activity * pass+( 1 | school )
mod<-lmer(formula,data)
car::Anova(mod)
afex::mixed(formula,data)

id <- rep(1:10,3)
age <- rep(c("59","59","70","67","66","70","70","68","71","57"),3)
sex <- rep(c("F","M","F","M","F","F","F","M","F","M"),3)
sequence <- rep(c("1","2","1","2","1","2","1","2","2","2"),3)
period <- c(rep(0,10),rep(1,10),rep(2,10))
Treatment <- c(rep("C",10), rep(c("A","B"),4), "B","B",rep(c("B","A"),4), "A","A") #C is baseline
lipid <- c(18,6,30,12,14,19,10,22,22,27,13,28,14,23,12,27,9,10,13,22,13,22,29,12,16,24,15,13,17,11)
DF <- data.frame(id,age,sex,sequence,period,Treatment,lipid)

library(lmerTest)
lm1 <- lmer(lipid~Treatment + sequence + period + sex + age + (1|id), data = DF, REML = F)
library(emmeans)
library(pbkrtest)
emm <- emmeans(lm1,"Treatment",lmer.df = "Satterthwaite")
pairs(emm, adjust = "fdr")

library(lme4)
library(nlme)
data("Ovary")
data<-wicksell
data$group<-factor(data$group)
data$time<-factor(data$time)

formula<-dv ~ 1 +  group+time + group:time+( 1 | subj )

mod<-lmer(formula,data = data)

as.data.frame(parameters::parameters(mod,effects="random",df="satterthwaite",ci))

mod00<-lm(dv~1,data=data)
mod01<-lmer(dv~1+(1|subj),data=data)

anova(mod,mod00)
anova(mod,mod01)



summary(mod0)

mod@optinfo$optimizer

summary(mod0,ddf="Kenward-Roger")

mod1<- lme(dv ~ 1 + group+ time + group:time,
  data =data,
  random = ~ 1 | subj )
mod1
mod0
table(data$time,data$subj)
table(data$time,data$subj)
data<-data[order(data$subj,data$time),]
mod2 <- update(mod1, correlation = corAR1(form = ~1|subj))
summary(mod2)
anova(mod1, mod2)
mod3<-mod1<- lme(dv ~ 1 + group*time,
                 data =data,
                 random = ~ 1 | subj,correlation = corAR1(form = ~ time | subj) )
mod3
library(nlme)
data("temptime")
names(temptime)
temptime$subj<-factor(temptime$subj)
data<-temptime
str(data)
mod4<-lme(y~timebin+I(timebin^2),data=data,random=~1+timebin|subj,correlation=corARMA(p = 1, q=1, form=~timebin|subj))

mod4
str(mod4)
ss<-summary(mod4)
ss$apVar
mod4$apVar
class(mod4$modelStruct$corStruct)
phi <- as.numeric(coef(mod4$modelStruct$corStruct, unconstrained = FALSE))

phi
str(phi)
mod5<-lme(y~timebin+I(timebin^2),data=data,random=~1+timebin|subj)
anova(mod5,mod4)
qq<-ACF(mod4,resType = "normalized",maxLag = 4)
qq
summary(mod5)
anova(mod4,mod5)
anova(mod5,mod4)

r1<-residuals(mod3)
r2<-c(0,r1[1:(length(r1)-1)])
cor(r1,r2)

plot(ACF(mod2,  maxLag = 4), alpha = 0.05)
summary(mod1)
summary(mod2)



set.seed(101)
d <- data.frame(y=10,x=1:300)
epsilon <- arima.sim(model = list(ar=0.9, ma=+0.2279), n = 300)

d$y <- d$y + epsilon
layout(matrix(1))
plot(d$x, d$y, bty="n", las=1, pch=19, cex=0.5)

library(stats)
ai_ <- arima(d$y, order=c(1, 0, 1), include.mean = TRUE)
library(tseries)
ai2_ <- arma(d$y, order = c(1, 1), include.intercept = TRUE)
library(FitARMA)
ai3_ <- FitARMA(d$y, order = c(1, 0, 1), demean = TRUE)

coef(ai_)
coef(ai2_)
coef(ai3_)



set.seed(101)

d <- data.frame(y=10,x=rep(1:30, 10),f=factor(sort(rep(1:10,30))))
epsilon <- arima.sim(model = list(ar=0.9, ma=+0.2279), n = 300)


d$y <- d$y + epsilon

m1 <- lme(y~x,random=~1|f,correlation=corARMA(form= ~ 1 | f, p=1, q=1),data=d)
summary(m1)
m1$modelStruct$corStruct
coef(m1$modelStruct$corStruct, unconstrained=FALSE)

m2 <- lme(y~x,random=~1|f,correlation=corARMA(p=1, q=1),data=d)
summary(m2)
coef(m2)
m2$modelStruct$corStruct
coef(m2$modelStruct$corStruct, unconstrained=FALSE)

cs1ARMA <- corARMA(c(0.95, 0.20), form = ~1 | f, p = 1, q = 1)
cs1ARMA. <- Initialize(cs1ARMA, data = d)
m3 <- lme(y~x,random=~1|f,correlation=cs1ARMA.,data=d)
summary(m3)
m3$modelStruct$corStruct
coef(m3$modelStruct$corStruct, unconstrained=FALSE)

cs1ARMA <- corARMA(c(0.95, 0.20), p = 1, q = 1)
cs1ARMA. <- Initialize(cs1ARMA, data = d)
m4 <- lme(y~x,random=~1|f,correlation=cs1ARMA.,data=d)
summary(m4)
m4$modelStruct$corStruct
coef(m4$modelStruct$corStruct, unconstrained=FALSE)


anova(m4,m0)

d <- data.frame(y=10,x=rep(1:30, 10),f=factor(sort(rep(1:10,30))))
epsilon <- arima.sim(model = list(ar=0.4,ma=.1), n = 300)

d$y <- d$y + epsilon
m0 <- lme(y~x+I(x^2),random=~1+x|f,data=d)
m1 <- lme(y~x+I(x^2),random=~1+x|f,correlation=corARMA(form=~1+x|f,p=1,q=1),data=d)
plot(d$x, d$y, bty="n", las=1, pch=19, cex=0.5,col=d$f)
summary(m0)
anova(m0,m1)

library(lmerTest)
data<-read.csv("extdata/dat3x2x2_mixed.csv")
names(data)

form<-y~1+x+wfac+(1|cluster)
.opt.<-"bobyqa"

model<-lmerTest::lmer(form,data,control = lme4::lmerControl(optimizer =.opt.))
gsub("opt",paste0("'",opt,"'"),model@call)
data$bfac
form2<-bfac~1+x+wfac+(1+x|cluster)

#model<-lme4::glmer(form2,data,control = lme4::glmerControl(optimizer =.opt.),family = binomial())

model@call$control<-lme4::lmerControl(optimizer=opt)
r.squared(model)
performance::check_convergence(model)
performance::r2(model,by_group=T)
r2<-performance::r2(model,by_group=F)

opt<-NULL
model@call
update(model,formula=form2)
ranova(model,reduce.terms = T)
nobars(form)
reforms<-findbars(form)
reforms<-lapply(reforms, function(re) as.character(re)[-1])

mod<-lm(1:100~runif(100))
coef(summary(mod))[2,c(1,2,4)]
expand.grid(c("a","b","c"),c("a","b","c"))
randomTerms<-list(list("x","cluster1"),list("(Intercept)","cluster1"))
terms<-lapply(reforms, function(re) strsplit(as.character(re)[2],"+",fixed=T)[[1]])
lapply(terms, function(t) if (length(t)>1) t(combn(t,length(t)-1)) else t)

lapply(seq_along(terms), function(i) {
              if (length(terms[[i]])==1) 
              reforms[-i] 
              else {
                 lforms<-reforms
                 for (j in length(terms[[i]]))
                       lforms[[i]]<-gsub(terms[[i]][[j]],"",reforms[[i]])
                 lforms
              }
})




lapply(terms,function(re) paste(re[1:(length(re)-1)],"|", re[length(re)]))
orig_rhs<-orig_form[[length(orig_form)]]
findbars(orig_form)

pp<-profile(model,which="theta_",prof.scale="varcov")
ci<-confint(pp,param="theta_",method="Wald")
ci<-confint(model,method="profile",parm = "theta_")
ci
ci[variances,]
.names<-names(lme4::getME(model,"theta"))
as.data.frame(lme4::VarCorr(model))
variances<-unlist(lapply(names(lme4::getME(model,"theta")), function(n) length(strsplit(n,".",fixed = T)[[1]][-1])==1))
summary(mod)
anova(mod)
model<-mod
vc<-as.data.frame(lme4::VarCorr(model))
params<-vc[is.na(vc[,3]),]
params$var1[is.na(params$var1)]<-""
params
form<-NULL
data<-NULL
library(lmerTest)
class(mod)
data<-model@frame
form<-y~1+x+(1|cluster)
do.call(parameters::parameters,list(model=mod,effects="fixed",df_method="satterthwaite"))
parameters::parameters(mod,)
ngrp<-vapply(model@flist,nlevels,1)
names(ngrp)
summary(model)

mod<-lm(1:100~rnorm(100))
parameters::parameters(mod,df_method="satterthwaite",exponentiate=FALSE)
class(mod)


a<-30
b<-list(eval(a),"d")
b
library(performance)
model <- lmer(Petal.Length ~ Sepal.Length +Sepal.Width+ (1 | Species), data = iris)
table(iris$Species)
p<-predict(model)
plot(p~iris$Sepal.Length)
model_performance(model)
model0<-lmer(Petal.Length ~ 1 + (1 | Species), data = iris)
model00<-lm(Petal.Length ~ 1 , data = iris)

2*(logLik(model)-logLik(model0))
performance::test_likelihoodratio(model00,model)

anova(model,model0,test="LRT")
summary(model)


if (1==0 && tra$tra=="1")
   print(1)
performance::r2_nakagawa
vnull<-insight::get_variance(insight::null_model(model))
vmod<-insight::get_variance(model)
1-(vmod$var.intercept/vnull$var.intercept)

summary(model)
r.squared(model)
performance::r2(model)


library(lmerTest)
data<-read.csv("extdata/dat3x2x2_mixed.csv")
data$wfac<-factor(data$wfac)
contrasts(data$wfac)<-contr.sum(2)/2
data$x<-data$x-mean(data$x)
model<-lmer(y~1+x*wfac+(1+x|cluster),data=data,control=lme4::lmerControl(optimizer = "bobyqa"))
model
summary(model)
stats::anova(model,type="3")

mdl<-model

sapply(
  lme4::VarCorr(mdl)[!sapply(unique(unlist(strsplit(names(lme4::ranef(mdl)),":|/"))), function(l) length(unique(mdl@frame[,l])) == nrow(mdl@frame))],
  function(Sigma) {
    X <- stats::model.matrix(mdl)
    Z <- X[,rownames(Sigma)]
    print(Sigma)
    sum(diag(Z %*% Sigma %*% t(Z)))/nrow(X) } )


contrasts(data$wfac)<-contr.treatment(2,base = 1)
data$x<-data$x-mean(data$x)
model<-lmer(y~1+x*wfac+(1+x|cluster),data=data,control=lme4::lmerControl(optimizer = "bobyqa"))
model
summary(model)
stats::anova(model,type="3")

contrasts(data$wfac)<-contr.treatment(2,base = 2)
data$x<-data$x-mean(data$x)
model<-lmer(y~1+x*wfac+(1+x|cluster),data=data,control=lme4::lmerControl(optimizer = "bobyqa"))
model
summary(model)
stats::anova(model,type="3")
aa<-emmeans::emmeans(model,spec="wfac")


model<-lm(y~x,data=data)
q<-parameters::parameters(model,bootstrap=T,robust=F)
q<-parameters::parameters(model,bootstrap=F,robust=T)

as.data.frame(q)


a<-list(c("F","M","D"),c("M","D","F"),c("D","F","M"))
a
g<-expand.grid(a,a,a)


sapply(1:nrow(g) ,function(x) length(unique(g[x,]))==length(g[x,]))
combn(a,m = 3)
combn(letters[1:4], 2)
library(lme4)
y<-data$y
model<-lmer(y~1+x+(1+x|cluster),data=data,control=lme4::lmerControl(optimizer = "bobyqa"))
lmerTest::ranova(model)


safeDeparse <- function(expr, width.cutoff=500L, backtick = mode(expr) %in%
                         c("call", "expression", "(", "function"),
                       control = c("keepInteger","showAttributes", "keepNA"),
                       nlines = -1L) {
  deparse(expr=expr, width.cutoff=width.cutoff, backtick=backtick,
          control=control, nlines=nlines)
}

deparse2 <- function(x) paste(safeDeparse(x), collapse = " ")

a<-findbars(formula(model))[[1]]
deparse2(a)

orig_form <- formula(model)
orig_rhs <- orig_form[[length(orig_form)]]
reforms <- lapply(findbars(orig_rhs), deparse2)

lmerTest:::get_newforms(a)

data$cluster<-factor(data$cluster)
data$x<-as.numeric(scale(data$x,center = T))
data$bfac<-factor(data$bfac)
form0<-y~1+(1+x|cluster)
model0<-lmer(form0,data,,control=lme4::lmerControl(optimizer = "bobyqa"))
form<-y~1+bfac+wfac+(1+x|cluster)
model<-lmer(form,data,control=lme4::lmerControl(optimizer = "bobyqa"))

library(lmerTest)
drop1(model,test = "Chisq")
anova(model0,model,test="Chisq")
performance::test_likelihoodratio(model0,model)
anova(model)
summary(model)
objects<-list(model0,model)
lls<-sapply(objects, insight::get_loglikelihood)
lls
chi2 <- abs(c(NA, -2 * diff(lls)))
chi2
-2*diff(lls)

form<-y~bfac
model<-lm(form,data)

b<-parameters::parameters(model,bootstrap=T)
est <- emmeans::emmeans(b,  ~ bfac)
b<-parameters::parameters(est)
b

b<-parameters::bootstrap_model(model)

referenceGrid <- emmeans::emmeans(b, ~bfac, type = "response", data = data)
ci_results<-summary(graphics::pairs(referenceGrid), adjust = "none")

referenceGrid <- emmeans::emmeans(model, ~bfac, type = "response", data = data)

results<-summary(graphics::pairs(referenceGrid), adjust = "none",infer = c(TRUE,TRUE))
results$lower.CL<-ci_results$lower.HPD
results$upper.CL<-ci_results$upper.HPD

names(data)

contrasts(data$bfac)
levels(data$bfac)<-c("___a","___b")
mod<-lm(y~-1+bfac,data=data)
summary(mod)
mod0<-lm(y~1,data=data)
anova(mod0,mod)

formula(mod)
attr(terms(mod),"intercept")
attr(terms(mod0),"intercept")


a<-paste("+(1 | ",tob64("CC"),")")
a
fromb64(a,"CC")

data<-read.csv2("../gamlj.github.io/data/beers_bars.csv")
head(data)
data$bar<-factor(data$bar)
data$beer<-as.numeric(as.character(data$beer))
data$smile<-as.numeric(as.character(data$smile))

mod<-lmer(smile~beer+(1+beer|bar),data=data)
model<-mod
cluster<-"bar"
re<-ranef(mod)[[cluster]]
re<-cbind(cluster=rownames(re),re)
re$beer_max<-re$beer+runif(1,0,.5)
re$beer_min<-re$beer-runif(1,0,.5)

library(ggplot2)
p<- ggplot(data = re)  
p<- p +  geom_point(aes(x=cluster, y=beer), stat = "identity") 
p<- p +  scale_fill_brewer(type = "seq", palette = 1) 
p <- p + geom_errorbarh(height=.1,aes(y="beer"),xmin="beer_min",xmax="beer_max")
p <- p +coord_flip()
p
