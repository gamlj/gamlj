library(emmeans)
data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#setwd("~/Skinner/Forge/jamovi/gamlj/extdata")


tapply(data$gre,data$admit,mean)
tapply(data$gre,data$admit,sd)
tapply(data$gre,list(data$admit,data$rank),mean)
tapply(data$gre,list(data$admit,data$rank),length)
tapply(data$gre,list(data$admit,data$rank),sd)
data$admit <- factor(data$admit)

formula <- as.formula(rank ~ admit )

model <- nnet::multinom(formula,data = data)
model
model$call$formula <- formula
summary(model)

ww<-ref_grid(model,cov.reduce = T)
ww
emmeans(ww, formula(~ rank|admit),mode = "prob")



write.csv(data,file="binary.csv")
data$rank <- factor(data$rank)
data$admit <- factor(data$admit)
formula <- as.formula(rank ~ admit )
model <- nnet::multinom(formula,data=data)
model$call$formula <- formula
emmeans(model, formula(~ rank|admit),mode = "prob",options = c(level=.90) )

formula <- as.formula(rank ~ admit + gre)
model <- nnet::multinom(formula,data=data)
model$call$formula <- formula
grid<-ref_grid(model,cov.reduce = function(x) c(mean(x)+.1*100,mean(x)-.1*100))
emmeans(grid, formula(~ rank|admit),mode = "prob")

library(car)
Anova(model,type = 3)
grid<-ref_grid(model,cov.reduce = function(x) c(mean(x)+1,mean(x)-1))
emmeans(grid, formula(~ rank|admit),mode = "prob")

rg<-ref_grid(model)
rg@grid

K=4
N=K*2*100
gre<-rnorm(N,0,1)
admit<-factor(rep(c(1:2),N/2))
rank<-as.numeric(admit)
rank<-ifelse(runif(N)>.5,rank+1,rank)
x<-factor(rep(c(1:2),each=N/2))

formula <- as.formula(rank ~ admit)
model <- nnet::multinom(formula)
model$call$formula <- formula
model
traceback()
grid<-ref_grid(model,cov.reduce = function(x) c(mean(x)-1,mean(x)+1))
grid<-ref_grid(model)
grid@grid
grid@V
grid@bhat
coef(model)
emmeans(grid, formula(~ rank|admit),mode = "prob",tol=0.00000000000000000000000000000001)
1:3 %*% 3
use.elts<-nrow(ww@grid)
ww@linfct<-ww@linfct[1:8,1:8]

.qf.non0
dat<-read.csv("../../gaml_docs/data/neuralgia.csv")
names(dat)

formula <- as.formula(Treatment ~ Sex + Age)
model <- nnet::multinom(formula,data=dat,model=T)
model
model$call$formula<-formula
dat$Age<-as.numeric(as.character(dat$Age))
ref_grid(model,cov.reduce = T)
emmeans(model, formula(~ Treatment|Sex),mode = "prob")


#x*y*z are all categorical (2*2*3 levels)
x<-c(1,2)
y<-c(1,2)
z<-c(1,2,3)
q1<-expand.grid(x,y,z)
q2<-expand.grid(x,y,z)
for (i in 1:50000) 
  q2<-rbind(q2,q1)
q<-as.data.frame(q2)
q$dep<-rnorm(nrow(q))
names(q)<-c("x","y","z","dep")
model<-lm(dep~x*y*z,data=q)
library(emmeans)
contrasts_sel_mod_hap <-emmeans(model, pairwise~x*y*z, adjust = "Tukey", CIs = TRUE, pbkrtest.limit = 239335)
summary(contrasts_sel_mod_hap)
table(q$x,q$y,q$z)


data<-read.csv("extdata/twobytwo.csv")
head(data)
data$fac<-factor(data$fac)
levels(data$fac)<-c("f1","f2")
data$bac<-factor(data$bac)
levels(data$bac)<-c("b1","b2")

model<-lm(y~fac*bac,data = data)
afun<-function(x,...) sandwich::vcovHC(x,type="HC3",...)
is.something(afun)
term<-c("fac","bac")
.revterm<-rev(term)
.term<-as.formula(paste0("pairwise~",jmvcore::composeTerm(.revterm)))

opts_list<-list(object=model,vcov.=afun,specs=.term)
(rg<-do.call(emmeans::emmeans,opts_list))


tableData<-summary(graphics::pairs(rg))
tableData
.cont <- as.character(tableData$contrast)
.cont <- gsub(" - ", "-", .cont, fixed = T)
.cont <- gsub(" / ", "/", .cont, fixed = T)

.labs <- sapply(.cont, function(a) {
  sapply(strsplit(as.character(a), "[- ,/]"), trimws, USE.NAMES = F, simplify = F)
})

labs <- do.call("rbind", .labs)

labs
.vars<-make.names(.revterm,unique = T)
cols <- c(paste0(.vars,"_lev1"),paste0(.vars,"_lev2"))
colnames(labs) <- cols
labs


opts_list<-list(object=model,specs=~fac*bac)
(rg<-do.call(emmeans::emmeans,opts_list))
a<-summary(graphics::pairs(rg))


opts_list<-list(object=model,vcov.=afun,specs=pairwise~fac*bac)
(rg<-do.call(emmeans::emmeans,opts_list))
parameters::parameters(rg$contrasts,ci_method="bcai")

.model<-parameters::bootstrap_model(model)
opts_list$object<-.model
(rg<-do.call(emmeans::emmeans,opts_list))
parameters::parameters(rg$contrasts,ci_method="quantile")
parameters::parameters(rg$contrasts,ci_method="bcai")


opts_list<-list(object=model,vcov.=afun,specs=~fac)
(rg<-do.call(emmeans::emmeans,opts_list))
opts_list$object<-.model
(rg<-do.call(emmeans::emmeans,opts_list))

lmtest::bgtest(model)
lmtest::bptest(model)


data<-read.csv("extdata/exercise.csv")
names(data)
form<-yendu~xage*zexer+I(zexer^2)
model<-lm(yendu~xage*zexer+I(zexer^2),data=data)
.model<-parameters::bootstrap_model(model,iterations = 100)
terms(model)
q<-emmeans::emtrends(model,specs="xage",var="I(zexer^2)",at=list(xage=c(30,40,50)),estName="estimate",ci_method="bcai")
aterm<-"zexer"
grep(paste0("I(",aterm),form,fixed=T)
form[[3]][[3]]
as.data.frame(parameters::model_parameters(q,ci_method="quantile"))
parameters::model_parameters(q,ci_method="bcai")

data<-read.csv("extdata/countriesandstuff.csv")
data$SVO<-factor(data$SVO)
data$GENDER<-factor(data$GENDER)
data$COUNTRYID<-factor(data$COUNTRYID)

model<-lmerTest::lmer(EC~SVO*GENDER+(1|COUNTRYID),data=data)

opts_list<-list(object=model,specs=~SVO*GENDER,lmer.df = "satterthwaite")
(rg<-do.call(emmeans::emmeans,opts_list))
opts_list<-list(object=model,specs=~SVO*GENDER,lmer.df = "kenward-roger")
(rg<-do.call(emmeans::emmeans,opts_list))

summary(graphics::pairs(rg),adjust="bonf")
summary(graphics::pairs(rg),adjust="holm")

opts_list<-list(object=model,specs=pairwise~SVO*GENDER,adjust="none")
(rg<-do.call(emmeans::emmeans,opts_list))
summary(rg)
summary(rg,adjust="holm")
summary(rg,adjust="bonf")
