library(lmerTest)
library(lme4)
dat<-read.csv("data/data3by2.csv")
dat$twogroups<-factor(dat$twogroups)
dat$threegroups<-factor(dat$threegroups)
contrasts(dat$twogroups)<-contr.sum(2)
contrasts(dat$threegroups)<-contr.sum(3)

model<-lm(y~twogroups+x+twogroups:x,data=dat)
lf.simpleEffects(model,"twogroups","x")

dat<-read.csv2("data/dat3x2x2_mixed.csv")
dat$cluster<-factor(dat$cluster)
dat$wfac<-factor(dat$wfac)
dat$bfac<-factor(dat$bfac)
library(lmerTest)
model<-lmer(y~(1|cluster)+wfac*bfac*x,data=dat)
#lf.simpleEffects(model,"x","wfac")
mf.confint(model,level=0.95)

## model logistic #####

dat<-read.csv("data/generalized.csv")
dat$bfac<-factor(dat$bfac)
dat$dic<-factor(dat$dic)
contrasts(dat$dic)<-contr.sum(2)
contrasts(dat$bfac)<-contr.sum(2)
dat$groups3<-factor(dat$groups3)
model<-glm(counts~x*bfac,data=dat,family = poisson())
#mf.confint(model,level=0.95)
#lf.meansTables(model,"bfac")
formula<-formula(~bfac)
q<-lsmeans::lsmeans(model, formula)
summary(pairs(q, adjust='tukey'))

qq<-lf.simpleEffects(model,"bfac","x")
a<-qq[[1]]
class(a)
a$estimate

####### multinomial ########à
library(nnet)
library(lsmeans)
names(dat)
dat$groups3<-factor(dat$group3)
contrasts(dat$groups3)
model<-multinom(groups3 ~bfac*dic, data = dat, model = TRUE)
lsm = lsmeans::lsmeans(model, ~ groups3|bfac:dic, mode = "latent")
#lsm = lsmeans::lsmeans(mult, ~ groups3|bfac, mode = "prob")
cmp = pairs(lsm,  by="groups3",interaction=F) 
cmp
########## this works ########
test = test(cmp, joint=TRUE, by="groups3") 
test
lp.preparePlotData(mult,"bfac")
summary(mult)
q<-lsmeans::lsmeans(model, formula)
ll<-aggregate(dat$y, list(dat$groups3,dat$bfac,dat$dic),length)
names(ll)<-c("groups3","bfac","dic","f")
pois<-glm(f~1+groups3*bfac+dic,data=ll,family = poisson())
summary(pois)
summary(mult)
ll
######### this is ok ##########
library(lsmeans)
lsm = lsmeans::lsmeans(model, ~ bfac, mode = "prob")
cmp = contrast(lsm, method="pairwise", ref=1) 
test = test(cmp, joint=TRUE, by="contrast") 


g1<-dat[dat$groups3!=2,]
contrasts(dat$dic)<-contr.treatment(2)
contrasts(dat$bfac)<-contr.treatment(2)
g1$groups3<-factor(g1$groups3)
model<-glm(groups3~ bfac *dic, data=g1, family=binomial())
summary(model)
summary(mult)


model<-glm(Freq ~ groups3 * bfac *dic, data=VA.tab, family=poisson)
summary(model)
means<-lsmeans::lsmeans(model,~groups3:bfac)
cc<-contrast(means,by="groups3")
summary(pairs(cc, adjust='tukey'))

VA.tab
data(VA,package = "MASS")
VA[,c("cell","treat")]
VA.tab <- table(VA[, c('cell', 'treat')])
model<-glm(Freq ~ cell * treat, data=VA.tab, family=poisson)

