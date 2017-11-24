library(lmerTest)
library(lme4)
dat<-read.csv("data/data3by2.csv")
dat$twogroups<-factor(dat$twogroups)
dat$threegroups<-factor(dat$threegroups)
contrasts(dat$twogroups)<-contr.sum(2)
contrasts(dat$threegroups)<-contr.sum(3)
names(dat)[2]<-"two groups"
model<-lm(y~`two groups`+threegroups+`two groups`:threegroups+x*`two groups`,data=dat)
mterms<-names(coef(model))
modelmatrix<-model.matrix(model,data)
(mlabs<-attr(terms(model),"term.label"))
(conts<-attr(modelmatrix,"contrasts"))
ndummy<-sapply(attr(model.matrix(model),"contrasts"),function(a) dim(a)[2])
ndummy
jmvcore::decomposeTerm("`c`:q")

for (mlab in mlabs ) {
   terms<-jmvcore::decomposeTerm(mlab)
   for (term in terms) 
     if (term %in% names(ndummy))
         print(ndummy[term])
}

levs<-levels(dat$`two groups`)
.contrastLabels(levs,"deviations")
levs<-levels(dat$threegroups)
.contrastLabels(levs,"deviations")
for (term in mlabs) {
   sterm<-strsplit(term,":",fixed = T)
   for (s in sterm)
      .contrastLabels(levels)
   }
ndummy

jmvcore::composeTerm(c("a","b"))
term<-jmvcore::composeTerm("two groups")
term<-formula(paste("~",term))
print(term)
referenceGrid<-emmeans::emmeans(model, term,type = "response",)
ss<-summary(pairs(referenceGrid))

referenceGrid<-emmeans::emmeans(model, ~`two groups`,type = "response")


summary(pairs(referenceGrid))

bsLevels=list()
for (i in seq_along(bs))
  bsLevels[[bs[i]]] <- levels(dat[[bs[i]]])
bsLevels
combin <- expand.grid(bsLevels[rev(ph)])
combin <- sapply(combin, as.character, simplify = 'matrix')
nrow(combin)
a<-NULL
cbind(rep(combin[1,],nrow(combin)),combin)
for(i in seq_len(nrow(combin)))
    a<-rbind(a,(cbind(combin[i,],combin)))
a
unique(a)
for (i in ncol(a):1)
           a<-a[order((a[,i])),]
unique(a)
q<-expand.grid(levels(dat$threegroups),levels(dat$twogroups))
do.call(expand.grid,as.list(q))
for(i in seq_len(dim(q)[1]))
   print(cbind(q[1,],q))
do.call(expand.grid,a)
referenceGrid<-emmeans::ref_grid(model, ~`twogroups`:`threegroups`,transform = "response")
summary(pairs(referenceGrid))
expand.grid(levels(dat$twogroups),levels(dat$threegroups))
levs1<-levels(dat$threegroups)
levs2<-levels(dat$twogroups)
referenceGrid
pairs(x = levs1)
pairs()

library(lmerTest)
dat<-read.csv("data/dat3x2x2_mixed.csv")
dat$cluster<-factor(dat$cluster)
dat$wfac<-factor(dat$wfac)
dat$bfac<-factor(dat$bfac)
contrasts(dat$wfac)<-contr.sum(2)
model<-lmer(y~(1+x|cluster)+x,data=dat)
summary(model)

preds<-predict(model)
library(ggplot2)
preds<-cbind(preds,dat)
cc<-coef(model)
cc<-cc$cluster
head(cc)
ccl<-unique(dat$cluster)
p <- ggplot(data=preds, aes(x=x, y=preds, group=factor(cluster))) +
  geom_point(aes(colour=cluster))+
  geom_abline(data = cc, aes(slope =x, intercept =`(Intercept)`,colour =ccl, alpha=ccl)) +
  labs(x="groupName", y="depName") +
  scale_y_continuous(limits=c(min(dat$y), max(dat$y))) +
p
iris$Petal.Width
hist(dat$x)
dat<-read.csv("inst/checdata.csv")
dat$clu<-factor(dat$clu)
dat$fac<-factor(dat$x)
contrasts(dat$fac)<-contr.sum(2)/2
dat$x<-dat$x/2
#dat$y<-dat$y-mean(dat$y)
model<-lmer(y~1+(1|clu)+(0+x|clu),data=dat)
ranef(model)
(cc<-coef(model))
cor(cc$clu)
summary(model)
tapply(dat$y-mean(dat$y),dat$clu,mean)
model<-lmer(y~1+(1+fac|clu),data=dat)
ranef(model)
(cc<-coef(model))
cor(cc$clu)
cc$clu[2]-cc$clu[1]
summary(model)

model<-lmer(y~1+(1+fac|clu),data=dat)
ranef(model)
(cc<-coef(model))
cor(cc$clu)
summary(model)


dat<-read.csv("data/generalized.csv")
dat$bfac<-factor(dat$bfac)
dat$dic<-factor(dat$dic)
contrasts(dat$dic)<-contr.sum(2)
contrasts(dat$bfac)<-contr.sum(2)
dat$groups3<-factor(dat$groups3)
model<-glm(counts~groups3,data=dat,family = gaussian())

model$family=="gaussian"

model<-glm(dic~groups3*bfac,data=dat,family = binomial())
term="group3"
term<-jmvcore::composeTerm(term)
referenceGrid<-emmeans::ref_grid(model, ~term,transform = "response")
summary(pairs(referenceGrid))

model$aic-(2*2)

model0<-glm(counts~1,data=dat,family = poisson())
r<-logLik(model0)
1-(f/r)
summary(model)
####### multinomial ########à
library(nnet)
library(emmeans)
names(dat)
contrasts(dat$groups3)

model<-multinom(bfac ~groups3+counts, data = dat, model = TRUE)
model<-multinom(groups3 ~bfac+y, data = dat, model = TRUE)
summary(model)
car::Anova(model)

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




