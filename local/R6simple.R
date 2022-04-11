library(jamm)
data(coopmedmod)
data$group<-ifelse(runif(length(data$prime))>.75,2,data$prime)
data$X<-rnorm(length(data$group))
data$prime<-factor(data$prime)
data$group<-factor(data$group)
nLevels<-length(levels(data$group))
dummy <- stats::contr.treatment(levels(data$group))
dimnames(dummy) <- NULL
coding <- matrix(rep(1/nLevels, prod(dim(dummy))), ncol=nLevels-1)
contrast <- (dummy - coding)
contrasts(data$group)<-contrast
model<-lm(BEH~group*SVO*EXP*X,data=data)

summary(model)
#ref<-emmeans::emmeans(model,specs="SVO",at=list(SVO=c(-50,0,50)))
e<-emmeans::emtrends(model,specs = c("EXP","group","X"), var = c("SVO"), at = list("EXP"=c(-15,0,50),X=c(1,0)))  

e<-emmeans::emtrends(model,specs = c("EXP","group","X"), var = c("SVO"), at = list("EXP"=c(-15,0,50),X=c(1,0)))  

referenceGrid<-emmeans::emmeans(model,specs=c("group","EXP","SVO", "X"),at=list("EXP"=c(-15,0,50),X=c(1,0),SVO=c(-20,0,20)),nesting = NULL,lmer.df = "Satterthwaite")

estimates<-emmeans::contrast(referenceGrid,
                             by=c("SVO", "X"),
                             interaction = list(.myfun.emmc))

estimates





cc<-emmeans::contrast(e,by="EXP",interaction=list(.myfun.emmc))

str(cc)
cc@levels$EXP<-c("a","b","c")

q<-as.data.frame(cc)

q$EXP<-factor(q$EXP)
levels(q$EXP)<-c("a","b","c")
as.character(q$EXP)
contrasts(data$group)

.myfun.emmc<-function(levels) {
    print(levels)
    nLevels<-length(levels)
    dummy <- stats::contr.treatment(levels)
    dimnames(dummy) <- NULL
    coding <- matrix(rep(1/nLevels, prod(dim(dummy))), ncol=nLevels-1)
    contrast <- (dummy - coding)
    contrast<-as.data.frame(MASS::ginv(t(contrast)))
    return(contrast)
}


q<-.myfun.emmc(c(1,2,3))
q[,1]
xx<-as.data.frame(e[c(2,5,8),])
xx$SVO.trend%*%q[,1]
xx$SVO.trend%*%q[,2]

moderators<-c("EXP","group","X")
var="SVO"
n<-length(moderators)
j<-n
params<-list()
anovas<-list()
while(j>1) {
        mods<-moderators[j:n]
        conditions<-lapply(seq_along(moderators),function(i) {
                          if (moderators[i]=="group")
                                return(NULL)
                           if (i<j) c(0,1) else c(-3,0,3)
                      })
        names(conditions)<-moderators
        conditions<-conditions[sapply(conditions, is.something)]
        emgrid<-emmeans::emtrends(model,specs = moderators, var = var, at = conditions) 
        resgrid<-emmeans::contrast(emgrid,by=mods,method=.myfun.emmc,interaction=".myfun")
        .names<-setdiff(moderators,mods)
        print(jmvcore::stringifyTerm(c(var,.names)))
        res<-as.data.frame(resgrid)
        names(res)[1:length(.names)]<-.names
        params[[length(params)+1]]<-res
        res<-emmeans::test(resgrid,by=mods,join=T)
        print(ncol(res))
        names(res)[(ncol(res)-3):ncol(res)]<-c("df1","df2","f","p")
        anovas[[length(anovas)+1]]<-res
        j<-j-1
}
params
anovas
car::Anova(model)

ff<-list.files("R/")
for (f in ff)
      source(paste0("R/",f))
options<-list("dep"="BEH",covs=c("SVO","EXP"),factors=c("prime"),
              simpleScale="mean_sd",
              scaling=list(list(var="SVO",type="centered"),list(var="EXP",type="centered")),
              dep_scale="centered",
              simpleScaleLabels="labels")
  

dm<-Datamatic$new(options,data)
data64<-dm$cleandata(data)
dm$variables[[tob64("SVO")]]
terms<-c("group","SVO","EXP")
terms64<-tob64(terms)
suppressWarnings({
      labels<-sapply(terms64,function(term) {
      rep(dm$variables[[term]]$levels_labels,each=dm$variables[[term]]$nlevels) 
      },simplify = FALSE)
})
labels<-labels[!sapply(labels,is.null)] 
labels
form<-"BEH~prime*SVO*EXP"
#form<-"BEH~prime"

form<-"BEH~prime + SVO"

form64<-tob64(form,names(data))
model<-lm(form64,data=data64)
x<-tob64("SVO")
terms<-c("group")
terms64<-tob64(terms)
conditions<-list(SVO=c(10,50,90))
(referenceGrid<-emmeans::ref_grid(model,x,at=conditions))

(referenceGrid<-emmeans::emmeans(model,x,at=conditions))
dd<-as.data.frame(referenceGrid)


data
model0<-lm(y1~1,data=data)

model<-lm(form,data=data)
ss<-summary(model)
class(ss$coefficients[1,])
anova(model0)

model0<-glm(group_b~1,data=data,family = binomial())

ano<-car::Anova(model0,test="Chisq",type=3, singular.ok=T)
ano
ano<-car::Anova(model0,test="F",type=3, singular.ok=T)
ano
atable<-ano
atable<-ano[!(rownames(ano) %in% c("Residuals","(Intercept)")),]




parameters::parameters(atable,)

class(ss)
ss
variable64<-("EXP")
term64<-c("group","SVO")
conditions<-list(SVO=c(10,50,90),EXP=c(1,2,3))
(referenceGrid<-emmeans::emmeans(model,specs=c(variable64,term64),at=conditions,lmer.df = "Satterthwaite",nesting = NULL))
#lev<-list(EXP=c("a","b","c"))
args<-list(model,specs = term64, var = variable64, at = conditions,infer=c(T,T))
est <- do.call(emmeans::emtrends, args)
est
estimates
a<-emmeans::test(estimates,join=TRUE)

mf.addEffectSize(a)

class(a)
as.data.frame(effectsize::F_to_eta2(a$F.ratio,df=a$df1,df_error = a$df2))[1]

update(a)
ci<-as.data.frame(confint(estiamtes))
ci[,c(ncol(ci)-1,ncol(ci))]
as.data.frame(estimates)

packageVersion("emmeans")

(referenceGrid<-emmeans::ref_grid(model,var=x,specs=NULL,at=conditions))

emmeans::emmeans(referenceGrid,specs=x)




ar<-c("a","b","c")
referenceGrid@matlevs  <-list(SVO=c("a","b","c"))
dd<-as.data.frame(emmeans::emmeans(referenceGrid,specs=x))
dd$SVO[dd$SVO==conditions$SVO[1]]<-ar[1]
dd
referenceGrid<-emmeans::emmeans(model,x,by=terms64,at=conditions)
as.data.frame(referenceGrid)
referenceGrid@grid[[x]]<-tob64(as.character(referenceGrid@grid[[x]]))
datavar<-dm$variables[[x]]
emm<-emmeans::contrast(referenceGrid,method=.local.emmc,datamatic=datavar)
emm$contrast



data("hsbdemo")
names(hsbdemo)
form<-prog~math
model<-nnet::multinom(form,data=hsbdemo,model = T)
as.data.frame(parameters::parameters(car::Anova(model,type=3)))
parameters::standardize_names(parameters::parameters(car::Anova(model,type=3)))

form<-math~read+write
model<-lm(form,hsbdemo)
parameters::standardize_names(parameters::parameters(car::Anova(model,type=3)))
a<-car::Anova(model,type=3)
a<-a[row.names(a)!="(Intercept)",]
w<-cbind(effectsize::eta_squared(a,partial=F)[1:2],
      effectsize::eta_squared(a)[2],
      effectsize::omega_squared(a)[2],
      effectsize::epsilon_squared(a)[2])
params<-parameters::model_parameters(a)
merge(params,w,by="Parameter",all= T,sort = F)
class(a)<-c("model_lm",class(a))
class(a)
mf.anova(a)

form<-math~1+write
model<-lm(form,hsbdemo)
a<-car::Anova(model,type=3)
class(a)<-c("model_lm",class(a))
a
a<-c("a","b")
b<-c(1,2)
paste(a,b,sep="=")


data<-read.csv2("data/wicksell.csv")
data$dv<-as.numeric(as.character(data$dv))
head(data)
data$time<-factor(data$time)
data$group<-factor(data$group)

model<-lm(dv~time*group,data=data)
a<-NULL
aa<-car::Anova(model,type=3)
aa<-aa[rownames(aa)!="(Intercept)",]
suppressMessages({
a<-effectsize::eta_squared(aa)
})
a
data$X<-rnorm(length(data$prime))
xdata<-data
xdata$EXP<-xdata$EXP+15

model<-lm(BEH~prime*SVO*EXP*X,data=data)
model1<-lm(BEH~prime*SVO*EXP*X,data=xdata)
summary(model1)
summary(model)

car::Anova(model1,type=3)
ref<-emmeans::emmeans(model,specs="SVO",at=list(SVO=c(-50,0,50)))
e<-emmeans::emtrends(model,specs = c("EXP","prime","X"), var = c("SVO"), at = list("EXP"=c(-15,0,50),"prime"=c(0,1),X=0))  
e
.myfun.emmc<-function(levs) {
a<--contr.sum(length(levs))
as.data.frame(a)
}

emmeans::contrast(e,by=c("EXP"),method=.myfun.emmc)

emmeans::test(e,by=c("prime","EXP"),join=T)
emmeans::test(e,by=c("prime"),join=T)




form<-"prime~BEH + SVO"
library(pathj)
data(pathjdata)
pathjdata$groups_a<-factor(pathjdata$groups_a)
glm(groups_a~x1,data=pathjdata,family="binomial")


model0<-glm(prime~1,data=data,family = binomial())

model<-glm(form,data=data,family = binomial())
summary(model)

blorr::blr_rsq_mcfadden(model)
blorr::blr_rsq_mcfadden_adj(model)

1-(logLik(model)[1]/logLik(model0)[1])

model$null.deviance

-2*logLik(model0)[1]



1-(model$deviance/model$null.deviance)

1-((model$deviance+2*length(model$coefficients))/model$null.deviance)


car::Anova(model,type=3,test.statistic="LR")
drop1(model)
anova(model0,model,test = "LRT")

dd<-data.frame(a=letters[1:3])
levels(dd$a)<-c(1:3)
as.numeric(as.character(dd$a))
