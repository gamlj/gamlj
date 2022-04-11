
data<-read.csv2("../gamlj.github.io/data/beers_bars.csv")
head(data)
data$bar<-factor(data$bar)
data$beer<-as.numeric(as.character(data$beer))
data$smile<-as.numeric(as.character(data$smile))
model0<-lmer(smile~beer+(1|bar),data=data)
model<-lmer(smile~beer+(1+beer|bar),data=data)

cluster<-"bar"
term<- "beer"
ci_width<-.95
z<-qnorm((1-ci_width)/2,lower.tail = F)
re<-as.data.frame(lme4::ranef(model))
re$est.ci.lower<- re$condval - z*re$condsd
re$est.ci.upper<- re$condval + z*re$condsd

### here by term and cluster 
redata<-re[re$grpvar==cluster & re$term==term,]
fe<-fixef(model)[[term]]
redata$index<-1:nrow(redata)
redata$effect<-redata$condval+fe
redata$est.ci.lower<-redata$est.ci.lower+fe
redata$est.ci.upper<-redata$est.ci.upper+fe
k<-nrow(redata)
p<- ggplot(data = redata,aes(y=index, x=effect,xmin=est.ci.lower,xmax=est.ci.upper))  
p<- p +  geom_point(stat = "identity", size=2) 
p <- p + geom_errorbar()
p <- p + scale_y_continuous(name = "Clusters", breaks=1:k, labels = redata$grp, trans="reverse")
p <- p + geom_vline(xintercept=fe, color="black", linetype="dashed", alpha=.5)
p <- p + theme_classic() + xlab("Coefficient") 
p



data<-read.csv2("../gamlj.github.io/data/beers_bars.csv")
head(data)
data$bar<-factor(data$bar)
data$beer<-as.numeric(as.character(data$beer))
data$smile<-as.numeric(as.character(data$smile))
model0<-lmer(smile~beer+(1|bar),data=data)
model<-lmer(smile~beer+(1+beer|bar),data=data)

cluster<-"bar"
term<- "beer"
ci_width<-.95
z<-qnorm((1-ci_width)/2,lower.tail = F)
re<-as.data.frame(lme4::ranef(model))
re$est.ci.lower<- re$condval - z*re$condsd
re$est.ci.upper<- re$condval + z*re$condsd

### here by term and cluster 
redata<-re[re$grpvar==cluster & re$term==term,]
fe<-fixef(model)[[term]]
redata$index<-1:nrow(redata)
redata$effect<-redata$condval+fe
redata$est.ci.lower<-redata$est.ci.lower+fe
redata$est.ci.upper<-redata$est.ci.upper+fe
k<-nrow(redata)
p<- ggplot(data = redata,aes(y=index, x=effect,xmin=est.ci.lower,xmax=est.ci.upper))  
p<- p +  geom_point(stat = "identity", size=2) 
p <- p + geom_errorbar()
p <- p + scale_y_continuous(name = "Clusters", breaks=1:k, labels = redata$grp, trans="reverse")
p <- p + geom_vline(xintercept=fe, color="black", linetype="dashed", alpha=.5)
p <- p + theme_classic() + xlab("Coefficient") 
p






res<-broom.mixed::tidy(model, effects = "ran_vals", conf.int = TRUE)
res<-as.data.frame(res)
res1<-res[res$term==term,]
res1
redata

data("hsbdemo")

data<-hsbdemo
head(hsbdemo)


.aestetics<-ggplot2::aes_string(x = "math", y = "write",   group = "ses", colour = "ses")

p <- ggplot2::ggplot()
#p <- p + ggplot2::scale_y_continuous(limits = NULL)

p <- p + ggplot2::geom_line(data = data, 
                            .aestetics,
                            size = 1.2)


p<-p+ggplot2::scale_x_continuous(labels= 1:6)

p

p$coordinates$modify_scales("a")
xaxis_ticks(on = TRUE, ...)
o<-ggplot2::ggplot_build(p)
o$layout$panel_params[[1]]$x$breaks

dd<-data.frame(a=1:4,b=letters[1:4])
aggregate(a~b,data=dd,mean)
pp<-ggpl
str(p$coordinates)

p$coordinates$setup_panel_guides
dodge<-ggplot2::position_dodge(0.2)
data<-aggregate(hsbdemo$write,by=list(hsbdemo$female,hsbdemo$ses),mean)
data<-aggregate(hsbdemo$write,by=list(hsbdemo$female),mean)
data
head(data)
names(data)<-c("female","ses","write")
names(data)<-c("female","write")

data$ymin<-data$write-3
data$ymax<-data$write+3

self<-list(options=list())
self$options$vars<-"y"
self$options$groups<-c("x","z")


d
.aestetics<-ggplot2::aes_string(x = "female", y = "write",   group = "ses", colour = "ses")

.aestetics<-ggplot2::aes_string(x = "female", y = "write",group=1)

a<-ggplot2::aes(x = x , y = y)

p <- ggplot2::ggplot()
#p <- p + ggplot2::scale_y_continuous(limits = NULL)

p <- p + ggplot2::geom_line(data = data, 
                            .aestetics,
                            size = 1.2)
p
#if (self$xFactor)
  p <- p +  ggplot2::geom_point(data=data,.aestetics,
                                shape = 16, size = 4,position=dodge)
  p<-p <- p + ggplot2::geom_errorbar(data = data, ggplot2::aes_string(ymin="ymin",ymax="ymax",x="female"),width=.5,  size = .9, position = dodge)


p
data
library(ggplot2)
df <- data.frame(x = c("a","b","c"), y = c(4, 1, 9))
base <- ggplot(df, aes(x, y))
base + geom_line(size = 10)



library(lme4)
levels(hdp$DID)[1]<-"a"
mod<-glmer(remission ~ IL6 +  CancerStage + LengthofStay + Experience +
             (1 | DID), data = hdp, family = binomial,nAGQ = 10)
summary(mod)
rdata<-pred.predictRandom(mod,"remission",groupName64 = "IL6",cluster="DID")
head(rdata)

#mod<-glmer(remission ~ IL6+
#             (1 | DID), data = hdp, family = binomial,nAGQ = 10)
#summary(mod)
#rdata<-pred.predictRandom(mod,"remission",groupName64 = "IL6",cluster="DID")
model=mod
cluster="DID"
groupName="IL6"
linesName="CancerStage"
plotsName=NULL
preds<-c(groupName,linesName,plotsName)
data<-model@frame
dd<-.predictRandom_continuous(model,groupName,cluster = cluster,linesName = linesName)
head(dd)


p <- ggplot2::ggplot(data=dd) 
p <- p + ggplot2::labs(x="x", y="predicted") 
p<-  p + ggplot2::geom_line(ggplot2::aes_string(x="group",y="y",group="cluster"),show.legend = F)

p
warnings()

pp<-ggeffects::ggpredict(mod,terms = c("CancerStage","IL6 [6.88,4.02,33]"), type="fe")
pp<-ggeffects::ggpredict(mod,terms = c("CancerStage","IL6 [6.88,4.02,33]","LengthofStay"), type="fe")
plot(pp,show.title = F,line.size = 1,connect.lines = T)
pp<-ggeffects::ggpredict(mod,terms = c("CancerStage","IL6"), type="fe")
pp<-ggeffects::ggpredict(mod,terms = c("IL6","CancerStage"), type="fe")
pp
pp$group
q<-plot(pp,connect.lines = T,use.theme = F)
q
q<-q+jmvcore::theme_default(palette = jmvcore::colorPalette())
q$theme
qq<-ggplot(pp, aes(x, predicted, group=group)) +  geom_line()
q$theme
qq<-qq+jmvcore::theme_default(palette = jmvcore::colorPalette())
qq

pp<-ggeffects::ggpredict(mod,terms = c("IL6"), type="re")
pp$predicted

image=list(state=list(data=pp))
image$state$data
gplots.oneWayPlot()
p<-plot(pp,ci = FALSE)
p
mean(hdp$IL6)+sd(hdp$IL6)

library(gamlj)
data("qsport")
names(qsport)
qsport$type
contrasts(qsport$type)<--contr.sum(2)
mod<-lm(performance~hours*type+I(hours^2)*type,data=qsport)
mod
pp<-ggeffects::ggpredict(mod,terms = c("hours [all]","type"))
#pp<-ggeffects::ggemmeans(mod,terms = c("hours"),)

setdiff(c("std.error", "conf.low"  ,"conf.high"),c("std.error","ciao"))
pp$y<-pp$predicted
.names<-names(pp)
a<-c(y="y",x="x")
ggplot2::aes_string(quote(a))
what<-setdiff(names(pp),c("std.error", "conf.low"  ,"conf.high","predicted"))
.aes<-ggplot2::aes_all(what)
.aes
p <- ggplot2::ggplot(data=pp) 
p<-  p + ggplot2::geom_smooth(method="auto",
                              .aes,show.legend = T)
p



p <- p + ggplot2::labs(x="x", y="predicted",group="group") 

 if (nlevels(pp$group)==1) {
         p<-  p + ggplot2::geom_smooth(method="auto",
                       ggplot2::aes_string(x="x",y="predicted"),show.legend = F)
 } else {
   p<-  p + ggplot2::geom_smooth(method="auto",
                                 ggplot2::aes_string(x="x",y="predicted",group="group",color="group"))
 }

if (errorType != '') {
#  p <- p + ggplot2::geom_errorbar(data=pp,aes_string(x="x", ymin="conf.low", ymax="conf.high"), width=.1,size=.8,show.legend = F)
  p <- p + ggplot2::geom_ribbon(aes_string(x="x", group="group",fill="group", ymin="conf.low", ymax="conf.high"),show.legend=F, alpha=.2)
  
}           

p
pp
table(qsport$hours)                         

library(lme4)
data("schoolexam")
data<-schoolexam[schoolexam$activity<4,]
data$math<-as.numeric(data$math)
ff<-pass ~ 1 + math + activity+( 1 | school )
mod<-glmer(ff,data=data,family = binomial())

library(gamlj)
library(lme4)
Sys.getlocale()
data("schoolexam")
schoolexam$activity<-factor(schoolexam$activity)
schoolexam$math<-schoolexam$math-mean(schoolexam$math)

formula = pass ~ 1 + math + activity+( 1 | school )
mod<-glmer(formula,data=schoolexam,family=binomial())
terms<-c("math","school")
sd(schoolexam$math)
condlist=list(math=pretty(schoolexam$math))
all.vars(mod@call)
ranef(mod)
est<-emmeans::emmeans(mod,specs=terms,at=condlist,nesting=NULL)
x<-1:10
cc<-coefficients(mod)
cc$school
data("schoolexam")
schoolexam$activity<-factor(schoolexam$activity)

gamlj::gamljGlmMixed(
  formula = pass ~ 1 + math + activity+( 1 | school ),
  data = schoolexam,
  plotHAxis = math,
  plotRandomEffects = TRUE,
  cimethod = "wald")


data("temptime")
data<-temptime
names(temptime)
data$cond<-factor(data$cond)
head(data)
gamlj::gamljMixed(
  formula = y ~ 1 + cond + timebin + I(timebin^2) + cond:timebin + cond:I(timebin^2)+( 1 + timebin  | subj ),
  data = data,
  plotHAxis = timebin,
  plotSepLines = cond,
  plotRandomEffects = TRUE)

formula = y ~ 1 + cond + timebin + I(timebin^2) + cond:timebin + cond:I(timebin^2)+( 1 + timebin  | subj )
library(lme4)
mod<-lmer(formula ,data = data)
mod@call
dd<-.predictRandom_continuous(mod,"timebin",cluster = "subj",linesName = "cond")
head(dd)


p <- ggplot2::ggplot(data=dd) 
p <- p + ggplot2::labs(x="x", y="predicted") 
p<-  p + ggplot2::geom_line(ggplot2::aes_string(x="group",y="y",group="cluster"),show.legend = F)

p

library(effects)
mod.cowles <- glm(volunteer ~ sex + neuroticism*extraversion,
                  data=Cowles, family=binomial)

ggeffects::values_at(rnorm(100))


eff.cowles <- allEffects(mod.cowles, xlevels=list(extraversion=seq(0, 24, 6)),
                         fixed.predictors=list(given.values=c(sexmale=0.5)))

Eff.ne <- Effect(c("neuroticism", "extraversion"), mod.cowles)

eff.cowles$`neuroticism:extraversion`

as.data.frame(eff.cowles[[2]])





# the following are equivalent:
eff.ne <- effect("neuroticism*extraversion", mod.cowles)
