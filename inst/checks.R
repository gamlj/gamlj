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

model<-glm(counts~x*bfac,data=dat,family = poisson())
#mf.confint(model,level=0.95)
lf.meansTables(model,"bfac")

qq<-lf.simpleEffects(model,"bfac","x")
a<-qq[[1]]
class(a)
a$estimate

####### multinomial ########à
library(nnet)
names(dat)
dat$groups3<-factor(group3)
model<-multinom(groups3 ~x*bfac*dic, data = dat, model = TRUE)
predict(model,type="probs")
lf.simpleEffects(model,"bfac","x")
#mf.confint(model,level=0.95)
terms(model)
preds<-mf.predict(model,dat)
data<-cbind(preds,dat)
head(data)
data$id<-seq_along(rownames(preds))
head(data)
library(reshape)
long<-reshape(data,varying=c("fit.1","fit.2","fit.3"),idvar="id",direction = "long")
library(ggplot2)
class(long$x)
groupName<-"bfac"
groupName<-"x"

linesName<-"bfac"
linesName<-NULL




.linesPlot<-function(data,theme,depName,groupName,title=NULL) {

  pnames<-names(data)[-1]
  data$id<-seq_len(length(data$group))
  long<-reshape(data,varying=pnames,idvar="id",direction = "long",v.names = "fit")
  depLabs<-paste(depName,"category",sep="\n")
  yLab<-paste("prob. of",depName,"category")
  dodge <- ggplot2::position_dodge(0)
  p <- ggplot2::ggplot(data=long, aes(x=group, y=fit, group=factor(time),colour=factor(time))) +
       geom_line(size=.8, position=dodge)+
       scale_y_continuous(limits=c(0,1)) 
  if (is.factor(long$group)) 
      p <- p + geom_point(shape=21, fill='white', size=3, position=dodge)
  
   p<-p+labs(x=groupName, y=yLab, colour=depLabs)   
   if (!is.null(title))
       p<-p+ ggtitle(title)
   p
}


.linesMultiPlot<-function(data,theme,depName,groupName,linesName=NULL) {
     
     if (!is.null(linesName)) {
      plots<-list()
      levels<-levels(plotData[["lines"]])
      for (level in levels) {
          sdata<-subset(data,lines==level)
          sdata$lines<-NULL
          title<-paste(linesName,"=",level)
          plots[[level]]<-.linesPlot(sdata,theme,depName,groupName,title = title)        
      }
      return(plots)
     } else
       return(.linesPlot(data,theme,depName,groupName))
}

groupName<-"x"
linesName<-"bfac"
#groupName<-"bfac"
#linesName<-"x"
linesName<-NULL

depName<-"groups3"
theme<-NULL

plotData<-lp.preparePlotData(model,groupName = groupName,linesName = linesName,plotsName = "dic")
head(plotData)
plotData$plots
data<-subset(plotData,plots==levels(plotData$plots)[1])
data$plots
p<-lp.linesMultiPlot(data,theme,depName,groupName = groupName,linesName = linesName)
print(class(p))
plotData<-lp.preparePlotData(model,groupName = groupName,linesName = NULL)

.linesMultiPlot(plotData,theme,depName,groupName = groupName,linesName = NULL)
library(nnet)

term<-c("bfac")
term<-c("bfac","dic")
tterm<-as.formula(paste("~",paste(term,collapse = ":")))
preds<-predict(update(model,tterm),type="probs")
pnames<-colnames(preds)
data<-cbind(preds,dat)
data$id<-seq_len(dim(data)[1])
long<-reshape(data,varying=pnames,idvar="id",direction = "long",v.names = "fit")
tab<-aggregate(long$fit,as.list(long[,c("time",term)]),mean)
names(tab)<-c("dep",term,"mean")

dat$groups3<-factor(dat$groups3)
model<-multinom(groups3 ~bfac, data = dat, model = TRUE)
lf.meansTables(model,"bfac")
data<-dat
summary(model)
dep<-"groups3"
data$groups3<-factor(data$groups3)
(levels<-levels(data[,dep]))
for (i in 2:length(levels)) {
      ldata<-subset(data,dep=levels[i])
      print(table(ldata[[dep]]))
}
ldata$groups3<-factor(ldata$groups3)
mod1<-glm(formula(model),ldata,family = binomial())
lsmeans::lsmeans(mod1,"bfac",type="response")
