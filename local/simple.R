library(jmvcore)

library(MASS)
library(emmeans)
.condition_values<-function(datavar,conditioning,span) {
  if (is.numeric(conditioning)){
    return(conditioning)
  }
  fun<-try(get(conditioning),silent = T)
  if (!isError(fun))
    return(fun(datavar))


  if (conditioning=="mean_sd") {
    .mean<-mean(datavar)
    .sd<-sd(datavar)
    return(round(c(.mean-.sd,.mean,.mean+.sd),digits = 3))
  }
  if (conditioning=="mean_offset") {
    .mean<-mean(datavar)
    return(round(c(.mean-span,.mean,.mean+span),digits = 3))
  }
  
  if (conditioning=="percent") {
    return(round(as.numeric(quantile(datavar,c(.25,.5,.75))),digits=3))
  }
  if (conditioning=="percent_offset") {
    return(round(quantile(datavar,c(.5-span,.5,.5+span)),digits=3))
  }
  
}

#### simple Effects estimates: parameters  table and anova table


simpleEstimates<-function(model,variable,moderator,threeway=NULL,conditioning="mean_sd",span=0,interval=95) {
  data<-mf.getModelData(model)
  interval=as.numeric(interval)/100
  ### prepare the conditioning values
  condlist<-list()
  .cond<-conditioning
  
  lnames<-"moderator"
  if (!is.null(unlist(threeway)))
    lnames<-c("threeway","moderator")
  
  preds<-unlist(c(threeway,moderator))
  for (name in preds)
    if (!is.factor(data[[name]])) {
      if (is.list(conditioning) && (name %in% names(conditioning)))
        .cond<-conditioning[[name]]
      mlevels<-.condition_values(data[[name]],conditioning=.cond,span=span)
      condlist[[name]]<-mlevels
    }  
  if (is.factor(data[[variable]])) {
    
    .internal.emmc<<-function(levs) {
      # remove possible average covariates values in the levels passed by emmeans
      levs<-sapply(levs,function(b) {
        v<-strsplit(b,",")[[1]]
        v[length(v)]
      })
      # get the contrast weights
      codes<-contrasts(data[[variable]])
      # transform the model matrix into the contrast matrix
      n<-length(levs)
      M <- as.data.frame(MASS::ginv(t(codes)))
      # set some labels
      names(M) <- lf.contrastLabels(levs,attr(data[[variable]],"jcontrast"))
      attr(M, "desc") <- attr(data[[variable]],"jcontrast")
      M
    }
    est<-emmeans::emmeans(model,specs=variable,by=preds,at=condlist)
    est<-emmeans::contrast(est,method=".internal")
    ci<-as.data.frame(confint(est,level = interval))
    params<-cbind(as.data.frame(est),ci[,c("lower.CL","upper.CL")])
    params$contrast<-as.character(params$contrast)
    names(params)[2:(length(lnames)+1)]<-lnames
    
  } else {
    est<-emmeans::emtrends(model,specs=preds,var=variable,at=condlist,options=list(level=.80))
    estf<-as.data.frame(est)
    ci<-estf[,c("lower.CL","upper.CL")]
    params<-cbind(emmeans::test(est),ci)
    names(params)[1:length(lnames)]<-lnames
    names(params)[length(lnames)+1]<-"estimate"
  }
  
  ff<-emmeans::test(est,joint=T,by=preds)
  names(ff)[1:length(lnames)]<-lnames
  
  ff<-ff[order(ff[,lnames[1]]),]
  params<-params[order(params[,lnames[1]]),]
  for (name in lnames) {
    ff[,name]<-as.character(ff[,name])
    params[,name]<-as.character(params[,name])
  }
  list(params,ff)
}


dat<-read.csv("extdata/dat3x2x2_mixed.csv")

dat$wfac3<-factor(dat$wfac3)
dat$wfac<-factor(dat$wfac)

dat$bfac<-factor(dat$bfac)
dat$cluster<-factor(dat$cluster)
library(lmerTest)
jcont<-"deviation"
contrasts(dat$wfac3)<-lf.createContrasts(levels(dat$wfac3),jcont)
attr(dat$wfac3,"jcontrast")<-jcont
contrasts(dat$wfac)<-lf.createContrasts(levels(dat$wfac),jcont)
attr(dat$wfac,"jcontrast")<-jcont

contrasts(dat$bfac)<-lf.createContrasts(levels(dat$bfac),jcont)
attr(dat$bfac,"jcontrast")<-jcont

contrasts(dat$bfac)<-lf.createContrasts(levels(dat$bfac),jcont)
attr(dat$bfac,"jcontrast")<-jcont
dat$x<-dat$x-mean(dat$x)
dat$z<-rnorm(length(dat$x))
model<-lmer(y~1+(1|cluster)+x*z*wfac3,data=dat,REML = T)
summary(model)
DEBUG=TRUE
model<-lmer(y~1+(1+wfac3|cluster)+bfac,data=dat,REML = T)
summary(model)
ranova(model,reduce.terms = T)
model<-lmer(y~1+(1|cluster)+bfac,data=dat,REML = T)
summary(model)
ranova(model,reduce.terms = T)

car::Anova(model,test="F")
ano<-do.call(lmerTest::anova,list(model))
ano<-ano[,c(5,3,4,6)]
DEBUG<-T
mf.anova(model)

library(lmerTest)
ranova(model)

q<-c(".dsdiau.1",".sdfi")
aa<-unlist(lapply(q, function(a) gsub("\\.[0-9]$","",a)))

library(gamlj)
install.packages(gamlj,)
gamlj::gamljMixed(
  data = dat,
  dep = "y",
  factors = "bfac",
  covs = "x",
  cluster = "cluster",
  randomTerms = list(
    c("Intercept", "cluster")),
  modelTerms = list(
    "x",
    "bfac",
    c("bfac", "x")),
  contrasts = list(
    list(
      var="bfac",
      type="deviation")),
  scaling = list(
    list(
      var="x",
      type="centered")),
  simpleVariable = "bfac",
  simpleModerator = "x")

as.formula(as.character(gg$info$asDF[2,2]))




library(gamlj)

gobj<-gamlj::gamljMixed(
  data = dat,
  dep = "y",
  factors = "bfac",
  covs = "x",
  cluster = "cluster",
  randomTerms = list(
    c("Intercept", "cluster")),
  modelTerms = list(
    "x",
    "bfac",
    c("bfac", "x")),
  contrasts = list(
    list(
      var="bfac",
      type="deviation")),
  scaling = list(
    list(
      var="x",
      type="centered")),
  plotHAxis = "x",
  plotSepLines = "bfac",
  simpleScale = "mean_offset",
  cvalue = 30,
  percvalue = 35)

  
  
gamlj_update(gobj,list("reml"=FALSE,"showParamsCI"=TRUE))
  
plotHAxis = "x"
plotSepLines = "bfac"
gamlj_update(gobj,list(plotHAxis = "x",plotSepLines = "bfac"))

gamlj_(gobj,list(plotHAxis = "x",plotSepLines = "bfac"))


tm<-theme_bw()  
gamlj_plot(gobj ,"x",seplines = "bfac",)

formals(gamljMixed)
  
  x<-gobj$options$plotHAxis        
  group<-gobj$options$plotSepLines    
  formula<-gamlj_model(gobj)
  GAMLj_DEBUG=T
  model<-  lmerTest::lmer(formula,data) 
  lp.preparePlotData(model,x,group,conditioning = "mean_sd")
  
