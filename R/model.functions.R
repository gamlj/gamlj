
##### function to check results ####

.which.class<-function(model) {
  if (class(model)[[1]]=="glm")
    return("glm")
  if ("lm" %in% class(model))
      return("lm")
  if ("lmerModLmerTest" %in% class(model))
    return("lmer")
  if ("merModLmerTest" %in% class(model))
      return("lmer")
  if ("lmerMod" %in% class(model))
      return("lmer")
  if ("multinom" %in% class(model))
    return("multinomial")
  
}

mf.aliased<-function(model) {
  if (.which.class(model)=="lm" || .which.class(model)=="glm") {  
  aliased<-alias(model)
  if (!is.null(aliased$Complete))
    return(TRUE)
  return(FALSE)
  }
  if (.which.class(model)=="lmer") {  
     rank<-attr(model@pp$X,"msgRankdrop")
     if (!is.null(rank))
        return(TRUE)
     return(FALSE)
  }
  FALSE
}

mf.predict_response<-function(model) {
  if (.which.class(model)=="glm" || .which.class(model)=="lm") {
  return(  predict(model,type="response"))    
  }
  if (.which.class(model)=="lmer") {
    return(  predict(model,re.form=~0))    
  }
} 
  
##### predictions for the plots #######

mf.predict<- function(x,...) UseMethod(".predict")


.predict.lm<-function(model,data=NULL,bars="none") {
    preds<-predict(model,data,se.fit = T,interval="confidence",type="response")
    if (bars=="none")
      return(data.frame(fit=preds$fit[,1]))
    if (bars=="ci") {
      fit<-preds$fit[,1]
      lwr<-preds$fit[,2]
      upr<-preds$fit[,3]
    }
    if (bars=="se") {
      fit<-preds$fit[,1]
      lwr<-preds$fit[,1]-preds$se.fit
      upr<-preds$fit[,1]+preds$se.fit
    }  
    return(as.data.frame(cbind(fit=fit,lwr=lwr,upr=upr)))
  }
  
.predict.glm<-function(model,data=NULL,bars="none") {
  
    preds<-predict(model,data,se.fit = T,type="link")
    if (bars=="none")
      return(data.frame(fit=model$family$linkinv(preds$fit)))
    if (bars=="ci")
        critval<-qnorm(0.975)
    if (bars=="se")
        critval<-1
    upr <- preds$fit + (critval * preds$se.fit)
    lwr <- preds$fit - (critval * preds$se.fit)
    fit<-preds$fit
    fit <- model$family$linkinv(fit)
    upr <- model$family$linkinv(upr)
    lwr <- model$family$linkinv(lwr)
    return(as.data.frame(cbind(fit,lwr,upr)))
  }

.predict.merModLmerTest<-function(model,data=NULL,bars="none") 
             return(.predict.lmer(model,data,bars))
  
.predict.lmer<-function(model,data=NULL,bars="none") {

      return(data.frame(fit=predict(model,data,re.form=~0)))
}

.predict.multinom<-function(model,data=NULL,bars="none") {
  
  return(data.frame(fit=predict(model,data,type="probs")))
}

##### get model data #########

mf.getModelData<- function(x,...) UseMethod(".getModelData")

.getModelData.default<-function(model) 
     return(model$model)

.getModelData.merModLmerTest<-function(model) 
     return(.getModelData.lmer(model))

.getModelData.lmerModLmerTest<-function(model) 
  return(.getModelData.lmer(model))

.getModelData.lmer<-function(model) 
  return(model@frame)




#### they are legacy now. Those function are not needed for simple effects and plots. They get a model as imput and gives back a function
#### to reestimate the same kind of model

mf.estimate<-function(model) {
  if (.which.class(model)=="lm") {  
     return(function(form,data) stats::lm(form,data))
  }  

  if (.which.class(model)=="lmer") {  
    return(function(form,data)
      do.call(lmerTest::lmer, list(formula=form, data=data,REML=!is.na(model@devcomp$cmp["REML"]))))
  }
  if (.which.class(model)=="glm") {  
    return(function(form,data)
      stats::glm(form,data,family=model$family))
  }  
  if (.which.class(model)=="multinomial") {  
    return(function(form,data)
       nnet::multinom(form,data,model = T))
  }  
  
}

############# produces summary in a somehow standard format ##########

mf.summary<- function(x,...) UseMethod(".mf.summary")

.mf.summary.default<-function(model) {
      return(FALSE)
}
  
.mf.summary.lm<-function(model){
  smr<-summary(model)
  ss<-as.data.frame(smr$coefficients)
  ss$df<-smr$df[2]
  colnames(ss)<-c("estimate","se","t","p","df")
  as.data.frame(ss,stringsAsFactors = F)
}

.mf.summary.merModLmerTest<-function(model)
       .mf.summary.lmer(model)

.mf.summary.lmerModLmerTest<-function(model)
  .mf.summary.lmer(model)

.mf.summary.lmerMod<-function(model)
  .mf.summary.lmer(model)

.mf.summary.lmer<-function(model) {
       ss<-summary(model)$coefficients
       ss<-as.data.frame(ss,stringsAsFactors = F)
       
      if (dim(ss)[2]==3) {
        
          ano<-car::Anova(model,test="F",type=3)
          lnames<-rownames(ss)
          matching<-which(lnames %in% rownames(ss))
          ss$df<-NA
          ss$df[matching]<-ano$Df.res[matching]
          ss$p<-NA
          ss$p[matching]<-ano$`Pr(>F)`[matching]
          ss<-ss[,c(1,2,4,3,5)]       
          if (any(is.null(ss$df)))
               attr(ss,"warning")<-"lmer.df"
      }
      colnames(ss)<-c("estimate","se","df","t","p")
      ss
}

.mf.summary.glm<-function(model) {
     
     ss<-summary(model)$coefficients
     expb<-exp(ss[,"Estimate"])  
     ss<-cbind(ss,expb)
     colnames(ss)<-c("estimate","se","z","p","expb")
     as.data.frame(ss,stringsAsFactors = F)
}

.mf.summary.multinom<-function(model) {
  
     sumr<-summary(model)
     rcof<-sumr$coefficients
     cof<-as.data.frame(matrix(rcof,ncol=1))
     names(cof)<-"estimate"
     cof$dep<-rownames(rcof)
     cof$variable<-rep(colnames(rcof),each=nrow(rcof))
     se<-matrix(sumr$standard.errors,ncol=1)
     cof$se<-as.numeric(se)
     cof$expb<-exp(cof$estimate)  
     cof$z<-cof$estimate/cof$se
     cof$p<-(1 - pnorm(abs(cof$z), 0, 1)) * 2
     ss<-as.data.frame(cof,stringsAsFactors = F)
     ss<-cof[order(ss$dep),]
     lab<-model$lab[1]
     ss$dep<-sapply(ss$dep, function(a) paste(a,"-",lab))
     ss
}
  
############# produces anova/deviance table in a somehow stadard format ##########
mf.anova<- function(x,...) UseMethod(".anova")

.anova.default<-function(model) {
 stop("no suitable model found") 
}
  
.anova.glm<-function(model) {

        ano<-car::Anova(model,test="LR",type=3,singular.ok=T)
        colnames(ano)<-c("test","df","p")
        as.data.frame(ano, stringsAsFactors = F)
}

.anova.multinom<-function(model) {

      ano<-car::Anova(model,test="LR",type=3,singular.ok=T)
      colnames(ano)<-c("test","df","p")
      as.data.frame(ano, stringsAsFactors = F)
      
  }
  
.anova.lm<-function(model) {
    ano<-car::Anova(model,test="F",type=3, singular.ok=T)
    colnames(ano)<-c("ss","df","f","p")
    dss<-ano[-1,]
    sumr<-summary(model)
    errDF<-sumr$fstatistic[3]
    modDF<-sumr$fstatistic[2]
    modF<-sumr$fstatistic[1]
    errSS<-dss$ss[length(dss$ss)]
    errMS<-errSS/errDF
    r2<-sumr$r.squared
    totalSS<-1/((1-r2)*(1/errSS))
    modSS<-totalSS-errSS
    modp<-1 - pf(modF, modDF, errDF) 
    modRow<-c(ss=modSS,df=modDF,F=modF,p=modp)
    ss<-rbind(modRow,dss)
    ss$ms<-ss$ss/ss$df
    ss$etaSq<-ss$ss/(totalSS)
    ss$etaSqP <- ss$ss / (ss$ss + errSS)
    ss$omegaSq <- (ss$ss - (ss$df * errMS)) / (totalSS + errMS)
    lt<-length(ss$df)
    ss[lt,c("f","p","etaSq","etaSqP","omegaSq")]<-NA
    ss
}

.anova.lmerModLmerTest<-function(model,df="Satterthwaite") 
   .anova.merModLmerTest(model,df) 
     
.anova.lmerMod<-function(model,df) 
       .anova.merModLmerTest(model,df) 
         
.anova.merMod<-function(model,df="Satterthwaite") 
           .anova.merModLmerTest(model,df) 
             
.anova.merModLmerTest<-function(model,df="Satterthwaite") {

  ### this check is needed for adjust to different versions of lmertest
  if (class(model)=="merModLmerTest")
        ano<-lmerTest::anova(model,ddf=df)
  else
        ano<-anova(model,ddf=df)
        
  if (dim(ano)[1]==0)
    return(ano)
  
  if (dim(ano)[2]==4) {
    ano<-.car.anova(model,df)
  } else {
  ano<-ano[,c(5,3,4,6)]
  attr(ano,"method")<-df
  attr(ano,"statistic")<-"F"
  }
  if (!all(is.na(ano[,3])==F)) {
    # lmerTest 2.0-33 does not produce the F-test for continuous IV if they appear in the model before
    # the factors. Here we try to fix it.
    info("lmerTest problems with df: try to fix it")
    whichone<-is.na(ano[,3])
    rn<-rownames(ano[whichone,])
    smr<-lmerTest::summary(model,ddf=df)
    coefz<-as.data.frame(smr$coefficients)
    coefz<-coefz[rownames(smr$coefficients) %in% rn,3:5]

    if (dim(coefz)[1]!=length(rn))
      ano<-.car.anova(model,df)
    else {
        coefz<-data.frame(coefz)
        
        if (dim(coefz)[2]==1)
        coefz<-as.data.frame(t(coefz))
        rownames(coefz)<-rn
        coefz[,2]<-coefz[,2]^2
        coefz$df1<-1
        coefz<-coefz[,c(2,4,1,3)]
        ano[rownames(ano) %in% rownames(coefz),]<-coefz
        attr(ano,"method")<-df
        attr(ano,"statistic")<-"F"
    }
  }
  # here we fix the column names depending on the estimation that succeeded
  if (attr(ano,"statistic")=="Chisq")
     names(ano)<-c("test","df1","p")
  else 
    names(ano)<-c("test","df1","df2","p")
  
  return(ano)
  
}

.car.anova<-function(model,df) {
  info("lmerTest failed: anava uses car::Anova")
  if (model@devcomp$dims["REML"]==0) 
    test<-"Chisq"
  else test<-"F"
  ano<-car::Anova(model,type=3,test=test)
  if (attr(terms(model),"intercept")==1)
    ano<-ano[-1,]
  attr(ano,"method")<-"Kenward-Roger"
  attr(ano,"statistic")<-test
  ano
}


mf.getModelFactors<-function(model) {
  names(attr(model.matrix(model),"contrasts"))
} 


mf.getModelMessages<-function(model) {
  message<-list()
  if (.which.class(model)=="lmer") {
  eigen<-model@optinfo$conv$lme4$messages[[1]]
  if (!is.null(eigen))
      message["eigen"]<-eigen
  }
  message
}


mf.give_family<-function(modelSelection) {
  
  if (modelSelection=="linear")
       return(gaussian())
  if (modelSelection=="logistic")
       return(binomial())
  if (modelSelection=="poisson")
    return(poisson())
  if (modelSelection=="multinomial")
    return("multinomial")
  if (modelSelection=="nb")
      return("nb")
  if (modelSelection=="poiover")
      return(quasipoisson())
  if (modelSelection=="probit")
    return(binomial("probit"))
  
  
  NULL  
}

mf.checkData<-function(options,data,modelType="linear") {
     dep=jmvcore::toB64(options$dep)
     ########## check the dependent variable ##########
     if (modelType %in% c("linear","mixed"))
       ### here I need the dv to be really numeric
       data[[dep]] <- as.numeric(as.character(data[[dep]]))  
     
       if ( any(is.na(data[[dep]])) ) {
          nice=paste0(toupper(substring(modelType,1,1)),substring(modelType,2,nchar(modelType)))
          return(paste(nice,"model requires a numeric dependent variable"))
       }
               
     if  (modelType=="poisson" || modelType=="nb" || modelType=="poiover") {
         ### here I need the dv to be really numeric
       data[[dep]] <- as.numeric(as.character(data[[dep]]))
       if (any(data[[dep]]<0))
             return("Poisson-like models require all positive numbers in the dependent variable")
     }
      if (modelType=="logistic" || modelType=="probit") {
          data[[dep]] <- factor(data[[dep]])
          if (length(levels(data[[dep]]))!=2)
               return(paste(modelType,"model requires two levels in the dependent variable"))
      } 
     if (modelType=="multinomial") {
            data[[dep]] <- factor(data[[dep]])
            if (length(levels(data[[dep]]))<3)
                   return("For 2-levels factors please use a logistic model")
     }
     ####### check the covariates usability ##########
     covs=options$covs
     
     for (cov in covs) {
       cov<-jmvcore::toB64(cov) 
       data[[cov]]<-as.numeric(as.character(data[[cov]]))
       if (any(is.na(data[[cov]])))
          return(paste("Covariate",jmvcore::fromB64(cov), "cannot be converted to a numeric variable"))
     }
     # check factors
     factors=options$factors
     
     for (factorName in factors) {
       factorName<-jmvcore::toB64(factorName)
       lvls <- base::levels(data[[factorName]])
       if (length(lvls) == 1)
         return(paste("Factor ",jmvcore::fromB64(factorName),"contains only a single level"))
       else if (length(lvls) == 0)
         return(paste("Factor ",jmvcore::fromB64(factorName),"contains no data"))
     }
     
     data
}


###### confidence intervals ##########
mf.confint<- function(x,...) UseMethod(".confint")

.confint.default<-function(model,level) 
  return(FALSE)

.confint.lm<-function(model,level) 
    return(confint(model,level = level))
  
.confint.glm<-function(model,level) {  
    ci<-confint(model,level = level)
    return(ci)
}

.confint.merModLmerTest<-function(model,level) 
                                return(.confint.lmer(model,level))

.confint.lmerModLmerTest<-function(model,level) 
  return(.confint.lmer(model,level))

.confint.lmer<-function(model,level)  {

      ci<-confint(model,method="Wald",level=level)
      ci<-ci[!is.na(ci[,1]),]
      if (is.null(dim(ci)))
          ci<-matrix(ci,ncol=2)
      return(ci)
  }

.confint.multinom <- function (object, level = 0.95, ...) 
  {
  ci<-confint(object,level=level)
  cim<-NULL
  for (i in seq_len(dim(ci)[3]))
     cim<-rbind(cim,(ci[,,i]))
  return(cim)
}

###### post hoc ##########
mf.posthoc<- function(x,...) UseMethod(".posthoc")

.posthoc.default<-function(model,term,adjust) {
#  term<-jmvcore::composeTerm(term)
  term<-as.formula(paste("~",term))
  data<-mf.getModelData(model)
  referenceGrid<-emmeans::emmeans(model, term,type = "response",data=data)
  table<-summary(pairs(referenceGrid),adjust=adjust)
  table[order(table$contrast),]
}

.posthoc.multinom<-function(model,term,adjust) {
  results<-try({
  dep<-names(attr(terms(model),"dataClass"))[1]
  dep<-jmvcore::composeTerm(dep)
  term<-jmvcore::composeTerm(term)
  terms<-paste(term,collapse = ":")
  tterm<-as.formula(paste("~",paste(dep,terms,sep = "|")))  
  data<-mf.getModelData(model)
  referenceGrid<-emmeans::emmeans(model,tterm,transform = "response",data=data)
  summary(pairs(referenceGrid, by=dep, adjust=adjust))
  })
  return(results)
  }

###### means tables legacy: now done with predict.R ##########
mf.means<- function(x,...) UseMethod(".means")

.means.default<-function(model,term) {
  data=mf.getModelData(model)
  nvar<-length(term)
  table<-emmeans::emmeans(model,term,type = "response",data=data)
  table<-as.data.frame(summary(table))
  ff<-paste0("c",1:nvar)
  colnames(table)<-c(ff,"lsmean","se","df","lower","upper")
  for (f in ff)
    table[[f]]<-as.character(table[[f]])
  table
}

.means.multinom<-function(model,term) {
  data<-mf.getModelData(model)
  try({
  dep<-names(attr(terms(model),"dataClass"))[1]
  dep<-jmvcore::composeTerm(dep)
  nvar<-length(term)
  term<-jmvcore::composeTerm(term)
  terms<-paste(term,collapse = ":")
  tterm<-as.formula(paste("~",paste(dep,terms,sep = "|")))  
  table<-emmeans::emmeans(model,tterm,type = "response",data=data)
  table<-as.data.frame(summary(table))
  ff<-paste0("c",1:nvar)
  colnames(table)<-c("dep",ff,"lsmean","se","df","lower","upper")
  table<-table[order(table$dep),]
  table$dep<-as.character(table$dep)
  for (f in ff)
       table[[f]]<-as.character(table[[f]])
  table
  }) 
}

mf.getAIC<- function(x,...) UseMethod(".getAIC")

.getAIC.default<-function(model)
     return(model$aic)

.getAIC.multinom<-function(model)
    return(model$AIC)

mf.getValueDf<- function(x,...) UseMethod(".getValueDf")

.getValueDf.default<-function(model) {
  value <- sum(residuals(model, type = "pearson")^2)
  result <- value/model$df.residual
  return(result)
}
.getValueDf.multinom<-function(model) {
    return(NULL)
}

mf.getResDf<- function(x,...) UseMethod(".getResDf")

.getResDf.default<-function(model) {
  return(model$df.residual)
}
.getResDf.multinom<-function(model) {
  return(model$edf)
}

