##### function to check results ####

.which.class<-function(model) {
  if (class(model)[[1]]=="glm")
    return("glm")
  if ("lm" %in% class(model))
      return("lm")
  if ("merModLmerTest" %in% class(model))
      return("lmer")
  if ("lmerMod" %in% class(model))
      return("lmer")
      
}

mf.aliased<-function(model) {
  if (.which.class(model)=="lm") {  
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

mf.predict<-function(model,data=NULL,bars="none") {
  if (.which.class(model)=="lm"){
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
  
  
  if (.which.class(model)=="glm"){
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
  
  if (.which.class(model)=="lmer"){
    return(data.frame(fit=predict(model,data,re.form=~0)))
  }

}


  

mf.getModelData<-function(model) {
  if (.which.class(model)=="lm")
           return(model$model)
  
  if (.which.class(model)=="lmer")
           return(model@frame)
  
  if (.which.class(model)=="glm")
           return(model$model)
  
  
}
#### those function are needed for simple effects and plots. They get a model as imput and gives back a function
#### to reestimate the same kind of model

mf.estimate<-function(model) {
  if (.which.class(model)=="lm") {  
     return(function(form,data)
       stats::lm(form,data))
  }  

  if (.which.class(model)=="lmer") {  
    return(function(form,data)
      do.call(lmerTest::lmer, list(formula=form, data=data,REML=!is.na(model@devcomp$cmp["REML"]))))
  }
  if (.which.class(model)=="glm") {  
    return(function(form,data)
      stats::glm(form,data,family=model$family))
  }  
  
  
}

mf.summary=function(model) {
  if (.which.class(model)=="lm")   
     ss<-summary(model)$coefficients
  if (.which.class(model)=="lmer")   
    ss<-lmerTest::summary(model)$coefficients
  if (.which.class(model)=="glm") {   
    ss<-summary(model)$coefficients
    expb<-exp(ss[,"Estimate"])  
    ss<-cbind(ss,expb)
    colnames(ss)<-c("estimate","se","z","p","expb")
  }
  
  as.data.frame(ss)
}

mf.anova=function(model) {
  if (.which.class(model)=="glm") {
     ano<-car::Anova(model,test="LR",type=3)
     return(ano)
  }
     
  if (.which.class(model)=="lm")
    return(car::Anova(model,test="F",type=3))
}

mf.lmeranova=function(model) {

  if (.which.class(model)=="lmer") {   
    
  ano<-lmerTest::anova(model)
  if (dim(ano)[1]==0)
    return(ano)
  if (dim(ano)[2]==4) {
    ano<-car::Anova(model,type=3,test="F")
    attr(ano,"method")<-"Kenward-Roger"
    return(ano)
  }
  ano<-ano[,c(5,3,4,6)]
  names(ano)<-c("F","df1","df2","p")
  attr(ano,"method")<-"Satterthwaite"
  
  if (!all(is.na(ano$df2)==F)) {
    isone<-rownames(ano[is.na(ano$df2),])
    ss<-summary(model)[['coefficients']]
    rows<-matrix(ss[rownames(ss)==isone,],nrow = length(isone))
    if (dim(rows)[1]>0) {
      who<-is.na(ano$df2)
      rows[,4]<-rows[,4]^2
      ano[who,"F"]<-rows[,4]
      ano[who,"p"]<-rows[,5]
      ano[who,"df2"]<-rows[,3]
    } else {
      ano<-car::Anova(model,type=3,test="F")
      attr(ano,"method")<-"Kenward-Roger"
      
    }
  }
  return(ano)
  }
}


mf.getModelFactors<-function(model) {
  names(model$contrasts)
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

mf.confint<-function(model,level) {
  if (.which.class(model)=="lm") {
    return(confint(model,level = level))
  }
  if (.which.class(model)=="glm") {
    return(confint(model,level = level))
  }
  
  if (.which.class(model)=="lmer") {
    ci<-confint(model,method="Wald")
    ci<-ci[!is.na(ci[,1]),]
    if (is.null(dim(ci)))
      ci<-matrix(ci,ncol=2)
    return(ci)
  }
  
}

mf.give_family<-function(modelSelection) {
  
  if (modelSelection=="linear")
       return(gaussian())
  if (modelSelection=="logistic")
       return(binomial())
  if (modelSelection=="poisson")
    return(poisson())
  
  
  NULL  
}

mf.checkData<-function(dep,data,modelType) {
  
     if (modelType=="linear")
       ### here I need the dv to be really numeric
       data[[dep]] <- as.numeric(as.character(data[[dep]]))  
     
     if  (modelType=="poisson") {
         ### here I need the dv to be really numeric
       data[[dep]] <- as.numeric(as.character(data[[dep]]))
       if (any(data[[dep]]<0))
             return("Poisson model requires all positive numbers in the dependent variable")
     }
      if (modelType=="logistic") {
          data[[dep]] <- factor(data[[dep]])
          if (length(levels(data[[dep]]))!=2)
               return("Logistic model requires two levels in the dependent variable")
      } 
     if (modelType=="multinomial")
            data[[dep]] <- factor(data[[dep]])
     data
}