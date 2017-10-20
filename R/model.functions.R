##### function to check results ####

.which.class<-function(model) {
  if ("lm" %in% class(model))
      return("lm")
  if (c("merModLmerTest","lmerMod") %in% class(model))
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
}

mf.predict<-function(model,data=NULL) {
  if (.which.class(model)=="lm"){
    return(predict(model,data))
  }
  if (.which.class(model)=="lmer"){
    return(predict(model,data,re.form=~0))
  }

}

  

mf.getModelData<-function(model) {
  if (.which.class(model)=="lm")
           return(model$model)
  
  if (.which.class(model)=="lmer")
     return(model@frame)
  
  
}

mf.estimate<-function(model) {
  if (.which.class(model)=="lm") {  
     function(form,data)
       stats::lm(form,data)
  }  

  if (.which.class(model)=="lmer") {  
    function(form,data)
      do.call(lmerTest::lmer, list(formula=form, data=data,REML=!is.na(model@devcomp$cmp["REML"])))
  }
}

mf.summary=function(model) {
  if (.which.class(model)=="lm")   
     ss<-summary(model)
  if (.which.class(model)=="lmer")   
    ss<-lmerTest::summary(model)
  
  as.data.frame(ss$coefficients)
}

mf.anova=function(model) {
  car::Anova(model,test="F",type=3)
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

