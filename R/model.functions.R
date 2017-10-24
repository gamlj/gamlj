##### function to check results ####

.which.class<-function(model) {
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
     return(function(form,data)
       stats::lm(form,data))
  }  

  if (.which.class(model)=="lmer") {  
    return(function(form,data)
      do.call(lmerTest::lmer, list(formula=form, data=data,REML=!is.na(model@devcomp$cmp["REML"]))))
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
  if (.which.class(model)=="lmer") {
    ci<-confint(model,method="Wald")
    ci<-ci[!is.na(ci[,1]),]
    if (is.null(dim(ci)))
      ci<-matrix(ci,ncol=2)
    return(ci)
  }
  
}