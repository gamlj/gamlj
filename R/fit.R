############# produces R2  ##########

fit.R2<- function(model,...) UseMethod(".R2") 

.R2.default<-function(model,obj) {
  mark(class(model))
  obj$errors<-list(topic="tab_r2",message=paste("R2 cannot be computed for model",class(model)))
  return(NULL)
}

.R2.glm<-function(model,obj) {
  
  alist       <-  list()
  # mcFadden and adjusted
  alist$r2    <-  1-(model$deviance/model$null.deviance)
  alist$ar2   <-  1-((model$deviance+2*length(model$coefficients))/model$null.deviance)
  
  if (alist$ar2<0)
    alist$ar2 <- 0
  
  if (any(sapply(alist,is.null)))
    obj$warnings  <-  list(topic="tab_r2",message="R-squared cannot be computed")
  
  results     <-  fit.compare_null_model(model)
  alist$test  <-  results$test
  alist$df    <-  results$df
  alist$p     <-  results$p
  
  return(list(alist))
}

.R2.multinom<-function(model,obj) {

  
  llfull<-stats::logLik(model)  ### model loglikelihood
  nullmodel <- stats::update(model, ~ 1,  evaluate = FALSE)
  nullmodel <- eval.parent(nullmodel)
  llnull<-stats::logLik(nullmodel)
  r2<-as.numeric(1-(llfull/llnull))  
  compare<-anova(nullmodel,model)
  

  alist       <-  list()
  # mcFadden 
  alist$r2    <-  as.numeric(1-(llfull/llnull))
  alist$test  <-  compare$`LR stat.`[2]
  alist$df    <-  compare$`   Df`[2]
  alist$p     <-  compare$`Pr(Chi)`[2]
  return(list(alist))
}

.R2.lm<-function(model,obj) {
  
  ss<-summary(model)
  results<-list()
  results$df1<-ss$fstatistic[["numdf"]]
  results$df2<-ss$fstatistic[["dendf"]]
  results$r1<-ss$r.squared
  results$r2<-ss$adj.r.squared
  if (hasName(ss,"fstatistic")) {
    results$f<-ss$fstatistic[["value"]]
    results$p<-stats::pf(results$f,results$df1,results$df1, lower.tail = FALSE)
  } else {
    obj$warnings<-list(topic="tab_r2",message="R-squared tests cannot be computed")
    
  }
  list(results)
  
}

######### model comparisons ########

fit.compare_null_model<- function(x,...) UseMethod(".compare_null_model")

.compare_null_model.default<-function(model) {
  
  data    <-  mf.getModelData(model)
  model0  <-  stats::update(model,~ 1 ,data=data,evaluate=T)
  results <-  stats::anova(model0,model,test = "LRT")
  results$test  <-  results$Deviance
  results$df    <-  results$Df
  results$p     <-  results$`Pr(>Chi)`
  results[2,]
}

.compare_null_model.negbin<-function(model) {
  
  data    <-  mf.getModelData(model)
  model0  <-  stats::update(model, ~1 ,data=data,evaluate=T)
  results <-  stats::anova(model0,model)
  results$test  <- results$`LR stat.`
  results$df    <- results$`   df`
  results$p    <- results$`Pr(Chi)`
  results[2,]
}


#### additional fit indices

fit.indices<- function(model,...) UseMethod(".fit") 

.fit.default<-function(model,obj) {
  return(NULL)
}

.fit.glm<-function(model,obj) {

  alist<-list()
  alist[[length(alist)+1]]<-list(value=as.numeric(stats::logLik(model)))
  alist[[length(alist)+1]]<-list(value=stats::extractAIC(model)[2])
  alist[[length(alist)+1]]<-list(value=stats::BIC(model))
  alist[[length(alist)+1]]<-list(value=model$deviance)
  alist[[length(alist)+1]]<-list(value=model$df.residual)
  value <- sum(stats::residuals(model, type = "pearson")^2)
  result <- value/stats::df.residual(model)
  alist[[length(alist)+1]]<-list(value=result)
  alist

}

.fit.multinom<-function(model,obj) {
  
  alist<-list()
  alist[[length(alist)+1]]<-list(value=as.numeric(stats::logLik(model)))
  alist[[length(alist)+1]]<-list(value=stats::extractAIC(model)[2])
  alist[[length(alist)+1]]<-list(value=stats::BIC(model))
  alist[[length(alist)+1]]<-list(value=model$deviance)
  alist
  
}
