######### rsquared ##########

mi.rsquared<- function(x,...) UseMethod(".rsquared")

.rsquared.default<-function(model) {
    ss<-summary(model)
    1-(model$deviance/ss$null.deviance)
}

.rsquared.multinom<-function(model) {
  nobject <- update(model, ~ 1,  evaluate = FALSE)
  nobject <- eval.parent(nobject)
  1-(model$deviance/nobject$deviance)
}


####### confergence ############################

mi.converged<- function(x,...) UseMethod(".converged")

.converged_test<-function(logicValue) {
  value<-ifelse(logicValue,"yes","no")
  comm<-ifelse(logicValue,"A solution was found","Results may be misleading")
  as.list(value,comm)
}

.converged.default<-function(model) {
  
    .converged_test(model$converged)
  
}
.converged.multinom<-function(model) {
  
    .converged_test(model$convergence==0)
  
}

########### aliazed coefficients #########

mi.aliased<- function(x,...) UseMethod(".aliased")

.aliased.default<-function(model) {
    aliased<-alias(model)
    (!is.null(aliased$Complete))
}

.aliased.lmer<-function(model) {
    rank<-attr(model@pp$X,"msgRankdrop")
    return((!is.null(rank)))
}
.aliased.multinom<-function(model) {
   ### to do 
   FALSE
  
}
