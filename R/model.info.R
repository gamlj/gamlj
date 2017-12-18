####### info table #################

mi.update<-function(table,modelType,data=NULL,dep=NULL) {
  if (modelType=="logistic") {
    levels<-levels(data[[dep]])
    level<-levels[length(levels)]
    table$setRow(rowKey="link",list(comm=paste0("log odd of ",dep,"=",level)))
    return(table)
  }
  return(table)
}


######### rsquared ##########

mi.rsquared<- function(x,...) UseMethod(".rsquared")

.rsquared.default<-function(model) {
   sumr<-summary(model)
    ### McFadden’s model #####
    llfull<-logLik(model)  ### model loglikelihood
    print(llfull)
    llnull<<-llfull-((sumr$null.deviance-sumr$deviance)/2) ## intercept only loglikelihood
    as.numeric(1-(llfull/llnull))
}

.rsquared.multinom<-function(model) {
  llfull<-logLik(model)  ### model loglikelihood
  nobject <- update(model, ~ 1,  evaluate = FALSE)
  nobject <- eval.parent(nobject)
  llnull<-logLik(nobject)
  as.numeric(1-(llfull/llnull))
}


####### confergence ############################

mi.converged<- function(x,...) UseMethod(".converged")

.converged_test<-function(logicValue) {
  value<-ifelse(logicValue,"yes","no")
  comm<-ifelse(logicValue,"A solution was found","Results may be misleading")
  list(value=value,comm=comm)
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

