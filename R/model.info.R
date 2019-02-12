
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
    ### McFadden’s model #####
#    llmodel<-logLik(model)  ### model loglikelihood
#    data<-model$model
#    nobject <- update(model, ~ 1,family= model$family$family,data=data)
#    llnull<-logLik(nobject) ### null model loglikelihood
#    as.numeric(1-llmodel/llnull)
    ### D-squared. For logistic is equivalente to McFadden. For gaussian yields the standard R-square
    ### which is not McFadden
    1- (model$deviance/model$null.deviance)
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
    aliased<-stats::alias(model)
    (!is.null(aliased$Complete))
}

.aliased.lmerMerMod<-function(model) {
    rank<-attr(model@pp$X,"msgRankdrop")
    return((!is.null(rank)))
}
.aliased.multinom<-function(model) {
   ### to do 
   FALSE
  
}



mi.getDummiesNames<-function(varname,data) {
  bname<-jmvcore::toB64(varname)
  if (!(bname %in% names(data)))
      return(varname)
  if (is.factor(data[[bname]]))
  {
    paste(varname,1:length(levels(data[[bname]])[-1]),sep="")
  } else 
    varname
}


mi.getAIC<- function(x,...) UseMethod(".getAIC")

.getAIC.default<-function(model)
  return(model$aic)

.getAIC.multinom<-function(model)
  return(model$AIC)

mi.getValueDf<- function(x,...) UseMethod(".getValueDf")

.getValueDf.default<-function(model) {
  value <- sum(residuals(model, type = "pearson")^2)
  result <- value/model$df.residual
  return(result)
}
.getValueDf.multinom<-function(model) {
  return(NULL)
}

mi.getResDf<- function(x,...) UseMethod(".getResDf")

.getResDf.default<-function(model) {
  return(model$df.residual)
}
.getResDf.multinom<-function(model) {
  return(model$edf)
}

mi.initContrastCode<-function(data,options,results,n64) {
  
  if (!options$showContrastCode) 
    return()
  
  factorsAvailable <- options$factors
  if (length(factorsAvailable)==0)
    return()
  tables<-results$main$contrastCodeTables
  for (fac in factorsAvailable) {
    rnames<-n64$nicenames(n64$contrasts(fac))
    clabs<-n64$contrastsLabels(fac)
    aTable<-tables$addItem(key=fac)
    codes<-round(t(contrasts(data[[jmvcore::toB64(fac)]])),digits=3)
    cnames<-colnames(codes)
    colnames(codes)<-paste0("c",1:length(cnames))
    codes<-cbind(rnames,clabs,codes)
    for (i in seq_along(cnames)) {
      aTable$addColumn(name=paste0("c",i), title=paste0("level=",cnames[i]), type='text')
    }
    for (i in 1:nrow(codes)) {
      aTable$addRow(rowKey=i, values=codes[i,])
    }
    int<-"sample mean"
    contr<-attr(data[[jmvcore::toB64(fac)]],"jcontrast")
    if (contr=="dummy")
      int=paste0(fac,"=",levels(data[[jmvcore::toB64(fac)]])[1])
    aTable$setNote("int", paste("Intercept computed for",int))
    
  }  
  
}




mi.explainPrediction<-function(modelType,data,dep){
  
  if (modelType %in% c("logistic")) {
    dlevs<-levels(data[[jmvcore::toB64(dep)]])
    dirvalue<-"P(y=1)/P(y=0)"
    dircomm<-paste("P(",dep,"=",dlevs[2],") / P(",dep,"=",dlevs[1],")")
    return(c(dirvalue,dircomm))
  }
  if (modelType %in% c("probit")) {
    dlevs<-levels(data[[jmvcore::toB64(dep)]])
    dirvalue<-"P(y=1)"
    dircomm<-paste("P(",dep,"=",dlevs[2],")")
    return(c(dirvalue,dircomm))
  }
  if (modelType %in% c("multinomial")) {
    dlevs<-levels(data[[jmvcore::toB64(dep)]])
    dirvalue<-"P(y=x)/P(x=0)"
    dircomm<-paste(paste0("P(",dep,"=",dlevs[-1],")"),paste0("P(",dep,"=",dlevs[1],")"),sep="/",collapse = " , ")
    return(c(dirvalue,dircomm))
  }
  
  return()
}



### this tells if a model term is dependent on the interaction
mi.is.scaleDependent<-function(model,term) {
  if (is.null(term))
    return(FALSE)
  try({
    modelterms<-terms(model)
    modelterms<-attr(modelterms,"term.labels")
    nterm<-paste(term,collapse = ":")
    count<-length(grep(nterm,modelterms,fixed=T))
    if (count>1)
      return(TRUE)
  })
  FALSE
}


mi.term.develop<-function(term){
  n<-.term.order(term)
  (2^n)-1
}

.term.order<-function(term) {
  
  length(unlist(strsplit(term,":",fixed=T)))
  
}

mi.interaction.term<-function(model,aList) {
  
  aList<-jmvcore::toB64(aList)
  ff<-colnames(attr(terms(model),"factors"))
  ff<-jmvcore::decomposeTerms(ff)
  for(f in ff)
    if(all(f %in% aList) & all(aList %in% f) )
      return(paste(f,collapse = ":"))
}





mi.dependencies<-function(model,term,what) {
  if (mi.is.scaleDependent(model,term))
    return(paste(what,"interactions",sep="."))
  else {
    modelterms<-terms(model)
    modelterms<-attr(modelterms,"term.labels")
    if (mi.term.develop(term)<length(modelterms))
      return(paste(what,"covariates",sep="."))
  }
  FALSE
}




