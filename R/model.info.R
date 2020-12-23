
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
  llfull<-stats::logLik(model)  ### model loglikelihood
  nobject <- stats::update(model, ~ 1,  evaluate = FALSE)
  nobject <- eval.parent(nobject)
  llnull<-stats::logLik(nobject)
  as.numeric(1-(llfull/llnull))
}


####### confergence ############################

mi.converged<- function(x,...) UseMethod(".converged")

.converged.default<-function(model) {

     if ("converged" %in% names(model))
        conv<-model$converged
     else
        conv<-TRUE
     conv
}
.converged.glmerMod<-function(model) 
  .converged.glmerMerMod(model)
    
.converged.glmerMerMod<-function(model) {
  
  if (!is.null(model@optinfo$conv$lme4$code))
    conv<-FALSE
  else
    conv<-TRUE
  conv
}

.converged.lmerMerMod<-function(model) {
            
              if (!is.null(model@optinfo$conv$lme4$code))
                  conv<-FALSE
              else
                  conv<-TRUE
              conv
}

.converged.multinom<-function(model) {

      model$convergence==0
}



########### aliazed coefficients #########

mi.aliased<- function(x,...) UseMethod(".aliased")

.aliased.default<-function(model) {
    aliased<-stats::alias(model)
    (!is.null(aliased$Complete))
}

.aliased.lm<-function(model) {
  if (model$rank==0)
    return(FALSE)
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
  return(round(stats::extractAIC(model)[2],digits = 3))


mi.getValueDf<- function(x,...) UseMethod(".getValueDf")

.getValueDf.default<-function(model) {
  value <- sum(stats::residuals(model, type = "pearson")^2)
  result <- value/df.residual(model)
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

.getResDf.glmerMod<-function(model) {
  ss<-summary(model)
  return(ss$AICtab["df.resid"])
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
    codes<-round(t(stats::contrasts(data[[jmvcore::toB64(fac)]])),digits=3)
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

mi.initInterceptInfo<-function(options, results) {
  mark(options$interceptInfo)
  if (!options$interceptInfo || !options$fixedIntercept) 
      return()
  aTable<-results$main$interceptTable
  aTable$addRow(rowKey=1, values=list(name="(Intercept)"))
}

mi.initEffectSizeInfo<-function(options, results, terms, ciWidth) {
  if (!options$effectSizeInfo) 
    return()
  aTable<-results$main$effectSizeTable
  aTable$getColumn('cilow')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
  aTable$getColumn('cihig')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
  i<-1
  j<-1

  for (term in terms) {
    aTable$addRow(rowKey=j,list(effect=lf.nicifyTerms(terms[[i]]),name=letter_eta2))
    aTable$addRow(rowKey=j+1,list(effect=lf.nicifyTerms(terms[[i]]),name=letter_peta2))
    aTable$addRow(rowKey=j+2,list(effect=lf.nicifyTerms(terms[[i]]),name=letter_pomega2))
    aTable$addRow(rowKey=j+3,list(effect=lf.nicifyTerms(terms[[i]]),name=letter_pepsilon2))

    aTable$addFormat(col=1, rowNo=j, format=jmvcore::Cell.BEGIN_GROUP)
    aTable$addFormat(col=1, rowNo=j, format=jmvcore::Cell.BEGIN_END_GROUP)
    i<-i+1
    j<-j+4
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
    modelterms<-stats::terms(model)
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
  ff<-colnames(attr(stats::terms(model),"factors"))
  ff<-jmvcore::decomposeTerms(ff)
  for(f in ff)
    if(all(f %in% aList) & all(aList %in% f) )
      return(paste(f,collapse = ":"))
}





mi.dependencies<-function(model,term,what) {
  if (mi.is.scaleDependent(model,term))
    return(paste(what,"interactions",sep="."))
  else {
    modelterms<-stats::terms(model)
    modelterms<-attr(modelterms,"term.labels")
    if (mi.term.develop(term)<length(modelterms))
      return(paste(what,"covariates",sep="."))
  }
  FALSE
}


mi.check_estimation<-function(model,n64) {
  if (jmvcore::isError(model)) {
      msg<-jmvcore::extractErrorMessage(model)
      if (msg=="Unknown error")
          msg="The estimation of the model could not be completed. Please refine the model"
      ### this should be made programatic #####
      msg<-gsub("nAGQ","Precision/speed parameter",msg,fixed=T)
      ###
      msg<-n64$translate(msg)
      jmvcore::reject(msg, code='error')
  }
}

mi.warn_estimation<-function(model,n64) {
  if (jmvcore::isError(model)) {
    msg<-jmvcore::extractErrorMessage(model)
    n64$translate(msg)
  }
}

###### check model characteristics ############

mi.model_check<- function(model) {
  att<-list(conv=mi.converged(model),
                 aliased=mi.aliased(model),
                 singular=mi.isSingular(model))
  attr(model,"infoTable")<-att  
  attr(model,"warning")<-mi.warnings(model)
  model
}
######## check for singularity of fit  ##########

mi.isSingular<- function(x,...) UseMethod(".mi.isSingular")

.mi.isSingular.default<-function(model) {
  return(FALSE)
}

.mi.isSingular.lmerMod<-function(model) {
 lme4::isSingular(model)
}

######## check model warnings ##########

mi.warnings<- function(x,...) UseMethod(".mi.warnings")

.mi.warnings.default<-function(model) {
  return(NULL)
}

.mi.warnings.glm<-function(model) {

   family<-family(model)

   if (family$family=="poisson" | family$family=="quasipoisson") 
     if (all.equal(as.numeric(model$y), as.integer(model$y))!=TRUE)
       return("Warnings: Poisson model requires the values of dependent variable to be integers. Results may be misleading ")

     if (length(grep("Negative Binomial",family$family))>0) 
       if (all.equal(as.numeric(model$y), as.integer(model$y))!=TRUE)
          return("Warnings: Negative Binomial model requires the values of dependent variable to be integers. Results may be misleading ")
}

.mi.warnings.glmerMod<-function(model) {
         .warnings<-.mi.warnings.lmerMod(model)
         family<-family(model)
         
         if (family$family=="poisson" | family$family=="quasipoisson")  {
           y<-model@frame[,1]
           if (all.equal(as.numeric(y), as.integer(y))!=TRUE)
             .warnings<-c(.warnings,"Warnings: Poisson model requires the values of dependent variable to be integers. Results may be misleading ")
         }
         if (length(grep("Negative Binomial",family$family))>0) {
           y<-model@frame[,1]
           if (all.equal(as.numeric(y), as.integer(y))!=TRUE)
             .warnings<-c(.warnings,"Warnings: Negative Binomial model requires the values of dependent variable to be integers. Results may be misleading ")
         }
         .warnings
}
.mi.warnings.lmerMod<-function(model) {
  msg<-model@optinfo$conv$lme4$messages
  return(msg)
  
}



