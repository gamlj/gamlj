

mf.getModelFactors<-function(model) {
  names(attr(stats::model.matrix(model),"contrasts"))
} 




############# some models are not built in standard way, here we fix them ##########
mf.fixModel<- function(x,...) UseMethod(".fixModel")

.fixModel.default<-function(model,obj=NULL,data=NULL) {
  return(model)
}



.fixModel.lmerModLmerTest<-function(model,obj=NULL,data=NULL) {
  
  if (lme4::isSingular(model))
      obj$warning<-list(topic="info",message=WARNS[["lmer.singular"]])

  return(model)
}

.fixModel.multinom<-function(model,obj=NULL,data=NULL) {
  
  model$call$formula <- stats::as.formula(model)
  return(model)
}





####### check if a model converged ###### 

mf.converged<- function(x,...) UseMethod(".converged")

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

.converged.lmerModLmerTest<-function(model) {

  
  if (!is.null(model@optinfo$conv$lme4$code))
    conv<-FALSE
  else
    conv<-TRUE
  conv
}

.converged.multinom<-function(model) {
  
  model$convergence==0
}







########### check if aliased #########

mf.aliased<- function(x,...) UseMethod(".aliased")

.aliased.default<-function(model) {
  aliased<-try_hard(stats::alias(model))
  if (is.something(aliased$error))
      return(FALSE)
  (!is.null(aliased$obj$Complete))
}

.aliased.glmerMod<-function(model) {
  rank<-attr(model@pp$X,"msgRankdrop")
  return((!is.null(rank)))
}

.aliased.lmerMod<-function(model) {
  rank<-attr(model@pp$X,"msgRankdrop")
  return((!is.null(rank)))
}
.aliased.lme<-function(model) {
  ### to do 
  FALSE
  
}

.aliased.multinom<-function(model) {
  ### to do 
  FALSE
  
}
.aliased.mmblogit<-function(model) {
  ### to do 
  FALSE
  
}

.aliased.polr<-function(model) {
  ### to do 
  FALSE
}

.aliased.clm<-function(model) {
  ### to do 
  FALSE
  
}

.aliased.clmm<-function(model) {
  ### to do 
  FALSE
  
}

##### standardize a model #########

mf.standardize<- function(x,...) UseMethod(".standardize")

.standardize.default<-function(model) {
  stop("I do not know how to standardize model of class",class(model))
}  
  
.standardize.lm<-function(model) {
  
  newdata<-model$model
  types<-attr(stats::terms(model),"dataClasses")
  for (name in names(types)) {
    if (types[[name]]=="numeric") 
      newdata[[name]] <- as.numeric(scale(newdata[[name]]))
  }
  stats::update(model,data=newdata)
  
}  


### model data extraction 

mf.data<- function(x,...) UseMethod(".data")

.data.default<-function(model) model$model

.data.glmerMod<-function(model) model@frame

.data.lmerModLmerTest<-function(model) model@frame

.data.lme<-function(model) model$data
