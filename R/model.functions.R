




mf.getModelFactors<-function(model) {
  names(attr(stats::model.matrix(model),"contrasts"))
} 




############# some models are not built in standard way, here we fix them ##########
mf.fixModel<- function(x,...) UseMethod(".fixModel")

.fixModel.default<-function(model,obj=NULL) {
  return(model)
}



.fixModel.lmerModLmerTest<-function(model,obj=NULL) {
  
  if (lme4::isSingular(model))
      obj$dispatcher$warnings<-list(topic="info",message=WARNS[["lmer.singular"]])

  return(model)
}

.fixModel.multinom<-function(model,obj=NULL) {
  
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
.aliased.multinom<-function(model) {
  ### to do 
  FALSE
  
}

.aliased.polr<-function(model) {
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

  
####### model update ##########


mf.update<- function(x,...) UseMethod(".update")

.update.default<-function(model,...) {

   data<-insight::get_data(model)
   stats::update(model,data=data,...)
}

.update.lmerModLmerTest<-function(model,...) {

  .args<-list(...)
  data<-insight::get_data(model)
  
  if (utils::hasName(.args,"formula")) {
    .formula<-stats::as.formula(.args$formula)
     test<-lme4::findbars(.formula)
    
     if (!is.something(test)) {
       warning("No random coefficients specified. A linear model is used instead.")
       .formula<-lme4::nobars(.formula)
       return(stats::lm(formula = .formula,data=data))
     }
  }
  
  stats::update(model,data=data,...)
}


.update.glmerMod<-function(model,...) {
  
  .args<-list(...)
  data<-insight::get_data(model)

  if (utils::hasName(.args,"formula")) {
    .formula<-stats::as.formula(.args$formula)
    test<-lme4::findbars(.formula)
    
    if (!is.something(test)) {
      .formula<-lme4::nobars(.formula)
      warning("No random coefficients specified. A generalized linear model is used instead.")
      mod<-stats::glm(formula = .formula,data=data,family=family(model))
      return(mod)
    }
  }
  
  stats::update(model,data=data,...)
}
