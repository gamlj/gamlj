
####### model update ##########
### we need some custom update to be sure that the returned model can be used within gamlj

mf.update<- function(x,...) UseMethod(".update")

.update.default<-function(model,...) {
  
  jinfo("default update is used")
  .args<-list(...)
   if (!utils::hasName(.args,"data")) 
       .args$data<-insight::get_data(model,source="frame")

  .args$object<-model
  do.call(stats::update,.args)
  
}

.update.lmerModLmerTest<-function(model,...) {
  
  .args<-list(...)
   if (!utils::hasName(.args,"data")) 
       .args$data<-insight::get_data(model,source="frame")
  
  if (utils::hasName(.args,"formula")) {
    .formula<-stats::as.formula(.args$formula)
    test<-lme4::findbars(.formula)
    
    if (!is.something(test)) {
      warning("No random coefficients specified in the reduced model. A linear model is used for comparison.")
      .formula<-lme4::nobars(.formula)
      return(stats::lm(formula = .formula,data=.args$data))
    }
  }
  .args$object<-model 
  do.call(stats::update, .args)
  
}


.update.glmerMod<-function(model,...) {
  
  .args<-list(...)
#   if (!utils::hasName(.args,"data")) 
#       .args$data<-model@frame
  
  if (utils::hasName(.args,"formula")) {
    .formula<-stats::as.formula(.args$formula)
    test<-lme4::findbars(.formula)
    
    if (!is.something(test)) {
      .formula<-lme4::nobars(.formula)
      warning("No random coefficients in the reduced model. A generalized linear model is used for comparison.")
      data<-model@frame
      mod<-stats::glm(formula = .formula,data=data,family=stats::family(model))
      return(mod)
    }
  }
  .args$object<-model 
  do.call(stats::update, .args)
}

.update.clmm<-function(model,...) {
  
  
  .args<-list(...)
  
  if (utils::hasName(.args,"data")) 
       data <- .args$data
  else
       data<-insight::get_data(model,source="frame")
  
  if (!utils::hasName(.args,"formula")) stop("clmm model update requires a formula")
  
  #  .formula<-stats::as.formula(.args$formula)
  .formula<-stats::as.formula(.args$formula)
  
  test<-lme4::findbars(.formula)
  
  if (!is.something(test)) {
    warning("No random coefficients in the nested model. A fixed effects ordinal model is used for comparison")
    .formula<-lme4::nobars(.formula)
    return(ordinal::clm(formula = .formula,data=data))
  }
  ordinal::clmm(.formula,data=data)
}


.update.mmblogit<-function(model,formula,...) {
  
  jinfo("GAMLj: mmblogit update is used")
  .args<-list(...)
  ### mclogit is quite unflexible with the class of formulas. 
  ### we should deparse them and reset as formulas
  .fixed  <- lme4::nobars(stats::formula(formula))
  .random <- lme4::findbars(stats::formula(formula))
  
   if (utils::hasName(.args,"data")) 
       .data <- .args$data
   else
       .data   <- model$data

  .random<-(lapply(.random,function(x) formula(paste("~",deparse(x)))))

  if (!is.something(.random)) {
    .fixed<-formula(paste(deparse(.fixed)))
   
    ### mclogit requires the data to be matrix
     warning("No random coefficients in the nested model. A fixed effects multinomial model is used for comparison. The validity of the tests may be questionnable.")
    ##  we do not use  because it does not accept intercept only models
    return(nnet::multinom(.fixed,.data))
    
  }
  mclogit::mblogit(.fixed,random=.random,data=.data, trace=FALSE,trace.inner=FALSE)
  
}


.update.lme<-function(model,formula, ...) {

  jinfo("lme update is used")
  form<-formula(formula)
  fixed<-lme4::nobars(form)
  .random<-lme4::findbars(form)
  .args <- list(...)
  if (utils::hasName(.args,"data")) 
       data <- .args$data
  else
       data<-model$data

  random<-lapply(.random, function(x) formula(paste("~",as.character(x)[2],"|",as.character(x)[3])))
  if (!is.something(random)) {
    warning("No random coefficients in the nested model. A fixed effects linear model is used for comparison. The validity of the tests may be questionnable.")
    return(stats::lm(fixed,data=data))
  }
  umodel<-stats::update(model,fixed = fixed,random=random,data=data)
  umodel$call$fixed<-fixed
  umodel$call$random<-random
  umodel$call[[1]]<-quote(nlme::lme.formula)
  umodel
}

