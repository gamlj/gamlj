
####### model update ##########


mf.update<- function(x,...) UseMethod(".update")

.update.default<-function(model,...) {
  
  jinfo("default update is used")
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
      warning("No random coefficients specified. A linear model is used for comparison.")
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
      warning("No random coefficients specified. A generalized linear model is used for comparison.")
      mod<-stats::glm(formula = .formula,data=data,family=stats::family(model))
      return(mod)
    }
  }
  
  stats::update(model,data=data,...)
}

.update.clmm<-function(model,...) {
  
  
  .args<-list(...)
  data<-insight::get_data(model)
  
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


.update.mmblogit<-function(model,formula) {
  
  jinfo("mmblogit update is used")
  ### mclogit is quite unflexible with the class of formulas. 
  ### we should deparse them and reset as formulas
  .fixed  <- lme4::nobars(stats::formula(formula))
  .random <- lme4::findbars(stats::formula(formula))
  .data   <- model$data
  .random<-(lapply(.random,function(x) formula(paste("~",deparse(x)))))

   
  
  if (!is.something(.random)) {
    .fixed<-formula(paste(deparse(.fixed)))
   
    ### mclogit requires the data to be matrix
     warning("No random coefficients in the nested model. A fixed effects multinomial model is used for comparison")
    ##  we do not use  because it does not accept intercept only models
    return(nnet::multinom(.fixed,.data))
    
  }

  mclogit::mblogit(.fixed,random=.random,data=.data)
  
}


.update.lme<-function(model,formula) {

  jinfo("lme update is used")
  form<-formula(formula)
  fixed<-lme4::nobars(form)
  .random<-lme4::findbars(form)
  random<-lapply(.random, function(x) formula(paste("~",as.character(x)[2],"|",as.character(x)[3])))
  data<-model$data
  umodel<-stats::update(model,fixed = fixed,random=random,data=data)
  umodel$call$fixed<-fixed
  umodel$call$random<-random
  umodel$call[[1]]<-quote(nlme::lme.formula)
  umodel
}

