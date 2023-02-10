############# produces parameters in a somehow standard format ##########

gparameters<- function(x,...) UseMethod(".parameters")

.parameters.default<-function(model,obj) {
  
  
  .bootstrap           <-  obj$options$ci_method %in% c("quantile","bcai")
  .iterations          <-  obj$options$boot_r
  .ci_method           <-  obj$options$ci_method
  .ci_width            <-  obj$ciwidth
  
  .se_method           <-  obj$options$se_method
  
  if (obj$option("se_method","standard")) 
    .se_method   <-  NULL
  
   if (obj$option("se_method","robust")) 
         .se_method   <-  obj$options$robust_method
  
  if (is.something(obj$boot_model)) .model<-obj$boot_model else .model<-model
  
  .coefficients        <-  as.data.frame(parameters::parameters(
    model,
    vcov=.se_method,
    ci=NULL,
  ),stringAsFactors=FALSE)


  names(.coefficients) <-  c("source","estimate","se","t","df","p")
  if (obj$option("estimates_ci")) {
    
    cidata            <-  as.data.frame(parameters::ci(.model,
                                                       ci=.ci_width,
                                                       ci_method=.ci_method))
    
    .coefficients$est.ci.lower<-cidata$CI_low
    .coefficients$est.ci.upper<-cidata$CI_high
    
  }
  
  if (obj$option("es","beta")) {
    
    if (obj$formulaobj$hasTerms) {
      ## if no CI are required, we do not bootstrap again 
      if (!obj$option("betas_ci")) { 
        ..bootstrap<-FALSE 
        .ci_method <-"wald"
      } else 
        ..bootstrap<-.bootstrap
      
      ### up to parameters 0.16.0, if bootstrap is required standardize does not work
      ### so we standardize before parameters() and feed the model to it
      
      opts_list<-list(model=mf.standardize(model),
                      bootstrap=..bootstrap,
                      ci_method=.ci_method,
                      ci=.ci_width,
                      iterations=.iterations                        
      )
      
      if (..bootstrap) {
        ginfo("ESTIMATE: we need to reboostrap for betas CI")
        
        ### check if we can go in paraller ###
        test<-try_hard(find.package("parallel"))
        if (isFALSE(test$error)) {
          opts_list[["n_cpus"]]<-parallel::detectCores()
          opts_list[["parallel"]]<-"multicore"
        }
        
      }
      estim<-do.call(parameters::parameters,opts_list)
      
      .coefficients$beta  <-  estim$Coefficient
      .coefficients$beta.ci.lower<-estim$CI_low
      .coefficients$beta.ci.upper<-estim$CI_high
      
      
    } else {
      .coefficients$beta  <-  0
      .coefficients$beta.ci.lower<-0
      .coefficients$beta.ci.upper<-0
      
    }
  }
  .coefficients
  
}


.parameters.glm<-function(model,obj) {
  
  .bootstrap           <-  obj$options$ci_method %in% c("quantile","bcai")
  .iterations          <-  obj$options$boot_r
  .ci_method           <-  obj$options$ci_method
  .ci_width            <-  obj$ciwidth

  if (is.something(obj$boot_model)) .model<-obj$boot_model else .model<-model
  
  .coefficients        <-  as.data.frame(parameters::parameters(
    model,
    ci=NULL,
  ),stringAsFactors=FALSE)
  

  .transnames<-list(source="Parameter",
    estimate="Coefficient",
    se="SE",
    test=c("z","t"),
    df="df_error",
    est.ci.lower="CI_low",est.ci.upper="CI_high")
  names(.coefficients)<-transnames(names(.coefficients),.transnames)
  
    
  if (obj$option("es","expb")) {
    estim            <-  as.data.frame(parameters::parameters(model,
                                                              exponentiate=TRUE,
                                                              bootstrap=.bootstrap,
                                                              iterations=.iterations,
                                                              ci=.ci_width,
                                                              ci_method=.ci_method))
    
    .coefficients$expb          <-  estim$Coefficient
    .coefficients$expb.ci.lower <-  estim$CI_low
    .coefficients$expb.ci.upper <-  estim$CI_high
  }

  if (obj$option("estimates_ci")) {
    
    cidata            <-  as.data.frame(parameters::ci(.model,
                                                       ci=.ci_width,
                                                       ci_method=.ci_method))
    
    .coefficients$est.ci.lower<-cidata$CI_low
    .coefficients$est.ci.upper<-cidata$CI_high
    
  }
  .coefficients
  
}

.parameters.multinom<-function(model,obj) {
  params<-.parameters.glm(model,obj)
  names(params)<-tolower(names(params))
  params
}

.parameters.mmblogit<-function(model,obj) {
  
      ss<-mclogit::getSummary.mblogit(model)
     .names<-dimnames(ss$coef)[[3]]
      alist<-list()
      for (i in seq_along(.names)) {
          one<-as.data.frame(ss$coef[,,i])
          one$response<-as.character(.names[i])
          ladd(alist)<-one
      }
      .coefficients<-as.data.frame(do.call("rbind",alist))
      .coefficients$source<-gsub("\\).",")",rownames(.coefficients))

      .transnames<-list(estimate="est",
                        test=c("stat"),
                        est.ci.lower="lwr",est.ci.upper="upr")
      names(.coefficients)<-transnames(names(.coefficients),.transnames)
    
      if (obj$option("es","expb")) {
         .coefficients$expb          <- exp(.coefficients$estimate)
         .coefficients$expb.ci.lower <- exp(.coefficients$est.ci.lower)
         .coefficients$expb.ci.upper <- exp(.coefficients$est.ci.upper)
      }
      ## clean names

      for (var in obj$datamatic$variables) {
       for (name in var$paramsnames64) {
            test<-grep(name,.coefficients$source)
            if (length(test)>0) .coefficients$source[test]<-name
       }
      }
      .coefficients
      
}

.parameters.polr<-function(model,obj) {
  params<-.parameters.glm(model,obj)
  params$label<-params$source
  check<-grep(LEVEL_SYMBOL,params$source,fixed=TRUE)
  params$source[check]<-"(Threshold)"
  params
}

.parameters.clmm<-function(model,obj) {
  
  params<-.parameters.polr(model,obj)
  params<-params[params$Effects=="fixed",]
  names(params)[4]<-"z"
  params
  
}


.parameters.lmerModLmerTest<-function(model,obj) {
  
  .bootstrap           <-  obj$options$ci_method %in% c("quantile","bcai")
  .iterations          <-  obj$options$boot_r
  .ci_method           <-  obj$options$ci_method
  .ci_width            <-  obj$ciwidth
  .df_method           <-  switch (obj$options$df_method,
                                   Satterthwaite = "satterthwaite",
                                   "Kenward-Roger" = "kenward"
  )
  
  .coefficients        <-  as.data.frame(parameters::parameters(
    model,
    ci=NULL,
    effects="fixed",
    ci_method=.df_method
  ),stringAsFactors=FALSE)
  
  names(.coefficients) <-  c("source","estimate","se","t","df","p")
  
  if (obj$option("estimates_ci")) {
    
    if (is.something(obj$boot_model)) .model<-obj$boot_model else .model<-model
    
    
    cidata            <-  as.data.frame(parameters::ci(.model,
                                                       ci=.ci_width,
                                                       ci_method=.ci_method))
    
    
    .coefficients$est.ci.lower<-cidata$CI_low
    .coefficients$est.ci.upper<-cidata$CI_high
    
  }
  return(.coefficients)
}

.parameters.glmerMod<-function(model,obj) {
  
  .bootstrap           <-  obj$options$ci_method %in% c("quantile","bcai")
  .iterations          <-  obj$options$boot_r
  .ci_method           <-  obj$options$ci_method
  .ci_width            <-  obj$ciwidth
  
  .coefficients        <-  as.data.frame(parameters::parameters(
    model,
    ci=NULL,
    effects="fixed",
  ),stringAsFactors=FALSE)
  
  names(.coefficients) <-  c("source","estimate","se","z","df","p")

  if (is.something(obj$boot_model)) .model<-obj$boot_model else .model<-model
  
  if (obj$option("es","expb")) {
    estim            <-  as.data.frame(parameters::parameters(model,
                                                              effects="fixed",
                                                              exponentiate=TRUE,
                                                              bootstrap=.bootstrap,
                                                              iterations=.iterations,
                                                              ci=.ci_width,
                                                              ci_method=.ci_method))
    
    .coefficients$expb          <-  estim$Coefficient
    .coefficients$expb.ci.lower <-  estim$CI_low
    .coefficients$expb.ci.upper <-  estim$CI_high
  }
  
  
  
  if (obj$option("estimates_ci")) {
    
    cidata            <-  as.data.frame(parameters::ci(.model,
                                                       effects="fixed",
                                                       ci=.ci_width,
                                                       ci_method=.ci_method))
    
    .coefficients$est.ci.lower<-cidata$CI_low
    .coefficients$est.ci.upper<-cidata$CI_high
    
  }
  
  return(.coefficients)
}


