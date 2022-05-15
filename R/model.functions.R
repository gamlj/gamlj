
##### get model data #########

mf.getModelData<- function(x,...) UseMethod(".getModelData")

    .getModelData.default<-function(model) {
         return(model$model)
}

.getModelData.glmerMod<-function(model) 
      return(.getModelData.lmer(model))

.getModelData.glmer<-function(model) 
      return(.getModelData.lmer(model))

.getModelData.merModLmerTest<-function(model) 
      return(.getModelData.lmer(model))

.getModelData.lmerModLmerTest<-function(model) 
      return(.getModelData.lmer(model))

.getModelData.lmer<-function(model) 
      return(model@frame)



############# produces to get parameters in a somehow standard format ##########

mf.parameters<- function(x,...) UseMethod(".parameters")

.parameters.default<-function(model,obj) {
   

      .bootstrap           <-  obj$options$ci_method %in% c("quantile","bcai")
      .iterations          <-  obj$options$boot_r
      .ci_method           <-  obj$options$ci_method
      .ci_width            <-  obj$ciwidth
      .se_method           <-  obj$option("se_method","robust")
      
      if (is.something(obj$boot_model)) .model<-obj$boot_model else .model<-model
        
      .coefficients        <-  as.data.frame(parameters::parameters(
                                                                   model,
                                                                   robust=.se_method,
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
        
        if (obj$hasTerms) {
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
    
    if ("Response" %in% names(.coefficients)) {
        goodnames<-c("source","estimate","se","t","df","p","response")
    }
    else
        goodnames<-c("source","estimate","se","t","df","p")

    names(.coefficients)<-goodnames
    
    
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

.parameters.multinom<-function(model,obj) 
        .parameters.glm(model,obj)
    
.parameters.polr<-function(model,obj) 
  .parameters.glm(model,obj)

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




############# produces summary in a somehow standard format ##########

mf.summary<- function(x,...) UseMethod(".mf.summary")

    .mf.summary.default<-function(model) {
          return(FALSE)
}

.fix_coeffs<-function(parameters, coeffs) {

    if (length(coeffs)==dim(parameters)[1]) {
        return(as.data.frame(parameters))
  }
  
  ### fix missing coefficients ########
    all_coefs<-data.frame(all=coeffs)
    rownames(all_coefs)<-names(coeffs)
    all_coefs$order<-1:length(all_coefs$all)
    res<-merge(parameters,all_coefs,by="row.names",all=T)
    res<-res[order(res$order),]
    rownames(res)<-res$Row.names
    res[,c("Row.names", "order","all")]<-NULL
  ########################
  res
}
  
.mf.summary.lm<-function(model){
    smr<-summary(model)
    params<-smr$coefficients
    params<-.fix_coeffs(params,smr$aliased)
    if (nrow(params)[1]==0)
        return(as.data.frame(NULL))
    params$df<-smr$df[2]
    colnames(params)<-c("estimate","se","t","p","df")
    params
  ###########################à
  as.data.frame(params,stringsAsFactors = F)
}

.mf.summary.merModLmerTest<-function(model)
       .mf.summary.lmer(model)

.mf.summary.lmerModLmerTest<-function(model)
      .mf.summary.lmer(model)

.mf.summary.glmerMod<-function(model) {
    smr<-summary(model)
    params<-stats::coef(smr)
    all_coefs<-lme4::fixef(model,add.dropped=T)
    params<-.fix_coeffs(params,all_coefs)
    if (nrow(params)[1]==0)
        return(as.data.frame(NULL))
    expb<-exp(params[,"Estimate"])  
    params<-cbind(params,expb)
    colnames(params)<-c("estimate","se","z","p","expb")
  as.data.frame(params,stringsAsFactors = F)
}
.mf.summary.lmerMod<-function(model)
      .mf.summary.lmer(model)

.mf.summary.lmer<-function(model) {
    ss<-summary(model)$coefficients
    ss<-as.data.frame(ss,stringsAsFactors = F)
       
    if (dim(ss)[2]==3) {
          ano<-car::Anova(model,test="F",type=3)
          lnames<-rownames(ss)
          matching<-which(lnames %in% rownames(ss))
          ss$df<-NA
          ss$df[matching]<-ano$Df.res[matching]
          ss$p<-NA
          ss$p[matching]<-ano$`Pr(>F)`[matching]
          ss<-ss[,c(1,2,4,3,5)]       
          if (any(is.null(ss$df)))
               attr(ss,"warning")<-"lmer.df"
    }
    colnames(ss)<-c("estimate","se","df","t","p")
  ss
}

.mf.summary.glm<-function(model) {
     smr<-summary(model)
     params<-smr$coefficients
     params<-.fix_coeffs(params,smr$aliased)
     if (nrow(params)[1]==0)
        return(as.data.frame(NULL))
     expb<-exp(params[,"Estimate"])  
     params<-cbind(params,expb)
     colnames(params)<-c("estimate","se","z","p","expb")
     as.data.frame(params,stringsAsFactors = F)
}

.mf.summary.multinom<-function(model) {

     sumr<-summary(model)
     rcof<-sumr$coefficients
     cof<-as.data.frame(matrix(rcof,ncol=1))
     names(cof)<-"estimate"
     cof$dep<-rownames(rcof)
     cof$variable<-rep(colnames(rcof),each=nrow(rcof))
     se<-matrix(sumr$standard.errors,ncol=1)
     cof$se<-as.numeric(se)
     cof$expb<-exp(cof$estimate)  
     cof$z<-cof$estimate/cof$se
     cof$p<-(1 - stats::pnorm(abs(cof$z), 0, 1)) * 2
     ss<-as.data.frame(cof,stringsAsFactors = F)
     ss<-cof[order(ss$dep),]
     lab<-model$lab[1]
     ss$dep<-sapply(ss$dep, function(a) paste(a,"-",lab))
     ss
}
  
############# produces anova/deviance table in a somehow standard format ##########
mf.anova<- function(x,...) UseMethod(".anova")

.anova.default<-function(model,obj) {
       stop(".anova: no suitable model found") 
}
  
.anova.glm<-function(model,obj) {

        if (!obj$hasTerms) {
          obj$dispatcher$warnings  <-  list(topic="main_anova",message="Omnibus tests cannot be computed")
          return(NULL)
        }

        test<-switch (obj$options$omnibus,
                LRT = "LR",
                wald= "Wald")

        anoobj        <-  try_hard(car::Anova(model,test=test,type=3,singular.ok=T))

        ### LR is less lenient than Wald
        if (test=="LR" & !isFALSE(anoobj$error)) {
           anoobj        <-  try_hard(car::Anova(model,test="Wald",type=3,singular.ok=T))
           obj$dispatcher$warnings  <-  list(topic="main_anova",message="Wald test was used because LR failed")
        }
        obj$dispatcher$errors    <-  list(topic="main_anova",message=anoobj$error)
        obj$dispatcher$warnings  <-  list(topic="main_anova",message=anoobj$warning)
        

        if (!isFALSE(anoobj$error))
            return(NULL)
        
        .anova           <-  as.data.frame(anoobj$obj,stringsAsFactors = F)
        .transnames<-list("test"=c("Chisq","LR Chisq"),df=c("Df","df1"),p=c("Pr(>Chisq)"))
        names(.anova)<-transnames(names(.anova),.transnames)

        .anova<-.anova[rownames(.anova)!="(Intercept)",]   
        .anova
        
}



.anova.multinom<-function(model,obj)
                      .anova.glm(model,obj)

.anova.polr<-function(model,obj) 
                      .anova.glm(model,obj)


.anova.lm<-function(model,obj) {

      .anova        <-  car::Anova(model,test="F",type=3,singular.ok=T)
      .anova<-.anova[!(rownames(.anova) %in% c("(Intercept)")),]
      anovatab<-.anova
      colnames(anovatab)<-c("ss","df","f","p")
      effss<-anovatab[!(rownames(anovatab) %in% c("Residuals")),]
      reds<-list(ss=anovatab$ss[rownames(anovatab)=="Residuals"],df=anovatab$df[rownames(anovatab)=="Residuals"])
  ## returns if model has no terms
      if (!obj$hasTerms) {
        tots<-list(ss=reds$ss,df=reds$df)
      return(list(reds,tots))
      }
      sumr<-summary(model)

  ### whole model ###
      f<-sumr$fstatistic[[1]]
      edf<-sumr$fstatistic[[3]]
      mdf<-sumr$fstatistic[[2]]
      p<-stats::pf(f,mdf,edf,lower.tail = F)
      modeta<-effectsize::F_to_eta2(f,mdf,edf)
      modomega<-effectsize::F_to_omega2(f,mdf,edf)
      modepsilon<-effectsize::F_to_epsilon2(f,mdf,edf)
      modss<-f*reds$ss*mdf/edf
      mods<-list(ss=modss,
             df= mdf,
             f=f,
             p=p,
             etaSq=modeta[[1]],
             etaSqP=modeta[[1]],
             omegaSq=modomega[[1]],
             omegaSqP=modomega[[1]],
             epsilonSq=modepsilon[[1]],
             epsilonSqP=modepsilon[[1]])
  
      tots<-list(ss=mods$ss+reds$ss,df=mdf+edf)
  
  #####
  # Here we need a correct to the computation of the effect sizes. To compute the non-partial indeces
  ## effectsize:: package uses as total sum of squares the sum of the effects SS (plus residuals)
  ## In unbalanced designs, the sum does not necessarely correspond to the model SS (plus residuals)
  ## so the estimation is biased. Eta-squared does not correspond to semi-partial r^2 any more
  ## and many properties of the non-partial indices are broken. 
  ## Thus, we fixed it by adding a bogus effect whose SS is exactly the discrepancy betweem
  ## the table SS and the model+error SS. In this way, the estimation uses the correct total SS
  #####
      diff<-mods$ss-sum(effss$ss)
      add<-data.frame(diff,1,1,0)
      names(add)<-names(.anova)
      .canova<-rbind(.anova,add)
      last<-dim(effss)[1]+1
      etap<-effectsize::eta_squared(.anova,partial = T,verbose = F)
      eta<-effectsize::eta_squared(.canova,partial = F,verbose = F)
      omegap<-effectsize::omega_squared(.anova,partial = T,verbose = F)
      omega<-effectsize::omega_squared(.canova,partial = F,verbose = F)
      epsilonp<-effectsize::epsilon_squared(.anova,partial = T,verbose = F)
      epsilon<-effectsize::epsilon_squared(.canova,partial = F,verbose = F)
  
      effss$etaSq<-eta[-last,2]
      effss$etaSqP<-etap[,2]

      effss$omegaSq<-omega[-last,2]
      effss$omegaSqP<-omegap[,2]
      effss$epsilonSq<-epsilon[-last,2]
      effss$epsilonSqP<-epsilonp[,2]
      reslist<-listify(effss)
      reslist<-append_list(reslist,reds,"Residuals")
      reslist<-append_list(reslist,tots,"Total")
      reslist<-prepend_list(reslist,mods,"Model")
      reslist    
  }

.anova.glmerMod<-function(model,df="Satterthwaite") {
  ginfo("using .anova.glmerMod")
  ano<-.car.anova(model)
  names(ano)<-c("test","df1","p")
  ano  
}
  
.anova.lmerModLmerTest<-function(model,obj) {

  if (!obj$hasTerms)
     return()
  
  df<-obj$options$df_method
  results        <-  try_hard(stats::anova(model,type="3",ddf=df))
  obj$dispatcher$errors    <-  list(topic="main_anova",message=results$error)
  obj$dispatcher$warnings  <-  list(topic="main_anova",message=results$warning)

  if (!isFALSE(results$error))
      return()
  
  ano<-results$obj
  if (dim(ano)[1]==0) {
    obj$dispatcher$warnings<-list(topic="main_anova",message="F-Tests cannot be computed without fixed effects")
    return(ano)
  }
  if (dim(ano)[2]==4) {
    ano<-.car.anova(model,df)
    obj$dispatcher$warnings<-list(topic="main_anova",message="Degrees of freedom computed with method Kenward-Roger")
    
  } 
    # lmerTest 2.0-33 does not produce the F-test for continuous IV if they appear in the model before
    # the factors. Here we try to fix it.
    # now we use lmerTest>3.1 that does not seem to have this problem. Check anyway in testing
  if (is.there("F value",names(ano)))
      names(ano)<-c("ss","ms","df1","df2","f","p")
  else
      stop("fix anova with chisq")

  return(ano)

}

.car.anova<-function(model,df) {
  ginfo("mf.anova uses car::Anova")
  if (model@devcomp$dims["REML"]==0) 
    test<-"Chisq"
  else test<-"F"
  ano<-car::Anova(model,type=3,test=test)
  if (attr(stats::terms(model),"intercept")==1)
    ano<-ano[-1,]
  attr(ano,"method")<-"Kenward-Roger"
  attr(ano,"statistic")<-test
  ano
}


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
  
  model$call$formula <- as.formula(model)
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

### sourcify syntax for all models ####

mf.sourcify<-function(option) {

  results<-list(type="transformed",option=option)
  .ignore<-c('factors', 'dep', 'covs', 'model_terms','.interface','.caller')
  .transform<-c("simple_effects","simple_moderators","plotHAxis","plotSepLines","plotSepPlots")

  if (!is.something(option$value))
      results$type="empty"
  
  if (option$name %in% .ignore)
    results$type="empty"
  
  if (option$name =='covs_scale') {
    results$option$value<-sourcifyList(option,"centered")
    results$type="complete"
  }
  if (option$name =='contrasts') {
    results$option$value<-sourcifyList(option,"simple")
    results$type="complete"
  }
  
  if (option$name %in% .transform) {
    results$option$value<-sourcifyVars(option$value)
  }
  results
}

##### standardize a model #########

mf.standardize<- function(x,...) UseMethod(".standardize")

.standardize.default<-function(model) {
  stop("I do not know how to standardize model of class",class(model))
}  
  
.standardize.lm<-function(model) {
  
  newdata<-model$model
  types<-attr(terms(model),"dataClasses")
  for (name in names(types)) {
    if (types[[name]]=="numeric") 
      newdata[[name]] <- as.numeric(scale(newdata[[name]]))
  }
  update(model,data=newdata)
  
}  

  
####### model update ##########


mf.update<- function(x,...) UseMethod(".update")

.update.default<-function(model,...) {

   data<-mf.getModelData(model)
   stats::update(model,data=data,...)
}

.update.lmerModLmerTest<-function(model,...) {

  .args<-list(...)
  data<-mf.getModelData(model)
  
  if (hasName(.args,"formula")) {
    .formula<-as.formula(.args$formula)
     test<-lme4::findbars(.formula)
    
     if (!is.something(test)) {
       warning("No random coefficients specified. A linear model is used instead.")
       return(stats::lm(formula = .formula,data=data))
     }
  }
  
  stats::update(model,data=data,...)
}


