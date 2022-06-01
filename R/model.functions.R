



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
  ## In unbalanced designs, the sum does not necessarily correspond to the model SS (plus residuals)
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

.anova.glmerMod<-function(model,obj) {
  ginfo("using .anova.glmerMod")
  ano<-.car.anova(model)
  names(ano)<-c("test","df","p")
  if (nrow(ano)==0) ano<-NULL
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
      warning("No random coefficients specified. A generalized linear model is used instead.")
      return(stats::glm(formula = .formula,data=data,family=family(model)))
    }
  }
  
  stats::update(model,data=data,...)
}
