############# produces anova/deviance table in a somehow standard format ##########
ganova<- function(x,...) UseMethod(".anova")

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

.anova.clmm<-function(model,obj) {

  ginfo("anova for clmm")
  ## at the moment ordinal::anova.clmm does not work and drop1 tests
  ## only the higher order term. So we go all the way with a custom
  ## drop. We also have to be careful when there is only one predictors,
  ## because drop1 will not work . This results in Type II testing

  if (!obj$hasTerms)
    return()

  results<-emmeans::joint_tests(model)
  names(results)<-c("source","df","df2","test","p")
  results$source<-fromb64(results$source)
  results
    

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


