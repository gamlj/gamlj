mf.addEffectSize<- function(x,...) UseMethod(".addEffectSize")


.addEffectSize.default<-function(atable) {
  return(atable)
}


.addEffectSize.summary_emm<-function(atable) {
  
  atable$etaSqP<-effectsize::F_to_eta2(atable$F.ratio,df=atable$df1,df_error = atable$df2)[,1]
  atable$omegaSq<-effectsize::F_to_omega2(atable$F.ratio,df=atable$df1,df_error = atable$df2)[,1]
  atable$epsilonSq<-effectsize::F_to_epsilon2(atable$F.ratio,df=atable$df1,df_error = atable$df2)[,1]
    
  
  return(atable)
}



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
  
      .bootstrap           <-  obj$options$cimethod=="boot"
      .coefficients        <-  as.data.frame(parameters::parameters(
                                                                   model,
                                                                   ci=obj$ciwidth,
                                                                   df_method=obj$optons$cimethod),stringAsFactors=FALSE)
     .coefficients$CI     <-  NULL
      names(.coefficients)[1:8] <-  c("source","estimate","se","ci.lower","ci.upper","t","df","p")
      if (.bootstrap) {
              bootci<-as.data.frame(parameters::parameters(model,ci=obj$ciwidth,bootstrap=TRUE),stringAsFactors=FALSE)
              .coefficients$ci.lower  <-  bootci$CI_low
              .coefficients$ci.upper  <-  bootci$CI_high
              obj$warnings<-list(topic="tab_coefficients",message="Bootstrap confidence intervals")
      }
      if (obj$option("effectSize","expb")) {
               ex            <-  as.data.frame(parameters::parameters(model,exponentiate=TRUE,bootstrap=.bootstrap))
               ex            <-  ex[,c("Coefficient","CI_low" ,"CI_high")]
               names(ex)     <-  c("expb","expb.ci.lower","expb.ci.upper")
              .coefficients  <-  cbind(.coefficients,ex)
      }
     if (obj$option("effectSize","beta")) {
            
       estim<-parameters::parameters(model,standardize="refit")
       .coefficients$beta  <-  estim$Coefficient
     }
      return(.coefficients)
}

.parameters.lmerModLmerTest<-function(model,obj) {
  
  results<-try_hard(summary(model,ddf=obj$options$dfmethod))
  
  if (!isFALSE(results$error)) {
    obj$errors<-list(topic="tab_coefficients",message=results$error)
    return()
  }
  
  obj$warnings<-list(topic="tab_coefficients",message=results$warning)
  
  .summary<-results$obj
    
  .coefficients         <-  as.data.frame(.summary$coefficients)
  names(.coefficients)  <-  c("estimate","se","df","t","p")
  .coefficients$source  <-  rownames(.coefficients)
  
  if (obj$options$showParamsCI)
    test<-try({
      method<-ifelse(obj$options$cimethod=="wald","Wald",obj$options$cimethod)
      ci   <-  confint(model,parm = "beta_",level = obj$ciwidth, method=method)
      colnames(ci)  <-  c("ci.lower","ci.upper")
     .coefficients  <-  cbind(.coefficients,ci)
    })
  if (jmvcore::isError(test)) {
    obj$warnings   <-   list(topic="tab_coefficients",message="Random effects C.I. cannot be computed")
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
          obj$warnings  <-  list(topic="tab_anova",message="Omnibus tests cannot be computed")
          return(NULL)
        }

        anoobj        <-  try_hard(car::Anova(model,test="LR",type=3,singular.ok=T))
        obj$errors    <-  list(topic="tab_anova",message=anoobj$error)
        obj$warnings  <-  list(topic="tab_anova",message=anoobj$warning)
        

        if (!isFALSE(anoobj$error))
            return(NULL)
        
        ano           <-  anoobj$obj
        colnames(ano) <-  c("test","df","p")
        as.data.frame(ano, stringsAsFactors = F)
}

.anova.multinom<-function(model,obj)
                      .anova.glm(model,obj)

.anova.polr<-function(model,obj) 
                      .anova.glm(model,obj)


.anova.lm<-function(model,obj) {

  anoobj        <-  try_hard(car::Anova(model,test="F",type=3,singular.ok=T))
  obj$errors    <-  list(topic="tab_anova",message=anoobj$error)
  obj$warnings  <-  list(topic="tab_anova",message=anoobj$warning)

  if (!isFALSE(anoobj$error))
         return(NULL)
      

  ano<-anoobj$obj[row.names(anoobj$obj)!="(Intercept)",]
  
  if (!obj$hasTerms) {

       results         <-  as.data.frame(rbind(ano,ano))
       results$source  <-  c("Residuals","Total")
       names(results)  <-  c("ss","df", "test", "p","source")
       return(results)
    
  }

  suppressMessages({
    
  w                          <-  cbind(effectsize::eta_squared(anoobj$obj,partial=F)[1:2],
                                       effectsize::eta_squared(anoobj$obj,partial = T)[2],
                                       effectsize::omega_squared(anoobj$obj)[2],
                                       effectsize::epsilon_squared(anoobj$obj)[2])

  
  })
  params                     <-  parameters::model_parameters(ano)
  results                    <-  merge(params,w,by="Parameter",all= T,sort = F)
  names(results)             <-  c("source","ss","df", "ms", "test", "p",  "etaSq", "etaSqP", "omegaSq","epsilonSq")
  tots                       <-  results[nrow(results),]
  tots$source                <-  "Total"
  tots$ss                    <-  sum(results$ss)
  tots$df                    <-  sum(results$df)
  results[nrow(results)+1,]  <-  tots
  results
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
  
  df<-obj$options$dfmethod
  results        <-  try_hard(stats::anova(model,type="3",ddf=df))
  obj$errors    <-  list(topic="tab_anova",message=results$error)
  obj$warnings  <-  list(topic="tab_anova",message=results$warning)

  if (!isFALSE(results$error))
      return()
  
  ano<-results$obj
  if (dim(ano)[1]==0) {
    obj$warnings<-list(topic="tab_anova",message="F-Tests cannot be computed without fixed effects")
    return(ano)
  }
  if (dim(ano)[2]==4) {
    ano<-.car.anova(model,df)
    obj$warnings<-list(topic="tab_anova",message="Degrees of freedom computed with method Kenward-Roger")
    
  } 
    # lmerTest 2.0-33 does not produce the F-test for continuous IV if they appear in the model before
    # the factors. Here we try to fix it.
    # now we use lmerTest>3.1 that does not seem to have this problem. Check anyway in testing
  test<-length(grep("F value",names(ano),fixed=T))>0
  if (test)
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




############# produces anova/deviance table in a somehow standard format ##########
mf.fixTable<- function(x,...) UseMethod(".fixtable")

.fixtable.default<-function(atable) {
  return(atable)
}

.fixtable.simple_lm<-function(atable) {
 
  atable$etaSqP<-as.numeric(effectsize::F_to_eta2(f = atable$test,df = atable$df1,df_error = atable$df2)[,1])
  atable$omegaSq<-as.numeric(effectsize::F_to_omega2(f = atable$test,df = atable$df1,df_error = atable$df2)[,1])
  atable$epsilonSq<-as.numeric(effectsize::F_to_omega2(f = atable$test,df = atable$df1,df_error = atable$df2)[,1])
  as.data.frame(atable)  
  
  
}


############# some models are not built in standard way, here we fix them ##########
mf.fixModel<- function(x,...) UseMethod(".fixModel")

.fixModel.default<-function(model,obj=NULL) {
  return(model)
}

.fixModel.lmerModLmerTest<-function(model,obj=NULL) {
  
  if (lme4::isSingular(model))
      obj$warnings<-list(topic="info",message=WARNS[["lmer.singular"]])

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







########### to be removed and update with mi.xxx #########

mf.aliased<- function(x,...) UseMethod(".aliased")

.aliased.default<-function(model) {
  aliased<-stats::alias(model)
  (!is.null(aliased$Complete))
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


########### set the model call for R in a reasonable way #########

mf.setModelCall<- function(x,...) UseMethod(".setModelCall")

.setModelCall.default<-function(model,info) {
  coptions<-paste(names(info$coptions),info$coptions,sep="=",collapse = ",")
  call<-paste0(info$command,"(",coptions,", data=data)")
  model$call<-call
  model
}

.setModelCall.lmerMod<-function(model,info) {
  coptions<-paste(names(info$coptions),info$coptions,sep="=",collapse = ",")
  call<-paste0(info$command,"(",coptions,", data=data)")
  model@call<-as.call(str2lang(call))
  model
}
.setModelCall.glmerMod<-function(model,info) 
        .setModelCall.lmerMod(model,info)



