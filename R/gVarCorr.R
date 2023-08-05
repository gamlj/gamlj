### function to regolarize VarCorr output across models

gVarCorr<- function(model,...) UseMethod(".VarCorr") 


.VarCorr.default<-function(model,obj) {
  

  varcov<-lme4::VarCorr(model)
  attr(varcov,"useSc")<-TRUE
  attr(varcov,"sc")<-sqrt(insight::get_variance_residual(model,tolerance=0))
  varcov<-as.data.frame(varcov)
  
  vmat<-varcov[is.na(varcov$var2),]
  cmat<-varcov[!is.na(varcov$var2),]
  if (nrow(cmat)==0) cmat<-NULL
  
  if (obj$option("re_ci")) {
    
    method<-switch(obj$options$ci_method,
                   wald    = {
                            obj$warning=list(topic="main_random",message="C.I are computed with the profile method.")
                            "profile"
                            },
                   profile  ="profile",
                   quantile ="boot",
                   bcai     = {
                     obj$warning=list(topic="main_random",message="C.I are computed with the bootstrap percent method.")
                      "boot"
                   }
      
    )
    opts<-list(object=model,
               method=method,
               parm="theta_",
               oldNames=FALSE
               )  
    if (method=="boot") {
      
      opts[["nsim"]]=obj$options$boot_r
      opts[["boot.type"]]="perc"
      
    }
    if (method=="profile") {
      opts[["prof.scale"]]=c("sdcor")
    }
    
    results<-try_hard(do.call(stats::confint,opts))
    
    if (!isFALSE(results$error)) 
       obj$warning<-list(topic="main_random",message="C.I cannot be computed.")
    else {   
          x<-try_hard(
            { cidata<-as.data.frame(results$obj)
              names(cidata)  <-  c("sd.ci.lower","sd.ci.upper")
              varci<-rbind(cidata[grep("sd_",rownames(cidata)),],c(NA,NA))
              covci<-cidata[grep("cor_",rownames(cidata)),]
              vmat<-cbind(vmat,varci)
    
              if (is.something(cmat)) {
                cmat$sd.ci.lower<-covci$sd.ci.lower
                cmat$sd.ci.upper<-covci$sd.ci.upper
              }
            })
      }
    }
  ### variances

  ngrp<-vapply(obj$model@flist,nlevels,1)
  .names<-names(ngrp)
  ## icc
  int<-which(vmat$var1 %in% "(Intercept)")
  vmat$icc<-NA
  for (i in int)
    vmat$icc[i]<-vmat$vcov[i]/(vmat$vcov[i]+insight::get_variance_distribution(model,verbose = FALSE))
  
  
  info<-paste("Number of Obs:", 
              obj$model@devcomp$dims[[1]],
              ", Number of groups:",
              paste(.names,ngrp,sep=" ",collapse = ", "),
              collapse="; ")
  obj$warning<-list(topic="main_random",message=info)
  
  vmat$groups  <- fromb64(vmat$grp)
  vmat$var1    <- fromb64(vmat$var1)
  vmat$var2    <- fromb64(vmat$var2)
  
  if (is.something(cmat)) {
    cmat$groups <-fromb64(cmat$grp)
    cmat$var1   <-fromb64(cmat$var1)
    cmat$var2   <-fromb64(cmat$var2)
  }
     
  list(vmat,cmat)  
}


.VarCorr.lme<-function(model,obj) {
  
  
  varcov<-lme4::VarCorr(model)
  varcov<-as.data.frame(varcov)
  varcov$vcov<-varcov$sdcor^2
  vmat<-varcov[is.na(varcov$var2),]
  cmat<-varcov[!is.na(varcov$var2),]
  if (nrow(cmat)==0) cmat<-NULL
  
  if (obj$option("re_ci")) {
    
    method<-switch(obj$options$ci_method,
                   wald    = {
                     obj$warning=list(topic="main_random",message="C.I are computed with the profile method.")
                     "profile"
                   },
                   profile  ="profile",
                   quantile ="boot",
                   bcai     = {
                     obj$warning=list(topic="main_random",message="C.I are computed with the bootstrap percent method.")
                     "boot"
                   }
                   
    )
    opts<-list(object=model,
               method=method,
               parm="theta_",
               oldNames=FALSE
    )  
    if (method=="boot") {
      
      opts[["nsim"]]=obj$options$boot_r
      opts[["boot.type"]]="perc"
      
    }
    if (method=="profile") {
      opts[["prof.scale"]]=c("sdcor")
    }
    
    results<-try_hard(do.call(stats::confint,opts))
    
    if (!isFALSE(results$error)) 
      obj$warning<-list(topic="main_random",message="C.I cannot be computed.")
    else {   
      x<-try_hard(
        { cidata<-as.data.frame(results$obj)
        names(cidata)  <-  c("sd.ci.lower","sd.ci.upper")
        varci<-rbind(cidata[grep("sd_",rownames(cidata)),],c(NA,NA))
        covci<-cidata[grep("cor_",rownames(cidata)),]
        vmat<-cbind(vmat,varci)
        
        if (is.something(cmat)) {
          cmat$sd.ci.lower<-covci$sd.ci.lower
          cmat$sd.ci.upper<-covci$sd.ci.upper
        }
        })
    }
  }
  ### variances

  
  ngrp<-model$dims$ngrps[model$dims$qvec==1]
  .names<-names(ngrp)
  ## icc
  int<-which(vmat$var1 %in% "(Intercept)")
  vmat$icc<-NA
  for (i in int)
    vmat$icc[i]<-vmat$vcov[i]/(vmat$vcov[i]+sigma(model)^2)
  if (obj$options$res_struct=="ar1")
                vmat$phi<-as.numeric(coef(model$modelStruct$corStruct,unconstrained = FALSE))

  info<-paste("Number of Obs:", 
              model$dims$N,
              ", Number of groups:",
              paste(.names,ngrp,sep=" ",collapse = ", "),
              collapse="; ")
  obj$warning<-list(topic="main_random",message=info)
  
  vmat$groups  <- fromb64(vmat$grp)
  vmat$var1    <- fromb64(vmat$var1)
  vmat$var2    <- fromb64(vmat$var2)
  
  if (is.something(cmat)) {
    cmat$groups <-fromb64(cmat$grp)
    cmat$var1   <-fromb64(cmat$var1)
    cmat$var2   <-fromb64(cmat$var2)
  }
  
  list(vmat,cmat)  
}


.VarCorr.clmm<-function(model,obj) {

  ## ordinal::VarCov S3 method does not seem to work as expected
  ## we fix it
  
  varcov<-ordinal::VarCorr(model)
  class(varcov)<-"VarCorr.merMod"
  attr(varcov,"useSc")<-TRUE
  attr(varcov,"sc")<-sqrt(insight::get_variance_residual(model,tolerance=0))
  varcov<-as.data.frame(varcov)
  varcov$groups<-fromb64(varcov$grp)
  varcov$var1<-fromb64(varcov$var1)
  varcov$var2<-fromb64(varcov$var2)
  
  vmat<-varcov[is.na(varcov$var2),]
  cmat<-varcov[!is.na(varcov$var2),]
  if (nrow(cmat)==0) cmat<-NULL
  
  if (obj$option("re_ci"))  obj$warning<-list(topic="main_random",message="Random effects C.I. not available for ordinal mixed models")

  ## icc
  int<-which(vmat$var1 %in% "(Intercept)")
  vmat$icc<-NA
  for (i in int)
    vmat$icc[i]<-vmat$vcov[i]/(vmat$vcov[i]+insight::get_variance_distribution(model,verbose = FALSE))
  
  .names <-names(model$dims$nlev.re)
  ngrp   <- as.numeric(model$dims$nlev.re)
  info<-paste("Number of Obs:", 
              model$dims$n,
              ", Number of groups:",
              paste(.names,ngrp,sep=" ",collapse = ", "),
              collapse="; ")
  
  obj$warning<-list(topic="main_random",message=info)
  list(vmat,cmat)
  
}
  
