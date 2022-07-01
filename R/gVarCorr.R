### function to regolarize VarCorr output across models

gVarCorr<- function(model,...) UseMethod(".VarCorr") 


.VarCorr.default<-function(model,obj) {
  

  vc<-as.data.frame(lme4::VarCorr(model))
  
  if (obj$option("re_ci")) {
    method     <-  ifelse(obj$options$ci_method=="wald","Wald",obj$options$ci_method)
    results    <-  try_hard(stats::profile(obj$model,which="theta_",optimizer=obj$model@optinfo$optimizer,prof.scale=c("varcov")))
    obj$dispatcher$warnings<-list(topic="main_random",message=results$warning)
    if (isFALSE(results$error)) {
      cidata         <-  as.data.frame(confint(results$obj,parm = "theta_",level = obj$ciwidth, method=method))
      names(cidata)  <-  c("var.ci.lower","var.ci.upper")
      vc<-cbind(vc,cidata)
      obj$dispatcher$warnings<-list(topic="main_random",message="C.I. are computed with the profile method")
    }
  }
  .transnames<-c(var="vcov",std="sdcor")
  names(vc)<-transnames(names(vc),.transnames)
  
  ngrp<-vapply(obj$model@flist,nlevels,1)
  .names<-names(ngrp)
  
  
  info<-paste("Number of Obs:", 
              obj$model@devcomp$dims[[1]],
              ", Number of groups:",
              paste(.names,ngrp,sep=" ",collapse = ", "),
              collapse="; ")
  attr(vc,"info")<-info
  vc  
}

.VarCorr.clmm<-function(model,obj) {

  ## ordinal::VarCov S3 method does not seem to work as expected
  ## we fix it
  
  vc<-ordinal::VarCorr(model)
  class(vc)<-"VarCorr.merMod"
  attr(vc,"useSc")<-TRUE
  attr(vc,"sc")<-sqrt(insight::get_variance_residual(model,tolerance=0))
  vc<-as.data.frame(vc)
  .transnames<-c(var="vcov",std="sdcor")
  names(vc)<-transnames(names(vc),.transnames)
  if (obj$option("re_ci"))  obj$dispatcher$warnings<-list(topic="main_random",message="C.I. not available for ordinal mixed models")

  .names <-names(model$dims$nlev.re)
  ngrp   <- as.numeric(model$dims$nlev.re)
  info<-paste("Number of Obs:", 
              model$dims$n,
              ", Number of groups:",
              paste(.names,ngrp,sep=" ",collapse = ", "),
              collapse="; ")
  attr(vc,"info")<-info
  vc
  
}
  
