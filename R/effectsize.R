
es.relativerisk<-function(obj) {

      model          <-  obj$model
      data           <-  model$data
      data$id_id_id  <- seq_len(dim(data)[1])
      depobj         <- obj$datamatic$variables[[tob64(obj$options$dep)]]
      levs           <- depobj$levels
      ciWidth        <- obj$ciwidth
      
      data[,depobj$name64]  <-  as.numeric(data[[depobj$name64]]==levs[2])

      results<-try_hard(geepack::geeglm(as.formula(obj$formula64),
                                        family = poisson(link = "log"),
                                        id = id_id_id, 
                                        corstr = "exchangeable", data = data)
      )
      obj$warnings   <-  list(topic="tab_relativerisk",message=results$warning)
      obj$errors     <-  list(topic="tab_relativerisk",message=results$error)
      if (!isFALSE(results$error))
         return()
      
      params<-as.data.frame(parameters::parameters(results$obj,exponentiate=TRUE))
      names(params)<-c("source","estimate","se","nothing", "ci.lower","ci.upper","test","df","p")
      
      if (params$source[1]=="(Intercept)")
             params <- params[-1,] 
      
    return(params)

}      

es.glm_variances<-function(model,ciwidth) {
  
  .anova<-car::Anova(model,type=3)
  .anova<-.anova[!(rownames(.anova) %in% c("(Intercept)")),]
  anovatab<-.anova
  colnames(anovatab)<-c("ss","df","f","p")
  effss<-anovatab$ss[!(rownames(anovatab) %in% c("Residuals"))]
  errss<-anovatab$ss[rownames(anovatab)=="Residuals"]
  sumr<-summary(model)
  
  ### whole model ###
  f<-sumr$fstatistic[[1]]
  edf<-sumr$fstatistic[[3]]
  mdf<-sumr$fstatistic[[2]]
  modss<-f*errss*mdf/edf
  #####
  # Here we need a correct to the computation of the effect sizes. To compute the non-partial indeces
  ## effectsize:: package uses as total sum of squares the sum of the effects SS (plus residuals)
  ## In unbalanced designs, the sum does not necessarely correspond to the model SS (plus residuals)
  ## so the estimation is biased. Eta-squared does not correspond to semi-partial r^2 any more
  ## and many properties of the non-partial indices are broken. 
  ## Thus, we fixed it by adding a bogus effect whose SS is exactly the discrepancy betweem
  ## the table SS and the model+error SS. In this way, the estimation uses the correct total SS
  #####
  diff<-modss-sum(effss)
  add<-data.frame(diff,1,1,0)
  names(add)<-names(.anova)
  .canova<-rbind(.anova,add)
  last<-length(effss)[1]+1
  eta<-effectsize::eta_squared(.canova,partial = F,ci=ciwidth,verbose=F)
  etap<-effectsize::eta_squared(.anova,partial = T,ci=ciwidth,verbose=F)
  omega<-  effectsize::omega_squared(.canova,partial = F,ci=ciwidth,verbose=F)
  omegap<-  effectsize::omega_squared(.anova,partial = T,ci=ciwidth,verbose=F)
  epsilon<-  effectsize::epsilon_squared(.canova,partial = F,ci=ciwidth,verbose=F)
  epsilonp<-  effectsize::epsilon_squared(.anova,partial = T,ci=ciwidth,verbose=F)
  alist<-list()
  for (i in seq_along(etap$Parameter)) {
    alist[[length(alist)+1]]<-list(..space..=eta[i,1],estimate=eta[i,2],ci.lower=eta[i,4],ci.upper=eta[i,5])
    alist[[length(alist)+1]]<-list(..space..=etap[i,1],estimate=etap[i,2],ci.lower=etap[i,4],ci.upper=etap[i,5])
    alist[[length(alist)+1]]<-list(..space..=omega[i,1],estimate=omega[i,2],ci.lower=omega[i,4],ci.upper=omega[i,5])
    alist[[length(alist)+1]]<-list(..space..=omegap[i,1],estimate=omegap[i,2],ci.lower=omegap[i,4],ci.upper=omegap[i,5])
    alist[[length(alist)+1]]<-list(..space..=epsilon[i,1],estimate=epsilon[i,2],ci.lower=epsilon[i,4],ci.upper=epsilon[i,5])
    alist[[length(alist)+1]]<-list(..space..=epsilonp[i,1],estimate=epsilonp[i,2],ci.lower=epsilonp[i,4],ci.upper=epsilonp[i,5])
    
  }
  
  return(alist)
}
  
  


